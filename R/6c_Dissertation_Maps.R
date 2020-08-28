############################# MAPS SETUP #######################################

# Load packages
pacman::p_load(gisr, baear, cartography, dplyr, fasterize, ggplot2, ggthemes,
  grid, leaflet, magick, mapview, OpenStreetMap, plotly, prettymapr, purrr,
  raster, rosm, rsvg, sf, tmap, tmaptools, viridis, units, webshot, zoo)
pacman::p_load(baear, gisr, ibmr)

options(stringsAsFactors = FALSE)
theme_update(plot.title = element_text(hjust = 0.5))

# Coordinate systems
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

# Rasters
base <- raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

# Directories
baea_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Data/BAEA"
nests_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Nests/Nests_rds")
turbine_dir <- file.path("C:/ArcGIS/Data/R_Input/BAEA")
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"

wind_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Wind")
maps_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Products/Maps"
report_maps_dir <- file.path("C:/Users/blake/Documents/PhD Program/Reports",
  "BAEA Project - 2018/Maps")

# Nests
nests_study_org <- readRDS(file.path(nests_dir, "nests_study.rds"))
nests_study <- st_as_sf(x = nests_study_org, coords = c("long", "lat"),
  crs = "+proj=longlat +datum=WGS84") %>% filter(!name %in% c("Davis", "Upper"))

# Bald Eagle Data
baea_org <- readRDS(file.path(baea_dir, "baea.rds"))
baea <- st_as_sf(x = baea_org, coords = c("long_utm", "lat_utm"),
  crs = 32619) #  crs = "+proj=longlat +datum=WGS84")

# Maine Outline
maine <- read_sf(file.path("C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp")) %>%
  st_transform(., crs = 4326) %>%
  mutate(state = "Maine")  %>%
  dplyr::select(state)

# Wind
wind_class <- read_sf(file.path(wind_dir, "Maine_Wind_High_Resolution",
  "maine_50mwind.shp")) %>%
  st_transform(crs = 32619)

# Turbines
turbines <- read_sf(file.path(turbine_dir, "wind_turbines.shp")) %>%
  st_transform(crs = 32619)

tmap_mode("plot")

## Maine Overview Map ------------------------------------------------------- ##

# Mapbox Baselayers
mapbox_url <- "https://api.mapbox.com/styles/v1/mapbox/"
mapbox_tile <- "/tiles/256/{z}/{x}/{y}"
mapbox_key <- paste0("?access_token=pk.eyJ1IjoiYmxha2VtYXNzZXkiLCJhIjoi",
  "Y2pseTYxYW56MDE4eDNwcXZxdmNtNmJ1eiJ9.cguQx1N8bIpciBnc2h3v_w")
om_type <- paste0(mapbox_url, "streets-v10", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "outdoors-v10", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "light-v9", mapbox_tile, mapbox_key)

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)


############################################################################# ##
#### -------------------------- CHAPTER 2 --------------------------------- ####
############################################################################# ##

#### ----------------------- Nests Overview Map --------------------------------

maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1.15, width = 2))

# Use "Tmap_baselayers.R" script to get other baselayers
maine_om = read_osm(maine_bb_sf, zoom = 7, minNumTiles = 9, type = om_nat_geo)

maine_bb_sf <- st_as_sfc(bb(maine %>% st_transform(., crs = crs(maine_bb_sf)),
  ext = 1.1))

maine_overview <- tm_layout(asp = .75) +
  tm_shape(maine_bb_sf) +
    tm_borders(col = NA) +
  tm_shape(maine_om) +
    tm_rgb()

nests_overview <- maine_overview +
  tm_layout(
    main.title = NULL,
    main.title.position = "center",
    main.title.size = 1.15,
    title.position = c(.65, .02),
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85, outside = FALSE,
    position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = .75, position = c(.72, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 3,
    position = c(.85, .88)) +
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_shape(maine) + tm_borders(col = "black") +  # ME outline overlays grid
  tm_shape(nests_study %>% filter(nest_site != "446R01")) +
  tm_symbols("yellow", size = .5) +
	tm_text("name", shadow = TRUE, auto.placement = TRUE, size = .75) +
  tm_xlab("") + tm_ylab("")

nests_overview

tmap_save(tm = nests_overview, filename = file.path(tex_dir, "Figures/Ch2",
  "Trapping_Sites_Overview.svg"), unit = "in", dpi = 300, height = 8, width = 6)

### ------------------------- Home Range Maps ----------------------------------

# Getting the ratio and background correct requires 3 components:
# 1) Getting enough coverage of basemap by adjusting bb() 'height'/'weight' args
# 2) Adjusting the openmap() 'zoom' if needed
# 3) Setting the tm_layout() 'asp' arg to a reasonable ratio

# Select id and year
table(baea$id, baea$year) # Determine available individual/year combos
#i <- "Norway";j  <- 2015 # for testing

homerange_akde <- readRDS(file.path("Output/Analysis/Homerange",
  "hr_akde.rds"))
baea_hr <- readRDS("Data/BAEA/baea_homerange.rds")

# Use "Tmap_baselayers.R" script to get other baselayers
maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1, width = 2))
maine_bb <- bb_poly(bb(maine_bb_sf, ext = 1.15))
maine_om = read_osm(maine_bb, zoom = 5, minNumTiles = 9, type = om_nat_geo)

# For mapping
for (i in unique(baea_hr$id)){
  baea_hr_i <- baea_hr %>% filter(id == i) %>% arrange(datetime)
  homerange_akde_i <- homerange_akde %>% filter(id == i)
  for (j in unique(baea_hr_i$year)){
    print(paste0("ID:", i, "; ", "Year:", j))
    baea_hr_k <- baea_hr_i %>% filter(year == j)
    homerange_akde_k <- homerange_akde_i %>% filter(year == j)
    akde_k <- homerange_akde_k %>% pull(hr_akde) %>% pluck(1)
    ud_95_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.95,
      level = 0.95)
    ud_95_sf_k <- st_as_sf(ud_95_sp_k) %>% slice(2)
    ud_50_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.5,
      level = 0.95)
    ud_50_sf_k <- st_as_sf(ud_50_sp_k) %>% slice(2)
    baea_sf_k <- st_as_sf(baea_hr_k, coords = c("long_utm", "lat_utm"),
      crs = 32619, agr = "constant")
    # mapview(list(ud_95_sf_k, ud_50_sf_k, baea_sf_k),
    #   zcol = list(NULL, NULL, NULL),
    #   legend = list(TRUE, FALSE, FALSE), homebutton = list(FALSE, TRUE, TRUE))

    # Filter data, create fightpaths
    baea_k <- baea_hr_k %>% st_as_sf(., coords = c("long_utm", "lat_utm"),
      crs = 32619)  %>%
      st_transform(., crs = as.character(OpenStreetMap::osm()))
    baea_k_lines <- baea_k %>% group_by(id) %>% arrange(datetime) %>%
      summarize(m = mean(year), do_union = FALSE) %>%
      st_cast("LINESTRING")

    # Get osm baselayer for baea_k
    baea_k_bb_sf <- st_as_sfc(bb(baea_k, relative = TRUE, height = 4, width =4))
    baea_k_om = read_osm(baea_k_bb_sf, minNumTiles = 21,
      type = om_nat_geo)  # may need to add and adjust 'zoom' arg
    baea_dist_sf <- st_as_sfc(bb(baea_k, relative = TRUE, height = 1, width =1))
    baea_k_x_dist <- as.numeric(approx_distances(bb(baea_dist_sf,
      ext = 1.15))[1])/1000/5
    baea_k_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0, baea_k_x_dist),
      scales::pretty_breaks(2))[1]))
    print(baea_k_x_breaks)

    # Home range, points, and flight paths
    baea_k_hr_paths <-
      tm_layout(asp = 1) +
      tm_shape(baea_k_om, raster.downsample = FALSE) +
        tm_rgb() +
      tm_shape(baea_k_lines) +
        tm_lines("#ffffff", lwd = 2, alpha = .25) +
      tm_shape(baea_k,
        bbox = bb(baea_k, ext = 1.15), is.master = TRUE) +
        tm_dots(size = 0.075, col = "#700074", alpha = .5) +
      tm_shape(ud_95_sf_k) +
        tm_polygons(col = "yellow", alpha = .15) +
      tm_shape(ud_95_sf_k) +
        tm_borders(col= "yellow", lwd = 2) +
      tm_shape(ud_50_sf_k) +
        tm_polygons(col = "red", alpha = .15) +
      tm_shape(ud_50_sf_k) +
        tm_borders(col= "red", lwd = 2) +
      tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
        main.title.position = "center",
        main.title.size = 1.15,
        title.snap.to.legend = TRUE) +
      tm_legend(title.size = 1, text.size = .85,
        outside = TRUE, position = c("right", "bottom")) +
      tm_scale_bar(text.size = .75,
        breaks = baea_k_x_breaks,
        position = c(.05, .01)) +
      tm_compass(type = "4star",  show.labels = 1, size = 2.5,
        position = c(.875, .875)) +
      tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "black", alpha = 1,
        ticks = TRUE, lines = FALSE, labels.col = "grey25",
        labels.format = list(format = "f", big.mark = ""),
        labels.inside.frame = FALSE) +
      tm_xlab("") + tm_ylab("")

    #baea_k_hr_paths
    # Maine Overview Map
    baea_k_bb = gisr::CreateMapExtentBB(baea_k, asp = 1, ext = 1.15)
    maine_overview <-
      tm_shape(maine_om, raster.downsample = FALSE) +
        tm_rgb() +
      tm_shape(maine) + # setting this as master sets lat/long
        tm_borders(col = "black") +
      tm_shape(baea_k_bb) +
        tm_borders(col = "red")

    # Export to TEMP Folder
    tmap_save(tm = baea_k_hr_paths, filename = file.path("C:/Temp",
      paste0(i, "_", j, ".svg")),
      insets_tm = maine_overview,
      insets_vp =  viewport(x = 0.881, y = 0.147, width = 0.2, height = 0.2),
      unit = "in", dpi = 300, height = 6, width = 6)

    # Export to LaTeX Folder
    tmap_save(tm = baea_k_hr_paths, filename = file.path(tex_dir,
      "Figures/Ch2/HR_Maps", paste0(i, "_", j, ".svg")),
      insets_tm = maine_overview,
      insets_vp =  viewport(x = 0.881, y = 0.147, width = 0.2, height = 0.2),
      unit = "in", dpi = 300, height = 6, width = 6)
  }
}

#### ----------- Con and Home Nest Distance Map --------------------------------

## Import Base
base <- raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))
nests_active <- readRDS(file="Data/Nests/Nests_rds/nests_active.RDS")
baea_terr <- readRDS("Data/BAEA/baea_terr.rds")
baea_dist <- readRDS("Data/BAEA/baea_dist.rds")
con_nest <- raster("Output/Analysis/Territorial/ConNest_All.tif")
con_nest_km <- con_nest/1000

nests_2016_sf <- nests_active %>%
  filter(active_2016 == TRUE) %>%
  transmute(long = long_utm, lat = lat_utm, nest_site) %>%
  st_as_sf(x = ., coords = c("long", "lat"),
  crs = wgs84n19)

nests_home_sf <- baea_terr %>% group_by(id) %>% slice(1) %>%
  dplyr::select(nest_long_utm, nest_lat_utm, nest_site)  %>%
  transmute(long = nest_long_utm, lat = nest_lat_utm, nest_site = nest_site) %>%
  st_as_sf(x = ., coords = c("long", "lat"),
  crs = wgs84n19) #%>% filter(!name %in% c("Davis", "Upper"))

nests_home_buff <- st_buffer(nests_home_sf, 30000)

con_nests_list <- st_contains(nests_home_buff, nests_2016_sf)
con_nests_sf <- nests_2016_sf %>% dplyr::slice(unlist(con_nests_list)) %>%
  filter(!nest_site %in% nests_home_sf$nest_site)

# Get osm baselayer

con_nest_bb_sf <- st_as_sfc(bb(con_nest, relative = TRUE, height = 2.5,
  width = 1.25)) # Gets the extent to download - should be larger than final map
con_nest_om <- read_osm(con_nest_bb_sf, zoom = 8, minNumTiles = 21,
  type = om_nat_geo)

# Conspecific Nest Map

nests_home_ext_sf <- st_as_sfc(bb(nests_home_sf %>%
  st_transform(., crs = crs(con_nest_om)), ext = 1.25))
  # sets the extent of the map - should be smaller than *_om raster

con_nest_map <-
  tm_layout(asp = 1) +
  tm_shape(nests_home_ext_sf, is.master = TRUE) +
    tm_fill(col = NA) +
  tm_shape(con_nest_om, raster.downsample = FALSE) +
    tm_rgb() +
 tm_shape(con_nest_km) +
  tm_raster("ConNest_All", palette = "-plasma", alpha = .8, style = "cont",
    title = "Conspecific and\n Home Nest Site\nDistance Metric (km)",
    legend.show = TRUE) +
  tm_shape(con_nests_sf) +
    tm_symbols(col = "grey20", shape = 4,  border.lwd = 2,  size = .2) +
    #tm_symbols(col = "yellow", border.lwd = 2,  size = .25) +
  tm_shape(nests_home_sf) +
    tm_symbols(col = "white", border.lwd = 2,  size = .25) +
    #tm_bubbles(col = "white", border.lwd = 2,  size = .2) +
  tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
    main.title.position = "center",
    main.title.size = 1.15,
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85,
    outside = FALSE, position = c("right", "bottom"), frame = TRUE,
    legend.bg.color = "white", legend.format = list(format = "f",
    big.mark = "")) +
  tm_compass(type = "4star", show.labels = 1, size = 3,
    position = c(.87, .87)) +
  tm_scale_bar(text.size = .75, breaks = c(0, 50, 100), position = c(.4, .01)) +
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = 0,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_xlab("") + tm_ylab("")
con_nest_map

tmap_save(tm = con_nest_map, filename = file.path(tex_dir, "Figures/Ch2",
  "Con_Nest_Distance_Map.svg"), unit = "in", dpi = 300, height = 6, width = 6.1)

#### ------------------ SSF Maps (WORK IN PROGRESS) ----------------------------

# Directories and files
mod_fit_dir = "Output/Analysis/SSF/Models"
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"
covars_crop_dir = file.path(ssf_raster_dir, "Covars_Crop")
step_type_dir = file.path(ssf_raster_dir, "Step_Type")
maine_raster_trim_file = "C:/ArcGIS/Data/BlankRaster/maine_trim.tif"

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

ssf_layers <- list.files(file.path(ssf_raster_dir, "Step_Types_Rescale_Prob"),
  pattern = "tif$")
nests <- ssf_layers %>%
  str_split(., pattern = "_") %>%
  sapply("[", 1) %>%
  unique(.)

i <- nests[1] # FOR TESTING
for (i in nests){
  nest_ssfs_i <- str_subset(ssf_layers, i)
  for (j in nest_ssfs_i){
    j <- nest_ssfs_i[1] # FOR TESTING
    ssf_i_j <- read_stars(file.path(ssf_raster_dir, "Step_Types_Rescale_Prob",
      j))
    if (j ==  nest_i_rasters[1]){
      bb_nest_i <- st_as_sf(ssf_i_j)
      bb_nest_i_om = read_osm(bb_nest_i, type = om_nat_geo)
      #zoom = 13, minNumTiles = 21,
    }

    # NEED TO ADD TO MAP BELOW (Compass, Scales, etc.)

    tm_shape(bb_nest_i_om, raster.downsample = FALSE) +
      tm_rgb() +
    tm_shape(ssf_i_j, raster.downsample = FALSE) +
      tm_raster(n = 20)

    # NEED TO COMBINE MAPS (either by step_type or by est

  }
}

############################################################################# ##
#### -------------------------- CHAPTER 4 --------------------------------- ####
############################################################################# ##

#### ------------------------- Wilson Scenarios Map ----------------------------

pacman::p_load(units, stringr)

# Filter data, create fightpaths
wilson <- nests_study %>% filter(name == "Wilson")  %>%
  st_transform(crs = 32619)

wilson_map_center <- wilson
sfc <- st_sfc(st_point(c(st_coordinates(wilson)[1] + 300,
  st_coordinates(wilson)[2] - 750)))
st_geometry(wilson_map_center) <- sfc
st_crs(wilson_map_center) <- 32619

wilson_bb <- st_buffer(wilson_map_center, 4000) %>% bb(.)
mapview(wilson_bb)

wind_wilson_n <- st_crop(wind_class, wilson_bb) %>% filter(WPC >= 3) %>%
  filter(ID %in% c(7139, 7172, 7173, 7201, 7236, 7261, 7262, 7263, 7292))

wind_wilson_s_all <- st_crop(wind_class, wilson_bb) %>% filter(WPC >= 3) %>%
  filter(ID %in% c(7626, 7659, 7660, 7662, 7701, 7702, 7762, 7778))
wind_7662 <- wind_class %>% filter(ID == 7662)
wind_7662_grid <- st_make_grid(wind_7662, 200)
wind_7662_grid_sub <- wind_7662_grid[c(8:12, 15:19)]
wind_7662_crop <- st_crop(wind_7662, wind_7662_grid_sub)
wind_wilson_s <- rbind(wind_wilson_s_all %>% filter(ID != 7662), wind_7662_crop)

wind_wilson_n_union <- st_union(wind_wilson_n)
set_units(wind_wilson_n_union %>% st_area(.), km^2)
wind_wilson_s_union <- st_union(wind_wilson_s)
set_units(wind_wilson_s_union %>% st_area(.), km^2)

# TEST SECTION (For placement of turbines in cells) ----------------------------

wind_wilson_n_grid <- sf::st_make_grid(bb(wind_wilson_n), 30) %>%
  st_cast("POLYGON")
# find Maine Raster cell corner closest to grid polygons?

in_footprint <- lengths(st_intersects(wind_wilson_n_grid, wind_wilson_n)) > 0

wind_wilson_n_rast <- st_sf(in_footprint = in_footprint, wind_wilson_n_grid) %>%
  filter(in_footprint == TRUE)

wind_wilson_n_rast

mapview(wind_wilson_n_rast)

ggplot() +
  geom_sf(aes(color = in_footprint), data = wind_wilson_n_rast)
  geom_sf(data = wind_wilson_n)

st_write(wind_wilson_n_rast, "Wilson_N_Grid.kml", driver='kml', update=TRUE)

wind_wilson_n
test %>% head()
library(tidyverse)
x = st_sf(a = "TEST", geom = test)
test2 <- x %>% slice(1:10)
test2 <- test %>% st_as_sf(.)

ggplot() +
  geom_sf(data = wind_wilson_n)+
  geom_sf(data = test2)


# END TEST SECTION -------------------------------------------------------------

wind_wilson_ns <- rbind(wind_wilson_n, wind_wilson_s)

# Get osm baselayer for wilson
wilson_bb_sf <- st_as_sfc(bb(wilson_bb, relative = TRUE, height = 1,
  width = 1))
wilson_om <- read_osm(wilson_bb_sf, zoom = 13, minNumTiles = 21,
  type = om_nat_geo)  # may need to add/adjust 'zoom'

wind_wilson_ns <- wind_wilson_ns %>%
  mutate(Rating = as.character(WPC)) %>%
  mutate(Rating = str_replace_all(Rating, "4", "(Good)")) %>%
  mutate(Rating = str_replace_all(Rating, "5", "(Excellent)")) %>%
  mutate(Rating = str_replace_all(Rating, "6", "(Outstanding)")) %>%
  mutate("Wind Power Class" = paste(WPC, Rating))

# Wilson Map
wilson_map <-
  tm_layout(asp = 1) +
  tm_shape(wilson_bb_sf, is.master = TRUE) +
    tm_fill(col = NA) +
  tm_shape(wilson_om) +
    tm_rgb() +
  tm_shape(wilson, title = "Wilson Nest") +
    tm_bubbles(col = "yellow",  border.lwd = 3,  size = .75) +
  tm_shape(wind_wilson_ns) +
    tm_fill("Wind Power Class", lwd = 2, alpha = .5,
      style = "cat", palette = "YlOrBr") + # brewer.pal(5, "RdGy")[3]
    tm_borders("black", lwd = .5) +
  tm_shape(wind_wilson_n_union) +
    tm_borders("black", lwd = 3) +
  tm_shape(wind_wilson_s_union) +
    tm_borders("black", lwd = 3) +
  tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
    main.title.position = "center",
    main.title.size = 1.15,
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85,
    outside = FALSE, position = c("center", "bottom")) +
  tm_scale_bar(size = .75, width = .2, breaks = c(0, 1, 2),
    position = c(.05, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    position = c(.85, .87)) +
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_xlab("") + tm_ylab("")
wilson_map
#tmaptools::palette_explorer()

# wilson Overview Map
wilson_overview_center <- wilson
sfc <- st_sfc(st_point(c(st_coordinates(wilson)[1] - 5000,
  st_coordinates(wilson)[2] - 70000)))
st_geometry(wilson_overview_center) <- sfc
st_crs(wilson_overview_center) <- 32619

wilson_overview_buff <- st_buffer(wilson_overview_center, 130000) %>% bb(.)
mapview(wilson_overview_buff)
wilson_overview_bb <- bb_poly(bb(wilson_overview_buff, ext = 1))
om_type <- paste0(esri_url, "NatGeo_World_Map", esri_tile)
wilson_overview_bb_om = read_osm(wilson_overview_bb, zoom = 6, minNumTiles = 21,
  type = om_nat_geo)

wilson_overview <-
  tm_shape(wilson_overview_bb_om, is.master = TRUE) +
    tm_rgb() +
  tm_shape(wilson_bb_sf) +
    tm_borders(col = "red", lwd = 3) +
  tm_scale_bar(size = .75, width = .2, breaks = c(0, 50, 100),
    position = c(.52, -.03))
wilson_overview

tmap_save(tm = wilson_map, filename = file.path(maps_dir, "Wilson_Buildout",
  "wilson_map.svg"), insets_tm = wilson_overview,
  insets_vp =  viewport(x = 0.85, y = 0.167, width = 0.25, height = 0.25),
  unit = "in", dpi = 300, height = 6, width = 6.1)

#### ------------------- Ellis Turbine Distance Map ----------------------------

pacman::p_load(units, stringr)

# Rasters
base <- raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

# Filter nest data
ellis <- nests_study %>% filter(name == "Ellis")  %>%
  st_transform(crs = 32619)

ellis_map_center <- ellis
sfc <- st_sfc(st_point(c(st_coordinates(ellis)[1] + 2250,
  st_coordinates(ellis)[2] - 700)))
st_geometry(ellis_map_center) <- sfc
st_crs(ellis_map_center) <- 32619

ellis_bb <- st_buffer(ellis_map_center, 5500) %>% bb(.)
mapview(ellis_bb)

wt_ellis <- st_crop(turbines, ellis_bb)
mapview(wt_ellis)

# Get osm baselayer for ellis
ellis_bb_sf <- st_as_sfc(bb(ellis_bb, relative = TRUE, height = 1,
  width = 1))
ellis_down <- read_osm(ellis_bb_sf, zoom = 13, minNumTiles = 21,
  type = om_nat_geo)  # may need to add/adjust 'zoom'
ellis_om <- RasterizeOMDownload(ellis_down)

# Ellis Map Raster
ellis_raster <- crop(base, as_Spatial(ellis_bb_sf))
wt_dist <- distanceFromPoints(ellis_raster, wt_ellis)
wt_dist[wt_dist > 2000] = NA
wt_dist_shift <- shift(wt_dist, 50000, 0)

ellis_ext_sf <- st_as_sfc(bb(st_buffer(ellis_map_center, 5500) %>%
  st_transform(., crs = crs(ellis_om)), ext = .95))

ellis_map <-
  tm_layout(asp = 1) +
  tm_shape(ellis_ext_sf) +
    tm_fill(col = NA) +
  tm_shape(ellis_om) +
    tm_rgb() +
  tm_shape(ellis, title = "Ellis Nest") +
    tm_bubbles(col = "yellow", border.lwd = 3,  size = .75) +
  tm_shape(wt_dist) +
    tm_raster("layer", palette = "-plasma", alpha = .6, style = "cont",
     legend.show = FALSE)  +
  tm_shape(wt_dist_shift) +
    tm_raster("layer", palette = "-plasma", alpha = 1, style = "cont",
      breaks = c(0, 500, 1000, 1500, 2000),
      title = "Turbine Distance (m)", legend.show = TRUE)  +
  tm_shape(turbines, title = "Wind Turbines") +
    tm_symbols(col = "black", shape = 4,  border.lwd = 2,  size = .25) +
  tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
    main.title.position = "center",
    main.title.size = 1.15,
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85,
    outside = FALSE, position = c("left", "top"), frame = TRUE,
    legend.bg.color = "white", legend.format = list(format = "f",
    big.mark = "")) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    position = c(.85, .87)) +
  tm_scale_bar(text.size = .75, breaks = c(0, 1, 2), position = c(.05, .01)) +
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_xlab("") + tm_ylab("")
ellis_map
#tmaptools::palette_explorer()

# ellis Overview Map
ellis_overview_center <- ellis
sfc <- st_sfc(st_point(c(st_coordinates(ellis)[1] + 50000,
  st_coordinates(ellis)[2] - 0)))
st_geometry(ellis_overview_center) <- sfc
st_crs(ellis_overview_center) <- 32619

ellis_overview_buff <- st_buffer(ellis_overview_center, 120000) %>% bb(.)
mapview(ellis_overview_buff)
ellis_overview_bb <- bb_poly(bb(ellis_overview_buff, ext = 1))
ellis_overview_bb_om <- read_osm(ellis_overview_bb, zoom = 6, minNumTiles = 21,
  type = om_type)

ellis_overview <-
  tm_shape(ellis_overview_bb_om, is.master = TRUE) +
    tm_rgb() +
  tm_shape(ellis_bb_sf) +
    tm_borders(col = "red", lwd = 3) +
  tm_scale_bar(size = .75, width = .2, breaks = c(0, 50, 100),
    position = c(.45, -.03))
ellis_overview

tmap_save(tm = ellis_map, filename = file.path(maps_dir, "Ellis_Turbines",
  "ellis_map.svg"), insets_tm = ellis_overview,
  insets_vp =  viewport(x = 0.85, y = 0.167, width = 0.25, height = 0.25),
  unit = "in", dpi = 300, height = 6, width = 6.1)

# Ellis Map Polygons (Not currently used, but may be useful)

wt_buff_400 <- st_buffer(wt_ellis, c(400)) %>% st_union(.)
wt_buff_600 <- st_buffer(wt_ellis, c(600)) %>% st_union(.)
wt_buff_800 <- st_buffer(wt_ellis, c(800)) %>% st_union(.)
wt_buff_1000 <- st_buffer(wt_ellis, c(1000)) %>% st_union(.)

wt_buffs_200 <- st_buffer(wt_ellis, c(200)) %>% st_union(.)
wt_buffs_400 <- st_sym_difference(wt_buff_400, wt_buff_200)
wt_buffs_600 <- st_sym_difference(wt_buff_600, wt_buff_400)
wt_buffs_800 <- st_sym_difference(wt_buff_800, wt_buff_600)
wt_buffs_1000 <- st_sym_difference(wt_buff_1000, wt_buff_800)

ellis_map_polys <-
  tm_layout(asp = 1) +
  tm_shape(ellis_bb_sf, is.master = TRUE) +
    tm_fill(col = NA) +
  tm_shape(ellis_om) +
    tm_rgb() +
  tm_shape(ellis, title = "Ellis Nest") +
    tm_bubbles(col = "yellow", border.lwd = 3,  size = .75) +
  tm_shape(wt_buffs_1000, title = "Wind Turbine Buffers") +
     tm_polygons(col = viridis(10, option = v_col)[3], alpha = v_alpha,
       border.lwd = 3) +
  tm_shape(wt_buffs_800, title = "Wind Turbine Buffers") +
     tm_polygons(col = viridis(10, option = v_col)[4], alpha = v_alpha,
       border.lwd = 3) +
  tm_shape(wt_buffs_600, title = "Wind Turbine buffers") +
     tm_polygons(col = viridis(10, option = v_col)[5], alpha = v_alpha,
       border.lwd = 3) +
  tm_shape(wt_buffs_400, title = "Wind Turbine Buffers") +
     tm_polygons(col = viridis(10, option = v_col)[6], alpha = v_alpha,
       border.lwd = 3) +
  tm_shape(wt_buffs_200, title = "Wind Turbine Buffers") +
     tm_polygons(col = viridis(10, option = v_col)[7], alpha = v_alpha,
       border.lwd = 3) +
  tm_shape(turbines, title = "Wind Turbines") +
    tm_symbols(col = "black", shape = 4,  border.lwd = 2,  size = .25) +
  tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
    main.title.position = "center",
    main.title.size = 1.15,
    title.snap.to.legend = TRUE) +
  tm_legend(legend.show = FALSE, title.size = 1, text.size = .85,
    outside = FALSE, position = c("left", "top"), frame = TRUE,
    legend.bg.color = "white", legend.format = list(format = "f",
    big.mark = "")) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    position = c(.85, .87)) +
  tm_scale_bar(size = .75, width = .2, breaks = c(0, 1, 2),
    position = c(.05, .01)) +
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_xlab("") + tm_ylab("")
ellis_map_polys




# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #

# ### Flightpath Maps ------------------------------------------------------------
#
# id_i = "Sandy"
# year_i = 2019
#
# # Filter data, create fightpaths
# baea_i <- baea %>% filter(id == id_i) %>% filter(year == year_i) %>%
#   st_transform(., crs = as.character(OpenStreetMap::osm()))
#
# baea_i_lines <- baea_i %>%
#   group_by(id) %>%
#   arrange(datetime) %>%
#   summarize(m = mean(year), do_union = FALSE) %>%
#   st_cast("LINESTRING")
#
# # Get osm baselayer for baea_i
# baea_i_bb_sf <- st_as_sfc(bb(baea_i, relative = TRUE, height = 3,
#   width = 2))
# baea_i_om = read_osm(baea_i_bb_sf, minNumTiles = 21, type = om_nat_geo)
#   # may need to add and adjust 'zoom' arg
#
# baea_i_x_dist <- as.numeric(approx_distances(bb(baea_i, ext = 1.15))[1])/1000/5
# baea_i_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0, baea_i_x_dist),
#   scales::pretty_breaks(3))[1]))
#
# # All flight paths and points
# baea_i_paths <-
#   tm_layout(asp = 1) +
#   tm_shape(baea_i_om) +
#     tm_rgb() +
#   tm_shape(baea_i_lines) +
#     tm_lines("#FFFF33", lwd = 2, alpha = .5) + # brewer.pal(9, "Set1")[6]
#   tm_shape(baea_i,
#     bbox = bb(baea_i, ext = 1.15), is.master = TRUE) +
#     tm_dots(size = 0.075, col = "#984EA3") +  # brewer.pal(9, "Set1")[4]
#   tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
#     main.title.position = "center",
#     main.title.size = 1.15,
#     title.snap.to.legend = TRUE) +
#   tm_legend(title.size = 1, text.size = .85,
#     outside = TRUE, position = c("right", "bottom")) +
#   tm_scale_bar(text.size = .75, breaks = baea_i_x_breaks,
#     position = c(.05, .01)) +
#   tm_compass(type = "4star",  show.labels = 1, size = 2.5,
#     position = c(.875, .875)) +
#   tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
#     labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
#     labels.inside.frame = FALSE) +
#   tm_xlab("") + tm_ylab("")
# baea_i_paths
#
# baea_i_bb = gisr::CreateMapExtentBB(baea_i, asp = 1, ext = 1.15)
#
# # Use "Tmap_baselayers.R" script to get other baselayers
# maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1,
#   width = 2))
# maine_baea_i <- sf::st_union(x = maine_bb_sf, y = baea_i_bb %>%
#   st_transform(st_crs(maine_bb_sf)))
# maine_i_bb <- bb_poly(bb(maine_baea_i, ext = 1.15))
# maine_i_om = read_osm(maine_i_bb_, zoom = 5, minNumTiles = 9, type = om_type)
#
# maine_i_overview <-
#   tm_shape(maine_i_om) +
#     tm_rgb() +
#   tm_shape(maine) + # setting this as master sets lat/long
#     tm_borders(col = "black") +
#   tm_shape(baea_i_bb) +
#     tm_borders(col = "red")
# maine_i_overview
#
# tmap_save(tm = baea_i_paths, filename = file.path(maps_dir, "Individuals",
#   paste0(year_i, "_", id_i, ".png")), insets_tm = maine_i_overview,
#   insets_vp =  viewport(x = 0.88, y = 0.152, width = 0.2, height = 0.2),
#   unit = "in", dpi = 300, height = 6, width = 6)
#
# # Create 2d kernel density data - isopleth lines, polygons, and rasters
# baea_i_smooth <- smooth_map(baea_i, cover = as(CreateExtentSF(baea_i, 1),
#   "Spatial"), nlevels = 10)
#
# ### Isopleth Maps --------------------------------------------------------------
#
# # Drop lowest density polygon
# baea_i_smooth_polys <- st_intersection(baea_i_smooth$polygons,
#   baea_i_smooth$polygons %>% arrange(level) %>% slice(-1))
#
# mapview(baea_i_smooth_polys)
#
# # Isolate highest density polygon
# baea_i_smooth_poly1 <- st_intersection(baea_i_smooth$polygons,
#   baea_i_smooth$polygons %>% arrange(rev(level)) %>% slice(1))
#
# # Download om for polys_i
# polys_i_bb_sf <- st_as_sfc(bb(baea_i_smooth_polys, relative = TRUE,
#   height = 1.15, width = 1.15))
# polys_i_om = read_osm(polys_i_bb_sf, minNumTiles = 21,
#   type = om_type)
# polys_i_om <- RasterizeOsMDownload(polys_i_down)
#
# # Download om for poly1_i
# poly1_i_bb_sf <- st_as_sfc(bb(baea_i_smooth_poly1, relative = TRUE, height = 2,
#   width = 2))
# poly1_i_om = read_osm(poly1_i_bb_sf, minNumTiles = 21, type = om_type)
#
# # All but lowest density isopleth
# tm_shape(polys_i_om) +
#   tm_raster() +
# tm_shape(baea_i_smooth$raster) +
#   tm_raster("count", alpha = .5) +
# tm_shape(baea_i_smooth$iso) +
#   tm_iso("black", size = .5, fontcolor="black")
#
# # Highest density isopleth
# tm_shape(poly1_i_om) +
#   tm_rgb() +
# tm_shape(baea_i_smooth_polys) +
#   tm_fill("level", alpha = .5) +
# tm_borders()
#
#
# ### ------------------------- Hexbin Maps --------------------------------------
#
# # Select id and year
# table(baea$id, baea$year) # Determine available individual/year combos
# id_i <- "Ellis"
# year_i <- 2018
#
# # Filter data, create fightpaths
# baea_i <- baea %>% filter(id == id_i) %>% filter(year == year_i)
#
# baea_i_hexgrid <- st_make_grid(bb_poly(baea_i), cellsize = 9000, square = FALSE)
# plot(baea_i_hexgrid)
# baea_i_hexs = aggregate(baea_i %>% transmute(pt = 1), baea_i_hexgrid, sum) %>%
#   filter(pt > 0)
# plot(baea_i_hexs)
# mapview(baea_i_hexs)
#
# # Download om for polys_i
# hexs_i_bb_sf <- CreateMapExtentBB(baea_i_hexs, ext = 1.15, asp = 1)
# hexs_i_om <- read_osm(hexs_i_bb_sf, minNumTiles = 21, type = om_type)
#
# baea_i_x_dist <- as.numeric(approx_distances(bb(baea_i, ext = 1.15))[1])/1000/5
# baea_i_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0, baea_i_x_dist),
#   scales::pretty_breaks(3))[1]))
#
# baea_i_hexbins <-
#   tm_layout(asp = 1) +
#   tm_shape(hexs_i_om) +
#     tm_rgb() +
# #  tm_shape(baea_i) +
# #    tm_dots(size = 0.075, col = "black") +
#   tm_shape(baea_i_lines) +
#     tm_lines("yellow", lwd = 2, alpha = .5) +
#   tm_shape(baea_i_hexs,
#       bbox = bb(baea_i, ext = 1.15), is.master = TRUE) +
#     tm_fill(col = 'pt', alpha = .5, palette = viridis(5, option = "D")) +
#     tm_borders(col = "black") +
#   tm_layout(main.title = paste0("Hexbins: ", id_i),
#     main.title.position = "center",
#     main.title.size = 1.15,
#     title.snap.to.legend = TRUE) +
#   tm_legend(title.size = 1, text.size = .85,
#     outside = FALSE, position = c("right", "bottom")) +
#   tm_scale_bar(size = .75, width = .2,
#     breaks = baea_i_x_breaks,
#     position = c(.05, .01)) +
#   tm_compass(type = "4star",  show.labels = 1, size = 2.5,
#     position = c(.875, .875)) +
#   tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
#     labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
#     labels.inside.frame = FALSE) +
#   tm_xlab("") + tm_ylab("")
# baea_i_hexbins

#
# baea_i, asp = 4, ext = 1.15
# sf_object = baea_i
# ext = 1.15
# asp = .5
#
# CreateMapExtentBB2 <- function(sf_object,
#                               ext = 1.15,
#                               asp = 1){
#   map_asp <- asp
#   sf_asp <- tmaptools::get_asp_ratio(sf_object, ext = 1.15)
# #  bb_point <- sf::st_centroid(sf::st_as_sfc(tmaptools::bb(sf_object, ext =ext)))
# #  bb_x <- sf::st_coordinates(bb_point)[1]
# #  bb_y <- sf::st_coordinates(bb_point)[2]
# #  bb_dists <- tmaptools::approx_distances(tmaptools::bb(sf_object, ext = ext),
# #    projection = sf::st_crs(sf_object))
# #  bb_width <- as.numeric(bb_dists)[1]
# #  bb_height <- as.numeric(bb_dists)[2]
# #  bb_asp <- bb_width / bb_height
#   if (sf_asp >= map_asp){
#     bb_width = ext     # width should not be reduced
#     bb_height = (sf_asp/map_asp)*ext
#   } else {
#     bb_width = (map_asp/sf_asp)*ext
#     bb_height = ext # height should not be reducted
#   }
#   map_bb <- sf::st_as_sfc(tmaptools::bb(sf_object, relative = TRUE,
#     width = bb_width, height = bb_height))
#   sf::st_crs(map_bb) <- sf::st_crs(sf_object)
#   return(map_bb)
# }
# mapview(map_bb) + mapview(baea_i_lines)
#
# maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1.15, width = 2))
# maine_bb_ext <- CreateOSMBaseBB(maine_bb_sf, type = "om_type")
#

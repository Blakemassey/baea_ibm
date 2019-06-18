# Load packages
pacman::p_load(gisr, baear, cartography, dplyr, grid, leaflet,magick, mapview,
  OpenStreetMap, plotly, prettymapr, purrr, raster, rosm, rsvg, sf, tmap,
  tmaptools, viridis, webshot)

# Rasters
base <- raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

# Directories
baea_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Data/BAEA"
nests_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Nests/Nests_rds")
wind_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Wind")
maps_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Products/Maps"
report_maps_dir <- file.path("C:/Users/blake/Documents/PhD Program/Reports",
  "BAEA Project - 2018/Maps")

# Nests
nests_study_org <- readRDS(file.path(nests_dir, "nests_study.rds"))
nests_study <- st_as_sf(x = nests_study_org, coords = c("long", "lat"),
  crs = "+proj=longlat +datum=WGS84") %>% filter(!name  %in% c("Davis", "Upper"))

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

## ----------------------------- CREATE MAPS -------------------------------- ##

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
om_type <- paste0(esri_url, "NatGeo_World_Map", esri_tile)


#### ----------------------- NESTS OVERVIEW MAPS -------------------------- ####

maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1.15, width = 2))
maine_bb_ext <- CreateOSMBaseBB(maine_bb_sf, type = "om_type")

# Use "Tmap_baselayers.R" script to get other baselayers
maine_down = OpenStreetMap::openmap(maine_bb_ext[[1]], maine_bb_ext[[2]],
  minNumTiles = 9, type = om_type)
maine_om <- RasterizeOMDownload(maine_down)

maine_overview <-  tm_layout(asp = .75) +
  tm_shape(maine_om) +
    tm_rgb() +
  tm_shape(maine,
    bbox = bb(maine, ext = 1.15), is.master = TRUE) + # this sets map crs
    tm_borders(col = "black")
maine_overview

nests_overview <- maine_overview +
  tm_layout(
    main.title = NULL,
    main.title.position = "center",
    main.title.size = 1.15,
    title.position = c(.65, .02),
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85, outside = FALSE,
    position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), size = .75, position = c(.68, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 3,
    position = c(.85, .88))+
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_shape(maine) + tm_borders(col = "black") +  # ME outline overlays grid
  tm_shape(nests_study %>% filter(nest_site != "446R01")) +
  tm_symbols("yellow", size = .5) +
	tm_text("name", shadow = TRUE, auto.placement = TRUE, size = .75) +
  tm_xlab("") + tm_ylab("")
nests_overview

tmap_save(tm = nests_overview, filename = file.path(maps_dir, "Trapping_Sites",
  "Trapping_Sites_Overview.svg"), unit = "in", dpi = 300, height = 8, width = 6)

#### ---------------------- BAEA INDIVIDUAL MAPS -------------------------- ####

# Getting the ratio and background correct requires 3 components:
# 1) Getting enough coverage of basemap by adjusting bb() 'height'/'weight' args
# 2) Adjusting the openmap() 'zoom' if needed
# 3) Setting the tm_layout() 'asp' arg to a reasonable ratio

# Select id and year
table(baea$id, baea$year) # Determine available individual/year combos
id_i <- "Sandy"
year_i <- 2018

### Flightpath Maps ------------------------------------------------------------

# Filter data, create fightpaths
baea_i <- baea %>% filter(id == id_i) %>% filter(year == year_i) %>%
  st_transform(., crs = as.character(OpenStreetMap::osm()))

baea_i_lines <- baea_i %>%
  group_by(id) %>%
  arrange(datetime) %>%
  summarize(m = mean(year), do_union = FALSE) %>%
  st_cast("LINESTRING")

# Get osm baselayer for baea_i
baea_i_bb_sf <- st_as_sfc(bb(baea_i, relative = TRUE, height = 3,
  width = 2))
baea_i_bb_ext <- CreateOSMBaseBB(baea_i_bb_sf, type = "om_type")
baea_i_down = OpenStreetMap::openmap(baea_i_bb_ext[[1]], baea_i_bb_ext[[2]],
  minNumTiles = 21, type = om_type)  # may need to add and adjust 'zoom' arg
baea_i_om <- RasterizeOMDownload(baea_i_down)

baea_i_x_dist <- as.numeric(approx_distances(bb(baea_i, ext = 1.15))[1])/1000/5
baea_i_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0, baea_i_x_dist),
  scales::pretty_breaks(3))[1]))

# All flight paths and points
baea_i_paths <-
  tm_layout(asp = 1) +
  tm_shape(baea_i_om) +
    tm_rgb() +
  tm_shape(baea_i_lines) +
    tm_lines("#ffffff", lwd = 2, alpha = .5) + # brewer.pal(5, "RdGy")[3]
  tm_shape(baea_i,
    bbox = bb(baea_i, ext = 1.15), is.master = TRUE) +
    tm_dots(size = 0.075, col = "#404040") +   # brewer.pal(5, "RdGy")[5]
  tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
    main.title.position = "center",
    main.title.size = 1.15,
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85,
    outside = TRUE, position = c("right", "bottom")) +
  tm_scale_bar(size = .75, width = .2,
    breaks = baea_i_x_breaks,
    position = c(.05, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    position = c(.875, .875)) +
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_xlab("") + tm_ylab("")
baea_i_paths

baea_i_bb = gisr::CreateMapExtentBB(baea_i, asp = 1, ext = 1.15)

# Use "Tmap_baselayers.R" script to get other baselayers
maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1,
  width = 2))
maine_baea_i <- sf::st_union(x = maine_bb_sf, y = baea_i_bb %>%
  st_transform(st_crs(maine_bb_sf)))
maine_i_bb <- bb_poly(bb(maine_baea_i, ext = 1.15))
maine_i_bb_ext <- CreateOSMBaseBB(maine_i_bb, type = "om_type")
maine_i_down = OpenStreetMap::openmap(maine_i_bb_ext[[1]],
  maine_i_bb_ext[[2]], zoom = 5, minNumTiles = 9, type = om_type)
maine_i_om <- RasterizeOMDownload(maine_i_down)

maine_i_overview <-
  tm_shape(maine_i_om) +
    tm_rgb() +
  tm_shape(maine) + # setting this as master sets lat/long
    tm_borders(col = "black") +
  tm_shape(baea_i_bb) +
    tm_borders(col = "red")
maine_i_overview

tmap_save(tm = baea_i_paths, filename = file.path(maps_dir, "Individuals",
  paste0(year_i, "_", id_i, ".svg")), insets_tm = maine_i_overview,
  insets_vp =  viewport(x = 0.855, y = 0.145, width = 0.2, height = 0.2),
  unit = "in", dpi = 300, height = 6, width = 6)

# Create 2d kernel density data - isopleth lines, polygons, and rasters
baea_i_smooth <- smooth_map(baea_i, cover = as(CreateExtentSF(baea_i, 1),
  "Spatial"), nlevels = 10)

### Isopleth Maps --------------------------------------------------------------

# Drop lowest density polygon
baea_i_smooth_polys <- st_intersection(baea_i_smooth$polygons,
  baea_i_smooth$polygons %>% arrange(level) %>% slice(-1))

mapview(baea_i_smooth_polys)

# Isolate highest density polygon
baea_i_smooth_poly1 <- st_intersection(baea_i_smooth$polygons,
  baea_i_smooth$polygons %>% arrange(rev(level)) %>% slice(1))

# Download om for polys_i
polys_i_bb_sf <- st_as_sfc(bb(baea_i_smooth_polys, relative = TRUE, height = 1.15,
  width = 1.15))
polys_i_bb_ext <- CreateOSMBaseBB(polys_i_bb_sf, type = "om_type")
polys_i_down = OpenStreetMap::openmap(polys_i_bb_ext[[1]], polys_i_bb_ext[[2]],
  minNumTiles = 21, type = om_type)
polys_i_om <- RasterizeOsMDownload(polys_i_down)

# Download om for poly1_i
poly1_i_bb_sf <- st_as_sfc(bb(baea_i_smooth_poly1, relative = TRUE, height = 2,
  width = 2))
poly1_i_bb_ext <- CreateOSMBaseBB(poly1_i_bb_sf, type = "om_type")
poly1_i_down = OpenStreetMap::openmap(poly1_i_bb_ext[[1]], poly1_i_bb_ext[[2]],
  minNumTiles = 21, type = om_type)
poly1_i_om <- RasterizeOsMDownload(poly1_i_down)

# All but lowest density isopleth
tm_shape(polys_i_om) +
  tm_raster() +
tm_shape(baea_i_smooth$raster) +
  tm_raster("count", alpha = .5) +
tm_shape(baea_i_smooth$iso) +
  tm_iso("black", size = .5, fontcolor="black")

# Highest density isopleth
tm_shape(poly1_i_om) +
  tm_rgb() +
tm_shape(baea_i_smooth_polys) +
  tm_fill("level", alpha = .5) +
tm_borders()


### ------------------------- Hexbin Maps --------------------------------------

# Select id and year
table(baea$id, baea$year) # Determine available individual/year combos
id_i <- "Ellis"
year_i <- 2018

# Filter data, create fightpaths
baea_i <- baea %>% filter(id == id_i) %>% filter(year == year_i)

baea_i_hexgrid <- st_make_grid(bb_poly(baea_i), cellsize = 9000, square = FALSE)
plot(baea_i_hexgrid)
baea_i_hexs = aggregate(baea_i %>% transmute(pt = 1), baea_i_hexgrid, sum) %>%
  filter(pt > 0)
plot(baea_i_hexs)
mapview(baea_i_hexs)

# Download om for polys_i
hexs_i_bb_sf <- CreateMapExtentBB(baea_i_hexs, ext = 1.15, asp = 1)
hexs_i_bb_ext <- CreateOSMBaseBB(hexs_i_bb_sf, type = "om_type")
hexs_i_down <- OpenStreetMap::openmap(hexs_i_bb_ext[[1]], hexs_i_bb_ext[[2]],
  minNumTiles = 21, type = om_type)
hexs_i_om <- RasterizeOsMDownload(hexs_i_down)

baea_i_x_dist <- as.numeric(approx_distances(bb(baea_i, ext = 1.15))[1])/1000/5
baea_i_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0, baea_i_x_dist),
  scales::pretty_breaks(3))[1]))

baea_i_hexbins <-
  tm_layout(asp = 1) +
  tm_shape(hexs_i_om) +
    tm_rgb() +
#  tm_shape(baea_i) +
#    tm_dots(size = 0.075, col = "black") +
  tm_shape(baea_i_lines) +
    tm_lines("yellow", lwd = 2, alpha = .5) +
  tm_shape(baea_i_hexs,
      bbox = bb(baea_i, ext = 1.15), is.master = TRUE) +
    tm_fill(col = 'pt', alpha = .5, palette = viridis(5, option = "D")) +
    tm_borders(col = "black") +
  tm_layout(main.title = paste0("Hexbins: ", id_i),
    main.title.position = "center",
    main.title.size = 1.15,
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85,
    outside = FALSE, position = c("right", "bottom")) +
  tm_scale_bar(size = .75, width = .2,
    breaks = baea_i_x_breaks,
    position = c(.05, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    position = c(.875, .875)) +
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_xlab("") + tm_ylab("")
baea_i_hexbins




#### ------------------- COVAR-SIGMA EXAMPLE MAPS ------------------------- ####

pacman::p_load(purrr, magick, rsvg, update = FALSE)

load_covars <- TRUE
if (isTRUE(load_covars)){
  covar1 <- raster("Data/GIS/covar1.tif")
  covar2 <- raster("Data/GIS/covar2.tif")
  covar3 <- raster("Data/GIS/covar3.tif")
  names(covar1) <- "elev"
  names(covar2) <- "develop"
  names(covar3) <- "gauss"
}

# Set Sigma Range for Sigma Combinations Models --------------------------------

combo_sigmas <- c(seq(0, 40, by = 10))

# Create Covar Sigma Rasters Brick ---------------------------------------------

covar_brick <- brick(c(
  tibble(sigma = combo_sigmas, covar = "covar1") %>% pmap(., SmoothRaster),
  tibble(sigma = combo_sigmas, covar = "covar2") %>% pmap(., SmoothRaster),
  tibble(sigma = combo_sigmas, covar = "covar3") %>% pmap(., SmoothRaster)))

# Download Basemaps ------------------------------------------------------------

# ESRI Baselayer, Must use OpenStreeMap::openmap() and 'om_type' argument
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_type <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

covar1_ext <- CreateMapExtentBB(st_as_sfc(bb(covar1)), 1.15, 10/7.5)
covar1_om_bb <- CreateOSMBaseBB(covar1_ext, type = "om_type")
covar1_om_down = OpenStreetMap::openmap(covar1_om_bb[[1]], covar1_om_bb[[2]],
  minNumTiles = 21, type = om_type, zoom = 12)
covar1_base <- RasterizeOMDownload(covar1_om_down)

covar12_ext <- CreateMapExtentBB(st_as_sfc(bb(covar1)), 1.25, 10/7.5)
covar12_om_bb <- CreateOSMBaseBB(covar12_ext, type = "om_type")
covar12_om_down = OpenStreetMap::openmap(covar12_om_bb[[1]], covar12_om_bb[[2]],
  minNumTiles = 21, type = om_type, zoom = 12)
covar12_base <- RasterizeOMDownload(covar12_om_down)

rm(esri_url, esri_tile, om_type, covar1_ext, covar1_om_bb, covar1_om_down,
   covar12_ext, covar12_om_bb, covar12_om_down)

covars <- c("covar1", "covar2", "covar3")
covar_pals <- c("viridis", "cividis", "inferno")
sigmas <- c(0, 10, 20)

covar_sigma_maps <- rep(list(NA),length(covars)*length(sigmas))

for (i in 1:length(covars)){
  if(all(!is.na(covar_sigma_maps))){
    covar_sigma_maps <- rep(list(NA),length(covars)*length(sigmas))
  }
  covar_i <- get(covars[i])
  covar_pal_i <- covar_pals[i]
  for (j in 1:length(sigmas)){
    sigma_j <- sigmas[j]
    covar_sigma <- subset(covar_brick, subset = paste0(names(covar_i), sigma_j))
    covar_sigma_map <- tm_shape(covar12_base) + tm_rgb() +
      tm_shape(st_as_sfc(bb(covar1)), ext = 1.25, is.master = TRUE) +
      tm_borders(col = NULL, lwd = 2) + tm_shape(covar_sigma) +
      tm_raster(palette = covar_pal_i, n = 20, alpha = 1) +
      tm_layout(asp =  10/7.5, outer.margins = rep(0, 4),
        title.bg.color = NA,
        title.size = 1.25,
        title.position = c("center", "TOP"), title.snap.to.legend = FALSE) +
      tm_legend(show = FALSE, title.size = .65, text.size = .5, frame = TRUE,
        outside = FALSE, position = c("right", "center"),
        legend.bg.color = "grey80") +
      tm_scale_bar(breaks = c(0, 2, 4), size = .35, position = c(.02, .00)) +
      tm_compass(type = "arrow",  show.labels = 0, size = .8,
        position = c(.90, .88))
    k <- min(which(is.na(covar_sigma_maps)))
    covar_sigma_maps[[k]] <- covar_sigma_map
  }
}

covar123_sigma0_10_20_facet_map <- tmap_arrange(
  covar_sigma_maps[[1]], covar_sigma_maps[[2]], covar_sigma_maps[[3]],
  covar_sigma_maps[[4]], covar_sigma_maps[[5]], covar_sigma_maps[[6]],
  covar_sigma_maps[[7]], covar_sigma_maps[[8]], covar_sigma_maps[[9]],
  nrow = 3, ncol = 3,
  asp = NA, outer.margins = rep(0, 4))

maps_file = file.path("Products/Maps/Covar_Sigmas",
  "Covar123_Sigma0_10_20Facet.png")
tmap_save(tm = covar123_sigma0_10_20_facet_map, filename = maps_file,
  unit = "in", dpi = 300, height = 5, width = 5)

# Create legends figure

covar_sigma_legends <- rep(list(NA), length(covars))
for (i in 1:length(covars)){
  covar_i <- get(covars[i])
  covar_pal_i <- covar_pals[i]
    sigma_j <- sigmas[1]
    covar_sigma <- subset(covar_brick, subset = paste0(names(covar_i), sigma_j))
    covar_sigma_legend <- tm_shape(covar_sigma) +
      tm_raster(palette = covar_pal_i, n = 10, alpha = 1, title = "Value") +
      tm_layout(legend.only = TRUE, #legend.bg.color = "grey80",
        outer.margins = rep(-1, 4), inner.margins = rep(-1, 4)) +
      tm_legend(text.size = 1, title.size = 1.5, design.mode = FALSE)
    k <- min(which(is.na(covar_sigma_legends)))
    covar_sigma_legends[[k]] <- covar_sigma_legend
}
covar123_sigma0_10_20_facet_legend <- tmap_arrange(
  covar_sigma_legends[[1]], covar_sigma_legends[[2]], covar_sigma_legends[[3]],
  nrow = 3, ncol = 1, outer.margins = rep(0, 4))
covar123_sigma0_10_20_facet_legend

legend_file = file.path("Products/Maps/Covar_Sigmas",
  "Covar123_Sigma0_10_20Facet_Legend.png")
tmap_save(tm = covar123_sigma0_10_20_facet_legend, filename = legend_file,
  unit = "in", dpi = 300, height = 5, width = 1)

covar_sigma_maps <- image_read(maps_file)
covar_sigma_legend <- image_read(legend_file)
covar_sigma_legend1 <- image_scale(image_trim(image_crop(covar_sigma_legend,
  "300x500")), "x450") # was "x475"
covar_sigma_legend2 <- image_scale(image_trim(image_crop(covar_sigma_legend,
  "300x500+0+500")), "x450")
covar_sigma_legend3 <- image_scale(image_trim(image_crop(covar_sigma_legend,
  "300x500+0+1000")), "x450")
backgrd <- image_blank(2000, 1600, color = "white")
covar_sigma_fig <- image_composite(backgrd, covar_sigma_maps,
    offset = "+275+100") %>%
  image_composite(., covar_sigma_legend1, offset = "+1800+115") %>%
  image_composite(., covar_sigma_legend2, offset = "+1800+615") %>%
  image_composite(., covar_sigma_legend3, offset = "+1800+1115") %>%
  image_annotate("Layer 1", size = 55, location = "+45+340") %>%
  image_annotate("Layer 2", size = 55, location = "+45+840") %>%
  image_annotate("Layer 3", size = 55, location = "+45+1340") %>%
  image_annotate("Original Scale", size = 55, location = "+360+25") %>%
  image_annotate("Sigma 10", size = 55, location = "+915+25") %>%
  image_annotate("Sigma 20", size = 55, location = "+1415+25")
covar_sigma_fig

maps_fig_file = file.path("Products/Maps/Covar_Sigmas",
  "Covar123_Sigma0_10_20Figure.png")
image_write(covar_sigma_fig, path = maps_fig_file, format = ".png")

#### ------------------------- WILSON SCENARIOS MAP ----------------------- ####

pacman::p_load(units, stringr)

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_type <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

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


# TEST SECTION (For placement of turbines in cells)

wind_wilson_n_grid <- sf::st_make_grid(bb(wind_wilson_n), 30) %>% st_cast("POLYGON")
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


# END TEST SECTION


wind_wilson_ns <- rbind(wind_wilson_n, wind_wilson_s)

# Get osm baselayer for wilson
wilson_bb_sf <- st_as_sfc(bb(wilson_bb, relative = TRUE, height = 1,
  width = 1))
wilson_bb_ext <- CreateOSMBaseBB(wilson_bb_sf, type = "om_type")
wilson_down <- OpenStreetMap::openmap(wilson_bb_ext[[1]], wilson_bb_ext[[2]],
  zoom = 13, minNumTiles = 21, type = om_type)  # may need to add/adjust 'zoom'
wilson_om <- RasterizeOMDownload(wilson_down)

wind_wilson_ns <- wind_wilson_ns %>%
  mutate(Rating = as.character(WPC)) %>%
  mutate(Rating = str_replace_all(Rating, "4", "(Good)")) %>%
  mutate(Rating = str_replace_all(Rating, "5", "(Excellent)")) %>%
  mutate(Rating = str_replace_all(Rating, "6", "(Outstanding)")) %>%
  mutate("Wind Power Class" = paste(WPC, Rating))

# Wilson Map
wilson_map <-
  tm_layout(asp = 1) +
  tm_shape(wilson_om, is.master = TRUE) +
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
wilson_overview_bb_ext <- CreateOSMBaseBB(wilson_overview_bb, type = "om_type")
wilson_overview_bb_down = OpenStreetMap::openmap(wilson_overview_bb_ext[[1]],
  wilson_overview_bb_ext[[2]], zoom = 6, minNumTiles = 21, type = om_type)
wilson_overview_bb_om <- RasterizeOMDownload(wilson_overview_bb_down)

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


# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #

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

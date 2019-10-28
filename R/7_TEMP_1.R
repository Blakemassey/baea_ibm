############################# MAPS SETUP #######################################

# Load packages
pacman::p_load(gisr, baear, cartography, dplyr, grid, leaflet, magick, mapview,
  OpenStreetMap, plotly, prettymapr, purrr, raster, rosm, rsvg, sf, tmap,
  tmaptools, viridis, webshot)

pacman::p_load(ctmm, devtools, dplyr, fitdistrplus, ggplot2, ggthemes,
  lubridate, mapview, move, raster, sf, tmaptools, units, zoo)
library(baear)
library(gisr)
library(ibmr)

options(stringsAsFactors = FALSE)
theme_update(plot.title = element_text(hjust = 0.5))

# Coordinate systems
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

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
om_type <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

#### ----------- Nest and Conspecific Distance Maps ----------------------------

## Import Base
base <- raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))
nests_active <- readRDS(file="Data/Nests/Nests_rds/nests_active.RDS")
baea_terr <- readRDS("Data/BAEA/baea_terr.rds")
#baea_dist_org <- readRDS("Data/BAEA/baea_dist.rds")
con_dist_nest <- raster("Output/Analysis/Territorial/ConDistNest_All.tif")

# # Fitting with con_nest > 75
# baea_dist <- baea_dist_org %>%
#   filter(!is.na(con_nest)) %>%
#   filter(con_nest > 0) %>%
#   dplyr::select(id, con_nest, con_nest_km, long_utm, lat_utm, lat, long) %>%
#   group_by(id) %>%
#   sample_n(2000, replace = TRUE) %>%
#   ungroup()

nests_2016 <- nests_active %>%
  filter(active_2016 == TRUE) %>%
  transmute(long = long_utm, lat = lat_utm)

nests <- baea_terr %>% group_by(id) %>% slice(1) %>%
    dplyr::select(nest_long_utm, nest_lat_utm)  %>%
    transmute(long = nest_long_utm, lat = nest_lat_utm)

con_dist_nest_gg <- ConvertRasterForGGPlot(con_dist_nest)


# Get osm baselayer for ellis
con_dist_nest_bb_sf <- st_as_sfc(bb(con_dist_nest, relative = TRUE, height = 1,
  width = 1))
con_dist_nest_bb_ext <- CreateOSMBaseBB(con_dist_nest_bb_sf, type = "om_type")
con_dist_nest_down <- OpenStreetMap::openmap(con_dist_nest_bb_ext[[1]],
  con_dist_nest_bb_ext[[2]], zoom = 7, minNumTiles = 21, type = om_type)  # may need to add/adjust 'zoom'
con_dist_nest_om <- RasterizeOMDownload(con_dist_nest_down)

# Ellis Map Raster

con_dist_nest_ext_sf <- st_as_sfc(bb(st_buffer(con_dist_nest_bb_sf, 5500) %>%
  st_transform(., crs = crs(con_dist_nest_om)), ext = .95))

con_dist_nest_map <-
  tm_layout(asp = 1) +
  tm_shape(con_dist_nest_ext_sf) +
    tm_fill(col = NA) +
  tm_shape(con_dist_nest_om) +
    tm_rgb() +
#  tm_shape(con_dist_nest) +
#    tm_raster("ConDistNest_All", palette = "-plasma", alpha = .6, style = "cont",
#      legend.show = FALSE)  #+
  tm_shape(con_dist_nest) +
    tm_raster("ConDistNest_All", palette = "-plasma", alpha = 1, style = "cont",
 #     breaks = c(0, 500, 1000, 1500, 2000),
      title = "Conspecific and Nest Distance (m)", legend.show = TRUE)
con_dist_nest_map


summary(con_dist_nest)

ggplot(con_dist_nest_gg, aes(x, y)) +
  geom_tile(aes(fill = value), interpolate=TRUE) +
  coord_fixed(ratio = 1) +
  scale_fill_distiller(breaks = seq(0,40000, by=5000), name="Meters",
    palette = "Blues", direction=-1) +
  geom_point(data = nests_2016, aes(long, lat), shape=24, alpha=.9,
    color="red", fill= "black", size=2, stroke=2) +
  geom_point(data = nests, aes(long, lat), shape=24, alpha=.9,
    color="blue", fill= "floralwhite", size=2, stroke=2) +
  geom_point(data = baea_dist, aes(long_utm, lat_utm), shape=4, alpha=.9,
    color="yellow", size=1, stroke=1.5) +
  geom_point(data = nests, aes(long, lat), shape=24, alpha=.9,
    color="blue", fill= "floralwhite", size=2, stroke=2) +
  theme_legend +
  ggtitle(paste("Stationary Locations")) + xlab("Longitude") +
  ylab("Latitude")

SaveGGPlot("Stationary Locations.png", image_output, bg = "white")

for (i in unique(baea$id)){
  con_dist_i <- raster(file.path("C:/Work/R/Workspace",
    "2016_Nests_Rasters/2016",paste0("ConDist_", i, ".tif")))

  con_dist_nest_i <- raster(file.path("C:/Work/R/Workspace",
    "2016_Nests_Rasters/2016",paste0("ConDistNest_", i, ".tif")))
  home_dist_i <- raster(file.path("C:/Work/R/Workspace/2016_Nests_Rasters",
    "2016", paste0("HomeDist_", i ,".tif")))
  con_nest_i <- overlay(home_dist_i, con_dist_nest_i,
    fun=function(x,y){round(x+y)})
  nest_i <-  baea %>% filter(id == i) %>% slice(1) %>%
    dplyr::select(nest_long_utm, nest_lat_utm)  %>%
    transmute(long = nest_long_utm, lat = nest_lat_utm)

  nests_2016_i <- nests_2016 %>%
    filter(long >= xmin(con_dist_nest_i) & long <= xmax(con_dist_nest_i)) %>%
    filter(lat >= ymin(con_dist_nest_i) & lat <= ymax(con_dist_nest_i))

  home_dist_i_gg <- ConvertRasterForGGPlot(home_dist_i)
  ggplot(home_dist_i_gg, aes(x, y)) +
    coord_fixed(ratio = 1) +
    geom_raster(aes(fill = value), interpolate=TRUE) + theme_legend +
    scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
    scale_fill_distiller(breaks = seq(0,40000, by=5000), name="Meters",
      palette = "Blues", direction=-1) +
    ggtitle(paste(i, "- Home Distance")) +
    xlab("Longitude") + ylab("Latitude") +
    geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
      color="red", fill= "black", size=2, stroke=2) +
    geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "white", size=2, stroke=2)
  SaveGGPlot(paste0(i, " - Home Distance.png"),
    file.path(image_output), bg = NA)

  con_dist_nest_i_gg <- ConvertRasterForGGPlot(con_dist_nest_i)
  ggplot(con_dist_nest_i_gg, aes(x, y)) +
    coord_fixed(ratio = 1) +
    geom_raster(aes(fill = value)) + theme_legend +
    scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
    scale_fill_gradient2(name="Meters", low="white", mid="grey", high="tan4") +
    ggtitle(paste(i, "- Conspecific Distance")) +
    xlab("Longitude") + ylab("Latitude")  +
    geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
      color="red", fill= "black", size=2, stroke=2) +
    geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "white", size=2, stroke=2)
  SaveGGPlot(paste0(i, " - Conspecific Distance.png"),
    file.path(image_output), bg = NA)

  con_dist_i_gg <- ConvertRasterForGGPlot(con_dist_i)
  ggplot(con_dist_i_gg, aes(x, y)) +
    coord_fixed(ratio = 1) +
    geom_raster(aes(fill = value)) + theme_legend +
    scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
    scale_fill_gradient2(name="Meters", high="white", mid="grey", low="tan4",
      midpoint=round((range(con_dist_i_gg$value)[2] -
          range(con_dist_i_gg$value)[1])/2)) +
    ggtitle(paste(i, "- Conspecific (actual) Distance")) +
    xlab("Longitude") + ylab("Latitude")  +
    geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
      color="red", fill= "black", size=2, stroke=2) +
    geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "white", size=2, stroke=2)
  SaveGGPlot(paste0(i, " - Conspecific (actual) Distance.png"),
    file.path(image_output), bg = NA)

  con_nest_i_gg <- ConvertRasterForGGPlot(con_nest_i)
  ggplot(con_nest_i_gg, aes(x, y)) +
    coord_fixed(ratio = 1) +
    geom_raster(aes(fill = value)) + theme_legend +
    scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
    scale_fill_gradientn(name = "Meters", colours = terrain.colors(10)) +
    ggtitle(paste(i, "- Con/Nest Distance")) +
    xlab("Longitude") + ylab("Latitude")  +
    geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
      color="red", fill= "black", size=2, stroke=2) +
    geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "white", size=2, stroke=2) +
  SaveGGPlot(paste0(i, " - Con_Nest Distance.png"), file.path(image_output),
    bg = NA)

  baea_dist_i <- baea_dist %>% filter(id == i)
  ggplot(con_nest_i_gg, aes(x, y)) +
    coord_fixed(ratio = 1) +
    geom_raster(aes(fill = value)) + theme_legend +
    scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
    scale_fill_gradientn(name = "Meters", colours = terrain.colors(10)) +
    ggtitle(paste(i, "- Con/Nest Distance")) +
    xlab("Longitude") + ylab("Latitude")  +
    geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
      color="red", fill= "black", size=2, stroke=2) +
    geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "white", size=2, stroke=2) +
    geom_point(data = baea_dist_i, aes(long_utm, lat_utm), shape=4, alpha=.9,
      color="black", size=2, stroke = 1.2)
  SaveGGPlot(paste0(i, " - Con_Nest Distance with GPS Locations.png"),
    file.path(image_output), bg = NA)
}

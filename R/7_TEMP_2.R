############################# MAPS SETUP #######################################

# Load packages
pacman::p_load(gisr, baear, cartography, dplyr, fasterize, ggplot2, ggthemes,
  grid, leaflet, lubridate,  magick, mapview, OpenStreetMap, plotly, prettymapr,
  purrr, raster, rosm, rsvg, sf, stars, stringr, tmap, tmaptools, viridis,
  units, webshot, zoo)
pacman::p_load(baear, gisr, ibmr)
pacman::p_load(tidyverse, sf, tmap, tmaptools, maptiles)

options(stringsAsFactors = FALSE)
theme_update(plot.title = element_text(hjust = 0.5))

# Coordinate systems
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_world_topo <- paste0(esri_url, "World_Topo_Map", esri_tile)
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

# Rasters
base <- raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

# Directories
baea_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Data/BAEA"
nests_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Nests/Nests_rds")
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"
turbine_dir <- file.path("C:/ArcGIS/Data/R_Input/BAEA")
wind_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Wind")
maps_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Products/Maps"
report_maps_dir <- file.path("C:/Users/blake/Documents/PhD Program/Reports",
  "BAEA Project - 2018/Maps")
states_dir <- file.path("C:/ArcGIS/Data/Boundaries/cb_2018_us_state_5m")
places_fl_dir <- file.path("C:/ArcGIS/Data/Boundaries/cb_2018_12_place_500k")

mod_dir <- "Output/Analysis/SSF/Models"
mod_fit_dir <- file.path(mod_dir, "model_fits")
mod_best_dir <- file.path(mod_dir, "model_fits_best")

# Model files
fits_best_file <- file.path(mod_best_dir, "model_fits_best.rds")
preds_tbl_file <- file.path(mod_best_dir, "preds_tbl.rds")

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

# States
states <- read_sf(file.path(states_dir, "cb_2018_us_state_5m.shp")) %>%
  st_transform(crs = 4326)

# Florida Places
places_fl <- read_sf(file.path(places_fl_dir, "cb_2018_12_place_500k.shp")) %>%
  st_transform(crs = 4326)

#### -------------------- Map of Webb Eagle Migration --------------------------

baea_webb_bb_sfc <- bb(x = c(-86.5, 25, -66.5, 47.5),
  current.projection = 4326) %>% st_bbox(.) %>% st_as_sfc(.)

# Webb Eagle
baea_webb <- baea %>% filter(id == "Webb") %>%
  st_transform(., crs = 4326)

# Basemaps
fullserver <- paste0("https://server.arcgisonline.com/ArcGIS/rest/services/",
  "/World_Physical_Map/MapServer/tile/{z}/{y}/{x}.jpg")
provider <- list(src = "World Physical", q = fullserver, sub = NA, cit = "")
webb_physical_osm <- maptiles::get_tiles(x = baea_webb_bb_sfc,
  cachedir = "C:/Temp/Maptiles", provider = provider, crop = TRUE,
  verbose = TRUE, zoom = 6, forceDownload = TRUE)

tmap_mode("view")
tmap_mode("plot")

# Create States text layer
states_text <- states %>%
  filter(STUSPS %in% c("CT", "DE", "FL", "GA", "MA", "MD", "ME", "NC", "NH",
    "NJ", "NY", "PA", "SC", "VA", "VT")) %>%
  dplyr::select(STUSPS, NAME) %>%
  st_centroid(.) %>%
  mutate(long = unlist(map(.$geometry,1)),
         lat = unlist(map(.$geometry,2))) %>%
  st_drop_geometry(.)

states_text[which(states_text$STUSPS == "CT"), "long"] <- -72.7
states_text[which(states_text$STUSPS == "CT"), "lat"] <- 41.6
states_text[which(states_text$STUSPS == "DE"), "long"] <- -75.2
states_text[which(states_text$STUSPS == "DE"), "lat"] <- 38.71
states_text[which(states_text$STUSPS == "FL"), "long"] <- -81.25
states_text[which(states_text$STUSPS == "FL"), "lat"] <- 27.5
states_text[which(states_text$STUSPS == "MA"), "long"] <- -71.8
states_text[which(states_text$STUSPS == "ME"), "long"] <- -69.0
states_text[which(states_text$STUSPS == "ME"), "lat"] <- 45.25
states_text[which(states_text$STUSPS == "MA"), "lat"] <- 42.4
states_text[which(states_text$STUSPS == "MD"), "long"] <- -76.95
states_text[which(states_text$STUSPS == "MD"), "lat"] <- 39.4
states_text[which(states_text$STUSPS == "NH"), "long"] <- -71.35
states_text[which(states_text$STUSPS == "NH"), "lat"] <- 43.15
states_text[which(states_text$STUSPS == "NJ"), "long"] <- -74.6
states_text[which(states_text$STUSPS == "NJ"), "lat"] <- 39.8
states_text[which(states_text$STUSPS == "NC"), "long"] <- -80.25
states_text[which(states_text$STUSPS == "NC"), "lat"] <- 35.8
states_text[which(states_text$STUSPS == "SC"), "long"] <- -81.7
states_text[which(states_text$STUSPS == "SC"), "lat"] <- 34.3
states_text_sf <- st_as_sf(states_text, coords = c("long", "lat"), crs = 4326)

# Filter data, create fightpaths
baea_webb_2015_summer <- baea_webb %>%
  filter(datetime <= as_date("2015-09-02")) %>%
  group_by(id) %>% arrange(datetime) %>%
  summarize(id = first(id), do_union = FALSE) %>%
  st_cast("LINESTRING")

baea_webb_2015_south <- baea_webb %>%
  filter(datetime > as_date("2015-09-02")) %>%
  filter(datetime <= as_date("2015-10-18")) %>%
  group_by(id) %>% arrange(datetime) %>%
  summarize(id = first(id), do_union = FALSE) %>%
  st_cast("LINESTRING")

baea_webb_2015_winter <- baea_webb %>%
  filter(datetime > as_date("2015-10-18")) %>%
  filter(datetime <= as_date("2016-04-05")) %>%
  group_by(id) %>% arrange(datetime) %>%
  summarize(id = first(id), do_union = FALSE) %>%
  st_cast("LINESTRING")

baea_webb_2016_north <- baea_webb %>%
  filter(datetime > as_date("2016-04-05")) %>%
  filter(datetime <= as_date("2016-05-08")) %>%
  group_by(id) %>% arrange(datetime) %>%
  summarize(id = first(id), do_union = FALSE) %>%
  st_cast("LINESTRING")

baea_webb_2016_summer <- baea_webb %>%
  filter(datetime > as_date("2016-05-08")) %>%
  filter(datetime <= as_date("2016-08-19")) %>%
  group_by(id) %>% arrange(datetime) %>%
  summarize(id = first(id), do_union = FALSE) %>%
  st_cast("LINESTRING")

baea_webb_2016_south <- baea_webb %>%
  filter(datetime > as_date("2016-08-19")) %>%
  filter(datetime <= as_date("2016-10-18")) %>%
  group_by(id) %>% arrange(datetime) %>%
  summarize(id = first(id), do_union = FALSE) %>%
  st_cast("LINESTRING")

baea_webb_2016_winter <- baea_webb %>%
  filter(datetime > as_date("2016-10-18")) %>%
  filter(datetime <= as_date("2017-04-15")) %>%
  group_by(id) %>% arrange(datetime) %>%
  summarize(id = first(id), do_union = FALSE) %>%
  st_cast("LINESTRING")

baea_webb_2017_north <- baea_webb %>%
  filter(datetime > as_date("2017-04-15")) %>%
  filter(datetime <= as_date("2017-05-14")) %>%
  group_by(id) %>% arrange(datetime) %>%
  summarize(id = first(id), do_union = FALSE) %>%
  st_cast("LINESTRING")

baea_webb_2017_summer <- baea_webb %>%
  filter(datetime > as_date("2017-05-14")) %>%
  group_by(id) %>% arrange(datetime) %>%
  summarize(id = first(id), do_union = FALSE) %>%
  st_cast("LINESTRING")

#tmaptools::palette_explorer()
col_south  <- viridisLite::plasma(6)[6] #brewer.pal(8, "Set1")[5]
col_north  <- viridisLite::plasma(6)[5] #brewer.pal(8, "Set1")[6]
col_winter <- viridisLite::plasma(6)[2] #brewer.pal(8, "Set1")[2]
col_summer <- viridisLite::plasma(6)[3] #brewer.pal(8, "Set1")[3]

flight_width = 2
flight_alpha = .85

# All flight paths and points
tmap_webb <-
  tm_layout(asp = .5) +
  tm_shape(baea_webb_bb_sfc, is.master = TRUE, ext = .935) +
    tm_borders(col = "red") +
  tm_shape(webb_physical_osm, raster.downsample = FALSE) +
     tm_rgb() +
  tm_shape(states) +
    tm_borders(col = "grey20", alpha = .8) +
  tm_shape(states_text_sf) +
    tm_text(text = "STUSPS", size = .55) +
  tm_layout(main.title = NULL,
    title.snap.to.legend = FALSE) +
  tm_scale_bar(text.size = .65, breaks = c(0, 200, 400),
    position = c(.495, 0)) +  #position = c(.675, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 2,
    position = c(.835, .025)) +
  tm_legend(title.size = .85, text.size = .55,
    outside = FALSE, position = c(.6, .35))

tmap_webb_paths_2015_16 <- tmap_webb +
  tm_shape(baea_webb_2015_summer) +
    tm_lines(col = col_summer, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2015_winter) +
    tm_lines(col = col_winter, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2015_south) +
    tm_lines(col = col_south, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2016_north) +
    tm_lines(col = col_north, lwd = flight_width, alpha = flight_alpha) +
  tm_add_legend('line',
  	col = viridisLite::plasma(6)[c(3, 6, 2, 5)],
  	border.col = "grey40",
  	size = 2,
    labels = c('2015 Summer', '2015 Fall Migration', '2015 Winter',
  	  '2016 Spring Migration'),
  	title = "Flight Paths")

tmap_webb_paths_2016_17 <- tmap_webb +
  tm_shape(baea_webb_2016_summer) +
    tm_lines(col = col_summer, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2016_winter) +
    tm_lines(col = col_winter, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2016_south) +
    tm_lines(col = col_south, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2017_north) +
    tm_lines(col = col_north, lwd = flight_width, alpha = flight_alpha) +
  tm_add_legend('line',
  	col = viridisLite::plasma(6)[c(3, 6, 2, 5)],
  	border.col = "grey40",
  	size = 2,
    labels = c('2016 Summer', '2016 Fall Migration', '2016 Winter',
  	  '2017 Spring Migration'),
  	title = "Flight Paths")

tmap_webb_2015_2017 <- tmap_arrange(list(tmap_webb_paths_2015_16,
  tmap_webb_paths_2016_17), ncol = 2, nrow = 1, widths = c(.5, .5), asp = NA)

tmap_save(tm = tmap_webb_2015_2017, filename = file.path(tex_dir, "Figures/Ch2",
  "Webb_Flights.svg"), unit = "in", dpi = 300, height = 6, width = 6, asp = NA)

#### -------------------- Map of Webb Florida Sites ----------------------------

# Florida Area
baea_webb_fl_bb_sfc <- bb(x = c(-83, 27, -80, 31),
  current.projection = 4326) %>% st_bbox(.) %>% st_as_sfc(.)

# Create States text layer
places_fl_sub <- places_fl %>%
  filter(NAME %in% c("Gainesville", "Daytona Beach", "Jacksonville", "Orlando",
    "Tampa"))

places_fl_text <- places_fl_sub %>%
  dplyr::select(NAME) %>%
  st_centroid(.) %>%
  mutate(long = unlist(map(.$geometry,1)),
         lat = unlist(map(.$geometry,2))) %>%
  st_drop_geometry(.)

places_fl_text[which(places_fl_text$NAME == "Orlando"), "long"] <- -81.76

places_fl_text_sf <- places_fl_text %>%
  mutate(long = long + .05) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

# Basemaps
fullserver <- paste0("https://server.arcgisonline.com/ArcGIS/rest/services/",
  "/World_Physical_Map/MapServer/tile/{z}/{y}/{x}.jpg")
provider <- list(src = "World Physical", q = fullserver, sub = NA, cit = "")
webb_fl_physical_osm <- maptiles::get_tiles(x = baea_webb_fl_bb_sfc,
  cachedir = "C:/Temp/Maptiles", provider = provider, crop = TRUE,
  verbose = TRUE, zoom = 8, forceDownload = TRUE)

# All flight paths and points
tmap_webb_fl <-
  tm_layout(asp = .7) +
  tm_shape(baea_webb_fl_bb_sfc, is.master = TRUE, ext = .935) +
    tm_borders(col = "red") +
  tm_shape(webb_fl_physical_osm, raster.downsample = FALSE) +
     tm_rgb() +
  tm_shape(states) +
    tm_borders(col = "grey20", alpha = .8) +
  tm_layout(main.title = NULL,
    title.snap.to.legend = FALSE) +
  tm_scale_bar(text.size = .65, breaks = c(0, 25, 50, 75),
    position = c(.3, 0)) +  #position = c(.675, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 2,
    position = c(.835, .875)) +
  tm_legend(title.size = .85, text.size = .55,
    outside = FALSE, position = c(.6, .665)) +
  tm_credits("Florida", size = .8, position=c(.065, .75)) +
  tm_credits("Georgia", size = .8, position=c(.033, .925))
#tmap_webb_fl

tmap_webb_fl_2015_16 <- tmap_webb_fl +
  tm_shape(baea_webb_2015_winter) +
    tm_lines(col = col_winter, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2015_south) +
    tm_lines(col = col_south, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2016_north) +
    tm_lines(col = col_north, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(places_fl_sub) +
    tm_dots(shape = 21, col = "red", size = .25, border.lwd = 1,
    border.col = "grey20") +
  tm_shape(places_fl_text_sf) +
    tm_text(text = "NAME", just = c("left", "top"), size = .55) +
  tm_add_legend('line',
  	col = viridisLite::plasma(6)[c(6, 2, 5)],
  	border.col = "grey40",
  	size = 3,
    labels = c('2015 Fall Migration', '2015 Winter',
  	  '2016 Spring Migration'),
  	title = "Flight Paths")
#tmap_webb_fl_2015_16

tmap_webb_fl_2016_17 <- tmap_webb_fl +
  tm_shape(baea_webb_2016_winter) +
    tm_lines(col = col_winter, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2016_south) +
    tm_lines(col = col_south, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2017_north) +
    tm_lines(col = col_north, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(places_fl_sub) +
    tm_dots(shape = 21, col = "red", size = .25, border.lwd = 1,
    border.col = "grey20") +
  tm_shape(places_fl_text_sf) +
    tm_text(text = "NAME", just = c("left", "top"), size = .55) +
  tm_add_legend('line',
  	col = viridisLite::plasma(6)[c(6, 2, 5)],
  	border.col = "grey40",
  	size = 3,
    labels = c('2016 Fall Migration', '2016 Winter',
  	  '2017 Spring Migration'),
  	title = "Flight Paths")

tmap_webb_fl_2015_2017 <- tmap_arrange(list(tmap_webb_fl_2015_16,
  tmap_webb_fl_2016_17), ncol = 2, nrow = 1, widths = c(.5, .5), asp = NA)

tmap_save(tm = tmap_webb_fl_2015_2017, filename = file.path(tex_dir,
  "Figures/Ch2", "Webb_Florida.svg"), unit = "in", dpi = 300, height = 6,
  width = 6, asp = NA)

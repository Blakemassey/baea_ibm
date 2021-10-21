#--------------------------- DISSERTATION MAPS --------------------------------#
# This script is for importing GIS datalayers and converting them to the proper
#  coordinate reference system (NAD83 UTM N19), extent, and resolution.
#------------------------------------------------------------------------------#

# Load packages, scripts, and input parameters ---------------------------------
pacman::p_load(gisr, baear, cartography, dplyr, fasterize, ggplot2, ggthemes,
  grid, leaflet, lubridate, magick, maptiles, mapview, OpenStreetMap, plotly,
  prettymapr, purrr, raster, rosm, rsvg, sf, stars, stringr, tmap, tmaptools,
  viridis, units, webshot, zoo)
suppressMessages(extrafont::loadfonts(device="win"))
pacman::p_load(baear, gisr, ibmr)
theme_update(plot.title = element_text(hjust = 0.5))

# Sim file and code Boolean parameters
sim_rds <- "sim_20210725-03.rds"

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
ridgeline_dir <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines"

# Files
sim_dir <- "C:/TEMP"
akde_dir <- file.path(sim_dir, sim_id, "AKDEs")
baea_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Data/BAEA"
nests_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Nests/Nests_rds")
places_fl_dir <- file.path("C:/ArcGIS/Data/Reference/cb_2018_12_place_500k")
places_me_dir <- file.path("C:/ArcGIS/Data/Reference/cb_2018_23_place_500k")
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"
states_dir <- file.path("C:/ArcGIS/Data/Reference/cb_2018_us_state_5m")
roads_dir <- file.path("C:/ArcGIS/Data/Reference/tl_2016_us_primaryroads")
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"
turbine_dir <- file.path("C:/ArcGIS/Data/R_Input/BAEA")
wind_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Wind")
wind_input_dir <- "Output/Analysis/Wind"
exp_turbines_dir <- "C:/ArcGIS/Data/R_Input/EXP"
maps_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Products/Maps"
report_maps_dir <- file.path("C:/Users/blake/Documents/PhD Program/Reports",
  "BAEA Project - 2018/Maps")
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

# Florida Places
places_fl <- read_sf(file.path(places_fl_dir, "cb_2018_12_place_500k.shp")) %>%
  st_transform(crs = 4326)
places_me <- read_sf(file.path(places_me_dir, "cb_2018_23_place_500k.shp")) %>%
  st_transform(crs = 4326)

# States
states <- read_sf(file.path(states_dir, "cb_2018_us_state_5m.shp")) %>%
  st_transform(crs = 4326)

# Ridgelines
ridge_poly_file <- file.path(ridgeline_dir, "ridge_poly.shp")

# Roads
roads <- read_sf(file.path(roads_dir, "tl_2016_us_primaryroads.shp")) %>%
  st_transform(crs = 4326)

# Wind
wind_class <- read_sf(file.path(wind_dir, "Maine_Wind_High_Resolution",
  "maine_50mwind.shp")) %>%
  st_transform(crs = 32619)

# Turbines
turbines <- read_sf(file.path(turbine_dir, "wind_turbines.shp")) %>%
  st_transform(crs = 32619)

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

# ESRI Basemaps
esri_natgeo_url <- paste0("https://server.arcgisonline.com/ArcGIS/rest/",
  "services/NatGeo_World_Map/MapServer/tile/{z}/{y}/{x}.jpg")
esri_natgeo_info <- list(src = "NatGeo World Map", q = esri_natgeo_url,
  sub = NA, cit = "")

tmap_mode("plot")

# CHAPTER 2 --------------------------------------------------------------------

# Nests Overview Map -----------------------------------------------------------

#maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1.15, width = 2))
maine_bb_sf <- st_as_sfc(bb(maine %>% st_transform(., crs = crs(maine_bb_sf)),
  ext = 1.15))

roads_crop <- st_crop(roads, maine_bb_sf) %>%
  filter(RTTYP == "I") %>%
  filter(FULLNAME == "I- 95" | FULLNAME == "I- 495") %>%
  filter(LINEARID != "110455961121")
mapview::mapview(roads_crop)

# Basemaps
fullserver <- paste0("https://server.arcgisonline.com/ArcGIS/rest/services/",
  "/World_Street_Map/MapServer/tile/{z}/{y}/{x}.jpg")
provider <- list(src = "Nat Geo", q = fullserver, sub = NA, cit = "")
maine_natgeo_osm <- maptiles::get_tiles(x = maine_bb_sf,
  cachedir = "C:/Temp/Maptiles", provider = provider, crop = TRUE,
  verbose = TRUE, zoom = 7, forceDownload = TRUE)

fullserver <- paste0("https://server.arcgisonline.com/ArcGIS/rest/services/",
  "/World_Physical_Map/MapServer/tile/{z}/{y}/{x}.jpg")
provider <- list(src = "Nat Geo", q = fullserver, sub = NA, cit = "")
maine_physical_osm <- maptiles::get_tiles(x = maine_bb_sf,
  cachedir = "C:/Temp/Maptiles", provider = provider, crop = TRUE,
  verbose = TRUE, zoom = 8, forceDownload = TRUE)

nests_study_sf <- nests_study %>%
  filter(nest_site != "446R01")

# Create States text layer
nests_text <- nests_study_sf %>%
  arrange(name) %>%
  dplyr::select(name) %>%
  st_centroid(.) %>%
  mutate(long = unlist(map(.$geometry,1)),
         lat = unlist(map(.$geometry,2))) %>%
  mutate(long = long + .065) %>%
  st_drop_geometry(.)

nests_text[which(nests_text$name == "Branch"), "long"] <- -68.51
nests_text[which(nests_text$name == "Branch"), "lat"] <- 44.628
nests_text[which(nests_text$name == "Crooked"), "long"] <- -68.45
nests_text[which(nests_text$name == "Crooked"), "lat"] <- 45.47
nests_text[which(nests_text$name == "Ellis"), "long"] <- -70.61
nests_text[which(nests_text$name == "Ellis"), "lat"] <- 44.635
nests_text[which(nests_text$name == "Eskutassis"), "long"] <- -68.6
nests_text[which(nests_text$name == "Eskutassis"), "lat"] <- 45.165
nests_text[which(nests_text$name == "Madagascal"), "long"] <- -68.29
nests_text[which(nests_text$name == "Madagascal"), "lat"] <- 45.248
nests_text[which(nests_text$name == "Musquash"), "long"] <- -68.1
nests_text[which(nests_text$name == "Musquash"), "lat"] <- 45.55
nests_text[which(nests_text$name == "Norway"), "long"] <- -70.54
nests_text[which(nests_text$name == "Norway"), "lat"] <- 44.275
nests_text[which(nests_text$name == "Onawa"), "long"] <- -69.32574
nests_text[which(nests_text$name == "Onawa"), "lat"] <- 45.415
nests_text[which(nests_text$name == "Phillips"), "long"] <- -68.532
nests_text[which(nests_text$name == "Phillips"), "lat"] <- 44.74
nests_text[which(nests_text$name == "Sandy"), "long"] <- -69.252
nests_text[which(nests_text$name == "Sandy"), "lat"] <- 44.575
nests_text[which(nests_text$name == "Sheepscot"), "long"] <- -69.67
nests_text[which(nests_text$name == "Sheepscot"), "lat"] <- 44.48
nests_text[which(nests_text$name == "Three"), "long"] <- -68.1925
nests_text[which(nests_text$name == "Three"), "lat"] <- 45.355
nests_text[which(nests_text$name == "Webb"), "lat"] <- 44.75
nests_text[which(nests_text$name == "Wilson"), "long"] <- -69.43
nests_text[which(nests_text$name == "Wilson"), "lat"] <- 45.55
nests_text_sf <- st_as_sf(nests_text, coords = c("long", "lat"), crs = 4326)

# Create States text layer
places_me_sub <- places_me %>%
  filter(NAME %in% c("Bangor", "Augusta", "Lewiston", "Portland"))

places_me_text <- places_me_sub %>%
  dplyr::select(NAME) %>%
  st_centroid(.) %>%
  mutate(long = unlist(map(.$geometry, 1)),
         lat = unlist(map(.$geometry, 2))) %>%
  st_drop_geometry(.)

places_me_text[which(places_me_text$NAME == "Augusta"), "lat"] <- 44.38
places_me_text[which(places_me_text$NAME == "Augusta"), "long"] <- -70.33
places_me_text[which(places_me_text$NAME == "Bangor"), "lat"] <- 44.90
places_me_text[which(places_me_text$NAME == "Bangor"), "long"] <- -69.2
places_me_text[which(places_me_text$NAME == "Lewiston"), "lat"] <- 44.05
places_me_text[which(places_me_text$NAME == "Lewiston"), "long"] <- -70.16
places_me_text[which(places_me_text$NAME == "Portland"), "lat"] <- 43.68
places_me_text[which(places_me_text$NAME == "Portland"), "long"] <- -70.94

places_me_text_sf <- places_me_text %>%
  mutate(long = long + .05) %>%
  mutate(lat = lat + .025) %>%
  mutate(NAME = str_to_upper(NAME)) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326)

nests_overview <- tm_layout(asp = .75, main.title = NULL) +
  tm_shape(maine_bb_sf, is.master = TRUE, ext = .85) +
    tm_borders(col = NA) +
  tm_shape(maine_physical_osm) +
    tm_rgb() +
  tm_shape(nests_study_sf) +
    tm_symbols("yellow", size = .4) +
  tm_shape(nests_text_sf) +
	  tm_text(text = "name", just = c("left", "top"), size = .9) +
  tm_shape(roads_crop) +
    tm_lines(col = "blue4", alpha = .8) +
  tm_shape(states) +
    tm_borders(col = "black", alpha = .8, lwd = 2.5) +
  tm_shape(places_me_sub) +
    tm_dots(shape = 21, col = "red", size = .25, border.lwd = 1,
    border.col = "grey20") +
  tm_shape(places_me_text_sf) +
    tm_text(text = "NAME", just = c("left", "top"), size = .7) +
  tm_credits("MAINE", size = 1.25, position = c(.44, .675)) +
	tm_logo(paste0("https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/",
	  "I-95.svg/200px-I-95.svg.png"), height = 1, position = c(.39, .39)) +
  tm_legend(title.size = 1, text.size = .9, outside = FALSE,
    position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = .9, position = c(.5, .01))+
  tm_compass(type = "4star",  show.labels = 1, size = 3, text.size = 1,
    position = c(.85, .05))
#nests_overview

tmap_save(tm = nests_overview, filename = file.path(tex_dir, "Figures/Ch2",
  "Trapping_Sites_Overview.svg"), unit = "in", dpi = 300, height = 8, width = 6)

# Map of Webb Eagle Migration --------------------------------------------------

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
states_text[which(states_text$STUSPS == "MD"), "lat"] <- 39.45
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

tmaptools::palette_explorer()
col_south  <- viridisLite::plasma(6)[6] #brewer.pal(8, "Set1")[5]
col_north  <- viridisLite::plasma(6)[5] #brewer.pal(8, "Set1")[6]
col_winter <- viridisLite::plasma(6)[2] #brewer.pal(8, "Set1")[2]
col_summer <- viridisLite::plasma(6)[3] #brewer.pal(8, "Set1")[3]

flight_width = 2.5
flight_alpha = .95

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
    tm_text(text = "STUSPS", size = .8) +
  tm_layout(main.title = NULL,
    title.snap.to.legend = FALSE) +
  tm_scale_bar(text.size = .9, breaks = c(0, 200, 400),
    position = c(.525, 0)) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.25, text.size = .9,
    position = c(.835, .025)) +
  tm_legend(title.size = 1.25, text.size = .9,
    outside = FALSE, position = c(.465, .2), legend.width = .65)

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
  	border.col = "grey40", lwd = 3,
    labels = c('2015 Breeding Season', '2015 Fall Migration',
      '2015/2016 Winter Season', '2016 Spring Migration'),
  	title = "     Flight Paths")

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
  	border.col = "grey40", lwd = 3,
    labels = c('2016 Breeding Season', '2016 Fall Migration',
      '2016/2017 Winter Season', '2017 Spring Migration'),
  	title = "     Flight Paths")

tmap_webb_2015_2017 <- tmap_arrange(list(tmap_webb_paths_2015_16,
  tmap_webb_paths_2016_17), ncol = 2, nrow = 1, widths = c(.5, .5), asp = NA)

tmap_save(tm = tmap_webb_2015_2017, filename = file.path(tex_dir, "Figures/Ch2",
  "Webb_Flights.svg"), unit = "in", dpi = 300, height = 6, width = 8, asp = NA)

# Map of Webb Florida Sites ----------------------------------------------------

# Florida Area
baea_webb_fl_bb_sfc <- bb(x = c(-83, 27.1, -79.5, 31.1),
  current.projection = 4326) %>% st_bbox(.) %>% st_as_sfc(.)

# Create States text layer
places_fl_sub <- places_fl %>%
  filter(NAME %in% c("Gainesville", "Daytona Beach", "Jacksonville", "Orlando",
    "Tampa"))

places_fl_text <- places_fl_sub %>%
  dplyr::select(NAME) %>%
  st_centroid(.) %>%
  mutate(long = unlist(map(.$geometry, 1)),
         lat = unlist(map(.$geometry, 2))) %>%
  st_drop_geometry(.)

places_fl_text[which(places_fl_text$NAME == "Jacksonville"), "lat"] <- 30.275
places_fl_text[which(places_fl_text$NAME == "Jacksonville"), "long"] <- -82
places_fl_text[which(places_fl_text$NAME == "Orlando"), "long"] <- -81.89

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
  tm_scale_bar(text.size = .9, breaks = c(0, 25, 50, 75),
    position = c(.3, 0)) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.25, text.size = .9,
    position = c(.835, .025)) +
  tm_legend(title.size = 1.25, text.size = .9, legend.width = .65,
    outside = FALSE, position = c(.47, .82)) +
  tm_credits("Florida", size = 1.1, position = c(.065, .75)) +
  tm_credits("Georgia", size = 1.1, position = c(.033, .925))

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
    tm_text(text = "NAME", just = c("left", "top"), size = .9) +
  tm_add_legend('line', col = viridisLite::plasma(6)[c(6, 2, 5)],
    border.col = "grey40", size = 3, lwd = 3,
    labels = c('2015 Fall Migration', '2015/2016 Winter Season',
      '2016 Spring Migration'), title = "     Flight Paths")

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
    tm_text(text = "NAME", just = c("left", "top"), size = .9) +
  tm_add_legend('line', col = viridisLite::plasma(6)[c(6, 2, 5)],
    border.col = "grey40", size = 3, lwd = 3,
    labels = c('2016 Fall Migration', '2016/2017 Winter Season',
      '2017 Spring Migration'), title = "     Flight Paths")

tmap_webb_fl_2015_2017 <- tmap_arrange(list(tmap_webb_fl_2015_16,
  tmap_webb_fl_2016_17), ncol = 2, nrow = 1, widths = c(.5, .5), asp = NA)

tmap_save(tm = tmap_webb_fl_2015_2017, filename = file.path(tex_dir,
  "Figures/Ch2", "Webb_Florida.svg"), unit = "in", dpi = 300, height = 6,
  width = 8, asp = NA)

# Home Range Maps --------------------------------------------------------------

# Getting the ratio and background correct requires 3 components:
# 1) Getting enough coverage of basemap by adjusting bb() 'height'/'width' args
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

# Con and Home Nest Distance Map -----------------------------------------------

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

# SSF for Maine - Individual ---------------------------------------------------

# This process assumes that the 'final' version of the SSF probablity layers
# is in the ssf_prob_dir

ssf_prob_dir <- file.path(ssf_raster_dir, "Step_Types_Prob")
ssf_prob_files <- list.files(ssf_prob_dir, pattern = "\\.tif", full.names =TRUE)
ssf_tmap_list <- vector(mode = "list", length = length(ssf_prob_files))

# Set to TRUE to run - NOT CURRENTLY INCLUDED IN TEXT
ssf_individual_maine <- TRUE

# For Individual Maps
for (i in seq_len(length(ssf_prob_files))){
  print(i)
  ssf_prob_i <- read_stars(ssf_prob_files[i])
  if (i ==  1){
    # Use "Tmap_baselayers.R" script to get other baselayers
    maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1.15,
      width = 1.35))
    maine_om = read_osm(maine_bb_sf, zoom = 7, minNumTiles = 9, type =
      om_nat_geo)
  }
  step_type_numeric <- basename(tools::file_path_sans_ext(ssf_prob_files[i]))
  step_type_text <- step_type_numeric %>%
    str_replace_all("1", "Cruise") %>%
    str_replace_all("2", "Flight") %>%
    str_replace_all("3", "Nest") %>%
    str_replace_all("4", "Perch") %>%
    str_replace_all("5", "Roost")
  step_type_arrow <- step_type_text %>%
    str_replace_all("_", "$\\\\rightarrow$ ") %>%
    latex2exp::TeX(.)
  ssf_prob_i_map <-
    tm_shape(maine_om) +
      tm_rgb() +
    tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1),
        alpha = .8,
        legend.reverse = TRUE, style = "cont", title = "Probability") +
    tm_scale_bar(breaks = c(0, 50, 100), text.size = .75,
      position = c(.75, .01)) +
    tm_compass(type = "4star",  show.labels = 1, size = 3,
      position = c(.85, .75)) +
    tm_shape(maine) + tm_borders(col = "black") +  # ME outline overlays grid
    tm_shape(nests_study %>% filter(nest_site != "446R01")) +
    tm_symbols(shape = 8, col = "black", size = .4) +
    tm_layout(#asp = .75,
      title.bg.color = "white",
      title.position = c("left", "top"),
      title.fontfamily = "Latin Modern Roman",
      title = step_type_arrow,
      #title.size = ,
      title.snap.to.legend = FALSE,
      legend.position = c(.80,.10),
      legend.outside = FALSE,
      legend.bg.color = "white",
      legend.title.fontfamily = "Latin Modern Roman",
      legend.text.fontfamily = "Latin Modern Roman",
      frame = FALSE) #+ tm_credits(step_type_arrow, position=c("right", "top"))
  ssf_prob_i_map

  if(ssf_individual_maine){
    tmap_save(tm = ssf_prob_i_map, filename = file.path(tex_dir, "Figures/Ch2",
      "SSF_Prob_Raster_Maps", paste0("SSF_Probability_Map_", step_type_text,
      ".png")), unit = "in", dpi = 300, height = 8, width = 6)
  }
}

# SSF for Maine - Combined -----------------------------------------------------

# This process assumes that the 'final' version of the SSF probablity layers
# is in the ssf_prob_dir

for (i in seq_len(length(ssf_prob_files))){
  print(i)
  ssf_prob_i <- read_stars(ssf_prob_files[i])
  step_type_numeric <- basename(tools::file_path_sans_ext(ssf_prob_files[i]))
  step_type <- step_type_numeric %>%
    str_replace_all("1", "Cruise") %>%
    str_replace_all("2", "Flight") %>%
    str_replace_all("3", "Nest") %>%
    str_replace_all("4", "Perch") %>%
    str_replace_all("5", "Roost") %>%
    str_replace_all("_", "$\\\\rightarrow$ ") %>%
    latex2exp::TeX(.)
  ssf_prob_i_map <-
    tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1),
        legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_layout(asp = .8,
        title.position = c("LEFT", "TOP"),
        title.fontfamily = "Latin Modern Roman",
        title = step_type,
        title.size = .5,
        title.snap.to.legend =  FALSE,
        legend.position = c("RIGHT", "BOTTOM"),
        legend.height = .4,
        legend.title.size = .45,
        legend.text.size = .4,
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman",
        frame = FALSE)
  ssf_prob_i_map
  ssf_tmap_list[[i]] <- ssf_prob_i_map
}

tmap_blank <-
  tm_shape(ssf_prob_i, raster.downsample = TRUE) +
  tm_raster(palette = "white", style = "cont") +
  tm_layout(asp = .75, legend.show = FALSE, frame = FALSE)

ssf_tmap_arrange <- tmap_arrange(
  ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]], tmap_blank,
  ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
  ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
  ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
  ssf_tmap_list[[13]],ssf_tmap_list[[14]],ssf_tmap_list[[15]],
  tmap_blank, ssf_tmap_list[[16]], ssf_tmap_list[[17]], tmap_blank,
  ncol = 4)

tmap_save(tm = ssf_tmap_arrange, filename = file.path(tex_dir, "Figures/Ch2",
  "SSF_Prob_Raster_Maps", "SSF_Probability_Maps_Overview.png"), unit = "in",
  dpi = 300, height = 8, width = 8*(.8))

# SSF at Nests - Combined ------------------------------------------------------

tmap_mode("plot")

# Nests
nests_sim <- nests_study %>% slice(c(2, 4, 7, 13)) %>% st_transform(wgs84n19)

# SSF Fits
ssf_fits_best_org <- readRDS(fits_best_file) #%>% slice(c(step_type_index))
ssf_fits_best <- ssf_fits_best_org

# For Individual Maps
for (j in seq_len(nrow(nests_sim))){
  # Get nest
  nest_j <- nests_sim %>% slice(j)
  nest_j_name <- nest_j %>% pull(name)
  # List for output
  ssf_tmap_list <- vector(mode = "list", length = 20)
  for (i in seq_len(nrow(ssf_fits_best))){
    step_type_i_numeric <- ssf_fits_best %>% slice(i) %>% pull(step_type) %>%
      str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
        "perch" = "4", "roost" = "5"))
    ssf_prob_file <- list.files(ssf_prob_dir, pattern =
      paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
    if (i ==  1){
      # Use "Tmap_baselayers.R" script to get other baselayers
      nest_bbox <- st_as_sfc(st_bbox(st_buffer(nest_j, dist = 10000)))
      nest_buffer <- st_buffer(nest_j, dist = 10000)
      nest_bb_sf <- st_as_sfc(bb(nest_buffer, relative = TRUE, height = 1.35,
        width = 1.35))
      Sys.sleep(1)
      nest_om = read_osm(nest_bb_sf, type = om_nat_geo, zoom = 11)
        #type = "osm", minNumTiles=9,
      nest_om_bb <- bb_poly(nest_om)
    }
    ssf_prob_i <- raster(ssf_prob_file) #%>% slice(1)
    ssf_prob_i_crop <- crop(ssf_prob_i, nest_buffer)
    ssf_prob_i_mask <- mask(ssf_prob_i_crop, nest_buffer)
    step_type_i_text <- step_type_i_numeric %>%
      str_replace_all("1", "Cruise") %>%
      str_replace_all("2", "Flight") %>%
      str_replace_all("3", "Nest") %>%
      str_replace_all("4", "Perch") %>%
      str_replace_all("5", "Roost")
    writeLines(paste0("Mapping: ", step_type_i_text))
    step_type_i_arrow <- step_type_i_text %>%
      str_replace_all("_", "$\\\\rightarrow$ ") %>%
      latex2exp::TeX(.)
    ssf_prob_i_nest_map <-
      tm_shape(nest_om) +
        tm_rgb() +
     tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
     tm_raster(palette = viridis(20, direction = 1), alpha = .6,
       legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_scale_bar(breaks = c(0, 5, 10), text.size = .4, lwd = .25,
        position = c(.03, .0)) +
      tm_compass(type = "4star", text.size = 0.55, show.labels = 1, size = 1.75,
        position = c(.8, .775), lwd = .25) +
      tm_shape(nests_sim) +
      tm_symbols(shape = 20, #border.col = "black", border.lwd = .5,
        col = "black", size = .075) +
      tm_layout(asp = .8,
        frame = NA, #"black",
        title.color = "black",
        title.bg.color = NA, #"ivory3",
        title.bg.alpha = .85,
        title.position = c(.275,.95),
        title.fontfamily = "Latin Modern Roman",
        title = step_type_i_arrow,
        title.size = .6,
        title.snap.to.legend = FALSE,
        legend.bg.color = "ivory1",
        legend.frame = "grey",
        legend.frame.lwd = 1,
        legend.height = .4,
        legend.title.size = .4,
        legend.text.size = .35,
        legend.position = c(.785,.007),
        legend.outside = FALSE,
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman")
        #+ tm_credits(step_type_arrow, position=c("right","top"))
    #ssf_prob_i_nest_map
    tmap_position <- switch(step_type_i_numeric,
      "1_1" = 1,  "1_2" = 2,  "1_4" = 3,
      "2_1" = 5,  "2_2" = 6,  "2_4" = 7,  "2_5" = 8,
      "3_1" = 9,  "3_2" = 10, "3_4" = 11, "3_5" = 12,
      "4_1" = 13, "4_2" = 14, "4_4" = 15, "4_5" = 16,
                  "5_2" = 18, "5_4" = 19)
    writeLines(as.character(tmap_position))
    ssf_tmap_list[[tmap_position]] <- ssf_prob_i_nest_map
  }

  tmap_blank <-
    tm_shape(nest_om_bb, is.master = TRUE) +
      tm_fill(col = "white") +
    tm_shape(nest_buffer, is.master = TRUE) +
      tm_polygons(col = "white", border.col = "white") +
    tm_layout(asp = .8, legend.show = FALSE, frame = FALSE)

  for (i in seq_len(length(ssf_tmap_list))){
    if(is.null(ssf_tmap_list[[i]])) ssf_tmap_list[[i]] <- tmap_blank
  }

  # For testing
  # ssf_tmap_nest_arrange <- tmap_arrange(
  #   ssf_tmap_list[[1]], tmap_blank, tmap_blank, tmap_blank,
  #   tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  #   tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  #   tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  #   tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  #   ncol = 4)

  # Arrange map of probability surfaces for testing
  ssf_tmap_nest_arrange <- tmap_arrange(
    ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]],
    ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
    ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
    ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
    ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
    ssf_tmap_list[[16]], ssf_tmap_list[[17]], ssf_tmap_list[[18]],
    ssf_tmap_list[[19]], ssf_tmap_list[[20]], ncol = 4)

  tmap_save(tm = ssf_tmap_nest_arrange, filename = file.path(tex_dir,
    "Figures/Ch2/SSF_Prob_Raster_Maps", "Nests", paste0(
    "SSF_Probability_Maps_", nest_j_name, ".png")), unit = "in", dpi = 300,
    height = 8, width = 8*.8)
}

# CHAPTER 3 --------------------------------------------------------------------

# Baea/Sim AKDE ----------------------------------------------------------------

# INSERT FROM 5d_SIM_Visualization_SIM


# CHAPTER 4 --------------------------------------------------------------------

# Wilson Nest Area -------------------------------------------------------------

# Filter nest data
nest_wilson <- nests_study %>% filter(name == "Wilson")  %>%
  st_transform(crs = 32619)

wilson_map_center <- nest_wilson
wilson_sfc <- st_sfc(st_point(c(st_coordinates(nest_wilson)[1],
  st_coordinates(nest_wilson)[2])))

st_geometry(wilson_map_center) <- wilson_sfc
st_crs(wilson_map_center) <- 32619

wilson_bb_sfc <- st_buffer(wilson_map_center, 8000) %>% bb(.) %>% st_as_sfc(.)
mapview(wilson_bb_sfc)

# Basemaps
wilson_natgeo_osm <- maptiles::get_tiles(x = wilson_bb_sfc,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 12, forceDownload = TRUE)

# Colors
nest_color <- "yellow"
wind_area_color <- "darkorange"
turbine_color <- "darkorange"

# Wilson Turbines --------------------------------------------------------------

wilson_wt_n = st_read(file.path(exp_turbines_dir, "wilson_n_turbines.shp"))
wilson_wt_s = st_read(file.path(exp_turbines_dir, "wilson_s_turbines.shp"))

wilson_wt_n_buff <- wilson_wt_n %>% st_buffer(56)
wilson_wt_s_buff <- wilson_wt_s %>% st_buffer(56)

mapview(wilson_wt_n_buff) + mapview(wilson_wt_s_buff)

# Wilson Overview Map ----------------------------------------------------------

wilson_overview_center <- nest_wilson
sfc <- st_sfc(st_point(c(st_coordinates(nest_wilson)[1] + 10000,
  st_coordinates(nest_wilson)[2] - 75000)))
st_geometry(wilson_overview_center) <- sfc
st_crs(wilson_overview_center) <- 32619

wilson_overview_buff <- st_buffer(wilson_overview_center, 120000) %>% bb(.)
mapview(wilson_overview_buff)
wilson_overview_bb <- bb_poly(bb(wilson_overview_buff, ext = 1))

wilson_overview_bb_osm <- maptiles::get_tiles(x = wilson_overview_bb,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 6, forceDownload = TRUE)

wilson_overview <-
  tm_shape(wilson_overview_bb_osm, is.master = TRUE) +
    tm_rgb() +
  tm_shape(wilson_bb_sfc) +
    tm_borders(col = "red", lwd = 3) +
  tm_scale_bar(text.size = .75, breaks = c(0, 50, 100),
    position = c(.3, -.03))
wilson_overview

# Wilson Wind Area Scenario Maps -----------------------------------------------

wilson_n_area <- readRDS(file.path(wind_input_dir, "wilson_n_area.rds"))
wilson_s_area <- readRDS(file.path(wind_input_dir, "wilson_s_area.rds"))

tmap_wilson_wind_areas <-
  tm_layout(asp = 1) +
  tm_shape(wilson_bb_sfc, is.master = TRUE, ext = .935) +
    tm_borders(col = "red") +
  tm_shape(wilson_natgeo_osm, raster.downsample = FALSE) +
    tm_rgb() +
  tm_shape(nest_wilson, title = "Wilson Nest") +
    tm_bubbles(col = nest_color, border.lwd = 1,  size = .4,
    border.col = "black") +
  tm_shape(wilson_n_area, title = "Wilson Nest") +
    tm_polygons(col = wind_area_color, border.col = "black",  lwd = 1) +
  tm_shape(wilson_s_area, title = "Wilson Nest") +
    tm_polygons(col = wind_area_color, border.col = "black",  lwd = 1) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    position = c(.85, .87)) +
  tm_scale_bar(text.size = .75, breaks = c(0, 1, 2), position = c(.05, .01))
tmap_wilson_wind_areas

tmap_save(tm = tmap_wilson_wind_areas, filename = file.path(tex_dir,
  "Figures/Ch4", "Wilson_Scenarios", "Wilson_Wind_Areas.svg"),
  insets_tm = wilson_overview, insets_vp =  viewport(x = 0.853, y = .141,
  width = 0.25, height = 0.25), unit = "in", dpi = 300, height = 6, width = 6.1)

# Wilson Turbine Scenario Maps -------------------------------------------------

# Basemaps
wilson_natgeo_osm <- maptiles::get_tiles(x = wilson_bb_sfc,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 12, forceDownload = TRUE)

tmap_wilson_c <-
  tm_layout(asp = 1) +
  tm_shape(wilson_bb_sfc, is.master = TRUE, ext = .935) +
    tm_borders(col = "red") +
  tm_shape(wilson_natgeo_osm, raster.downsample = FALSE) +
    tm_rgb() +
  tm_shape(nest_wilson, title = "Wilson Nest") +
    tm_bubbles(col = nest_color, border.lwd = 1,  size = .4,
    border.col = "black") +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    position = c(.85, .87)) +
  tm_scale_bar(text.size = .75, breaks = c(0, 1, 2), position = c(.05, .01))

tmap_wilson_n <- tmap_wilson_c +
  tm_shape(wilson_wt_n_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color, border.col = "black",  lwd = 1)

tmap_wilson_s <- tmap_wilson_c +
  tm_shape(wilson_wt_s_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color, border.col = "black",  lwd = 1)

tmap_wilson_ns <- tmap_wilson_n +
  tm_shape(wilson_wt_n_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color, border.col = "black",  lwd = 1) +
  tm_shape(wilson_wt_s_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color, border.col = "black",  lwd = 1)

tmap_save(tm = tmap_wilson_c, filename = file.path(tex_dir, "Figures/Ch4",
  "Wilson_Scenarios", "Wilson_C.svg"), insets_tm = wilson_overview,
  insets_vp =  viewport(x = 0.853, y = .141, width = 0.25, height = 0.25),
  unit = "in", dpi = 300, height = 6, width = 6.1)
tmap_save(tm = tmap_wilson_n, filename = file.path(tex_dir, "Figures/Ch4",
  "Wilson_Scenarios", "Wilson_N.svg"), insets_tm = wilson_overview,
  insets_vp =  viewport(x = 0.853, y = .141, width = 0.25, height = 0.25),
  unit = "in", dpi = 300, height = 6, width = 6.1)
tmap_save(tm = tmap_wilson_s, filename = file.path(tex_dir, "Figures/Ch4",
  "Wilson_Scenarios", "Wilson_S.svg"), insets_tm = wilson_overview,
  insets_vp =  viewport(x = 0.853, y = .141, width = 0.25, height = 0.25),
  unit = "in", dpi = 300, height = 6, width = 6.1)
tmap_save(tm = tmap_wilson_ns, filename = file.path(tex_dir, "Figures/Ch4",
  "Wilson_Scenarios", "Wilson_NS.svg"), insets_tm = wilson_overview,
  insets_vp =  viewport(x = 0.853, y = .141, width = 0.25, height = 0.25),
  unit = "in", dpi = 300, height = 6, width = 6.1)

# Wilson SSF Maps --------------------------------------------------------------

# SSF Fits
ssf_fits_best_org <- readRDS(fits_best_file) #%>% slice(c(step_type_index))
ssf_fits_best <- ssf_fits_best_org

exp_dir <- "C:/ArcGIS/Data/R_Input/EXP"
exp_scenarios <- list.dirs(exp_dir, recursive = FALSE)

# Get nest
nest_wilson <- nests_study %>% slice(c(5)) %>% st_transform(wgs84n19)
nest_wilson_name <- nest_wilson %>% pull(name)

# For Individual Scenario Maps
for (j in seq_len(length(exp_scenarios))){
  exp_scenario_j <- exp_scenarios[j]
  exp_scenario_j_name <- basename(exp_scenarios[j])
  ssf_tmap_list <- vector(mode = "list", length = 20)
  for (i in seq_len(nrow(ssf_fits_best))){
    step_type_i_numeric <- ssf_fits_best %>% slice(i) %>% pull(step_type) %>%
      str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
        "perch" = "4", "roost" = "5"))
    ssf_prob_dir <- file.path(exp_scenario_j, "Step_Types_Prob")
    ssf_prob_file <- list.files(ssf_prob_dir, pattern =
      paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
    if (i ==  1){
      # Use "Tmap_baselayers.R" script to get other baselayers
      nest_bbox <- st_as_sfc(st_bbox(st_buffer(nest_wilson, dist = 10000)))
      nest_buffer <- st_buffer(nest_wilson, dist = 10000)
      nest_bb_sf <- st_as_sfc(bb(nest_buffer, relative = TRUE, height = 1.35,
        width = 1.35))
      Sys.sleep(1)
      nest_om = read_osm(nest_bb_sf, type = om_nat_geo, zoom = 11)
        #type = "osm", minNumTiles=9,
      nest_om_bb <- bb_poly(nest_om)
    }
    ssf_prob_i <- raster(ssf_prob_file) #%>% slice(1)
    ssf_prob_i_crop <- crop(ssf_prob_i, nest_buffer)
    ssf_prob_i_mask <- mask(ssf_prob_i_crop, nest_buffer)
    step_type_i_text <- step_type_i_numeric %>%
      str_replace_all("1", "Cruise") %>%
      str_replace_all("2", "Flight") %>%
      str_replace_all("3", "Nest") %>%
      str_replace_all("4", "Perch") %>%
      str_replace_all("5", "Roost")
    writeLines(paste0("Mapping: ", step_type_i_text))
    step_type_i_arrow <- step_type_i_text %>%
      str_replace_all("_", "$\\\\rightarrow$ ") %>%
      latex2exp::TeX(.)
    ssf_prob_i_nest_map <-
      tm_shape(nest_om) +
        tm_rgb() +
     tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
     tm_raster(palette = viridis(20, direction = 1), alpha = .6,
       legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_scale_bar(breaks = c(0, 5, 10), text.size = .4, lwd = .25,
        position = c(.03, .0)) +
      tm_compass(type = "4star", text.size = 0.55, show.labels = 1, size = 1.75,
        position = c(.8, .775), lwd = .25) +
      tm_shape(nest_wilson) +
      tm_symbols(shape = 20, #border.col = "black", border.lwd = .5,
        col = "black", size = .075) +
      tm_layout(asp = .8,
        frame = NA, #"black",
        title.color = "black",
        title.bg.color = NA, #"ivory3",
        title.bg.alpha = .85,
        title.position = c(.275,.95),
        title.fontfamily = "Latin Modern Roman",
        title = step_type_i_arrow,
        title.size = .6,
        title.snap.to.legend = FALSE,
        legend.bg.color = "ivory1",
        legend.frame = "grey",
        legend.frame.lwd = 1,
        legend.height = .4,
        legend.title.size = .4,
        legend.text.size = .35,
        legend.position = c(.785,.007),
        legend.outside = FALSE,
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman")
        #+ tm_credits(step_type_arrow, position=c("right","top"))
    #ssf_prob_i_nest_map
    tmap_position <- switch(step_type_i_numeric,
      "1_1" = 1,  "1_2" = 2,  "1_4" = 3,
      "2_1" = 5,  "2_2" = 6,  "2_4" = 7,  "2_5" = 8,
      "3_1" = 9,  "3_2" = 10, "3_4" = 11, "3_5" = 12,
      "4_1" = 13, "4_2" = 14, "4_4" = 15, "4_5" = 16,
                  "5_2" = 18, "5_4" = 19)
    writeLines(as.character(tmap_position))
    ssf_tmap_list[[tmap_position]] <- ssf_prob_i_nest_map
  }

  tmap_blank <-
    tm_shape(nest_om_bb, is.master = TRUE) +
      tm_fill(col = "white") +
    tm_shape(nest_buffer, is.master = TRUE) +
      tm_polygons(col = "white", border.col = "white") +
    tm_layout(asp = .8, legend.show = FALSE, frame = FALSE)

  for (i in seq_len(length(ssf_tmap_list))){
    if(is.null(ssf_tmap_list[[i]])) ssf_tmap_list[[i]] <- tmap_blank
  }

  # Arrange map of probability surfaces for testing
  ssf_tmap_nest_arrange <- tmap_arrange(
    ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]],
    ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
    ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
    ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
    ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
    ssf_tmap_list[[16]], ssf_tmap_list[[17]], ssf_tmap_list[[18]],
    ssf_tmap_list[[19]], ssf_tmap_list[[20]], ncol = 4)

  tmap_save(tm = ssf_tmap_nest_arrange, filename = file.path(tex_dir,
    "Figures/Ch4/SSF_Prob_Raster_Maps", paste0("SSF_Probability_Maps_",
    exp_scenario_j_name, ".png")), unit = "in", dpi = 300,
    height = 8, width = 8*.8)
}


# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #

# ## Wilson Scenarios Map ---------------------------------------------------- #
#
# pacman::p_load(units, stringr)
#
# # Filter data, create fightpaths
# wilson <- nests_study %>% filter(name == "Wilson")  %>%
#   st_transform(crs = 32619)
#
# wilson_map_center <- wilson
# sfc <- st_sfc(st_point(c(st_coordinates(wilson)[1] + 300,
#   st_coordinates(wilson)[2] - 750)))
# st_geometry(wilson_map_center) <- sfc
# st_crs(wilson_map_center) <- 32619
#
# wilson_bb <- st_buffer(wilson_map_center, 4000) %>% bb(.)
# mapview(wilson_bb)
#
# wind_wilson_n <- st_crop(wind_class, wilson_bb) %>% filter(WPC >= 3) %>%
#   filter(ID %in% c(7139, 7172, 7173, 7201, 7236, 7261, 7262, 7263, 7292))
#
# wind_wilson_s_all <- st_crop(wind_class, wilson_bb) %>% filter(WPC >= 3) %>%
#   filter(ID %in% c(7626, 7659, 7660, 7662, 7701, 7702, 7762, 7778))
# wind_7662 <- wind_class %>% filter(ID == 7662)
# wind_7662_grid <- st_make_grid(wind_7662, 200)
# wind_7662_grid_sub <- wind_7662_grid[c(8:12, 15:19)]
# wind_7662_crop <- st_crop(wind_7662, wind_7662_grid_sub)
# wind_wilson_s <- rbind(wind_wilson_s_all %>% filter(ID != 7662), wind_7662_crop)
#
# wind_wilson_n_union <- st_union(wind_wilson_n)
# set_units(wind_wilson_n_union %>% st_area(.), km^2)
# wind_wilson_s_union <- st_union(wind_wilson_s)
# set_units(wind_wilson_s_union %>% st_area(.), km^2)
#
# # TEST SECTION (For placement of turbines in cells) ------------------------ #
#
# wind_wilson_n_grid <- sf::st_make_grid(bb(wind_wilson_n), 30) %>%
#   st_cast("POLYGON")
# # find Maine Raster cell corner closest to grid polygons?
#
# in_footprint <- lengths(st_intersects(wind_wilson_n_grid, wind_wilson_n)) > 0
#
# wind_wilson_n_rast <- st_sf(in_footprint = in_footprint, wind_wilson_n_grid) %>%
#   filter(in_footprint == TRUE)
#
# wind_wilson_n_rast
#
# mapview(wind_wilson_n_rast)
#
# ggplot() +
#   geom_sf(aes(color = in_footprint), data = wind_wilson_n_rast)
#   geom_sf(data = wind_wilson_n)
#
# st_write(wind_wilson_n_rast, "Wilson_N_Grid.kml", driver='kml', update=TRUE)
#
# wind_wilson_n
# test %>% head()
# library(tidyverse)
# x = st_sf(a = "TEST", geom = test)
# test2 <- x %>% slice(1:10)
# test2 <- test %>% st_as_sf(.)
#
# ggplot() +
#   geom_sf(data = wind_wilson_n)+
#   geom_sf(data = test2)
#
#
# # END TEST SECTION --------------------------------------------------------- #
#
# wind_wilson_ns <- rbind(wind_wilson_n, wind_wilson_s)
#
# # Get osm baselayer for wilson
# wilson_bb_sf <- st_as_sfc(bb(wilson_bb, relative = TRUE, height = 1,
#   width = 1))
# wilson_om <- read_osm(wilson_bb_sf, zoom = 13, minNumTiles = 21,
#   type = om_nat_geo)  # may need to add/adjust 'zoom'
#
# wind_wilson_ns <- wind_wilson_ns %>%
#   mutate(Rating = as.character(WPC)) %>%
#   mutate(Rating = str_replace_all(Rating, "4", "(Good)")) %>%
#   mutate(Rating = str_replace_all(Rating, "5", "(Excellent)")) %>%
#   mutate(Rating = str_replace_all(Rating, "6", "(Outstanding)")) %>%
#   mutate("Wind Power Class" = paste(WPC, Rating))
#
# # Wilson Map
# wilson_map <-
#   tm_layout(asp = 1) +
#   tm_shape(wilson_bb_sf, is.master = TRUE) +
#     tm_fill(col = NA) +
#   tm_shape(wilson_om) +
#     tm_rgb() +
#   tm_shape(wilson, title = "Wilson Nest") +
#     tm_bubbles(col = "yellow",  border.lwd = 3,  size = .75) +
#   tm_shape(wind_wilson_ns) +
#     tm_fill("Wind Power Class", lwd = 2, alpha = .5,
#       style = "cat", palette = "YlOrBr") + # brewer.pal(5, "RdGy")[3]
#     tm_borders("black", lwd = .5) +
#   tm_shape(wind_wilson_n_union) +
#     tm_borders("black", lwd = 3) +
#   tm_shape(wind_wilson_s_union) +
#     tm_borders("black", lwd = 3) +
#   tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
#     main.title.position = "center",
#     main.title.size = 1.15,
#     title.snap.to.legend = TRUE) +
#   tm_legend(title.size = 1, text.size = .85,
#     outside = FALSE, position = c("center", "bottom")) +
#   tm_scale_bar(size = .75, width = .2, breaks = c(0, 1, 2),
#     position = c(.05, .01)) +
#   tm_compass(type = "4star",  show.labels = 1, size = 2.5,
#     position = c(.85, .87)) +
#   tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
#     labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
#     labels.inside.frame = FALSE) +
#   tm_xlab("") + tm_ylab("")
# wilson_map
# #tmaptools::palette_explorer()
#
# # wilson Overview Map
# wilson_overview_center <- wilson
# sfc <- st_sfc(st_point(c(st_coordinates(wilson)[1] - 5000,
#   st_coordinates(wilson)[2] - 70000)))
# st_geometry(wilson_overview_center) <- sfc
# st_crs(wilson_overview_center) <- 32619
#
# wilson_overview_buff <- st_buffer(wilson_overview_center, 130000) %>% bb(.)
# mapview(wilson_overview_buff)
# wilson_overview_bb <- bb_poly(bb(wilson_overview_buff, ext = 1))
# om_type <- paste0(esri_url, "NatGeo_World_Map", esri_tile)
# wilson_overview_bb_om = read_osm(wilson_overview_bb, zoom = 6, minNumTiles = 21,
#   type = om_nat_geo)
#
# wilson_overview <-
#   tm_shape(wilson_overview_bb_om, is.master = TRUE) +
#     tm_rgb() +
#   tm_shape(wilson_bb_sf) +
#     tm_borders(col = "red", lwd = 3) +
#   tm_scale_bar(size = .75, width = .2, breaks = c(0, 50, 100),
#     position = c(.52, -.03))
# wilson_overview
#
# tmap_save(tm = wilson_map, filename = file.path(maps_dir, "Wilson_Buildout",
#   "wilson_map.svg"), insets_tm = wilson_overview,
#   insets_vp =  viewport(x = 0.85, y = 0.167, width = 0.25, height = 0.25),
#   unit = "in", dpi = 300, height = 6, width = 6.1)
#
# ------------------- Ellis Turbine Distance Map ---------------------------- #
#
# pacman::p_load(units, stringr)
#
# # Rasters
# base <- raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))
#
# # Filter nest data
# ellis <- nests_study %>% filter(name == "Ellis")  %>%
#   st_transform(crs = 32619)
#
# ellis_map_center <- ellis
# sfc <- st_sfc(st_point(c(st_coordinates(ellis)[1] + 2250,
#   st_coordinates(ellis)[2] - 700)))
# st_geometry(ellis_map_center) <- sfc
# st_crs(ellis_map_center) <- 32619
#
# ellis_bb <- st_buffer(ellis_map_center, 5500) %>% bb(.)
# mapview(ellis_bb)
#
# wt_ellis <- st_crop(turbines, ellis_bb)
# mapview(wt_ellis)
#
# # Get osm baselayer for ellis
# ellis_bb_sf <- st_as_sfc(bb(ellis_bb, relative = TRUE, height = 1,
#   width = 1))
# ellis_down <- read_osm(ellis_bb_sf, zoom = 13, minNumTiles = 21,
#   type = om_nat_geo)  # may need to add/adjust 'zoom'
# ellis_om <- RasterizeOMDownload(ellis_down)
#
# # Ellis Map Raster
# ellis_raster <- crop(base, as_Spatial(ellis_bb_sf))
# wt_dist <- distanceFromPoints(ellis_raster, wt_ellis)
# wt_dist[wt_dist > 2000] = NA
# wt_dist_shift <- shift(wt_dist, 50000, 0)
#
# ellis_ext_sf <- st_as_sfc(bb(st_buffer(ellis_map_center, 5500) %>%
#   st_transform(., crs = crs(ellis_om)), ext = .95))
#
# ellis_map <-
#   tm_layout(asp = 1) +
#   tm_shape(ellis_ext_sf) +
#     tm_fill(col = NA) +
#   tm_shape(ellis_om) +
#     tm_rgb() +
#   tm_shape(ellis, title = "Ellis Nest") +
#     tm_bubbles(col = "yellow", border.lwd = 3,  size = .75) +
#   tm_shape(wt_dist) +
#     tm_raster("layer", palette = "-plasma", alpha = .6, style = "cont",
#      legend.show = FALSE)  +
#   tm_shape(wt_dist_shift) +
#     tm_raster("layer", palette = "-plasma", alpha = 1, style = "cont",
#       breaks = c(0, 500, 1000, 1500, 2000),
#       title = "Turbine Distance (m)", legend.show = TRUE)  +
#   tm_shape(turbines, title = "Wind Turbines") +
#     tm_symbols(col = "black", shape = 4,  border.lwd = 2,  size = .25) +
#   tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
#     main.title.position = "center",
#     main.title.size = 1.15,
#     title.snap.to.legend = TRUE) +
#   tm_legend(title.size = 1, text.size = .85,
#     outside = FALSE, position = c("left", "top"), frame = TRUE,
#     legend.bg.color = "white", legend.format = list(format = "f",
#     big.mark = "")) +
#   tm_compass(type = "4star",  show.labels = 1, size = 2.5,
#     position = c(.85, .87)) +
#   tm_scale_bar(text.size = .75, breaks = c(0, 1, 2), position = c(.05, .01)) +
#   tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
#     labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
#     labels.inside.frame = FALSE) +
#   tm_xlab("") + tm_ylab("")
# ellis_map
# #tmaptools::palette_explorer()
#
# # ellis Overview Map
# ellis_overview_center <- ellis
# sfc <- st_sfc(st_point(c(st_coordinates(ellis)[1] + 50000,
#   st_coordinates(ellis)[2] - 0)))
# st_geometry(ellis_overview_center) <- sfc
# st_crs(ellis_overview_center) <- 32619
#
# ellis_overview_buff <- st_buffer(ellis_overview_center, 120000) %>% bb(.)
# mapview(ellis_overview_buff)
# ellis_overview_bb <- bb_poly(bb(ellis_overview_buff, ext = 1))
# ellis_overview_bb_om <- read_osm(ellis_overview_bb, zoom = 6, minNumTiles = 21,
#   type = om_type)
#
# ellis_overview <-
#   tm_shape(ellis_overview_bb_om, is.master = TRUE) +
#     tm_rgb() +
#   tm_shape(ellis_bb_sf) +
#     tm_borders(col = "red", lwd = 3) +
#   tm_scale_bar(size = .75, width = .2, breaks = c(0, 50, 100),
#     position = c(.45, -.03))
# ellis_overview
#
# tmap_save(tm = ellis_map, filename = file.path(maps_dir, "Ellis_Turbines",
#   "ellis_map.svg"), insets_tm = ellis_overview,
#   insets_vp =  viewport(x = 0.85, y = 0.167, width = 0.25, height = 0.25),
#   unit = "in", dpi = 300, height = 6, width = 6.1)
#
# # Ellis Map Polygons (Not currently used, but may be useful)
#
# wt_buff_400 <- st_buffer(wt_ellis, c(400)) %>% st_union(.)
# wt_buff_600 <- st_buffer(wt_ellis, c(600)) %>% st_union(.)
# wt_buff_800 <- st_buffer(wt_ellis, c(800)) %>% st_union(.)
# wt_buff_1000 <- st_buffer(wt_ellis, c(1000)) %>% st_union(.)
#
# wt_buffs_200 <- st_buffer(wt_ellis, c(200)) %>% st_union(.)
# wt_buffs_400 <- st_sym_difference(wt_buff_400, wt_buff_200)
# wt_buffs_600 <- st_sym_difference(wt_buff_600, wt_buff_400)
# wt_buffs_800 <- st_sym_difference(wt_buff_800, wt_buff_600)
# wt_buffs_1000 <- st_sym_difference(wt_buff_1000, wt_buff_800)
#
# ellis_map_polys <-
#   tm_layout(asp = 1) +
#   tm_shape(ellis_bb_sf, is.master = TRUE) +
#     tm_fill(col = NA) +
#   tm_shape(ellis_om) +
#     tm_rgb() +
#   tm_shape(ellis, title = "Ellis Nest") +
#     tm_bubbles(col = "yellow", border.lwd = 3,  size = .75) +
#   tm_shape(wt_buffs_1000, title = "Wind Turbine Buffers") +
#      tm_polygons(col = viridis(10, option = v_col)[3], alpha = v_alpha,
#        border.lwd = 3) +
#   tm_shape(wt_buffs_800, title = "Wind Turbine Buffers") +
#      tm_polygons(col = viridis(10, option = v_col)[4], alpha = v_alpha,
#        border.lwd = 3) +
#   tm_shape(wt_buffs_600, title = "Wind Turbine buffers") +
#      tm_polygons(col = viridis(10, option = v_col)[5], alpha = v_alpha,
#        border.lwd = 3) +
#   tm_shape(wt_buffs_400, title = "Wind Turbine Buffers") +
#      tm_polygons(col = viridis(10, option = v_col)[6], alpha = v_alpha,
#        border.lwd = 3) +
#   tm_shape(wt_buffs_200, title = "Wind Turbine Buffers") +
#      tm_polygons(col = viridis(10, option = v_col)[7], alpha = v_alpha,
#        border.lwd = 3) +
#   tm_shape(turbines, title = "Wind Turbines") +
#     tm_symbols(col = "black", shape = 4,  border.lwd = 2,  size = .25) +
#   tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
#     main.title.position = "center",
#     main.title.size = 1.15,
#     title.snap.to.legend = TRUE) +
#   tm_legend(legend.show = FALSE, title.size = 1, text.size = .85,
#     outside = FALSE, position = c("left", "top"), frame = TRUE,
#     legend.bg.color = "white", legend.format = list(format = "f",
#     big.mark = "")) +
#   tm_compass(type = "4star",  show.labels = 1, size = 2.5,
#     position = c(.85, .87)) +
#   tm_scale_bar(size = .75, width = .2, breaks = c(0, 1, 2),
#     position = c(.05, .01)) +
#   tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
#     labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
#     labels.inside.frame = FALSE) +
#   tm_xlab("") + tm_ylab("")
# ellis_map_polys



# tmap_blank <-
#   tm_shape(ssf_prob_i, raster.downsample = TRUE) +
#     tm_raster(col = "white", style = "cont") +
#   tm_layout(asp = .8, legend.show = FALSE, frame = FALSE)
#
# # TEST arrangement and position of main.title, legend, etc.
# ssf_tmap_arrange_test <- tmap_arrange(
#   tmap_blank, tmap_blank, tmap_blank, tmap_blank,
#   tmap_blank, tmap_blank, tmap_blank, tmap_blank,
#   tmap_blank, tmap_blank, ssf_tmap_list[[10]], tmap_blank,
#   tmap_blank, tmap_blank, tmap_blank, tmap_blank,
#   tmap_blank, tmap_blank, tmap_blank, tmap_blank,
#   ncol = 4)
#
#tmap_save(tm = ssf_tmap_arrange_test, filename =  file.path("C:/TEMP",
#  "SSF_Probability_Maps_Overview.svg"), unit = "in", dpi = 300, height = 8,
#  width = 6)


# ### Flightpath Maps ------------------------------------------------------- ##
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
# ### Isopleth Maps -------------------------------------------------------- ##
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
# ### ------------------------- Hexbin Maps -------------------------------- ##
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

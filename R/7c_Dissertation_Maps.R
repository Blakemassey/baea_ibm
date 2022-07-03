#--------------------------- DISSERTATION MAPS --------------------------------#
# This script is for generating all of the maps for my dissertation
#------------------------------------------------------------------------------#

# Load packages, scripts, and input parameters ---------------------------------
pacman::p_load(gisr, baear, cartography, ctmm, dplyr, fasterize, ggplot2,
  ggthemes, grid, leaflet, lubridate, magick, maptiles, mapview, OpenStreetMap,
  plotly, prettymapr, purrr, raster, rosm, rsvg, sf, stars, stringr, tidyr,
  tmap, tmaptools, viridis, units, webshot, zoo)
suppressMessages(extrafont::loadfonts(device="win"))
pacman::p_load(baear, gisr, ibmr)
theme_update(plot.title = element_text(hjust = 0.5))

# Variables
sim_id <- "sim_20210725"
sims <- c(77,85,86,87,88)
match_baea <- TRUE

# Coordinate systems
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

# Directories
baea_hr_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Data/BAEA"
exp_output_dir <- "Output/Experiment"
exp_turbines_dir <- "C:/ArcGIS/Data/R_Input/EXP"
line_density_dir <- "Line_Density_Rasters"
line_density_agg_dir <- "Agg_10"
ridgeline_dir <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines"
sim_dir <- "C:/TEMP"
akde_dir <- file.path(sim_dir, sim_id, "AKDEs")
baea_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Data/BAEA"
nests_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Nests/Nests_rds")
places_fl_dir <- file.path("C:/ArcGIS/Data/Reference/cb_2018_12_place_500k")
places_me_dir <- file.path("C:/ArcGIS/Data/Reference/cb_2018_23_place_500k")
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"
ssf_prob_dir <- file.path(ssf_raster_dir, "Step_Types_Prob")
states_dir <- file.path("C:/ArcGIS/Data/Reference/cb_2018_us_state_5m")
roads_dir <- file.path("C:/ArcGIS/Data/Reference/tl_2016_us_primaryroads")
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"
turbine_dir <- file.path("C:/ArcGIS/Data/R_Input/BAEA")
wind_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Wind")
wind_input_dir <- "Output/Analysis/Wind"
gis_exp_dir <- "C:/ArcGIS/Data/R_Input/EXP"
maps_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Products/Maps"
report_maps_dir <- file.path("C:/Users/blake/Documents/PhD Program/Reports",
  "BAEA Project - 2018/Maps")
mod_dir <- "Output/Analysis/SSF/Models"
mod_fit_dir <- file.path(mod_dir, "model_fits")
mod_best_dir <- file.path(mod_dir, "model_fits_best")
sim_id_dir <- file.path("C:/TEMP", paste0(sim_id, "-"))
sim_step_data_dir <- "Step_Data"
me_turbines_input_dir <- "C:/ARCGIS/Data/Wind"

# Files
base_file <- file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")
baea_hr_file <- file.path(baea_hr_dir, "baea_homerange.rds")
fits_best_file <- file.path(mod_best_dir, "model_fits_best.rds")
preds_tbl_file <- file.path(mod_best_dir, "preds_tbl.rds")
me_turbines_buff_file <- file.path(me_turbines_input_dir,
  "ME_Turbines_Buffer.rds")
ridge_poly_file <- file.path(ridgeline_dir, "ridge_poly.shp")
maine_file <- "C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp"
ssf_prob_files <- list.files(ssf_prob_dir, pattern = "\\.tif", full.names =TRUE)
ssf_tmap_list <- vector(mode = "list", length = length(ssf_prob_files))

# Base Raster
base <- raster(base_file)

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

# Colors
turbine_color <- "darkorange"
nest_color <- "#FF7F00" # orange
wind_area_color <- "#984EA3" # dark purple
turbine_color <- "white"
turbine_color_present <- "white"
turbine_color_absent <- "black"

# Map component sizes
credits_size <- 1
nest_size <- .35

# Map Options
tm_latex <- tm_layout(legend.title.fontfamily = "Latin Modern Roman",
  legend.text.fontfamily = "Latin Modern Roman")

tmap_mode("plot")

# CHAPTER 2 --------------------------------------------------------------------

# Nests Overview Map -----------------------------------------------------------

#maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1.15, width = 2))
maine_bb_sf <- st_as_sfc(bb(maine %>% st_transform(., crs = crs(maine)),
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

nests_overview <- tm_layout(fontfamily = "Latin Modern Roman",
    asp = .75, main.title = NULL) +
  tm_shape(maine_bb_sf, is.master = TRUE, ext = .85) +
    tm_borders(col = NA) +
  tm_shape(maine_physical_osm) +
    tm_rgb() +
  tm_shape(nests_study_sf) +
    tm_symbols(nest_color, size = .4, border.col = "black", border.lwd = 1.25) +
  tm_shape(nests_text_sf) +
	  tm_text(text = "name", fontface = "bold", just = c("left", "top"),
	    size = .9) +
  tm_shape(roads_crop) +
    tm_lines(col = "blue4", alpha = .8) +
  tm_shape(states) +
    tm_borders(col = "black", alpha = .8, lwd = 2.5) +
  tm_shape(places_me_sub) +
    tm_dots(shape = 21, col = "grey50", size = .25, border.lwd = 1,
    border.col = "grey20") +
  tm_shape(places_me_text_sf) +
    tm_text(text = "NAME", just = c("left", "top"), size = .7) +
  tm_credits("MAINE", size = 1.25, position = c(.44, .675)) +
  tm_logo(file = "Data/Assets/Icons/200px-I-95.svg.png", height = 1,
    position = c(.39, .39)) +
  tm_legend(title.size = 1, text.size = .9, outside = FALSE,
    position = c("right", "bottom")) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.75, text.size = 1.25,
    position = c(.8, .85)) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.2, position = c(.4, .01))
#nests_overview

tmap_save(tm = nests_overview, filename = file.path(tex_dir, "Figures/Ch2",
  "Trapping_Sites_Overview.svg"), unit = "in", dpi = 300, height = 8, width = 6)

# Map of Webb Eagle Migration --------------------------------------------------

baea_webb_bb_sfc <- bb(x = c(-86.5, 24, -66.5, 48.5),
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

if(FALSE) tmaptools::palette_explorer()
col_south  <- viridisLite::plasma(6)[6] #brewer.pal(8, "Set1")[5]
col_north  <- viridisLite::plasma(6)[5] #brewer.pal(8, "Set1")[6]
col_winter <- viridisLite::plasma(6)[2] #brewer.pal(8, "Set1")[2]
col_summer <- viridisLite::plasma(6)[3] #brewer.pal(8, "Set1")[3]

flight_width = 2.5
flight_alpha = .95

# All flight paths and points
tmap_webb <-
  tm_layout(asp = .5, fontfamily = "Latin Modern Roman") +
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
  tm_scale_bar(text.size = 1.25, breaks = c(0, 200, 400),
    position = c(.575, 0)) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.25, text.size = 1,
    position = c(.835, .7)) +
  tm_legend(title.size = 1.5, text.size = 1, legend.width = .65,
    outside = FALSE, position = c(.4, .15))

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
  	#col = viridisLite::plasma(6)[c(3, 6, 2, 5)],
    col = c(col_summer, col_south, col_winter, col_north),
  	border.col = "grey40", lwd = 3, size = 2,
    labels = c('2015 Breeding Season', '2015 Fall Migration',
      '2015/16 Winter Season', '2016 Spring Migration'),
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
  tm_latex +
  tm_add_legend('line',
  	#col = viridisLite::plasma(6)[c(3, 6, 2, 5)],
    col = c(col_summer, col_south, col_winter, col_north),
  	border.col = "grey40", lwd = 3, size = 2,
    labels = c('2016 Breeding Season', '2016 Fall Migration',
      '2016/17 Winter Season', '2017 Spring Migration'),
  	title = "     Flight Paths")

tmap_webb_2015_2017 <- tmap_arrange(list(tmap_webb_paths_2015_16,
  tmap_webb_paths_2016_17), ncol = 2, nrow = 1, widths = c(.5, .5), asp = NA)

tmap_save(tm = tmap_webb_2015_2017, filename = file.path(tex_dir, "Figures/Ch2",
  "Webb_Flights.svg"), unit = "in", dpi = 300, height = 6, width = 8, asp = NA)

# Map of Webb Florida Sites ----------------------------------------------------

# Florida Area
#baea_webb_fl_bb_sfc <- bb(x = c(-83, 27.6, -79.5, 31.6),
#  current.projection = 4326) %>% st_bbox(.) %>% st_as_sfc(.)
baea_webb_fl_bb_sfc <- bb(x = c(-83, 27.4, -79.5, 31.8),
  current.projection = 4326) %>% st_bbox(.) %>% st_as_sfc(.)

# Create States text layer
places_fl_sub <- places_fl %>%
  filter(NAME %in% c("Gainesville", "Daytona Beach", "Jacksonville", "Orlando",
    "Tampa")) %>%
  mutate(NAME = str_to_upper(NAME)) %>%
  mutate(NAME = str_replace(NAME, "DAYTONA BEACH", "DAYTONA\nBEACH"))

places_fl_text <- places_fl_sub %>%
  dplyr::select(NAME) %>%
  st_centroid(.) %>%
  mutate(long = unlist(map(.$geometry, 1)),
         lat = unlist(map(.$geometry, 2))) %>%
  st_drop_geometry(.)

places_fl_text[which(places_fl_text$NAME == "JACKSONVILLE"), "lat"] <- 30.25
places_fl_text[which(places_fl_text$NAME == "JACKSONVILLE"), "long"] <- -82.2
places_fl_text[which(places_fl_text$NAME == "ORLANDO"), "long"] <- -82.15
places_fl_text[which(places_fl_text$NAME == "TAMPA"), "lat"] <- 28.15

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
  tm_layout(fontfamily = "Latin Modern Roman", asp = .5) +
  tm_shape(baea_webb_fl_bb_sfc, is.master = TRUE, ext = .935) +
    tm_borders(col = "grey80") +
  tm_shape(webb_fl_physical_osm, raster.downsample = FALSE) +
    tm_rgb() +
  tm_shape(states) +
    tm_borders(col = "grey20", alpha = .8) +
  tm_layout(main.title = NULL,
    title.snap.to.legend = FALSE) +
  tm_latex +
  tm_compass(type = "4star",  show.labels = 1, size = 2.25, text.size = 1,
    position = c(.82, .865)) +
  tm_scale_bar(text.size = 1.25, breaks = c(0, 50, 100),
    position = c(.25, -0.01)) +
  tm_legend(title.size = 1.5, text.size = 1, legend.width = .65,
    outside = FALSE, position = c(.47, .69)) +
  tm_credits("GEORGIA", size = 1.1, position = c(.011, .81),
    fontfamily = "Latin Modern Roman") +
  tm_credits("FLORIDA", size = 1.1, position = c(.013, .565),
    fontfamily = "Latin Modern Roman")

tmap_webb_fl_2015_16 <- tmap_webb_fl +
  tm_shape(baea_webb_2015_winter) +
    tm_lines(col = col_winter, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2015_south) +
    tm_lines(col = col_south, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2016_north) +
    tm_lines(col = col_north, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(places_fl_sub) +
    tm_dots(shape = 21, col = "grey50", size = .25, border.lwd = 1,
    border.col = "grey10") +
  tm_shape(places_fl_text_sf) +
    tm_text(text = "NAME", just = c("left", "top"), size = .9,
      fontfamily = "Latin Modern Roman") +
  tm_add_legend('line', #col = viridisLite::plasma(6)[c(6, 2, 5)],
    col = c(col_south, col_winter, col_north),
    border.col = "grey40", size = 2, lwd = 3,
    labels = c('2015 Fall Migration', '2015/16 Winter Season',
      '2016 Spring Migration'), title = "     Flight Paths")

tmap_webb_fl_2016_17 <- tmap_webb_fl +
  tm_shape(baea_webb_2016_winter) +
    tm_lines(col = col_winter, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2016_south) +
    tm_lines(col = col_south, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(baea_webb_2017_north) +
    tm_lines(col = col_north, lwd = flight_width, alpha = flight_alpha) +
  tm_shape(places_fl_sub) +
    tm_dots(shape = 21, col = "grey50", size = .25, border.lwd = 1,
    border.col = "grey10") +
  tm_shape(places_fl_text_sf) +
    tm_text(text = "NAME", just = c("left", "top"), size = .9,
      fontfamily = "Latin Modern Roman") +
  tm_add_legend('line',
    #col = viridisLite::plasma(6)[c(6, 2, 5)],
    col = c(col_south, col_winter, col_north),
    border.col = "grey40", size = 2, lwd = 3,
    labels = c('2016 Fall Migration', '2016/17 Winter Season',
      '2017 Spring Migration'), title = "     Flight Paths")

tmap_webb_fl_2015_2017 <- tmap_arrange(list(tmap_webb_fl_2015_16,
  tmap_webb_fl_2016_17), ncol = 2, nrow = 1, widths = c(.5, .5), asp = NA)

tmap_save(tm = tmap_webb_fl_2015_2017, filename = file.path(tex_dir,
  "Figures/Ch2", "Webb_Florida.svg"), unit = "in", dpi = 300, height = 6,
  width = 8, asp = NA)

# Con and Home Nest Distance Map -----------------------------------------------

# Import Files
nests_active <- readRDS(file = "Data/Nests/Nests_rds/nests_active.RDS")
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
con_nest_om <- read_osm(con_nest_bb_sf, zoom = 7, minNumTiles = 21,
  type = om_nat_geo)

# Conspecific Nest Map

nests_home_ext_sf <- st_as_sfc(bb(nests_home_sf %>%
  st_transform(., crs = st_crs(3857)), ext = 1.25))
  # sets the extent of the map - should be smaller than *_om raster

con_nest_map <-
  tm_layout(asp = 1, fontfamily = "Latin Modern Roman") +
  tm_shape(nests_home_ext_sf, is.master = TRUE, ext = 1.1) +
    tm_fill(col = NA) +
  tm_shape(con_nest_om, raster.downsample = FALSE) +
    tm_rgb() +
  tm_shape(con_nest_km) +
  tm_raster("ConNest_All", palette = "-viridis", alpha = .8, style = "cont",
    title = "Conspecific\n     and\nHome Nest\n  Distance\nMetric (km)",
    legend.reverse = TRUE, legend.show = TRUE) +
  tm_shape(con_nests_sf) +
    tm_symbols(col = "grey20", shape = 4,  border.lwd = 2,  size = .35) +
  tm_shape(nests_home_sf) +
    tm_symbols(col = nest_color, border.lwd = 2,  size = .35,
      border.col = "black", border.alpha = 1) +
  tm_layout(outer.bg.color = 'grey80',
    main.title = NULL,
    title.snap.to.legend = TRUE) +
  tm_legend(show = FALSE) +
  tm_compass(type = "4star", show.labels = 1, size = 2.5, text.size = 1.25,
    position = c(.835, .835)) +
  tm_scale_bar(text.size = 1.25, breaks = c(0, 50, 100),
    position = c(.4, -0.01)) +
  tm_xlab("") + tm_ylab("")

con_nest_map_file <- file.path("C:/TEMP/TEMP_Images",
  "Con_Nest_Distance_Map.png")
tmap_save(tm = con_nest_map, filename = con_nest_map_file, unit = "in",
  dpi = 300, height = 6, width = 6)

legend_only <- tm_shape(con_nest_km) +
  tm_raster("ConNest_All", palette = "-viridis", alpha = .8, style = "cont",
    title = "Conspecific\n     and\nHome Nest\n  Distance\nMetric (km)",
    legend.reverse = TRUE, legend.show = TRUE) +
  tm_legend(legend.only = TRUE,
    title.size = 1.55,
    text.size = 1.25,
    position = c("LEFT", "TOP"),
    frame = FALSE,
    legend.bg.color = "white",
    legend.format = list(format = "f", big.mark = ""),
    fontfamily = "Latin Modern Roman")
legend_file <- file.path("C:/TEMP/TEMP_Images",
  "Con_Nest_Distance_Map_Legend.png")
tmap_save(tm = legend_only, filename = legend_file, unit = "in", dpi = 300,
  height = 4, width = 2.25)

con_nest_map_img <- con_nest_map_file %>%
  image_read(.) %>%
  image_trim(.)
legend_img <- legend_file %>%
  image_read(.) %>%
  image_trim(.)

backgrd <- image_blank(2125, 1730, color = "white")

con_nest_map_fig <- backgrd %>%
  image_composite(., con_nest_map_img, offset = "+0+0") %>%
  image_composite(., legend_img, offset = "+1780+400") %>%
  image_flatten(.)
con_nest_map_fig

# Export PNG
con_nest_fig_png_file = file.path(tex_dir, "Figures/Ch2",
  "Con_Nest_Distance_Map.png")
image_write(con_nest_map_png_fig, path = con_nest_fig_png_file, format = ".png")

# Export SVG
con_nest_fig_svg_file = file.path(tex_dir, "Figures/Ch2",
  "Con_Nest_Distance_Map.svg")
image_write(con_nest_map_fig, path = con_nest_fig_svg_file, format = "svg")

# SSF for Maine - Combined -----------------------------------------------------

# This process assumes that the 'final' version of the SSF probablity layers
# is in the ssf_prob_dir

for (i in seq_len(length(ssf_prob_files))){
  print(i)
  ssf_prob_i <- read_stars(ssf_prob_files[i])
  ssf_prob_i_max <- round(maxValue(raster(ssf_prob_files[i])), 2)
  ssf_prob_i_min <- round(minValue(raster(ssf_prob_files[i])), 2)
  if((ssf_prob_i_max) >= .75 & (ssf_prob_i_min) <= .5){
    raster_breaks <- seq(0, 1, by = .5)
    #legend_label <- "Legend\n   A"
    legend_label <- "A"
    legend_a_only <- tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
        legend.reverse = TRUE, style = "cont",
        title = "A)\nProbability") +
      tm_layout(legend.only = TRUE,
        legend.title.size = 1.25,
        legend.text.size = 1,
        fontfamily = "Latin Modern Roman")
    legend_a_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_A.png"))
    if(!file.exists(legend_a_file)){
      tmap_save(tm = legend_a_only, filename = legend_a_file, unit = "in",
        dpi = 300, height = .9, width = .9)
    }
  }
  if((ssf_prob_i_max) >= .75 & (ssf_prob_i_min) >= .5) {
    raster_breaks <- seq(.5, 1, by = .25)
    #legend_label <- "Legend\n   B"
    legend_label <- "B"
    legend_b_only <- tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
        legend.reverse = TRUE, style = "cont",
        title = "B)\nProbability") +
      tm_layout(legend.only = TRUE,
        legend.title.size = 1.25,
        legend.text.size = 1,
        fontfamily = "Latin Modern Roman")
    legend_b_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_B.png"))
    if(!file.exists(legend_b_file)){
      tmap_save(tm = legend_b_only, filename = legend_b_file, unit = "in",
        dpi = 300, height = .9, width = .9)
    }
  }
  if((ssf_prob_i_max) <= .5){
    raster_breaks <- seq(0, .5, by = .25)
    #legend_label <- "Legend\n   C"
    legend_label <- "C"
    legend_c_only <- tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
        legend.reverse = TRUE, style = "cont",
        title = "C)\nProbability") +
      tm_layout(legend.only = TRUE,
        legend.title.size = 1.25,
        legend.text.size = 1,
        fontfamily = "Latin Modern Roman")
    legend_c_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_C.png"))
    if(!file.exists(legend_c_file)){
      tmap_save(tm = legend_c_only, filename = legend_c_file, unit = "in",
        dpi = 300, height = .9, width = .9)
    }
  }
  step_type_numeric <- basename(tools::file_path_sans_ext(ssf_prob_files[i]))
  step_type_latex <- step_type_numeric %>% # This is no longer used
    str_replace_all("1", "Cruise") %>%
    str_replace_all("2", "Flight") %>%
    str_replace_all("3", "Nest") %>%
    str_replace_all("4", "Perch") %>%
    str_replace_all("5", "Roost") %>%
    str_replace_all("_", "$\\\\rightarrow$ ") %>%
    latex2exp::TeX(.)
  step_type <- step_type_numeric %>%
    str_replace_all("1", "Cruise") %>%
    str_replace_all("2", "Flight") %>%
    str_replace_all("3", "Nest") %>%
    str_replace_all("4", "Perch") %>%
    str_replace_all("5", "Roost")
  print(paste0(step_type, " ", ssf_prob_i_min, " ", ssf_prob_i_max))

  start_behavior <- str_split(step_type, "_") %>% pluck(., 1) %>% .[1]
  end_behavior <- str_split(step_type, "_") %>% pluck(., 1) %>% .[2]
  step_type <- paste0("$\\overset{", start_behavior,
    "$\\rightarrow$ phantom(x)}{", end_behavior, "}$") %>%
    latex2exp::TeX(.)
  ssf_prob_i_map <-
    tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
        legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_layout(asp = .8,
        fontfamily = "Latin Modern Roman",
        title.position = c("LEFT", "top"),
        title.fontfamily = "Latin Modern Roman",
        title = step_type,
        title.size = .7,
        title.snap.to.legend =  FALSE,
        legend.show = FALSE,
        legend.position = c("RIGHT", "BOTTOM"),
        legend.height = .5,
        legend.title.size = .5,
        legend.text.size = .75,
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman",
        frame = FALSE) +
      tm_credits(legend_label, position = c(.65, .1))
  ssf_prob_i_map
  ssf_tmap_list[[i]] <- ssf_prob_i_map
}

tmap_blank <- tm_shape(ssf_prob_i, raster.downsample = TRUE) +
  tm_raster(palette = "white", style = "cont") +
  tm_layout(asp = .75, legend.show = FALSE, frame = FALSE)

ssf_tmap_arrange <- tmap_arrange(
  ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]], tmap_blank,
  tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, ssf_tmap_list[[16]], ssf_tmap_list[[17]], tmap_blank,
  ncol = 4)

# ORIGINAL
ssf_tmap_arrange <- tmap_arrange(
  ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]], tmap_blank,
  ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
  ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
  ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
  ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
  tmap_blank, ssf_tmap_list[[16]], ssf_tmap_list[[17]], tmap_blank,
  ncol = 4)

maps_file <- file.path("C:/TEMP/TEMP_Images", "SSF_Maps.png")
tmap_save(tm = ssf_tmap_arrange, filename = maps_file, unit = "in",
  dpi = 300, height = 8, width = 8*(.8))

map_img <- maps_file %>% image_read(.) %>% image_trim(.)
legend_a_img <- legend_a_file %>% image_read(.) %>% image_trim(.)
legend_b_img <- legend_b_file %>% image_read(.) %>% image_trim(.)
legend_c_img <- legend_c_file %>% image_read(.) %>% image_trim(.)

backgrd <- image_blank(1850, 2395, color = "white")

ssf_maps_fig <- backgrd %>%
  image_composite(., map_img, offset = "+0+0") %>%
  image_composite(., legend_a_img, offset = "+1390+1930") %>%
  image_composite(., legend_c_img, offset = "+1620+2050") %>%
  image_composite(., legend_b_img, offset = "+1390+2170")

# Export
ssf_maps_fig_file <- file.path(tex_dir, "Figures/Ch2",
  "SSF_Prob_Raster_Maps", "SSF_Probability_Maps_Overview.png")
image_write(ssf_maps_fig, path = ssf_maps_fig_file, format = ".png")

file.remove(maps_file)
file.remove(legend_a_file)
file.remove(legend_b_file)
file.remove(legend_c_file)

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

    ssf_prob_i_max <- round(maxValue(ssf_prob_i_mask), 2)
    ssf_prob_i_min <- round(minValue(ssf_prob_i_mask), 2)
    if((ssf_prob_i_max) >= .75 & (ssf_prob_i_min) <= .5){
      raster_breaks <- seq(0, 1, by = .5)
      #legend_label <- "Legend\n   A"
      legend_label <- "A"
      legend_a_only <- tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
        tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
          legend.reverse = TRUE, style = "cont",
          title = "A)\nProbability") +
        tm_layout(legend.only = TRUE,
          outer.margins = c(0, 0, 0, 0),
          legend.title.size = 1.35,
          legend.text.size = 1.1,
          fontfamily = "Latin Modern Roman")
      legend_a_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_A.png"))
      if(!file.exists(legend_a_file)){
        tmap_save(tm = legend_a_only, filename = legend_a_file, unit = "in",
          dpi = 300, height = .95, width = .95)
      }
    }
    if((ssf_prob_i_max) >= .75 & (ssf_prob_i_min) >= .5) {
      raster_breaks <- seq(.5, 1, by = .25)
      #legend_label <- "Legend\n   B"
      legend_label <- "B"
      legend_b_only <- tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
        tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
          legend.reverse = TRUE, style = "cont",
          title = "B)\nProbability") +
        tm_layout(legend.only = TRUE,
          outer.margins = c(0, 0, 0, 0),
          legend.title.size = 1.35,
          legend.text.size = 1.1,
          fontfamily = "Latin Modern Roman")
      legend_b_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_B.png"))
      if(!file.exists(legend_b_file)){
        tmap_save(tm = legend_b_only, filename = legend_b_file, unit = "in",
          dpi = 300, height = .95, width = .95)
      }
    }
    if((ssf_prob_i_max) <= .5){
      raster_breaks <- seq(0, .5, by = .25)
      #legend_label <- "Legend\n   C"
      legend_label <- "C"
      legend_c_only <- tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
        tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
          legend.reverse = TRUE, style = "cont",
          title = "C)\nProbability") +
        tm_layout(legend.only = TRUE,
          outer.margins = c(0, 0, 0, 0),
          legend.title.size = 1.35,
          legend.text.size = 1.1,
          fontfamily = "Latin Modern Roman")
      legend_c_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_C.png"))
      if(!file.exists(legend_c_file)){
        tmap_save(tm = legend_c_only, filename = legend_c_file, unit = "in",
          dpi = 300, height = .95, width = .95)
      }
    }

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
      tm_scale_bar(breaks = c(0, 5, 10), text.size = .65, lwd = .25,
        position = c(.03, .0)) +
      tm_compass(type = "4star", text.size = 0.75, show.labels = 1, size = 1.5,
        position = c(.795, .675), lwd = .25) +
      tm_shape(nests_sim) +
      tm_symbols(shape = 21, border.col = "black", border.lwd = 1,
        col = nest_color, size = .125) +
      tm_layout(asp = .8,
        fontfamily = "Latin Modern Roman",
        frame = NA,
        title.color = "black",
        title.bg.color = NA, #"ivory3",
        title.bg.alpha = .85,
        title.position = c(.215,.95),
        title.fontfamily = "Latin Modern Roman",
        title.fontface = "bold",
        title = step_type_i_arrow,
        title.size = .75,
        title.snap.to.legend = FALSE,
        legend.show = FALSE)  +
        tm_credits(legend_label, bg.color = "white", position = c(.875, .05),
          just = "center", size = 0.7, width = .105)

    tmap_position <- switch(step_type_i_numeric,
      "1_1" = 1,  "1_2" = 2,  "1_4" = 3,
      "2_1" = 5,  "2_2" = 6,  "2_4" = 7,  "2_5" = 8,
      "3_1" = 9,  "3_2" = 10, "3_4" = 11, "3_5" = 12,
      "4_1" = 13, "4_2" = 14, "4_4" = 15, "4_5" = 16,
                  "5_2" = 18, "5_4" = 19)
    writeLines(as.character(tmap_position))
    ssf_prob_i_nest_map
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
  if(FALSE){
    ssf_tmap_nest_arrange <- tmap_arrange(
      ssf_tmap_list[[1]], tmap_blank, tmap_blank, tmap_blank,
      tmap_blank, tmap_blank, tmap_blank, tmap_blank,
      tmap_blank, tmap_blank, tmap_blank, tmap_blank,
      tmap_blank, tmap_blank, tmap_blank, tmap_blank,
      tmap_blank, tmap_blank, tmap_blank, tmap_blank,
      ncol = 4)
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

  map_file <- file.path("C:/TEMP/TEMP_Images", "SSF_Maps.png")

  tmap_save(tm = ssf_tmap_nest_arrange, filename = map_file, unit = "in",
    dpi = 300, height = 8, width = 8*.8)

  map_img <- map_file %>% image_read(.) %>% image_trim(.)
  legend_a_img <- legend_a_file %>% image_read(.) %>% image_trim(.)
  legend_b_img <- legend_b_file %>% image_read(.) %>% image_trim(.)
  legend_c_img <- legend_c_file %>% image_read(.) %>% image_trim(.)

  backgrd <- image_blank(1900, 2395, color = "white")

  ssf_maps_fig <- backgrd %>%
    image_composite(., map_img, offset = "+0+0") %>%
    image_composite(., legend_a_img, offset = "+1460+1925") %>%
    image_composite(., legend_b_img, offset = "+1460+2170") %>%
    image_composite(., legend_c_img, offset = "+1670+2047")
  ssf_maps_fig

  # Export
  ssf_nest_map_fig_file <- file.path(tex_dir, "Figures/Ch2",
    "SSF_Prob_Raster_Maps/Nests", paste0("SSF_Probability_Maps_", nest_j_name,
    ".png"))
  image_write(ssf_maps_fig, path = ssf_nest_map_fig_file, format = ".png")

  file.remove(map_file)
  file.remove(legend_a_file)
  file.remove(legend_b_file)
  file.remove(legend_c_file)
}

# CHAPTER 3 --------------------------------------------------------------------

# BAEA/Sim Step Density Maps ---------------------------------------------------

# Import files
me_turbines_buff <- readRDS(me_turbines_buff_file)
baea_hr <- readRDS(baea_hr_file)
ridge_poly <- read_sf(ridge_poly_file)
nests_sim <- nests_study %>% slice(c(2, 4, 7, 13)) %>% st_transform(wgs84n19)

maine <- read_sf(maine_file) %>%
  st_transform(., crs = wgs84) %>%
  mutate(state = "Maine")  %>%
  dplyr::select(state)

# Use "Tmap_baselayers.R" script to get other baselayers
maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1, width = 2))
maine_bb <- bb_poly(bb(maine_bb_sf, ext = 1.15))
maine_om <- read_osm(maine_bb, zoom = 5, minNumTiles = 9, type = om_nat_geo)

for (i in seq_len(length(sims))){
  sim_i <- sims[i]
  sim_step_data_i <- file.path(paste0(sim_id_dir, sim_i),
    sim_step_data_dir,
    paste0(sim_id, "-", sim_i, "_01_step_data.rds")) %>%
    readRDS(.)
  if(i == 1){
    sim_step_data <- sim_step_data_i
  } else {
    sim_step_data <- bind_rows(sim_step_data, sim_step_data_i)
  }
}

for (j in unique(sim_step_data$baea_id)){
  sim_step_data_j <- sim_step_data %>%
    filter(baea_id == j) %>%
    dplyr::select(id, baea_id, baea_year, step_data_matched) %>%
    unnest(., cols = step_data_matched) %>%
    as_tibble(.) %>%
    dplyr::select(sim = id, x, y) %>%
    st_as_sf(., coords = c("x", "y"), crs = 32619,
      agr = "constant")

  baea_year <- sim_step_data %>%
    filter(baea_id == j) %>%
    pull(baea_year) %>% unique(.)
  baea_step_id <- baea_hr %>% filter(id == j) %>%
    arrange(datetime) %>%
    filter(year(date) == baea_year) %>%
    as_tibble(.) %>%
    dplyr::select(baea = id, long_utm, lat_utm) %>%
    st_as_sf(., coords = c("long_utm", "lat_utm"), crs = 32619,
      agr = "constant")

  baea_id_bb1_sfc <- st_as_sfc(bb(baea_step_id, relative = TRUE, height = 1,
      width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))

  # Get sim map bb (for final map extent)
  sim_j_bb1_sfc <- sim_step_data_j %>%
    st_as_sf(., coords = c("x", "y"), crs = wgs84n19, agr = "constant") %>%
    bb(., relative = TRUE, height = 1, width = 1) %>%
    st_as_sfc(.) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))

  # Get combined bb
  combined_bb1_sfc <- st_union(sim_j_bb1_sfc, baea_id_bb1_sfc) %>%
    st_transform(., crs = crs(base))

  # Crop base
  base_clip_aggregated <- base %>%
    crop(., bb(combined_bb1_sfc, ext = 1)) %>%
    aggregate(., fact = 20)

  # Rasterize points
  sim_data_j_raster <- sim_step_data_j %>%
    as_Spatial(.) %>%
    rasterize(., base_clip_aggregated, fun = "count") %>%
    subset(., subset = "sim")
  baea_data_id_raster <- baea_step_id %>%
    as_Spatial(.) %>%
    rasterize(., base_clip_aggregated, fun = "count") %>%
    subset(., subset = "baea")

  sim_data_j_raster_standardized <- sim_data_j_raster /
    cellStats(sim_data_j_raster, stat = "sum")
  baea_data_id_raster_standardized <- baea_data_id_raster /
    cellStats(baea_data_id_raster, stat = "sum")

  # Clip wind turbines
  me_turbines_buff_clip <- me_turbines_buff %>%
    st_transform(., crs = st_crs(combined_bb1_sfc)) %>%
    st_crop(., bb(combined_bb1_sfc, ext = 2))
  # Clip ridgeline polygon
  ridge_poly_clip <- ridge_poly %>%
    st_transform(., st_crs(combined_bb1_sfc)) %>%
    st_crop(., bb(combined_bb1_sfc, ext = 2))
  # Get combined map scalebar distances
  combined_dist_sf <- st_as_sfc(bb(combined_bb1_sfc, relative = TRUE,
    height = 1, width = 1))
  combined_x_dist <- as.numeric(approx_distances(bb(combined_dist_sf,
    ext = 1.15))[1])/1000/5
  combined_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0,
    combined_x_dist), scales::pretty_breaks(2))[1]))

  # Get osm baselayer for sim_step_sf_k and baea_step_sf_id
  sim_k_bb2_sfc <- st_as_sfc(bb(sim_j_bb1_sfc, relative = TRUE,
    height = 2, width = 2)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  baea_id_bb2_sfc <- st_as_sfc(bb(baea_step_id, relative = TRUE,
    height = 2, width = 2)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  combined_om <- st_union(sim_k_bb2_sfc, baea_id_bb2_sfc) %>%
    read_osm(., minNumTiles = 21, zoom = 10, type = om_nat_geo)
    # may need to add and adjust 'zoom' arg

  # Inset map
  combined_bb = CreateMapExtentBB(combined_bb1_sfc, asp = 1, ext = 1.15)
  maine_overview <-
    tm_layout(frame.lwd = .4) +
    tm_shape(maine_om, raster.downsample = FALSE) +
    tm_rgb() +
    tm_shape(maine) + # setting this as master sets lat/long
    tm_borders(col = "black") +
    tm_shape(combined_bb) +
    tm_borders(col = "red")

  density_base_map <-
    tm_shape(combined_om, raster.downsample = FALSE) +
    tm_rgb() +
    tm_layout(asp = 1,
      fontfamily = "Latin Modern Roman",
      main.title = NULL) +
    tm_legend(show = FALSE) +
    tm_scale_bar(text.size = 1.5,
      breaks = combined_x_breaks,
      position = c(.05, .01)) +
    tm_compass(type = "4star",
      show.labels = 1,
      size = 2.5,
      text.size = 1.25,
      position = c(.84, .82)) +
    tm_xlab("") + tm_ylab("")

  density_ridges_map <-
    tm_shape(nests_sim) +
    tm_symbols(shape = 21, border.col = "black", border.lwd = 1,
      col = nest_color, size = .25) +
    tm_shape(ridge_poly_clip) +
    tm_borders(col = "wheat4", alpha = .6) +
    tm_shape(ridge_poly_clip) +
    tm_fill("forestgreen", alpha = .4) +
    tm_shape(me_turbines_buff_clip, title = "Wind Turbines") +
    tm_polygons(col = turbine_color,
      border.col = "black",  lwd = 1)

  raster_breaks <- pretty(c(0,
    max(cellStats(baea_data_id_raster_standardized, "max"),
    cellStats(sim_data_j_raster_standardized, "max"))))

  baea_id_map <- density_base_map +
    tm_shape(baea_data_id_raster_standardized, raster.downsample = FALSE,
      bbox = bb(combined_bb1_sfc, ext = 1.25), is.master = TRUE) +
    tm_raster(alpha = .9, palette = "viridis", legend.reverse = TRUE,
      title = "DEFAULT", style = "cont", breaks = raster_breaks) +
    density_ridges_map +
    tm_layout(main.title = paste0(j, " - Empirical Data"),
      main.title.size = 1.85,
      main.title.position = "center")

    #tm_credits(paste0(j, " - Empirical Data"), bg.color = NA,
    #  position = c("center", "TOP"), size = 1.75,
    #  fontface = "bold")

  sim_j_map <- density_base_map +
    tm_shape(sim_data_j_raster_standardized,
      bbox = bb(combined_bb1_sfc, ext = 1.25), is.master = TRUE) +
    tm_raster(alpha = .9, palette = "viridis", legend.reverse = TRUE,
      title = "DEFAULT", style = "cont", breaks = raster_breaks) +
    density_ridges_map +
    tm_layout(main.title = paste0(j, " - Simulation Data (n = 10)"),
      main.title.size = 1.85,
      main.title.position = "center")

    #tm_credits(, bg.color = NA,
    #  position = c("center", "TOP"), size = 1.75,
    #  fontface = "bold")

  legend_only <- tm_shape(sim_data_j_raster_standardized,
      bbox = bb(combined_bb1_sfc, ext = 1.25), is.master = TRUE) +
    tm_raster(alpha = .9, palette = "viridis", legend.reverse = TRUE,
      title = "Habitat Use\nProportion", style = "cont",
      breaks = raster_breaks) +
    tm_layout(legend.only = TRUE,
      legend.text.size = .75,
      legend.title.size = 1,
      fontfamily = "Latin Modern Roman")

  # Export to LaTeX Folder
  baea_id_map_file <- file.path("C:/TEMP/TEMP_Images",
    paste0("BAEA_", j, ".png"))
  tmap_save(tm = baea_id_map, filename = baea_id_map_file,
    unit = "in", dpi = 300, height = 6, width = 6)

  sim_j_map_file <- file.path("C:/TEMP/TEMP_Images",
    paste0("SIM_", j, ".png"))
  tmap_save(tm = sim_j_map, filename = sim_j_map_file,
    unit = "in", dpi = 300, height = 6, width = 6)

  legend_file <- file.path("C:/TEMP/TEMP_Images",
    paste0("Legend_", j, ".png"))
  tmap_save(tm = legend_only, filename = legend_file, unit = "in", dpi = 300,
    height = 3, width = 1.5)

  overview_file <- file.path("C:/TEMP/TEMP_Images",
    paste0("Overview_", j, ".png"))
  tmap_save(tm = maine_overview, filename = overview_file, unit = "in",
    dpi = 300, height = 1, width = .85)

  baea_map_img <- baea_id_map_file %>%
    image_read(.) %>%
    image_trim(.) %>%
    image_resize(., "864x864")

  sim_map_img <- sim_j_map_file %>%
    image_read(.) %>%
    image_trim(.) %>%
    image_resize(., "864x864")

  legend_img <- legend_file %>%
    image_read(.) %>%
    image_trim(.)
  legend_img_height <- image_info(legend_img) %>%
    pull (height)
  legend_y_offset <- 865 - legend_img_height - 250

  overview_img <- overview_file %>%
    image_read(.) %>%
    image_trim(.)

  backgrd <- image_blank(height = 865, width = 1915, color = "white")

  density_fig <- backgrd %>%
    image_composite(., baea_map_img, offset = "+0+0") %>%
    image_composite(., sim_map_img, offset = "+835+0") %>%
    image_composite(., legend_img, offset = paste0("+1670+",legend_y_offset))%>%
    image_composite(., overview_img, offset = "+1665+650")
  density_fig

  # Export PNG
  density_fig_file = file.path(tex_dir, "Figures/Ch3", "Density_Maps",
    paste0(j, "_BAEA_Sim_Density_Map.png"))
  image_write(density_fig, path = density_fig_file, format = "png")

  # Export SVG
  if(FALSE){
    density_fig_file = file.path(tex_dir, "Figures/Ch3", "Density_Maps",
      paste0(j, "_BAEA_Sim_Density_Map.svg"))
    image_write(density_fig, path = density_fig_file, format = "svg",
      flatten = TRUE)
  }

  # Clean up files
  if(TRUE){
    file.remove(baea_id_map_file)
    file.remove(sim_j_map_file)
    file.remove(legend_file)
    file.remove(overview_file)
  }
}

# CHAPTER 4 --------------------------------------------------------------------

# Select nest
nest_str <- c("Wilson", "Grand_Lake")[1]
nest_lower <- str_to_lower(nest_str)
nest_title <- str_to_title(nest_str)

# Get Map Data -----------------------------------------------------------------

# Nest overview map
nest <- readRDS(file.path(wind_input_dir, paste0(nest_lower, "_nest.rds")))
nest_map_center <- readRDS(file.path(wind_input_dir,
  paste0(nest_lower, "_map_center.rds")))
nest_bb_sfc <- st_buffer(nest_map_center, 8000) %>% bb(.) %>% st_as_sfc(.)

nest_overview_center <- nest

if(nest_str == "Wilson"){
  sfc <- st_sfc(st_point(c(st_coordinates(nest)[1] + 10000,
    st_coordinates(nest)[2] - 78000))) # 83000
}
if(nest_str == "Grand_Lake"){
  sfc <- st_sfc(st_point(c(st_coordinates(nest)[1] - 55000,
    st_coordinates(nest)[2] - 45000))) #30000
}

st_geometry(nest_overview_center) <- sfc
st_crs(nest_overview_center) <- 32619

nest_overview_buff <- st_buffer(nest_overview_center, 110000) %>% bb(.)
mapview(nest_overview_buff)
nest_overview_bb <- bb_poly(bb(nest_overview_buff, ext = 1))

# Nest basemap
nest_natgeo_osm <- maptiles::get_tiles(x = nest_bb_sfc,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 12, forceDownload = TRUE)

nest_overview_bb_osm <- maptiles::get_tiles(x = nest_overview_bb,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 6, forceDownload = TRUE)

# Turbines and Wind Area
nest_wt_n = st_read(file.path(gis_exp_dir, nest_title,
  paste0(nest_lower , "_n_turbines.shp")))
nest_wt_s = st_read(file.path(gis_exp_dir, nest_title,
  paste0(nest_lower , "_s_turbines.shp")))

# Turbine Buffers
nest_wt_n_buff <- nest_wt_n %>% st_buffer(56) %>%
  mutate(id = paste0("N-", str_pad(1:n(), width = 2, side = "left",
    pad = "0"))) %>%
  dplyr::select(id)
nest_wt_s_buff <- nest_wt_s %>% st_buffer(56) %>%
  mutate(id = paste0("S-", str_pad(1:n(), width = 2, side = "left",
    pad = "0"))) %>%
  dplyr::select(id)

nest_n_area <- readRDS(file.path(wind_input_dir, paste0(nest_lower,
  "_n_area.rds")))
nest_s_area <- readRDS(file.path(wind_input_dir, paste0(nest_lower,
  "_s_area.rds")))

# Maine basemap
maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1, width = 2))
maine_bb <- bb_poly(bb(maine_bb_sf, ext = 1.15))
maine_om <- read_osm(maine_bb, zoom = 5, minNumTiles = 9, type = om_nat_geo)

# SSF Fits
ssf_fits_best_org <- readRDS(fits_best_file) #%>% slice(c(step_type_index))
ssf_fits_best <- ssf_fits_best_org

# Scenario directories
exp_scenarios <- list.dirs(file.path(gis_exp_dir, nest_title), recursive =FALSE)

# Collision Risk
exp_flight_collision_risk <- readRDS(file.path(exp_output_dir,
  paste0("flight_collision_risk_", nest_lower, ".rds")))

# Nest Overview Map ------------------------------------------------------------

nest_overview <-
  tm_layout(fontfamily = "Latin Modern Roman", asp = 1, inner.margins = -.02) +
  tm_shape(nest_overview_bb_osm, is.master = TRUE) +
    tm_rgb() +
  tm_shape(nest_bb_sfc) +
    tm_borders(col = "red", lwd = 3) +
  tm_scale_bar(text.size = 1, breaks = c(0, 50, 100),
    position = c(.2, -.03))
nest_overview

# Nest Wind Area Scenario Maps -------------------------------------------------

tmap_nest_wind_areas <-
  tm_layout(asp = 1, fontfamily = "Latin Modern Roman") +
  tm_shape(nest_bb_sfc, is.master = TRUE, ext = .935) +
    tm_borders(col = "red") +
  tm_shape(nest_natgeo_osm, raster.downsample = FALSE) +
    tm_rgb() +
  tm_shape(nest, title = paste0(nest_title, " Nest")) +
    tm_bubbles(col = nest_color, border.lwd = 1,  size = .35,
    border.col = "black") +
  tm_shape(nest_n_area, title = paste0(nest_title, " Nest")) +
    tm_polygons(col = wind_area_color, border.col = "black",  lwd = 1) +
  tm_shape(nest_s_area, paste0(nest_title, " Nest")) +
    tm_polygons(col = wind_area_color, border.col = "black",  lwd = 1) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.75, text.size = 1.25,
    position = c(.825, .825)) +
  tm_scale_bar(text.size = 1.2, breaks = c(0, 2, 4), position = c(.05, .01))
tmap_nest_wind_areas

tmap_save(tm = tmap_nest_wind_areas, filename = file.path(tex_dir,
  "Figures/Ch4", "Maps_Scenarios", nest_title,
  paste0(nest_title, "_Wind_Areas.svg")),
  insets_tm = nest_overview, insets_vp =  viewport(x = 0.853, y = .141,
  width = 0.25, height = 0.25), unit = "in", dpi = 300, height = 6, width = 6.1)

# Nest Turbine Scenario Maps -------------------------------------------------

if(nest_str == "Wilson") map_ext <- .8
if(nest_str == "Grand_Lake") map_ext <- .825

tmap_nest_base <-
  tm_layout(asp = 1, fontfamily = "Latin Modern Roman") +
  tm_shape(nest_bb_sfc, is.master = TRUE, ext = map_ext) + #.935
    tm_borders(col = "red") +
  tm_shape(nest_natgeo_osm, raster.downsample = FALSE) +
    tm_rgb() +
  tm_shape(nest, title = paste0(nest_title, " Nest")) +
    tm_bubbles(col = nest_color, border.lwd = 1,  size = .35,
    border.col = "black") +
  tm_compass(type = "4star",  show.labels = 1, size = 2,
    position = c(.83, .825)) +
  tm_scale_bar(text.size = .75, breaks = c(0, 2, 4), position = c(.05, .01))

tmap_nest_c <- tmap_nest_base +
  tm_credits("Control", #fontfamily = "Latin Modern Roman",
    size = 1, position = c(.0175, .91))

tmap_nest_n <- tmap_nest_base +
  tm_shape(nest_wt_n_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color,
      border.col = "black",  lwd = 1)+
  tm_credits("North", #fontfamily = "Latin Modern Roman",
    size = 1, position = c(.0175, .91))

tmap_nest_s <- tmap_nest_base +
  tm_shape(nest_wt_s_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color, border.col = "black",  lwd = 1) +
  tm_credits("South", #fontfamily = "Latin Modern Roman",
    size = 1, position = c(.0175, .91))

tmap_nest_ns <- tmap_nest_base +
  tm_shape(nest_wt_n_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color, border.col = "black",  lwd = 1) +
  tm_shape(nest_wt_s_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color, border.col = "black",  lwd = 1) +
  tm_credits("North and South", #fontfamily = "Latin Modern Roman",
    size = 1, position = c(.0175, .91))

# Arrange map of probability surfaces for testing
tmap_scenario_arrange <- tmap_arrange(tmap_nest_c, tmap_nest_n,
  tmap_nest_s, tmap_nest_ns, ncol = 2)

tmap_save(tm = tmap_scenario_arrange, filename = file.path(tex_dir,
  "Figures/Ch4/Maps_Scenarios", nest_title, paste0("All_Scenarios.png")),
  unit = "in", dpi = 300, height = 6, width = 8*.8)

# Nest SSF Maps ----------------------------------------------------------------

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
      nest_bbox <- st_as_sfc(st_bbox(st_buffer(nest, dist = 10000)))
      nest_buffer <- st_buffer(nest, dist = 10000)
      #nest_bb_sf <- st_as_sfc(bb(nest_buffer, relative = TRUE, height = 1.35,
      #  width = 1.35))
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

    ssf_prob_i_max <- round(maxValue(ssf_prob_i_mask), 2)
    ssf_prob_i_min <- round(minValue(ssf_prob_i_mask), 2)
    if((ssf_prob_i_max) >= .75 & (ssf_prob_i_min) <= .5){
      raster_breaks <- seq(0, 1, by = .5)
      legend_label <- "A"
      legend_a_only <- tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
        tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
          legend.reverse = TRUE, style = "cont",
          title = "A)\nProbability") +
        tm_layout(legend.only = TRUE,
          outer.margins = c(0, 0, 0, 0),
          legend.title.size = 1.35,
          legend.text.size = 1.1,
          fontfamily = "Latin Modern Roman")
      legend_a_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_A.png"))
      if(!file.exists(legend_a_file)){
        tmap_save(tm = legend_a_only, filename = legend_a_file, unit = "in",
          dpi = 300, height = .95, width = .95)
      }
    }
    if((ssf_prob_i_max) >= .75 & (ssf_prob_i_min) >= .5) {
      raster_breaks <- seq(.5, 1, by = .25)
      legend_label <- "B"
      legend_b_only <- tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
        tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
          legend.reverse = TRUE, style = "cont",
          title = "B)\nProbability") +
        tm_layout(legend.only = TRUE,
          outer.margins = c(0, 0, 0, 0),
          legend.title.size = 1.35,
          legend.text.size = 1.1,
          fontfamily = "Latin Modern Roman")
      legend_b_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_B.png"))
      if(!file.exists(legend_b_file)){
        tmap_save(tm = legend_b_only, filename = legend_b_file, unit = "in",
          dpi = 300, height = .95, width = .95)
      }
    }
    if((ssf_prob_i_max) <= .5){
      raster_breaks <- seq(0, .5, by = .25)
      legend_label <- "C"
      legend_c_only <- tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
        tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
          legend.reverse = TRUE, style = "cont",
          title = "C)\nProbability") +
        tm_layout(legend.only = TRUE,
          outer.margins = c(0, 0, 0, 0),
          legend.title.size = 1.35,
          legend.text.size = 1.1,
          fontfamily = "Latin Modern Roman")
      legend_c_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_C.png"))
      if(!file.exists(legend_c_file)){
        tmap_save(tm = legend_c_only, filename = legend_c_file, unit = "in",
          dpi = 300, height = .95, width = .95)
      }
    }

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
      tm_scale_bar(breaks = c(0, 5, 10), text.size = .65, lwd = .25,
        position = c(.03, .0)) +
      tm_compass(type = "4star", text.size = 0.75, show.labels = 1, size = 1.5,
        position = c(.795, .675), lwd = .25) +
      tm_shape(nest) +
      tm_symbols(shape = 21, border.col = "black", border.lwd = 1,
        col = nest_color, size = .125) +
      tm_layout(asp = .8,
        fontfamily = "Latin Modern Roman",
        frame = NA,
        title.color = "black",
        title.bg.color = NA, #"ivory3",
        title.bg.alpha = .85,
        title.position = c(.215,.95),
        title.fontfamily = "Latin Modern Roman",
        title.fontface = "bold",
        title = step_type_i_arrow,
        title.size = .75,
        title.snap.to.legend = FALSE,
        legend.show = FALSE)  +
        tm_credits(legend_label, bg.color = "white", position = c(.875, .05),
          just = "center", size = 0.7, width = .105)

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

  map_file <- file.path("C:/TEMP/TEMP_Images", "SSF_Maps.png")

  tmap_save(tm = ssf_tmap_nest_arrange, filename = map_file, unit = "in",
    dpi = 300, height = 8, width = 8*.8)

  map_img <- map_file %>% image_read(.) %>% image_trim(.)
  legend_a_img <- legend_a_file %>% image_read(.) %>% image_trim(.)
  legend_b_img <- legend_b_file %>% image_read(.) %>% image_trim(.)
  legend_c_img <- legend_c_file %>% image_read(.) %>% image_trim(.)

  backgrd <- image_blank(1900, 2395, color = "white")

  ssf_maps_fig <- backgrd %>%
    image_composite(., map_img, offset = "+0+0") %>%
    image_composite(., legend_a_img, offset = "+1460+1925") %>%
    image_composite(., legend_b_img, offset = "+1460+2170") %>%
    image_composite(., legend_c_img, offset = "+1670+2047")
  ssf_maps_fig

  ssf_nest_map_fig_file <- file.path(tex_dir,
    "Figures/Ch4/Maps_SSF_Probability", nest_title, paste0(exp_scenario_j_name,
    ".png"))
  image_write(ssf_maps_fig, path = ssf_nest_map_fig_file, format = ".png")

  file.remove(map_file)
  file.remove(legend_a_file)
  file.remove(legend_b_file)
  file.remove(legend_c_file)

}

# Nest Path Density Maps -------------------------------------------------------

for (j in c("Cruise", "Flight")){
  exp_paths_raster_c <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_", nest_title, "_", j, "_C.rds")))
  exp_paths_raster_n <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_", nest_title, "_", j, "_N.rds")))
  exp_paths_raster_ns <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_", nest_title, "_", j, "_NS.rds")))
  exp_paths_raster_s <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_", nest_title, "_", j, "_S.rds")))

  # Get bb (for final map extent)
  paths_c_bb1_sfc <- st_as_sfc(bb(exp_paths_raster_c, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  paths_n_bb1_sfc <- st_as_sfc(bb(exp_paths_raster_n, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  paths_ns_bb1_sfc <- st_as_sfc(bb(exp_paths_raster_ns, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  paths_s_bb1_sfc <- st_as_sfc(bb(exp_paths_raster_s, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))

  # Get combined bb
  combined_bb <- st_union(paths_c_bb1_sfc, paths_n_bb1_sfc,
      paths_ns_bb1_sfc, paths_s_bb1_sfc) %>%
    st_transform(., crs = crs(base)) %>%
    bb(., relative = TRUE, height = 1, width = 1, asp.limit = 1)
  combined_bb_sfc <- combined_bb %>%
    st_as_sfc(.)

  combined_bb_om = maptiles::get_tiles(x = combined_bb,
    cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
    verbose = TRUE, zoom = 10, forceDownload = TRUE)
  if(FALSE) mapview(combined_bb)

  # Base map
  paths_density_map_base <-
    tm_layout(asp = 1, fontfamily = "Latin Modern Roman") +
    tm_shape(combined_bb_sfc, is.master = TRUE, ext = .935) +
      tm_borders(col = "red") +
    tm_shape(combined_bb_om, raster.downsample = FALSE) +
      tm_rgb() +
    tm_compass(type = "4star",  show.labels = 1, size = 2,
      position = c(.83, .825)) +
    tm_scale_bar(text.size = 1, breaks = c(0, 5, 10),
      position = c(.05, .01)) +
    tm_layout(fontfamily = "Latin Modern Roman",
      asp = 1,
      outer.margins = 0,
      inner.margins = 0,
      frame = "black",
      title.color = "black",
      title.bg.color = NA, #"ivory3",
      title.bg.alpha = .85,
      title.position = c(.275,.95),
      title.fontfamily = "Latin Modern Roman",
      title.fontface = "bold",
      title.size = 1, #.75
      title.snap.to.legend = FALSE,
      legend.show = FALSE)
  paths_density_map_base

  turbines_n_present <-  tm_shape(nest_wt_n_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color_present,
      border.col = turbine_color_present,
      lwd = 1)

  turbines_n_absent <-  tm_shape(nest_wt_n_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color_absent,
      border.col = turbine_color_absent, lwd = 1)

  turbines_s_present <-  tm_shape(nest_wt_s_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color_present,
      border.col = turbine_color_present, lwd = 1)

  turbines_s_absent <-  tm_shape(nest_wt_s_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color_absent,
      border.col = turbine_color_absent, lwd = 1)

  tm_nest <- tm_shape(nest) +
    tm_symbols(shape = 21, border.col = "black", border.lwd = 1.25,
      col = nest_color, size = .35)

  paths_density_map_c <- paths_density_map_base +
    tm_shape(exp_paths_raster_c, raster.downsample = FALSE) +
    tm_raster(palette = plasma(20, direction = 1), alpha = .7,
      legend.reverse = TRUE, style = "log10_pretty",
      title = "Path Density") +
    turbines_n_absent +
    turbines_s_absent +
    tm_nest +
    tm_credits("Control",
      size = 1, position = c(.0175, .91))

  paths_density_map_n <- paths_density_map_base +
    tm_shape(exp_paths_raster_n, raster.downsample = FALSE) +
    tm_raster(palette = plasma(20, direction = 1), alpha = .7,
      legend.reverse = TRUE, style = "log10_pretty",
      title = "Path Density") +
    turbines_n_present +
    turbines_s_absent +
    tm_nest +
    tm_credits("North",
      size = 1, position = c(.0175, .91))

  paths_density_map_s <- paths_density_map_base +
    tm_shape(exp_paths_raster_s, raster.downsample = FALSE) +
    tm_raster(palette = plasma(20, direction = 1), alpha = .7,
      legend.reverse = TRUE, style = "log10_pretty",
      title = "Path Density") +
    turbines_n_absent +
    turbines_s_present +
    tm_nest +
    tm_credits("South",
      size = 1, position = c(.0175, .91))

  paths_density_map_ns <- paths_density_map_base +
    tm_shape(exp_paths_raster_ns, raster.downsample = FALSE) +
    tm_raster(palette = plasma(20, direction = 1), alpha = .7,
      legend.reverse = TRUE, style = "log10_pretty",
      title = "Path Density") +
    turbines_n_present +
    turbines_s_present +
    tm_nest +
    tm_credits("North and South", #fontfamily = "Latin Modern Roman",
      size = 1, position = c(.0175, .91))

  # Arrange map of probability surfaces for testing
  tmap_paths_density_arrange <- tmap_arrange(paths_density_map_c,
    paths_density_map_n, paths_density_map_s, paths_density_map_ns, ncol = 2)

  tmap_save(tm = tmap_paths_density_arrange, filename = file.path(
    "C:/TEMP/TEMP_Images", paste0("Path_Density_", j, ".png")),
    unit = "in", dpi = 300, height = 6, width = 8*.8)

  paths_density_map_img <- file.path(
    "C:/TEMP/TEMP_Images", paste0("Path_Density_", j, ".png")) %>%
  image_read(.) %>%
  image_trim(.)

  legend_only <- tm_shape(exp_paths_raster_c, raster.downsample = FALSE) +
    tm_raster(palette = plasma(20, direction = 1), alpha = .7,
      legend.reverse = TRUE, style = "log10_pretty",
      title = "Path Density") +
    tm_layout(legend.only = TRUE,
      fontfamily = "Latin Modern Roman",
      legend.title.size = 1.2, #1
      legend.text.size = .8)
  tmap_save(tm = legend_only, filename = file.path(
    "C:/TEMP/TEMP_Images",paste0("Path_Density_", j, "_Legend.png")),
    unit = "in", dpi = 300, height = 3, width = 3)

  legend_img <- file.path(
      "C:/TEMP/TEMP_Images", paste0("Path_Density_", j, "_Legend.png")) %>%
    image_read(.) %>%
    image_trim(.)

  backgrd <- image_blank(2280, 1764, color = "white")

  covar_sigma_fig <- backgrd %>%
    image_composite(., paths_density_map_img, offset = "+0+0") %>%
    image_composite(., legend_img, offset = "+1910+750")

  # Export
  maps_fig_file = file.path(tex_dir, "Figures/Ch4/Maps_Path_Density",
    nest_title, paste0("Path_Density_", j, ".png"))
  image_write(covar_sigma_fig, path = maps_fig_file, format = ".png")

}

# Nest Collision Risk Maps ---------------------------------------------------

n_turbines_scenario_north <- exp_flight_collision_risk %>%
  filter(scenario == "North") %>%
  dplyr::select(exp_id, scenario, n_turbine_tally) %>%
  unnest(n_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(intersects_prop = intersects_n/sum(intersects_n)) %>%
  mutate(id = paste0("N-", str_pad(turbine, width = 2, side = "left",
    pad = "0"))) %>%
  dplyr::select(scenario, id, intersects_n, intersects_prop) %>%
  left_join(nest_wt_n_buff, .) %>%
  mutate(scenario = "North",
    intersects_n = replace_na(intersects_n, 0),
    intersects_prop = replace_na(intersects_prop, 0))

s_turbines_scenario_south <- exp_flight_collision_risk %>%
  filter(scenario == "South") %>%
  dplyr::select(exp_id, scenario, s_turbine_tally) %>%
  unnest(s_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(intersects_prop = intersects_n/sum(intersects_n)) %>%
  mutate(id = paste0("S-", str_pad(turbine, width = 2, side = "left",
    pad = "0"))) %>%
  dplyr::select(scenario, id, intersects_n, intersects_prop) %>%
  left_join(nest_wt_s_buff, .) %>%
  mutate(scenario = "South",
    intersects_n = replace_na(intersects_n, 0),
    intersects_prop = replace_na(intersects_prop, 0))

n_turbines_scenario_northsouth <- exp_flight_collision_risk %>%
  filter(scenario == "North and South") %>%
  dplyr::select(exp_id, scenario, n_turbine_tally) %>%
  unnest(n_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(id = paste0("N-", str_pad(turbine, width = 2, side = "left",
    pad = "0"))) %>%
  dplyr::select(scenario, id, intersects_n) %>%
  left_join(nest_wt_n_buff, .) %>%
  mutate(scenario = "North and South",
    intersects_n = replace_na(intersects_n, 0))

s_turbines_scenario_northsouth <- exp_flight_collision_risk %>%
  filter(scenario == "North and South") %>%
  dplyr::select(exp_id, scenario, s_turbine_tally) %>%
  unnest(s_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(id = paste0("S-", str_pad(turbine, width = 2, side = "left",
    pad = "0"))) %>%
  dplyr::select(scenario, id, intersects_n) %>%
  left_join(nest_wt_s_buff, .) %>%
  mutate(scenario = "North and South",
    intersects_n = replace_na(intersects_n, 0))

ns_turbines_scenario_northsouth <- bind_rows(n_turbines_scenario_northsouth,
  s_turbines_scenario_northsouth) %>%
  mutate(intersects_prop = intersects_n/sum(intersects_n))


# Nest Overview Map --

# Nest overview map
nest <- readRDS(file.path(wind_input_dir, paste0(nest_lower, "_nest.rds")))
nest_map_center <- readRDS(file.path(wind_input_dir,
  paste0(nest_lower, "_map_center.rds")))

if(nest_str == "Wilson"){
  sfc <- st_sfc(st_point(c(st_coordinates(nest_map_center)[1],
    st_coordinates(nest_map_center)[2] - 500))) # 83000
  st_geometry(nest_map_center) <- sfc
  st_crs(nest_map_center) <- 32619
}

nest_bb_sfc <- st_buffer(nest_map_center, 8000) %>% bb(.) %>% st_as_sfc(.)

nest_overview_center <- nest

if(nest_str == "Wilson"){
  sfc <- st_sfc(st_point(c(st_coordinates(nest)[1] + 10000,
    st_coordinates(nest)[2] - 78000))) # 83000
}
if(nest_str == "Grand_Lake"){
  sfc <- st_sfc(st_point(c(st_coordinates(nest)[1] - 55000,
    st_coordinates(nest)[2] - 45000))) #30000
}

st_geometry(nest_overview_center) <- sfc
st_crs(nest_overview_center) <- 32619

nest_overview_buff <- st_buffer(nest_overview_center, 110000) %>% bb(.)
nest_overview_bb <- bb_poly(bb(nest_overview_buff, ext = 1))

# Nest basemap
nest_natgeo_osm <- maptiles::get_tiles(x = nest_bb_sfc,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 12, forceDownload = TRUE)

nest_overview_bb_osm <- maptiles::get_tiles(x = nest_overview_bb,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 6, forceDownload = TRUE)

nest_overview <-
  tm_layout(asp = 1,
    inner.margins = -.02,
    fontfamily = "Latin Modern Roman",
    frame.lwd = 3
    ) +
  tm_shape(nest_overview_bb_osm, is.master = TRUE) +
    tm_rgb() +
  tm_shape(nest_bb_sfc) +
    tm_borders(col = "red", lwd = 3) +
  tm_scale_bar(text.size = 1.35, breaks = c(0, 50, 100),
    position = c(.275, -0))
nest_overview

tmap_save(tm = nest_overview, filename = file.path(tex_dir,
  "Figures/Ch4", "Maps_Turbine_Transits", nest_title, "Transits_Overview.svg"),
  width = 2.5, height = 2.5, units = "in")

# Nest Map Template --

tmap_nest <-
  tm_layout(fontfamily = "Latin Modern Roman",
    asp = 1,
    frame.lwd = 3,
    legend.position = c(.693, .0015),
    legend.bg.color = "grey",
    legend.frame = "black",
    legend.text.size = 1.4,
    legend.title.size = 1.75) +
  tm_shape(nest_bb_sfc, is.master = TRUE, ext = .8) + #.935
    tm_borders(col = "red") +
  tm_shape(nest_natgeo_osm, raster.downsample = FALSE) +
    tm_rgb() +
  tm_shape(nest) +
    tm_bubbles(col = nest_color, border.lwd = 1,  size = .6, #.4
      border.col = "black") +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    text.size = 1.5, #.8
    position = c(.8, .8)) +
  tm_scale_bar(text.size = 1.75,
    breaks = c(0, 2, 4), position = c(.05, .01)) +
  tm_layout()
tmap_nest

# Nest North Turbines Transits Maps ------------------------------------------

tmap_nest_n_turbines_transits <-
  tmap_nest +
  tm_shape(n_turbines_scenario_north) +
    tm_bubbles(col = "intersects_prop", border.lwd = 2,  size = .5, #.25
      palette = "Purples", legend.col.reverse = TRUE,
      border.col = "black", title.col = "     Transits\n   Proportion") +
  tm_credits("North", size = 2.25, position = c(.015, .885))
tmap_nest_n_turbines_transits

tmap_save(tm = tmap_nest_n_turbines_transits, filename = file.path(tex_dir,
  "Figures/Ch4", "Maps_Turbine_Transits", nest_title, "Transits_North.svg"),
  height = 6, width = 6)

# Nest South Turbines Transits Maps --------------------------------------------

tmap_nest_s_turbines_transits <-
  tmap_nest +
  tm_shape(s_turbines_scenario_south) +
    tm_bubbles(col = "intersects_prop", border.lwd = 2,  size = .5,
      palette = "Purples", legend.col.reverse = TRUE,
      border.col = "black", title.col = "     Transits\n   Proportion") +
  tm_credits("South", size = 2.25, position = c(.015, .885))
tmap_nest_s_turbines_transits

tmap_save(tm = tmap_nest_s_turbines_transits, filename = file.path(tex_dir,
  "Figures/Ch4", "Maps_Turbine_Transits", nest_title, "Transits_South.svg"),
  height = 6, width = 6)

# Nest All Turbines Transits Maps ----------------------------------------------

tmap_nest_ns_turbines_transits <-
  tmap_nest +
  tm_shape(ns_turbines_scenario_northsouth) +
    tm_bubbles(col = "intersects_prop", border.lwd = 2,  size = .5,
      palette = "Purples", legend.col.reverse = TRUE,
      border.col = "black", title.col = "     Transits\n   Proportion") +
  tm_credits("North and South", size = 2.25, position = c(.015, .885))
tmap_nest_ns_turbines_transits

tmap_save(tm = tmap_nest_ns_turbines_transits, filename = file.path(tex_dir,
  "Figures/Ch4", "Maps_Turbine_Transits", nest_title, "Transits_NorthSouth.svg"),
  height = 6, width = 6)

n_map_img <- file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_North.svg") %>%
  image_read(.) %>%
  image_trim(.)

s_map_img <- file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_South.svg") %>%
  image_read(.) %>%
  image_trim(.)

ns_map_img <- file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_NorthSouth.svg") %>%
  image_read(.) %>%
  image_trim(.)

overview_map_img <- file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_Overview.svg") %>%
  image_read(.) %>%
  image_trim(.)

backgrd <- image_blank(height = 1128, width = 1128, color = "white")

transits_fig <- backgrd %>%
  image_composite(., n_map_img, offset = "+0+0") %>%
  image_composite(., s_map_img, offset = "+574+0") %>%
  image_composite(., ns_map_img, offset = "+287+574") %>%
  image_composite(., overview_map_img, offset = "+880+875")
transits_fig

# Export
maps_fig_file = file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
  nest_title, "Transits_Combined.png")
image_write(transits_fig, path = maps_fig_file, format = "png")

maps_fig_file = file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
  nest_title, "Transits_Combined.svg")
image_write(transits_fig, path = maps_fig_file, format = "svg",
  flatten = TRUE)

# Clean up files
file.remove(file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_North.svg"))
file.remove(file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_South.svg"))
file.remove(file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_NorthSouth.svg"))
file.remove(file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_Overview.svg"))

# APPENDICES -------------------------------------------------------------------

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
      tm_layout(asp = 1, fontfamily = "Latin Modern Roman") +
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
      tm_scale_bar(text.size = 1.25,
        breaks = baea_k_x_breaks,
        position = c(.05, .01)) +
      tm_compass(type = "4star",  show.labels = 1, size = 2.5,
        position = c(.875, .875)) +
#      tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "black", alpha = 1,
#        ticks = TRUE, lines = FALSE, labels.col = "grey25",
#        labels.format = list(format = "f", big.mark = ""),
#        labels.inside.frame = FALSE) +
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

# SSF for Maine - Individual ---------------------------------------------------

# This process assumes that the 'final' version of the SSF probablity layers
# is in the ssf_prob_dir

# Set to TRUE to run - THESE MAPS ARE NOT CURRENTLY INCLUDED IN DISSERTATION
ssf_individual_maine <- FALSE

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
    tm_symbols(shape = 8, col = nest_color, size = .4) +
    tm_layout(#asp = .75,
      fontfamily = "Latin Modern Roman",
      title.bg.color = "white",
      title.position = c("left", "top"),
      title.fontfamily = "Latin Modern Roman",
      title = step_type_arrow,
      title.size = 1,
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

######################## OLD CODE ##############################################

# SSF for Maine - Combined ---

# This process assumes that the 'final' version of the SSF probablity layers
# is in the ssf_prob_dir

# for (i in seq_len(length(ssf_prob_files))){
#   print(i)
#   ssf_prob_i <- read_stars(ssf_prob_files[i])
#   step_type_numeric <- basename(tools::file_path_sans_ext(ssf_prob_files[i]))
#   step_type_latex <- step_type_numeric %>% # This is no longer used
#     str_replace_all("1", "Cruise") %>%
#     str_replace_all("2", "Flight") %>%
#     str_replace_all("3", "Nest") %>%
#     str_replace_all("4", "Perch") %>%
#     str_replace_all("5", "Roost") %>%
#     str_replace_all("_", "$\\\\rightarrow$ ") %>%
#     latex2exp::TeX(.)
#   step_type <- step_type_numeric %>%
#     str_replace_all("1", "Cruise") %>%
#     str_replace_all("2", "Flight") %>%
#     str_replace_all("3", "Nest") %>%
#     str_replace_all("4", "Perch") %>%
#     str_replace_all("5", "Roost")
#   start_behavior <- str_split(step_type, "_") %>% pluck(., 1) %>% .[1]
#   end_behavior <- str_split(step_type, "_") %>% pluck(., 1) %>% .[2]
#   step_type <- paste0("$\\overset{", start_behavior,
#     "$\\rightarrow$ phantom(x)}{", end_behavior, "}$") %>%
#     latex2exp::TeX(.)
#
#   ssf_prob_i_map <-
#     tm_shape(ssf_prob_i, raster.downsample = FALSE) +
#       tm_raster(palette = viridis(20, direction = 1),
#         legend.reverse = TRUE, style = "cont", title = "Probability") +
#       tm_layout(asp = .8,
#         fontfamily = "Latin Modern Roman",
#         title.position = c("LEFT", "top"),
#         title.fontfamily = "Latin Modern Roman",
#         title = step_type,
#         title.size = .7,
#         title.snap.to.legend =  FALSE,
#         legend.position = c("RIGHT", "BOTTOM"),
#         legend.height = .4,
#         legend.title.size = .5,
#         legend.text.size = .45,
#         legend.title.fontfamily = "Latin Modern Roman",
#         legend.text.fontfamily = "Latin Modern Roman",
#         frame = FALSE)
#   ssf_prob_i_map
#   ssf_tmap_list[[i]] <- ssf_prob_i_map
# }
#
# tmap_blank <-
#   tm_shape(ssf_prob_i, raster.downsample = TRUE) +
#   tm_raster(palette = "white", style = "cont") +
#   tm_layout(asp = .75, legend.show = FALSE, frame = FALSE)
#
# ssf_tmap_arrange <- tmap_arrange(
#   ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]], tmap_blank,
#   tmap_blank, tmap_blank, tmap_blank,
#   tmap_blank, tmap_blank, tmap_blank,
#   tmap_blank, tmap_blank, tmap_blank,
#   tmap_blank, tmap_blank, tmap_blank,
#   tmap_blank, ssf_tmap_list[[16]], ssf_tmap_list[[17]], tmap_blank,
#   ncol = 4)
#
# # ORIGINAL
# ssf_tmap_arrange <- tmap_arrange(
#   ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]], tmap_blank,
#   ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
#   ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
#   ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
#   ssf_tmap_list[[13]],ssf_tmap_list[[14]],ssf_tmap_list[[15]],
#   tmap_blank, ssf_tmap_list[[16]], ssf_tmap_list[[17]], tmap_blank,
#   ncol = 4)
#
# tmap_save(tm = ssf_tmap_arrange, filename = file.path(tex_dir, "Figures/Ch2",
#   "SSF_Prob_Raster_Maps", "SSF_Probability_Maps_Overview.png"), unit = "in",
#   dpi = 300, height = 8, width = 8*(.8))


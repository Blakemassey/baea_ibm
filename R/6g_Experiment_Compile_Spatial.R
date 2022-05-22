#------------------ Experiment Compile Spatial --------------------------------#
# This script is used to create visual a 'sim' object
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------
pacman::p_load(cartography, ctmm, dplyr, fasterize, forcats, gplots, ggplot2,
  ggthemes, grid, leaflet, lubridate, magick, mapview, move, OpenStreetMap,
  plotly, prettymapr, purrr, raster, rosm, rsvg, sf, spatstat, stringr, s2,
  tmap, tmaptools, viridis, units, webshot, zoo)
pacman::p_load(baear, gisr, ibmr)
set_thin_PROJ6_warnings(TRUE)
theme_update(plot.title = element_text(hjust = 0.5))
suppressMessages(extrafont::loadfonts(device = "win"))
#devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/ibmr")

# Variables
site <- "Wilson" # "Grand_Lake"
agg_factor <- 10
exp_ids <- 1:20 %>%
  str_pad(., width = 2, side = "left", pad = "0")
mapping <- FALSE

# Experiment simulation files in TEMP directory
exp_vec <- list.dirs("C:/TEMP", recursive = FALSE, full.names = TRUE) %>%
  str_subset(., paste0(site, "_[:alpha:]{1,}-")) %>%
  str_subset(., exp_ids %>% paste0("-", .) %>% paste0(., collapse = "|"))

# Directories
input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
exp_dir <- "C:/TEMP"
exp_step_data_dir <- "Step_Data"
exp_output_dir <- "Output/Experiment"
ridge_file_dir <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines"
wind_output_dir <- "Output/Analysis/Wind"
line_density_dir <- "Line_Density_Rasters"

# Files
ridge_poly_file <- file.path(ridge_file_dir, "ridge_poly.shp")
ridge_line_file <- file.path(ridge_file_dir, "ridge_line.shp")

# Plot themes
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))
theme_blank <- theme(legend.position = "none",
  text = element_text(family = "Latin Modern Roman"),
  plot.title = element_text(size=14),
  panel.grid = element_blank(), axis.title = element_blank(),
  axis.text = element_blank(), axis.ticks = element_blank(),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA))
theme_update(plot.title = element_text(hjust = 0.5))

# Behavior colors
behavior_colors <- CreateColorsByMetadata(file = file.path("Data/Assets",
  "behavior_colors.csv"), metadata_id = "behavior")
cruise_flight_colors <- behavior_colors %>% .[1:2]
sex_colors <- tibble(Female = col2hex("yellow"), Male = col2hex("tomato"))

## Import Data -----------------------------------------------------------------

# Import base
base <- raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

# Compile all step_data
step_data_all <-
  file.path(exp_vec, "Step_Data") %>%
  list.files(., pattern = "_step_data.rds$", full.names = TRUE) %>%
  map(readRDS) %>%
  bind_rows()

# Compile all lines_sf
step_lines_all <-
  file.path(exp_vec, "Step_Data") %>%
  list.files(., pattern = "_lines_sf.rds$", full.names = TRUE) %>%
  map(readRDS) %>%
  bind_rows()

glimpse(step_lines_all)

step_lines_bb <- st_as_sfc(bb(step_lines_all, relative = TRUE, asp.limit = 1))
base_crop <- crop(base, as_Spatial(step_lines_bb))
base_crop_agg <- aggregate(base_crop, fact = agg_factor)

for (i in c("Cruise", "Flight")){
  step_lines_i <- step_lines_all %>%
    filter(behavior_line == i)
  for (j in c("C", "N", "NS", "S")){
    print(paste0("Starting - Scenario: ", i, "; BehaviorLine: ", j))

    # Filter data
    step_lines_j <- step_lines_i %>%
      filter(scenario == j)

    # Rasterize lines
    step_lines_j_sp <- as_Spatial(step_lines_j %>% dplyr::select(geom))
    step_lines_j_rasterize <- rasterize(step_lines_j_sp, base_crop_agg,
      fun = 'count', background = 0)
    step_lines_j_rasterize[step_lines_j_rasterize <= 0] <- NA

    # Visual check
    if (mapping){
      #plot(step_lines_j_rasterize)
      dens_color <- viridis(cellStats(step_lines_j_rasterize, "max"))
      mapview(step_lines_j_rasterize, col.regions = dens_color, na.color = NA)+
      mapview(step_lines_j)
    }

    # Create aggregation factor directory
    agg_factor_dir <- file.path(line_density_dir, paste0("Agg_",
      str_pad(agg_factor, width = 2, side = "left", pad = "0")))
    if(!dir.exists(file.path(exp_output_dir, agg_factor_dir))){
      dir.create(file.path(exp_output_dir, agg_factor_dir))
    }

    # Save RDS
    saveRDS(step_lines_j_rasterize, file.path(exp_output_dir, agg_factor_dir,
      paste0("Exp_Lines_", site, "_", i, "_", j,".rds")))

    # Clean up objects
    rm(step_lines_j_sp, step_lines_j_rasterize)
  }
}

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

### OLD CODE -------------------------------------------------------------------

# sim_perch_dist_i <- list.files(path = file.path(sim_dir_i,
#   sim_calibration_dir), pattern = "sim_perch_dist_*")  %>%
#   map(~ readRDS(file.path(sim_dir_i, sim_calibration_dir, .))) %>%
#   reduce(bind_rows)
#
# for (i in seq_len(length(sims))){
#   sim_i <- sims[i]
#   sim_dir_i <- paste0(sim_dir, str_pad(sim_i, 2, side = "left", pad = "0"))
#   sim_perch_dist_i <- list.files(path = file.path(sim_dir_i,
#       sim_calibration_dir), pattern = "sim_perch_dist_*")  %>%
#     map(~ readRDS(file.path(sim_dir_i, sim_calibration_dir, .))) %>%
#     reduce(bind_rows)
#   if(i == 1){
#     sim_perch_dist <- sim_perch_dist_i
#   } else {
#     sim_perch_dist <- bind_rows(sim_perch_dist, sim_perch_dist_i)
#   }
# }
#
# # All sim files in TEMP directory
# if(FALSE){
#   sim_only_dir <- list.dirs("C:/TEMP", recursive = FALSE) %>%
#     str_subset(., "[:alpha:]-[:digit:]{2}$")
#   sim_rds_vec <- vector(mode = 'character', length = 0)
#   for (m in seq_len(length(sim_only_dir))){
#     sim_only_dir_m <- sim_only_dir[m]
#     sim_rds_m <- list.files(path = sim_only_dir_m, pattern = ".rds$")
#     sim_rds_vec <- append(sim_rds_vec, sim_rds_m)
#   }
#   sim_rds_vec <- (sim_rds_vec[!is.na(sim_rds_vec) & sim_rds_vec != ""])
#   if(FALSE) sim_rds_vec <- sim_rds_vec[c(2)]
# }
#
# # Boolean parameters
# create_kml <- FALSE
# calculate_akde <- TRUE
# map_akde <- TRUE
# view_maps <- TRUE
#
# # Coordinate systems
# wgs84 <- 4326 # WGS84 Lat/Long
# wgs84n19 <- 32619 # WGS84 UTM 19N
# crs_wgs84n19 <- CRS(SRS_string = "EPSG:32619")
#
# # ESRI Baselayers
# esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
# esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
# om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)
#
# # Base
# base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")
#
# # Maine Outline
# maine <- read_sf(file.path("C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp")) %>%
#   st_transform(., crs = wgs84) %>%
#   mutate(state = "Maine")  %>%
#   dplyr::select(state)
#
# # Use "Tmap_baselayers.R" script to get other baselayers
# maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1, width = 2))
# maine_bb <- bb_poly(bb(maine_bb_sf, ext = 1.15))
# maine_om <- read_osm(maine_bb, zoom = 5, minNumTiles = 9, type = om_nat_geo)
#
# # Directories
# input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
# sim_dir <- "C:/TEMP"
# sim_calibration_dir <- "Calibration"
# baea_calibration_dir <- "Output/Sim/Calibration"
# ridgeline_dir <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines"
# baea_akde_dir <- "Output/Analysis/Homerange"
# baea_hr_dir <- "Data/BAEA"
# wind_input_dir <- "C:/ARCGIS/Data/Wind"
# exp_turbines_dir <- "C:/ArcGIS/Data/R_Input/EXP"
#
# # Files
# me_turbines_buff_file <- file.path(wind_input_dir, "ME_Turbines_Buffer.rds")
# uswtdb_file <- file.path(wind_input_dir, "USGS_Wind_Turbine_2018-12-21",
#   "uswtdb_v1_2_20181001.shp")
# ridge_poly_file <- file.path(ridgeline_dir, "ridge_poly.shp")
# baea_akde_file <- file.path(baea_akde_dir, "hr_akde.rds")
# baea_hr_file <- file.path(baea_hr_dir, "baea_homerange.rds")
#
# # Theme (for blank background)
# theme_blank <- theme(legend.position = "none",
#   text = element_text(family = "Latin Modern Roman"),
#   plot.title = element_text(size=14),
#   panel.grid = element_blank(), axis.title = element_blank(),
#   axis.text = element_blank(), axis.ticks = element_blank(),
#   panel.background = element_rect(fill = "transparent", colour = NA),
#   plot.background = element_rect(fill = "transparent", colour = NA))
#
# # Colors
# nest_color <- "yellow"
# turbine_color <- "darkorange"
#
# # Read Sim and Visualize -------------------------------------------------------
#
# # Import wind turbines, subset, and buffer
# if(map_akde){
#   if(!file.exists(me_turbines_buff_file)){
#   uswtdb <- st_read(uswtdb_file)
#   me_turbines <- uswtdb %>%
#     filter(t_state == "ME")
#   me_turbines_radius <- me_turbines %>% pull("t_rd")
#   me_turbines_buff <- me_turbines %>%
#     st_buffer(dist = me_turbines_radius)
#   saveRDS(me_turbines_buff, me_turbines_buff_file)
#   # Clean up objects
#   rm(uswtdb_file, uwstdb, me_turbines, me_turbines_radius)
#   } else {
#     me_turbines_buff <- readRDS(me_turbines_buff_file)
#     # Clean up objects
#     rm(me_turbines_buff_file)
#   }
# }
#
# # Ridgeline Data
# if(map_akde){
#   ridge_poly <- read_sf(ridge_poly_file)
#   baea_hr <- readRDS(baea_hr_file)
#   baea_akde <- readRDS(baea_akde_file)
# }
#
# # Clean up objects
# rm(uswtdb_file, me_turbines, me_turbines_radius, ridgeline_dir, baea_akde_dir,
#   baea_hr_dir, ridge_poly_file, baea_hr_file, baea_akde_file)
#
# # Run for sim_rds_vec
# for (m in seq_len(length(sim_rds_vec))){
#   sim_rds <- sim_rds_vec[m]
#   print(sim_rds)
#
#   # Read in sim_out file
#   # File Directory and ID
#   sim_id <- tools::file_path_sans_ext(sim_rds)
#   sim_runs <- readRDS(file.path(sim_dir, sim_id, sim_rds))
#   akde_dir <- file.path(sim_dir, sim_id, "AKDEs")
#
#   for (i in seq_len(length(sim_runs))){
#     print(paste0("Starting run ", i, " of ", length(sim_runs), " at ",
#       GetDateTime()))
#     sim_out <- sim_runs %>% pluck(i)
#     sim_step_data <- CompileAllAgentsStepData(sim = sim_out) %>%
#       mutate(behavior = as.factor(behavior)) %>%
#       group_by(id) %>%
#         mutate(step_type = paste0(behavior, "_", lead(behavior))) %>%
#         mutate(previous_step_type = lag(step_type)) %>%
#       ungroup() %>%
#       filter(!is.na(datetime))
#     sim_step_data <- ConvertStepDataCoordinates(sim_step_data)
#
#     sim_step_data$behavior <- fct_recode(sim_step_data$behavior, "Cruise" = "1",
#       "Flight" = "2", "Nest" = "3", "Perch" = "4", "Roost" = "5")
#     sim_step_data$behavior <- as.character(sim_step_data$behavior)
#
#     # Create KML -----------------------------------------------------------------
#     if(create_kml){
#       # KMLs of Points and Flights
#       kml_dir = file.path(sim_dir, sim_id, "KMLs")
#
#       if(!dir.exists(kml_dir)){
#         dir.create(kml_dir)
#       }
#       for (j in unique(sim_step_data$id)){
#         print(paste0("Starting kml for ", j, " at ", GetDateTime()))
#         sim_step_data_j <- sim_step_data %>% filter(id == j)
#         ExportKMLTelemetry(sim_step_data_j, lat = "lat", long = "long",
#           alt = NULL, speed = NULL, file = paste0(sim_id, "_", str_pad(i, 2,
#           side = "left", "0"),  "_", str_pad(j, 2, side = "left", "0"), ".kml"),
#           icon_by_sex = TRUE, behavior = "behavior", point_color ="behavior",
#           output_dir = kml_dir)
#       }
#     }
#
#     # Calculate AKDE -------------------------------------------------------------
#     if(calculate_akde){
#       if(!dir.exists(akde_dir)){
#         dir.create(akde_dir)
#       }
#       akde_file <- file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
#         side = "left", "0"), ".rds"))
#       # Calculate akde (if it doesn't already exist)
#       if(!file.exists(akde_file)){
#         for (j in unique(sim_step_data$id)){
#           print(paste0("Starting adke for ", j, " at ", GetDateTime()))
#           sim_hr_j <- sim_step_data %>% filter(id == j) %>% arrange(datetime)
#           for (k in unique(year(sim_hr_j$datetime))){
#             sim_hr_k <- sim_hr_j %>% filter(year(datetime) == k)
#             print(paste0("Run:" ,i, ", ID:", j, ", Year:", k))
#             move_k <- move::move(x = sim_hr_k$x, y = sim_hr_k$y,
#               time = as.POSIXct(sim_hr_k$datetime, format = "%Y-%m-%d %H:%M:%S",
#               tz = "NYC"), data = sim_hr_k, proj = crs_wgs84n19, animal = j,
#               sensor = "GPS")
#             # Compute movement models
#             telemetry_k <- as.telemetry(move_k)
#             guess_k <- ctmm.guess(telemetry_k, interactive = FALSE)
#             fit_k <- ctmm.fit(telemetry_k, guess_k)
#             # Compute akde object
#             akde_k <- akde(telemetry_k, fit_k)
#             if(j == unique(sim_step_data$id) %>% .[1] &&
#                 k == unique(year(sim_hr_j$datetime)) %>% .[1]) {
#               hr_akde <- tibble(id = NA, year = NA, hr_akde = list(akde_k)) %>%
#                 slice(0)
#               print("Created hr_akde")
#             }
#             hr_akde <- bind_rows(hr_akde, tibble(id = j, year = k,
#               hr_akde = list(akde_k)))
#           }
#           rm(sim_hr_j, move_k, telemetry_k, guess_k, fit_k, akde_k, j, k)
#         }
#         saveRDS(hr_akde, file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
#           side = "left", "0"), ".rds")))
#         rm(hr_akde)
#       }
#     }
#
#     # Map AKDEs -----------------------------------------------------------------
#     if(map_akde){
#       # Check for akde
#       sim_akde_file <- file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
#         side = "left", "0"), ".rds"))
#       if(!file.exists(sim_akde_file)){
#           print(paste0("AKDE missing for: ", sim_id, "_", str_pad(i, 2,
#             side = "left", "0")), " Map SKIPPED.")
#       } else {
#         print(paste0("AKDE found for: ", sim_id, "_", str_pad(i, 2,
#           side = "left", "0")))
#         sim_akde <- readRDS(sim_akde_file)
#         for (j in unique(sim_step_data$id)){
#           sim_step_data_j <- sim_step_data %>% filter(id == j) %>%
#             arrange(datetime)
#           sim_akde_j <- sim_akde %>% filter(id == j)
#           for (k in unique(year(sim_step_data_j$datetime))){
#             print(paste0("ID:", j, "; ", "Year:", k))
#
#             # Create Points and UDs
#             sim_step_k <- sim_step_data_j %>% filter(year(datetime) == k) %>%
#               st_as_sf(., coords = c("x", "y"), crs = wgs84n19, agr ="constant")
#             sim_akde_k <- sim_akde_j %>% filter(year == k) %>% pull(hr_akde) %>%
#               pluck(1)
#             sim_ud_95_k <- SpatialPolygonsDataFrame.UD(sim_akde_k,
#               level.UD = 0.95, level = 0.95) %>% st_as_sf(.) %>% slice(2)
#             sim_ud_50_k <- SpatialPolygonsDataFrame.UD(sim_akde_k,
#               level.UD = 0.5, level = 0.95) %>% st_as_sf(.) %>% slice(2)
#
#             # Create Flightpaths
#             sim_lines_k <- sim_step_k %>% group_by(id) %>% arrange(datetime) %>%
#               dplyr::summarize(m = mean(year(datetime)), do_union = FALSE,
#                 .groups = "drop") %>% st_cast("LINESTRING")
#
#             # Map Points and Flightpaths
#             if(view_maps){
#               mapview(list(sim_step_k, sim_lines_k),
#                 zcol = list("behavior", NULL),
#                 legend = list(TRUE, FALSE),
#                 homebutton = list(TRUE, FALSE))
#             }
#
#             # Create Nest Point
#             nest_id_j <- sim_out$agents$input %>% filter(id == j) %>%
#                pull(nest_id)
#             nest_j <- sim_out$agents$input %>% filter(id == j) %>%
#               st_as_sf(., coords = c("start_x", "start_y"), crs = 32619) %>%
#               st_transform(., crs = 4326)
#
#             # Get map bb (for final map extent)
#             sim_k_bb1_sfc <- st_as_sfc(bb(sim_step_k, relative = TRUE,
#               height = 1, width = 1)) %>%
#               st_transform(., crs = as.character(OpenStreetMap::osm()))
#
#             # Experiment turbines
#             if(str_detect(sim_id, "_C-")){
#               wilson_wt <- st_read(file.path(exp_turbines_dir,
#                 "wilson_n_turbines.shp")) %>% slice(0)
#             }
#             if(str_detect(sim_id, "_N-")){
#               wilson_wt <- st_read(file.path(exp_turbines_dir,
#                 "wilson_n_turbines.shp"))
#             }
#             if(str_detect(sim_id, "_S-")){
#               wilson_wt <- st_read(file.path(exp_turbines_dir,
#                 "wilson_s_turbines.shp"))
#             }
#             if(str_detect(sim_id, "_NS-")){
#               wilson_wt_n <- st_read(file.path(exp_turbines_dir,
#                 "wilson_n_turbines.shp"))
#               wilson_wt_s <- st_read(file.path(exp_turbines_dir,
#                 "wilson_s_turbines.shp"))
#               wilson_wt <- bind_rows(wilson_wt_n, wilson_wt_s)
#             }
#             wilson_wt_buff <- wilson_wt %>% st_buffer(56)
#             if(FALSE) mapview(wilson_wt_buff)
#
#             # Clip ridgeline polygon
#             ridge_poly_clip <- ridge_poly %>%
#               st_transform(., st_crs(sim_k_bb1_sfc)) %>%
#               st_crop(., bb(sim_k_bb1_sfc, ext = 2))
#
#             # Get map scalebar distances
#             dist_sf <- st_as_sfc(bb(sim_k_bb1_sfc, relative = TRUE,
#               height = 1, width = 1))
#             x_dist <- as.numeric(approx_distances(bb(dist_sf,
#               ext = 1.15))[1])/1000/5
#             x_breaks <- as.numeric(unlist(scales::cbreaks(c(0,
#               x_dist), scales::pretty_breaks(2))[1]))
#
#             # Get osm baselayer for sim_step_sf_k and baea_step_sf_id
#             sim_k_bb2_sfc <- st_as_sfc(bb(sim_step_k, relative = TRUE,
#               height = 2, width = 2)) %>%
#               st_transform(., crs = as.character(OpenStreetMap::osm()))
#             sim_om <- sim_k_bb2_sfc %>%
#               read_osm(., minNumTiles = 21,
#               type = om_nat_geo)  # may need to add and adjust 'zoom' arg
#
#             # Home range, points, and flight paths
#             if(nrow(wilson_wt_buff) > 0){
#               sim_k_map <-
#                 tm_layout(asp = 1) +
#                 tm_shape(sim_om, raster.downsample = FALSE) +
#                   tm_rgb() +
#                 tm_shape(sim_lines_k) +
#                   tm_lines("#ffffff", lwd = 2, alpha = .35) +
#                 tm_shape(sim_step_k, bbox = bb(sim_k_bb1_sfc, ext = 1.1),
#                     is.master = TRUE) +
#                   tm_dots(size = 0.075, col = "#700074", alpha = .5) +
#                 tm_shape(ridge_poly_clip) +
#                   tm_borders(col = "wheat4", alpha = .6) +
#                 tm_shape(ridge_poly_clip) +
#                   tm_fill("forestgreen", alpha = .4) +
#                 tm_shape(wilson_wt_buff) +
#                   tm_polygons(col = turbine_color, border.col = "black",
#                     lwd = 1) +
#                 tm_shape(sim_ud_95_k) +
#                   tm_polygons(col = "yellow", alpha = .15) +
#                 tm_shape(sim_ud_95_k) +
#                   tm_borders(col= "yellow", lwd = 2) +
#                 tm_shape(sim_ud_50_k) +
#                   tm_polygons(col = "red", alpha = .15) +
#                 tm_shape(sim_ud_50_k) +
#                   tm_borders(col = "red", lwd = 2) +
#                 tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
#                   main.title.position = "center",
#                   main.title.size = 1.15,
#                   title.snap.to.legend = TRUE) +
#                 tm_legend(title.size = 1, text.size = .85,
#                   outside = TRUE, position = c("right", "bottom")) +
#                 tm_scale_bar(text.size = .65,
#                   breaks = x_breaks,
#                   position = c(.05, .01)) +
#                 tm_compass(type = "4star",  show.labels = 1, size = 2.2,
#                   position = c(.875, .86)) +
#                 tm_credits(sim_id, bg.color = "white", position = c("LEFT",
#                   "TOP")) +
#                 tm_xlab("") + tm_ylab("")
#             } else {
#               sim_k_map <-
#                 tm_layout(asp = 1) +
#                 tm_shape(sim_om, raster.downsample = FALSE) +
#                   tm_rgb() +
#                 tm_shape(sim_lines_k) +
#                   tm_lines("#ffffff", lwd = 2, alpha = .35) +
#                 tm_shape(sim_step_k, bbox = bb(sim_k_bb1_sfc, ext = 1.1),
#                     is.master = TRUE) +
#                   tm_dots(size = 0.075, col = "#700074", alpha = .5) +
#                 tm_shape(ridge_poly_clip) +
#                   tm_borders(col = "wheat4", alpha = .6) +
#                 tm_shape(ridge_poly_clip) +
#                   tm_fill("forestgreen", alpha = .4) +
#                 tm_shape(sim_ud_95_k) +
#                   tm_polygons(col = "yellow", alpha = .15) +
#                 tm_shape(sim_ud_95_k) +
#                   tm_borders(col= "yellow", lwd = 2) +
#                 tm_shape(sim_ud_50_k) +
#                   tm_polygons(col = "red", alpha = .15) +
#                 tm_shape(sim_ud_50_k) +
#                   tm_borders(col = "red", lwd = 2) +
#                 tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
#                   main.title.position = "center",
#                   main.title.size = 1.15,
#                   title.snap.to.legend = TRUE) +
#                 tm_legend(title.size = 1, text.size = .85,
#                   outside = TRUE, position = c("right", "bottom")) +
#                 tm_scale_bar(text.size = .65,
#                   breaks = x_breaks,
#                   position = c(.05, .01)) +
#                 tm_compass(type = "4star",  show.labels = 1, size = 2.2,
#                   position = c(.875, .86)) +
#                 tm_credits(sim_id, bg.color = "white", position = c("LEFT",
#                   "TOP")) +
#                 tm_xlab("") + tm_ylab("")
#             }
#             # Inset map
#             combined_bb = CreateMapExtentBB(sim_k_bb1_sfc, asp = 1,
#               ext = 1.15)
#             maine_overview <-
#               tm_shape(maine_om, raster.downsample = FALSE) +
#                 tm_rgb() +
#               tm_shape(maine) + # setting this as master sets lat/long
#                 tm_borders(col = "black") +
#               tm_shape(combined_bb) +
#                 tm_borders(col = "red")
#
#             # Export to sim akde folder
#             sim_k_map_file <- file.path(akde_dir,
#               paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"), "-",
#                  str_pad(j, 2, side = "left", "0"), ".png"))
#             tmap_save(tm = sim_k_map, filename = sim_k_map_file,
#               insets_tm = maine_overview,
#               insets_vp =  viewport(x = 0.881, y = 0.1, width = 0.2,
#                 height = 0.2),
#               unit = "in", dpi = 300, height = 4, width = 4)
#           }
#         }
#       }
#     }
#   }
# }

# Export to LaTeX Folder ------------------------------------------- #
# tmap_save(tm = sim_k_hr_paths, filename = file.path(tex_dir,
#   "Figures/Ch3/Sim_HR_Maps", paste0(i, "_", j, ".svg")),
#   insets_tm = maine_overview,
#   insets_vp =  viewport(x = 0.881, y = 0.147, width = 0.2,
#     height = 0.2),
#   unit = "in", dpi = 300, height = 6, width = 6)

# tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "black",
#   alpha = 1, ticks = TRUE, lines = FALSE, labels.col = "grey25",
#   labels.format = list(format = "f", big.mark = ""),
#   labels.inside.frame = FALSE) +

# Create Raster of Locations
# base_k <- crop(base, as(st_as_sfc(st_bbox(sim_step_sf_k)) %>%
#   st_transform(., crs = wgs84n19), "Spatial"), snap = "out")
#
# sim_raster_k <- rasterize(sim_step_sf_k, base_k, field = 1,
#   fun = 'count', background = NA, mask = FALSE, update = FALSE,
#   updateValue = 'all', na.rm = TRUE)

# Maps of Point Locations and Path Maps

# Rasters of Location Density

# destination_raster <- rasterize(destination_xy, prob_raster, field = 1,
#    fun = 'sum', background = NA, mask = FALSE, update = FALSE,
#    updateValue = 'all', filename = "", na.rm = TRUE)
#
# locs_dir = "Output/Sim/01_BehaviorMove/Plots/Daily_Locations"
# for (i in unique(sim_step_data$id)){
#   PlotLocationSunriseSunset(df = sim_step_data %>% filter(id == i),
#     by = "id", color_factor = "behavior", individual = "", start = "", end = "",
#     breaks = "14 days", tz = "Etc/GMT+5", addsolartimes = FALSE, wrap = TRUE)
#   SaveGGPlot(file.path(locs_dir ,paste0("DailyLocs_", str_pad(i, 2,
#     side = "left", "0"), ".png")))
# }
#
# title_sim = "Daily Behavior Distributions (simulated data)"
# PlotBehaviorProportionBar(sim_step_data, title = title_sim)
# SaveGGPlot("Results/Sim/01_BehaviorMove/Behavior/Proportion_Bar_SIM.png")
#
# # Plots of original behavior
# baea_behavior <- readRDS(file="Data/Baea/baea_behavior.rds")
# behave_dir <- "Results/Analysis/Plots/Behavior"
#
# PlotLocationSunriseSunset(df=baea_behavior %>% as.data.frame() %>%
#     filter(id == "Three"),
#   by = "id", color_factor = "behavior", individual = "", start = "2015-03-20",
#   end = "2015-09-20", breaks = "14 days", tz = "Etc/GMT+5",
#   addsolartimes = FALSE, wrap = TRUE)
# SaveGGPlot(file.path(behave_dir ,paste0("DailyLocs_Three.png")))
#
# PlotLocationSunriseSunset(df = baea_behavior %>% as.data.frame() %>%
#     filter(id == "Ellis"),
#   by = "id", color_factor = "behavior", individual = "",
#   start = "2016-03-20", end = "2016-09-20", breaks = "10 days",
#   tz = "Etc/GMT+5", addsolartimes = FALSE, wrap = TRUE)
# SaveGGPlot(file.path(behave_dir ,paste0("DailyLocs_Ellis.png")))

# destination_raster <- rasterize(destination_xy, prob_raster, field = 1,
#   fun = 'sum', background = NA, mask = FALSE, update = FALSE,
#   updateValue = 'all', filename = "", na.rm = TRUE)
#
# plot(prob_raster)
# plot(destination_raster)
# Plot3DRaster(destination_raster, col = viridis::viridis(20),
#   main = "Probability Plot")
#
# nest_locs <- sim_step_data %>% dplyr::filter(behavior == 3)
#
# # Plots of Sim Daily Behavior
# locs_dir = "Output/Sim/01_BehaviorMove/Plots/Daily_Locations"
# for (i in unique(sim_step_data$id)){
#   PlotLocationSunriseSunset(df = sim_step_data %>% filter(id == i),
#     by = "id", color_factor = "behavior", individual = "", start = "", end = "",
#     breaks = "14 days", tz = "Etc/GMT+5", addsolartimes = FALSE, wrap = TRUE)
#   SaveGGPlot(file.path(locs_dir ,paste0("DailyLocs_", str_pad(i, 2,
#     side = "left", "0"), ".png")))
# }
# title_sim = "Daily Behavior Distributions (simulated data)"
# PlotBehaviorProportionBar(sim_step_data, title = title_sim)
# SaveGGPlot("Results/Sim/01_BehaviorMove/Behavior/Proportion_Bar_SIM.png")
#
#
# # Plots of Empirical Daily Behavior
# baea_behavior <- readRDS(file="Data/Baea/baea_behavior.rds")
# behave_dir <- "Results/Analysis/Plots/Behavior"
#
# PlotLocationSunriseSunset(df=baea_behavior %>% as.data.frame() %>%
#     filter(id == "Three"),
#   by = "id", color_factor = "behavior", individual = "", start = "2015-03-20",
#   end = "2015-09-20", breaks = "14 days", tz = "Etc/GMT+5",
#   addsolartimes = FALSE, wrap = TRUE)
# SaveGGPlot(file.path(behave_dir ,paste0("DailyLocs_Three.png")))
#
# PlotLocationSunriseSunset(df = baea_behavior %>% as.data.frame() %>%
#     filter(id == "Ellis"),
#   by = "id", color_factor = "behavior", individual = "",
#   start = "2016-03-20", end = "2016-09-20", breaks = "10 days",
#   tz = "Etc/GMT+5", addsolartimes = FALSE, wrap = TRUE)
# SaveGGPlot(file.path(behave_dir ,paste0("DailyLocs_Ellis.png")))


# # Calculate akde
# for (i in unique(sim_step_data$id)){
#   sim_hr_i <- sim_step_data %>% filter(id == i) %>% arrange(datetime)
#   for (j in unique(year(sim_hr_i$datetime))){
#     sim_hr_k <- sim_hr_i %>% filter(year(datetime) == j)
#     print(paste0("ID:", i, "; ", "Year:", j))
#     move_k <- move(x = sim_hr_k$x, y = sim_hr_k$y,
#       time = as.POSIXct(sim_hr_k$datetime, format = "%Y-%m-%d %H:%M:%S",
#       tz = "NYC"), data = sim_hr_k, proj = wgs84n19, animal = i,
#       sensor = "GPS")
#     # Compute movement models
#     telemetry_k <- as.telemetry(move_k)
#     guess_k <- ctmm.guess(telemetry_k, interactive = FALSE)
#     fit_k <- ctmm.fit(telemetry_k, guess_k)
#     # Compute akde object
#     akde_k <- akde(telemetry_k, fit_k)
#     if(i == unique(sim_step_data$id) %>% .[1] &&
#         j == unique(year(sim_hr_i$datetime)) %>% .[1]) {
#       hr_akde <- tibble(id = NA, year = NA, hr_akde = list(akde_k)) %>%
#         slice(0)
#       print("Created hr_land_metrics and hr_akde")
#     }
#     hr_akde <- bind_rows(hr_akde, tibble(id = i, year = j,
#       hr_akde = list(akde_k)))
#   }
#   rm(sim_hr_i, move_k, telemetry_k, guess_k, fit_k, akde_k, i, j)
# }
# #saveRDS(hr_akde, "C:/TEMP/hr_akde_20200823-02.rds")
# hr_akde <- readRDS("C:/TEMP/hr_akde_20200823-02.rds")

#----------------------- Sim Visualization Sim --------------------------------#
# This script is used to create visual a 'sim' object
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------
pacman::p_load(cartography, ctmm, dplyr, fasterize, forcats, ggplot2,
  ggpubr, ggthemes, grid, leaflet, lubridate, magick, mapview, move,
  OpenStreetMap, plotly, prettymapr, purrr, raster, rosm, rsvg, sf, stringr, s2,
  tmap, tmaptools, viridis, units, webshot, zoo)
pacman::p_load(baear, gisr, ibmr)
set_thin_PROJ6_warnings(TRUE)
theme_update(plot.title = element_text(hjust = 0.5))
suppressMessages(extrafont::loadfonts(device = "win"))
#devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/ibmr")

# Parameters
sim_rds_vec <- "sim_20210725-63.rds"
behavior_type <- "stationary"  # "air" or "stationary"

# All sim files in TEMP directory
if(FALSE){
  sim_only_dir <- list.dirs("C:/TEMP", recursive = FALSE) %>%
    str_subset(., "[:digit:]-[:digit:]{2}$")
  sim_rds_vec <- vector(mode = 'character', length = 0)
  for (m in seq_len(length(sim_only_dir))){
    sim_only_dir_m <- sim_only_dir[m]
    sim_rds_m <- list.files(path = sim_only_dir_m, pattern = ".rds$")
    sim_rds_vec <- append(sim_rds_vec, sim_rds_m)
  }
  sim_rds_vec <- (sim_rds_vec[!is.na(sim_rds_vec) & sim_rds_vec != ""])
  if(FALSE) sim_rds_vec <- sim_rds_vec[c(49:61)]
}

# Parameters
create_kml <- FALSE
calculate_akde <- FALSE
recalculate_akde <- FALSE
match_baea <- TRUE
map_akde <- TRUE
view_maps <- FALSE

# Coordinate systems
wgs84 <- 4326 # WGS84 Lat/Long
wgs84n19 <- 32619 # WGS84 UTM 19N
crs_wgs84n19 <- CRS(SRS_string = "EPSG:32619")

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

# Base
base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")

# Maine Outline
maine <- read_sf(file.path("C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp")) %>%
  st_transform(., crs = wgs84) %>%
  mutate(state = "Maine")  %>%
  dplyr::select(state)

# Use "Tmap_baselayers.R" script to get other baselayers
maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1, width = 2))
maine_bb <- bb_poly(bb(maine_bb_sf, ext = 1.15))
maine_om <- read_osm(maine_bb, zoom = 5, minNumTiles = 9, type = om_nat_geo)

# Directories
input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
sim_dir <- "C:/TEMP"
sim_calibration_dir <- "Calibration"
baea_calibration_dir <- "Output/Sim/Calibration"
ridgeline_dir <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines"
baea_akde_dir <- "Output/Analysis/Homerange"
baea_hr_dir <- "Data/BAEA"
wind_input_dir <- "C:/ARCGIS/Data/Wind"

# Files
uswtdb_file <- file.path(wind_input_dir, "USGS_Wind_Turbine_2018-12-21",
  "uswtdb_v1_2_20181001.shp")
me_turbines_buff_file <- file.path(wind_input_dir, "ME_Turbines_Buffer.rds")
ridge_poly_file <- file.path(ridgeline_dir, "ridge_poly.shp")
baea_akde_file <- file.path(baea_akde_dir, "hr_akde.rds")
baea_hr_file <- file.path(baea_hr_dir, "baea_homerange.rds")

# Theme (for blank background)
theme_blank <- theme(legend.position = "none",
  text = element_text(family = "Latin Modern Roman"),
  plot.title = element_text(size=14),
  panel.grid = element_blank(), axis.title = element_blank(),
  axis.text = element_blank(), axis.ticks = element_blank(),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA))

# Colors
nest_color <- "yellow"
turbine_color <- "darkorange"

# Read Sim and Visualize -------------------------------------------------------

# Import wind turbines, subset, and buffer
if(map_akde){
  if(!file.exists(me_turbines_buff_file)){
  uswtdb <- st_read(uswtdb_file)
  me_turbines <- uswtdb %>%
    filter(t_state == "ME")
  me_turbines_radius <- me_turbines %>% pull("t_rd")
  me_turbines_buff <- me_turbines %>%
    st_buffer(dist = me_turbines_radius)
  saveRDS(me_turbines_buff, me_turbines_buff_file)
  # Clean up objects
  rm(uswtdb_file, uwstdb, me_turbines, me_turbines_radius)
  } else {
    me_turbines_buff <- readRDS(me_turbines_buff_file)
    # Clean up objects
    rm(me_turbines_buff_file)
  }
}

# Ridgeline Data
if(map_akde){
  ridge_poly <- read_sf(ridge_poly_file)
  baea_hr <- readRDS(baea_hr_file)
  baea_akde <- readRDS(baea_akde_file)
  # Clean up objects
  rm(ridgeline_dir, ridge_poly_file, baea_hr_file, baea_akde_file)
}

# Run for sim_rds_vec
for (m in seq_len(length(sim_rds_vec))){
  sim_rds <- sim_rds_vec[m]
  print(sim_rds)
  # Read in sim_out file
  sim_id <- tools::file_path_sans_ext(sim_rds)
  sim_runs <- readRDS(file.path(sim_dir, sim_id, sim_rds))
  akde_dir <- file.path(sim_dir, sim_id, "AKDEs")
  for (i in seq_len(length(sim_runs))){
    print(paste0("Starting run ", i, " of ", length(sim_runs), " at ",
      GetDateTime()))
    sim_out <- sim_runs %>% pluck(i)
    sim_step_data <- CompileAllAgentsStepData(sim = sim_out) %>%
      mutate(behavior = as.factor(behavior)) %>%
      group_by(id) %>%
        mutate(step_type = paste0(behavior, "_", lead(behavior))) %>%
        mutate(previous_step_type = lag(step_type)) %>%
      ungroup() %>%
      filter(!is.na(datetime))
    sim_step_data <- ConvertStepDataCoordinates(sim_step_data)
    sim_step_data$behavior <- fct_recode(sim_step_data$behavior, "Cruise" = "1",
      "Flight" = "2", "Nest" = "3", "Perch" = "4", "Roost" = "5")
    sim_step_data$behavior <- as.character(sim_step_data$behavior)

    # Create KML ---------------------------------------------------------------
    if(create_kml){
      # KMLs of Points and Flights
      kml_dir = file.path(sim_dir, sim_id, "KMLs")
      if(!dir.exists(kml_dir)){
        dir.create(kml_dir)
      }
      for (j in unique(sim_step_data$id)){
        print(paste0("Starting kml for ", j, " at ", GetDateTime()))
        sim_step_data_j <- sim_step_data %>% filter(id == j)
        ExportKMLTelemetry(sim_step_data_j, lat = "lat", long = "long",
          alt = NULL, speed = NULL, file = paste0(sim_id, "_", str_pad(i, 2,
          side = "left", "0"),  "_", str_pad(j, 2, side = "left", "0"), ".kml"),
          icon_by_sex = TRUE, behavior = "behavior", point_color ="behavior",
          output_dir = kml_dir)
      }
    }

    # Calculate AKDE -----------------------------------------------------------
    if(calculate_akde){
      if(!dir.exists(akde_dir)){
        dir.create(akde_dir)
      }
      if(match_baea){
        akde_file <- file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
          side = "left", "0"), "_matched.rds"))
      } else {
        saveRDS(hr_akde, file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
          side = "left", "0"), ".rds")))
      }
      # Calculate akde (if it doesn't already exist)
      if(!file.exists(akde_file) | recalculate_akde == TRUE){
        for (j in unique(sim_step_data$id)){
          print(paste0("Starting adke for ", j, " at ", GetDateTime()))
          sim_hr_j <- sim_step_data %>% filter(id == j) %>% arrange(datetime)
          if(match_baea){
            # Create Nest Point
            nest_id_j <- sim_out$agents$input %>% filter(id == j) %>%
               pull(nest_id)
            nest_j <- sim_out$agents$input %>% filter(id == j) %>%
              st_as_sf(., coords = c("start_x", "start_y"), crs = 32619) %>%
              st_transform(., crs = 4326)
            # Pick baea to match sim
            if(nest_id_j == "282A"){
              baea_id <- "Ellis"
              baea_year <- 2016
            }
            if(nest_id_j == "659A"){
              baea_id <- "Hebron"
              baea_year <- 2016
            }
            if(nest_id_j == "446R01"){
              baea_id <- "Musquash"
              baea_year <- 2016
            }
            if(nest_id_j == "423R01"){
              baea_id <- "Sandy"
              baea_year <- 2017
            }
            if(nest_id_j == "434R01"){
              baea_id <- "Wilson"
              baea_year <- 2015
            }
            baea_step_id <- baea_hr %>% filter(id == baea_id) %>%
              arrange(datetime) %>% st_as_sf(., coords = c("long_utm",
              "lat_utm"), crs = 32619, agr = "constant") %>%
              filter(year(date) == baea_year)
            for (k in unique(year(sim_hr_j$datetime))){
              sim_hr_k <- sim_hr_j %>% filter(year(datetime) == k)
              # Get vector of baea datetime and match closest sim values so
              # the sim values are very close to the baea values
              baea_vec <- baea_step_id %>% pull(datetime) %>%
                with_tz(., tzone = "Etc/GMT+5")
              year(baea_vec) <- k
              sim_vec <- sim_hr_k %>% pull(datetime) %>% with_tz(.,
                tzone = "Etc/GMT+5")
              for (p in seq_len(length(baea_vec))) {
                baea_vec_p <- baea_vec[p]
                closest_match <- as.difftime(baea_vec_p - sim_vec) %>%
                  as.numeric(.) %>%
                  abs(.) %>%
                  which.min(.)
                sim_datetime_p <- sim_vec[closest_match]
                if(p == 1){
                  sim_vec_out <- sim_datetime_p
                } else {
                  sim_vec_out <- c(sim_vec_out, sim_datetime_p)
                }
                sim_vec <- sim_vec[-closest_match]
              }
              # Subset sim data to match baea datetimes
              sim_hr_k <- sim_hr_k %>%
                filter(datetime %in% sim_vec_out) %>%
                arrange(datetime)
              # Compare datetime between baea and sim
              if(FALSE){
                gg_baea_i <- baea_step_id %>%
                    mutate(dt_day = date(datetime),
                      dt_time = hour(datetime) + (minute(datetime)/60)) %>%
                  ggplot(data = ., aes(x = dt_day, y = dt_time)) +
                  geom_point(color = "darkgreen") +
                  labs(x = "Date", y = "Hour", title = paste0("BAEA: ",
                    baea_id))
                gg_sim_i <- sim_hr_k %>%
                    mutate(dt_day = date(datetime),
                      dt_time = hour(datetime) + (minute(datetime)/60)) %>%
                  ggplot(data = ., aes(x = dt_day, y = dt_time)) +
                  geom_point(color = "navyblue") +
                  labs(x = "Date", y = "Hour", title = paste0("Sim: ",
                    unique(sim_hr_k$id)))
                ggarrange(gg_baea_i + rremove("x.text") + rremove("xlab"),
                  gg_sim_i, ncol = 1, nrow = 2)
              }
            }
            print(paste0("Run:", i, ", ID:", j, ", Year:", k))
            move_k <- move::move(x = sim_hr_k$x, y = sim_hr_k$y,
              time = as.POSIXct(sim_hr_k$datetime, format = "%Y-%m-%d %H:%M:%S",
              tz = "NYC"), data = sim_hr_k, proj = crs_wgs84n19, animal = j,
              sensor = "GPS")
            # Compute movement models
            telemetry_k <- as.telemetry(move_k)
            guess_k <- ctmm.guess(telemetry_k, interactive = FALSE)
            fit_k <- ctmm.fit(telemetry_k, guess_k)
            # Compute akde object
            akde_k <- akde(telemetry_k, fit_k)
            if(j == unique(sim_step_data$id) %>% .[1] &&
                k == unique(year(sim_hr_j$datetime)) %>% .[1]) {
              hr_akde <- tibble(id = NA, year = NA, hr_akde = list(akde_k)) %>%
                slice(0)
              print("Calculated hr_akde")
            }
            hr_akde <- bind_rows(hr_akde, tibble(id = j, year = k,
              hr_akde = list(akde_k)))
          }
          rm(sim_hr_j, move_k, telemetry_k, guess_k, fit_k, akde_k, j, k)
        }
        if(match_baea){
        saveRDS(hr_akde, file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
          side = "left", "0"), "_matched.rds")))
        } else {
        saveRDS(hr_akde, file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
          side = "left", "0"), ".rds")))
        }
        rm(hr_akde)
      }
    }

    # Map AKDEs ----------------------------------------------------------------
    if(map_akde){
      # Check for akde
      if(match_baea){
        sim_akde_file <- file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
          side = "left", "0"), "_matched.rds"))
      } else {
        sim_akde_file <- file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
          side = "left", "0"), ".rds"))
      }
      if(!file.exists(sim_akde_file)){
        print(paste0("AKDE missing for: ", sim_id, "_", str_pad(i, 2,
          side = "left", "0")), " Map SKIPPED.")
      } else {
        print(paste0("AKDE found for: ", sim_id, "_", str_pad(i, 2,
          side = "left", "0")))
        sim_akde <- readRDS(sim_akde_file)
        for (j in unique(sim_step_data$id)){
          sim_step_data_j <- sim_step_data %>% filter(id == j) %>%
            arrange(datetime)
          sim_akde_j <- sim_akde %>% filter(id == j)
          for (k in unique(year(sim_step_data_j$datetime))){
            print(paste0("ID:", j, "; ", "Year:", k))
            # Create nest point and baea data
            nest_id_j <- sim_out$agents$input %>% filter(id == j) %>%
               pull(nest_id)
            nest_j <- sim_out$agents$input %>% filter(id == j) %>%
              st_as_sf(., coords = c("start_x", "start_y"), crs = 32619) %>%
              st_transform(., crs = 4326)
            # Pick baea to match sim
            if(nest_id_j == "282A"){
              baea_id <- "Ellis"
              baea_year <- 2016
            }
            if(nest_id_j == "659A"){
              baea_id <- "Hebron"
              baea_year <- 2016
            }
            if(nest_id_j == "446R01"){
              baea_id <- "Musquash"
              baea_year <- 2016
            }
            if(nest_id_j == "423R01"){
              baea_id <- "Sandy"
              baea_year <- 2017
            }
            if(nest_id_j == "434R01"){
              baea_id <- "Wilson"
              baea_year <- 2015
            }
            baea_step_id <- baea_hr %>% filter(id == baea_id) %>%
              arrange(datetime) %>% st_as_sf(., coords = c("long_utm",
              "lat_utm"), crs = 32619, agr = "constant") %>%
              filter(year(date) == baea_year) %>%
              mutate(behavior = if_else(speed < 5, "Stationary", "Air"))
            baea_akde_id <- baea_akde %>%  filter(id == baea_id) %>%
              filter(year == baea_year) %>% pull(hr_akde) %>% pluck(1)
            baea_ud_95_id <- SpatialPolygonsDataFrame.UD(baea_akde_id,
                level.UD = 0.95, level = 0.95) %>% st_as_sf(.) %>% slice(2)
            baea_ud_50_id <- SpatialPolygonsDataFrame.UD(baea_akde_id,
              level.UD = 0.5, level = 0.95) %>% st_as_sf(baea_ud_50_sp_id) %>%
              slice(2)
            # Filter data, create fightpaths
            baea_lines_id <- baea_step_id %>% group_by(id) %>%
              arrange(datetime) %>%
              summarize(m = mean(year), do_union = FALSE) %>%
              st_cast("LINESTRING")
            # Create sim points and UDs
            sim_step_k <- sim_step_data_j %>% filter(year(datetime) == k) %>%
              st_as_sf(., coords = c("x", "y"), crs = wgs84n19, agr ="constant")
            if(match_baea){
              # Get vector of baea datetime and match closest sim values so
              # the sim values are very close to the baea values
              baea_vec <- baea_step_id %>% pull(datetime) %>%
                with_tz(., tzone = "Etc/GMT+5")
              year(baea_vec) <- k
              sim_vec <- sim_step_k %>% pull(datetime) %>% with_tz(.,
                tzone = "Etc/GMT+5")
              for (p in seq_len(length(baea_vec))) {
                baea_vec_p <- baea_vec[p]
                closest_match <- as.difftime(baea_vec_p - sim_vec) %>%
                  as.numeric(.) %>%
                  abs(.) %>%
                  which.min(.)
                sim_datetime_p <- sim_vec[closest_match]
                if(p == 1){
                  sim_vec_out <- sim_datetime_p
                } else {
                  sim_vec_out <- c(sim_vec_out, sim_datetime_p)
                }
                sim_vec <- sim_vec[-closest_match]
              }
              # Subset sim data to match baea datetimes
              sim_step_k <- sim_step_k %>%
                filter(datetime %in% sim_vec_out) %>%
                arrange(datetime)
            }
            sim_akde_k <- sim_akde_j %>% filter(year == k) %>% pull(hr_akde) %>%
              pluck(1)
            sim_ud_95_k <- SpatialPolygonsDataFrame.UD(sim_akde_k,
              level.UD = 0.95, level = 0.95) %>% st_as_sf(.) %>% slice(2)
            sim_ud_50_k <- SpatialPolygonsDataFrame.UD(sim_akde_k,
              level.UD = 0.5, level = 0.95) %>% st_as_sf(.) %>% slice(2)
            # Create Flightpaths
            sim_lines_k <- sim_step_k %>% group_by(id) %>% arrange(datetime) %>%
              dplyr::summarize(m = mean(year(datetime)), do_union = FALSE,
                .groups = "drop") %>% st_cast("LINESTRING")
            # Map Points and Flightpaths
            if(view_maps){
              mapview(list(sim_step_k, sim_lines_k),
                zcol = list("behavior", NULL),
                legend = list(TRUE, FALSE),
                homebutton = list(TRUE, FALSE))
            }
            # Get combined map bb (for final map extent) and
            sim_k_bb1_sfc <- st_as_sfc(bb(sim_step_k, relative = TRUE,
              height = 1, width = 1)) %>%
              st_transform(., crs = as.character(OpenStreetMap::osm()))
            baea_id_bb1_sfc <- st_as_sfc(bb(baea_step_id, relative = TRUE,
              height = 1, width = 1)) %>%
              st_transform(., crs = as.character(OpenStreetMap::osm()))
            combined_bb1_sfc <- st_union(sim_k_bb1_sfc, baea_id_bb1_sfc)
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
            sim_k_bb2_sfc <- st_as_sfc(bb(sim_step_k, relative = TRUE,
              height = 2, width = 2)) %>%
              st_transform(., crs = as.character(OpenStreetMap::osm()))
            baea_id_bb2_sfc <- st_as_sfc(bb(baea_step_id, relative = TRUE,
              height = 2, width = 2)) %>%
              st_transform(., crs = as.character(OpenStreetMap::osm()))
            combined_om <- st_union(sim_k_bb2_sfc, baea_id_bb2_sfc) %>%
              read_osm(., minNumTiles = 21,
              type = om_nat_geo)  # may need to add and adjust 'zoom' arg

            if(behavior_type == "air"){
              baea_step_id_behavior <- baea_step_id %>%
                filter(speed > 4)
              sim_step_k_behavior <- sim_step_k %>%
                filter(behavior %in% c("Cruise", "Flight"))
            }
            if(behavior_type == "stationary"){
              baea_step_id_behavior <- baea_step_id %>%
                filter(speed < 5)
              sim_step_k_behavior <- sim_step_k %>%
                filter(behavior %in% c("Nest", "Perch", "Roost"))
            }

            # Home range, points, and flight paths
            baea_id_map <-
              tm_layout(asp = 1) +
              tm_shape(combined_om, raster.downsample = FALSE) +
                tm_rgb() +
              tm_shape(baea_lines_id) +
                tm_lines("#ffffff", lwd = 2, alpha = .35) +
              tm_shape(baea_step_id_behavior,
                  bbox = bb(combined_bb1_sfc, ext = 1.1),
                  is.master = TRUE) +
                tm_dots(size = 0.075, col = "#700074", alpha = .5) +
              tm_shape(ridge_poly_clip) +
                tm_borders(col = "wheat4", alpha = .6) +
              tm_shape(ridge_poly_clip) +
                tm_fill("forestgreen", alpha = .4) +
              tm_shape(me_turbines_buff_clip, title = "Wind Turbines") +
                tm_polygons(col = turbine_color,
                  border.col = "black",  lwd = 1) +
              tm_shape(baea_ud_95_id) +
                tm_polygons(col = "yellow", alpha = .15) +
              tm_shape(baea_ud_95_id) +
                tm_borders(col= "yellow", lwd = 2) +
              tm_shape(baea_ud_50_id) +
                tm_polygons(col = "red", alpha = .15) +
              tm_shape(baea_ud_50_id) +
                tm_borders(col= "red", lwd = 2) +
              tm_layout(main.title = NULL,
                main.title.position = "center",
                main.title.size = 1.15,
                title.snap.to.legend = TRUE) +
              tm_legend(title.size = 1, text.size = .85,
                outside = TRUE, position = c("right", "bottom")) +
              tm_scale_bar(text.size = .65,
                breaks = combined_x_breaks,
                position = c(.05, .01)) +
              tm_compass(type = "4star",  show.labels = 1, size = 2.2,
                position = c(.875, .86)) +
              tm_credits(paste0(baea_id, " - ", baea_year, "\n", "Total (n = ",
                nrow(baea_step_id), ")", "\n", str_to_title(behavior_type),
                " (n = ", nrow(baea_step_id_behavior), ")"),
                bg.color = "white",
                position = c("LEFT", "TOP")) +
              tm_xlab("") + tm_ylab("")
            # Home range, points, and flight paths
            sim_k_map <-
              tm_layout(asp = 1) +
              tm_shape(combined_om, raster.downsample = FALSE) +
                tm_rgb() +
              tm_shape(sim_lines_k) +
                tm_lines("#ffffff", lwd = 2, alpha = .35) +
              tm_shape(sim_step_k_behavior,
                  bbox = bb(combined_bb1_sfc, ext = 1.1),
                  is.master = TRUE) +
                tm_dots(size = 0.075, col = "#700074", alpha = .5) +
              tm_shape(ridge_poly_clip) +
                tm_borders(col = "wheat4", alpha = .6) +
              tm_shape(ridge_poly_clip) +
                tm_fill("forestgreen", alpha = .4) +
              tm_shape(me_turbines_buff_clip, title = "Wind Turbines") +
                tm_polygons(col = turbine_color,
                  border.col = "black",  lwd = 1) +
              tm_shape(sim_ud_95_k) +
                tm_polygons(col = "yellow", alpha = .15) +
              tm_shape(sim_ud_95_k) +
                tm_borders(col= "yellow", lwd = 2) +
              tm_shape(sim_ud_50_k) +
                tm_polygons(col = "red", alpha = .15) +
              tm_shape(sim_ud_50_k) +
                tm_borders(col = "red", lwd = 2) +
              tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
                main.title.position = "center",
                main.title.size = 1.15,
                title.snap.to.legend = TRUE) +
              tm_legend(title.size = 1, text.size = .85,
                outside = TRUE, position = c("right", "bottom")) +
              tm_scale_bar(text.size = .65,
                breaks = combined_x_breaks,
                position = c(.05, .01)) +
              tm_compass(type = "4star",  show.labels = 1, size = 2.2,
                position = c(.875, .86)) +
              tm_credits(paste0(sim_id, "\n", "Total (n = ", nrow(sim_step_k),
                ")", "\n", str_to_title(behavior_type), " (n = ",
                nrow(sim_step_k_behavior), ")"),
                bg.color = "white", position = c("LEFT", "TOP")) +
              tm_xlab("") + tm_ylab("")
            # Inset map
            combined_bb = CreateMapExtentBB(combined_bb1_sfc, asp = 1,
              ext = 1.15)
            maine_overview <-
              tm_shape(maine_om, raster.downsample = FALSE) +
                tm_rgb() +
              tm_shape(maine) + # setting this as master sets lat/long
                tm_borders(col = "black") +
              tm_shape(combined_bb) +
                tm_borders(col = "red")
            # Export to TEMP Folder
            baea_id_map_file <- file.path("C:/Temp",
              paste0("baea_", baea_id, "_", baea_year, ".png"))
            tmap_save(tm = baea_id_map, filename = baea_id_map_file,
              insets_tm = maine_overview,
              insets_vp =  viewport(x = 0.881, y = 0.1, width = 0.2,
                height = 0.2),
              unit = "in", dpi = 300, height = 4, width = 4)
            # Export to TEMP Folder
            sim_k_map_file <- file.path("C:/TEMP",
              paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"), "-",
                str_pad(j, 2, side = "left", "0"), ".png"))
            tmap_save(tm = sim_k_map, filename = sim_k_map_file,
              insets_tm = maine_overview,
              insets_vp =  viewport(x = 0.881, y = 0.1, width = 0.2,
                height = 0.2),
              unit = "in", dpi = 300, height = 4, width = 4)
            # Create Tex Strings
            tex_head <- tibble(
              tex_str = c(paste0(baea_id, " (", baea_year, ")"), "Simulation"),
              tex_name = c("baea_label", "sim_label"))
            tex_df <- bind_rows(
              bind_rows(tex_head) %>% mutate(title_size = 13))
            # Create Tex Text Plots
            for (m in seq_len(nrow(tex_df))){
              tex_str_m <- tex_df %>% slice(m) %>% pull(tex_str)
              tex_name_m <- tex_df %>% slice(m) %>% pull(tex_name)
              title_size_m <- tex_df %>% slice(m) %>% pull(title_size)
              gg_tex <- ggplot() + theme_blank +
                labs(title = latex2exp::TeX(tex_str_m)) +
                theme(plot.title = element_text(size = title_size_m))
              ggsave(file = "C:/TEMP/tex_str.png", plot = gg_tex,
                     width = 5, height = .75)
              tex_m <- image_trim(image_read("C:/TEMP/tex_str.png"))
              file.remove("C:/TEMP/tex_str.png")
              assign(paste0("tex_", tex_name_m), tex_m)
            }
            rm(m, tex_df, tex_name_m, tex_str_m, tex_m, tex_head)
            # Compose final figure
            sim_k_map_inset <- image_read(sim_k_map_file, density = 300)
            baea_id_map_inset <- image_read(baea_id_map_file, density = 300)
            # Image background (ORIGINAL VERSION for full PowerPoint slide)
            # backgrd <- image_blank(2400, 1800, color = "white")
            # sim_baea_map_fig <- backgrd %>%
            #   image_composite(., baea_id_map_inset, offset = "+0+300") %>%
            #   image_composite(., sim_k_map_inset, offset ="+1200+300") %>%
            #   image_composite(., tex_baea_label, offset = "+500+250") %>%
            #   image_composite(., tex_sim_label, offset = "+1650+250")
            # Image background
            backgrd <- image_blank(2400, 1300, color = "white")
            sim_baea_map_fig <- backgrd %>%
              image_composite(., baea_id_map_inset, offset = "+0+100") %>%
              image_composite(., sim_k_map_inset, offset ="+1200+100") %>%
              image_composite(., tex_baea_label, offset = "+500+50") %>%
              image_composite(., tex_sim_label, offset = "+1650+50")
            # Export to sim akde folder
            if(match_baea){
              sim_baea_map_fig_file <- file.path(akde_dir,
                paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"), "-",
                  str_pad(j, 2, side = "left", "0"), "_matched_", behavior_type,
                  ".png"))
            } else {
              sim_baea_map_fig_file <- file.path(akde_dir,
                paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"), "-",
                  str_pad(j, 2, side = "left", "0"), "_", behavior_type,".png"))
            }
            if(exists(sim_baea_map_fig_file)) file.remove(sim_baea_map_fig_file)
            image_write(sim_baea_map_fig, path = sim_baea_map_fig_file,
              format = ".png")
            # Delete temp maps
            file.remove(baea_id_map_file)
            file.remove(sim_k_map_file)
          }
        }
      }
    }
  }
}

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

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

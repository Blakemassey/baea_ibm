#----------------------- Sim Visualization Sim --------------------------------#
# This script is used to create visual a 'sim' object
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------
pacman::p_load(cartography, ctmm, dplyr, fasterize, forcats, ggplot2,
  ggpubr, ggthemes, grid, leaflet, lubridate, magick, mapview, move,
  OpenStreetMap, plotly, prettymapr, purrr, raster, rosm, rsvg, sf, stringr, s2,
  tidyr, tmap, tmaptools, viridis, units, webshot, zoo)
pacman::p_load(baear, gisr, ibmr)
set_thin_PROJ6_warnings(TRUE)
theme_update(plot.title = element_text(hjust = 0.5))
suppressMessages(extrafont::loadfonts(device = "win"))

# Individual sim file
sim_rds_vec <- "sim_20210725-81.rds"

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
  if(FALSE) sim_rds_vec <- sim_rds_vec[c(2:53)]
}

# Boolean parameters
create_kml <- FALSE
calculate_akde <- TRUE
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
sim_step_data_dir <- "Step_Data"
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
  print(paste0("Starting: ", sim_rds))
  # Read in sim_out file
  sim_id <- tools::file_path_sans_ext(sim_rds)
  sim_runs <- file.path(sim_dir, sim_id, sim_step_data_dir) %>%
    dir(., pattern = "data.rds") %>%
    str_remove_all(., paste0(sim_id, "_")) %>%
    str_remove_all(., "_step_data.rds")
  akde_dir <- file.path(sim_dir, sim_id, "AKDEs")
  for (i in seq_len(length(sim_runs))){
    print(paste0("Starting run ", i, " of ", length(sim_runs), " at ",
      GetDateTime()))
    sim_run_i <- sim_runs[i]
    sim_step_data_i <- file.path(sim_dir, sim_id, sim_step_data_dir,
      paste0(sim_id, "_", sim_run_i, "_step_data.rds")) %>%
      readRDS(.)
    # Create KML ---------------------------------------------------------------
    if(create_kml){
      # KMLs of Points and Flights
      kml_dir = file.path(sim_dir, sim_id, "KMLs")
      if(!dir.exists(kml_dir)){
        dir.create(kml_dir)
      }
      for (j in unique(sim_step_data_i$id)){
        print(paste0("Starting kml for ", j, " at ", GetDateTime()))
        sim_step_data_j <- sim_step_data_i %>% filter(id == j) %>%
          select(id, step_data) %>% unnest(., cols = step_data)
        ExportKMLTelemetry(sim_step_data_j, lat = "lat", long = "long",
          alt = NULL, speed = NULL, file = paste0(sim_id, "_", str_pad(i, 2,
          side = "left", "0"),  "_", str_pad(j, 2, side = "left", "0"), ".kml"),
          icon_by_sex = TRUE, behavior = "behavior", point_color = "behavior",
          output_dir = kml_dir)
      }
    }

    # Calculate AKDE -----------------------------------------------------------
    if(!dir.exists(akde_dir)){
      dir.create(akde_dir)
    }
    if(match_baea){
      akde_file <- file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
        side = "left", "0"), "_matched.rds"))
    } else {
      akde_file <- file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
        side = "left", "0"), ".rds"))
    }
    # Calculate akde (if it doesn't already exist)
    if(!file.exists(akde_file) | recalculate_akde == TRUE){
      for (j in unique(sim_step_data_i$id)){
        print(paste0("Starting adke for ", j, " at ", GetDateTime()))
        if(match_baea){
          sim_hr_j <- sim_step_data_i %>% filter(id == j) %>%
            select(id, step_data_matched) %>%
            unnest(., cols = step_data_matched) %>%
            arrange(datetime) %>% as.data.frame(.)
        } else {
          sim_hr_j <- sim_step_data_i %>% filter(id == j) %>%
            select(id, step_data) %>%
            unnest(., cols = step_data) %>%
            arrange(datetime) %>% as.data.frame(.)
        }
        print(paste0("Run:", i, ", ID:", j))
        move_j <- move::move(x = sim_hr_j$x, y = sim_hr_j$y,
          time = as.POSIXct(sim_hr_j$datetime, format = "%Y-%m-%d %H:%M:%S",
          tz = "NYC"), data = sim_hr_j, proj = crs_wgs84n19, animal = j,
          sensor = "GPS")
        # Compute movement models
        telemetry_j <- as.telemetry(move_j)
        guess_j <- ctmm.guess(telemetry_j, interactive = FALSE)
        fit_j <- ctmm.fit(telemetry_j, guess_j)
        # Compute akde object
        akde_j <- akde(telemetry_j, fit_j)
        print("Calculated akde")
        if(j == (unique(sim_step_data_i$id) %>% .[1])) {
          akde_all <- tibble(id = NA, hr_akde = list(akde_j)) %>%
            slice(0)
        }
        akde_all <- bind_rows(akde_all, tibble(id = j, hr_akde = list(akde_j)))
        rm(sim_hr_j, move_j, telemetry_j, guess_j, fit_j, akde_j, j)
      }
      saveRDS(akde_all, akde_file)
      rm(akde_all)
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
      sim_akde <- readRDS(sim_akde_file)
      for (j in unique(sim_step_data_i$baea_id)){
        sim_step_data_j <- sim_step_data_i %>%
          filter(baea_id == j)
        if(match_baea){
          sim_map_data_j <- sim_step_data_j %>%
            select(id, step_data_matched) %>%
            unnest(., cols = step_data_matched) %>%
            as.data.frame(.)
        } else {
          sim_map_data_j <- sim_step_data_j %>%
            select(id, step_data) %>%
            unnest(., cols = step_data) %>%
            as.data.frame(.)
        }
        baea_id <- sim_step_data_j %>% pull(baea_id) %>% unique(.)
        baea_year <- sim_step_data_j %>% pull(baea_year) %>% unique(.)
        baea_step_id <- baea_hr %>% filter(id == baea_id) %>%
          arrange(datetime) %>%
          filter(year(date) == baea_year) %>%
          st_as_sf(., coords = c("long_utm", "lat_utm"), crs = 32619,
            agr = "constant")
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
        # Get baea map bb (for final map extent)
        baea_id_bb1_sfc <- st_as_sfc(bb(baea_step_id, relative = TRUE,
          height = 1, width = 1)) %>%
          st_transform(., crs = as.character(OpenStreetMap::osm()))

        # Get sim map bb (for final map extent)
        sim_j_bb1_sfc <- sim_map_data_j %>%
          st_as_sf(., coords = c("x", "y"), crs = wgs84n19, agr ="constant") %>%
          bb(., relative = TRUE, height = 1, width = 1) %>%
          st_as_sfc(.) %>%
          st_transform(., crs = as.character(OpenStreetMap::osm()))

        # Get combined bb
        combined_bb1_sfc <- st_union(sim_j_bb1_sfc, baea_id_bb1_sfc)

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
          read_osm(., minNumTiles = 21,
          type = om_nat_geo)  # may need to add and adjust 'zoom' arg
        # Inset map
        combined_bb = CreateMapExtentBB(combined_bb1_sfc, asp = 1, ext = 1.15)
        maine_overview <-
          tm_shape(maine_om, raster.downsample = FALSE) +
            tm_rgb() +
          tm_shape(maine) + # setting this as master sets lat/long
            tm_borders(col = "black") +
          tm_shape(combined_bb) +
            tm_borders(col = "red")

        baea_step_id_air <- baea_step_id %>%
          filter(speed >= 5)
        baea_step_id_stationary <- baea_step_id %>%
          filter(speed < 5)

        # Map for baea (home range, points, and flight paths)
        baea_j_map_base <-
          tm_layout(asp = 1) +
          tm_shape(combined_om, raster.downsample = FALSE) +
          tm_rgb() +
          tm_shape(baea_lines_id) +
            tm_lines("#ffffff", lwd = 2, alpha = .35)

        baea_j_map_all <- baea_j_map_base +
          tm_shape(baea_step_id, bbox = bb(combined_bb1_sfc, ext = 1.1),
            is.master = TRUE) +
          tm_dots(size = 0.075, col = "#700074", alpha = .5)

        baea_j_map_air <- baea_j_map_base +
          tm_shape(baea_lines_id) +
            tm_lines("#ffffff", lwd = 2, alpha = .35) +
          tm_shape(baea_step_id_air, bbox = bb(combined_bb1_sfc, ext = 1.1),
            is.master = TRUE) +
            tm_dots(size = 0.075, col = "#700074", alpha = .5)

        baea_j_map_stationary <- baea_j_map_base +
          tm_shape(baea_step_id_stationary,
            bbox = bb(combined_bb1_sfc, ext = 1.1),
            is.master = TRUE) +
            tm_dots(size = 0.075, col = "#700074", alpha = .5)

        baea_j_map_parts <-
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
          tm_xlab("") + tm_ylab("")

        baea_j_map_all_final <- baea_j_map_all +
          baea_j_map_parts +
          tm_credits(paste0(baea_id, " - ",baea_year, "\n(n = ",
            nrow(baea_step_id), ")"), bg.color = "white",
            position = c("LEFT", "TOP"))
        baea_j_map_air_final <- baea_j_map_air +
          baea_j_map_parts +
          tm_credits(paste0(baea_id, " - ", baea_year, "\n", "Total (n = ",
            nrow(baea_step_id), ")\nAir (n = ", nrow(baea_step_id_air), ")"),
            bg.color = "white",
            position = c("LEFT", "TOP"))
        baea_j_map_stationary_final <- baea_j_map_stationary +
          baea_j_map_parts +
          tm_credits(paste0(baea_id, " - ", baea_year, "\n", "Total (n = ",
            nrow(baea_step_id), ")\nStationary (n = ",
            nrow(baea_step_id_stationary), ")"),
            bg.color = "white",
            position = c("LEFT", "TOP"))

        # Export to TEMP Folder
        baea_j_map_all_file <- file.path("C:/Temp/TEMP_Maps",
          paste0("baea_", baea_id, "_all.png"))
        baea_j_map_air_file <- file.path("C:/Temp/TEMP_Maps",
          paste0("baea_", baea_id, "_air.png"))
        baea_j_map_stationary_file <- file.path("C:/Temp/TEMP_Maps",
          paste0("baea_", baea_id, "_stationary.png"))
        tmap_save(tm = baea_j_map_all_final, filename = baea_j_map_all_file,
          insets_tm = maine_overview,
          insets_vp =  viewport(x = 0.881, y = 0.095, width = 0.2, height =0.2),
          unit = "in", dpi = 300, height = 4, width = 4)
        tmap_save(tm = baea_j_map_air_final, filename = baea_j_map_air_file,
          insets_tm = maine_overview,
          insets_vp =  viewport(x = 0.881, y = 0.095, width = 0.2, height =0.2),
          unit = "in", dpi = 300, height = 4, width = 4)
        tmap_save(tm = baea_j_map_stationary_final,
          filename = baea_j_map_stationary_file, insets_tm = maine_overview,
          insets_vp =  viewport(x = 0.881, y = 0.095, width = 0.2, height =0.2),
          unit = "in", dpi = 300, height = 4, width = 4)

        #Clean up objects
        rm(baea_j_map_all_final, baea_j_map_air_final,
          baea_j_map_stationary_final)

        # Make map for each sim
        for (k in unique(sim_map_data_j$id)){
          sim_akde_k <- sim_akde %>% filter(id == k)
          print(paste0("ID:", k))
          # Create nest point and baea data
          nest_id_k <- sim_step_data_i %>% filter(id == k) %>%
             pull(nest_id)
          nest_k <- sim_step_data_i %>% filter(id == k) %>%
            select(id, sex, nest_id, start_x, start_y) %>%
            st_as_sf(., coords = c("start_x", "start_y"), crs = 32619) %>%
            st_transform(., crs = 4326)
          # Create sim points and UDs
          sim_step_k <- sim_map_data_j %>%
            filter(id == k) %>%
            arrange(datetime) %>%
            st_as_sf(., coords = c("x", "y"), crs = wgs84n19, agr ="constant")
          sim_akde_k <- sim_akde_k %>% pull(hr_akde) %>%
            pluck(1)
          sim_ud_95_k <- SpatialPolygonsDataFrame.UD(sim_akde_k,
            level.UD = 0.95, level = 0.95) %>% st_as_sf(.) %>% slice(2)
          sim_ud_50_k <- SpatialPolygonsDataFrame.UD(sim_akde_k,
            level.UD = 0.5, level = 0.95) %>% st_as_sf(.) %>% slice(2)
          # Create flight paths
          sim_lines_k <- sim_step_k %>% group_by(id) %>% arrange(datetime) %>%
            dplyr::summarize(m = mean(year(datetime)), do_union = FALSE,
              .groups = "drop") %>% st_cast("LINESTRING")
          # Map points and flight paths
          if(view_maps){
            mapview(list(sim_step_k, sim_lines_k),
              zcol = list("behavior", NULL),
              legend = list(TRUE, FALSE),
              homebutton = list(TRUE, FALSE))
          }

          sim_step_k_air <- sim_step_k %>%
            filter(behavior %in% c("Cruise", "Flight"))
          sim_step_k_stationary <- sim_step_k %>%
            filter(behavior %in% c("Nest", "Perch", "Roost"))

          # Home range, points, and flight paths
          sim_k_map_base <-
            tm_layout(asp = 1) +
            tm_shape(combined_om, raster.downsample = FALSE) +
            tm_rgb() +
            tm_shape(sim_lines_k) +
            tm_lines("#ffffff", lwd = 2, alpha = .35)

          sim_k_map_all <- sim_k_map_base +
            tm_shape(sim_step_k, bbox = bb(combined_bb1_sfc, ext = 1.1),
              is.master = TRUE) +
            tm_dots(size = 0.075, col = "#700074", alpha = .5)
          sim_k_map_air <- sim_k_map_base +
            tm_shape(sim_step_k_air, bbox = bb(combined_bb1_sfc, ext = 1.1),
              is.master = TRUE) +
            tm_dots(size = 0.075, col = "#700074", alpha = .5)
          sim_k_map_stationary <- sim_k_map_base +
            tm_shape(sim_step_k_stationary, bbox = bb(combined_bb1_sfc,
              ext = 1.1), is.master = TRUE) +
            tm_dots(size = 0.075, col = "#700074", alpha = .5)

          sim_k_map_parts <-
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
            tm_xlab("") + tm_ylab("")

          sim_k_map_all_final <- sim_k_map_all +
            sim_k_map_parts +
            tm_credits(paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"),
              "-", str_pad(k, 2, side = "left", "0"),
              "\n(n = ", nrow(sim_step_k), ")"),
              bg.color = "white", position = c("LEFT", "TOP"))
          sim_k_map_air_final <- sim_k_map_air +
            sim_k_map_parts +
            tm_credits(paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"),
              "-", str_pad(k, 2, side = "left", "0"),
              "\n", "Total (n = ", nrow(sim_step_k),
              ")", "\nAir (n = ", nrow(sim_step_k_air), ")"),
              bg.color = "white", position = c("LEFT", "TOP"))
          sim_k_map_stationary_final <- sim_k_map_stationary +
            sim_k_map_parts +
            tm_credits(paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"),
              "-", str_pad(k, 2, side = "left", "0"),
              "\n", "Total (n = ", nrow(sim_step_k),
              ")", "\nStationary (n = ", nrow(sim_step_k_stationary), ")"),
              bg.color = "white", position = c("LEFT", "TOP"))

          # Export to TEMP Folder
          sim_k_map_all_file <- file.path("C:/TEMP/TEMP_Maps",
            paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"), "-",
              str_pad(k, 2, side = "left", "0"), "_all.png"))
          sim_k_map_air_file <- file.path("C:/TEMP/TEMP_Maps",
            paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"), "-",
              str_pad(k, 2, side = "left", "0"), "_air.png"))
          sim_k_map_stationary_file <- file.path("C:/TEMP/TEMP_Maps",
            paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"), "-",
              str_pad(k, 2, side = "left", "0"), "_stationary.png"))

          tmap_save(tm = sim_k_map_all_final, filename = sim_k_map_all_file,
            insets_tm = maine_overview,
            insets_vp =  viewport(x = 0.881, y = 0.095, width = 0.2,
              height = 0.2),
            unit = "in", dpi = 300, height = 4, width = 4)
          tmap_save(tm = sim_k_map_air_final, filename = sim_k_map_air_file,
            insets_tm = maine_overview,
            insets_vp =  viewport(x = 0.881, y = 0.095, width = 0.2,
              height = 0.2),
            unit = "in", dpi = 300, height = 4, width = 4)
          tmap_save(tm = sim_k_map_stationary_final,
            filename = sim_k_map_stationary_file, insets_tm = maine_overview,
            insets_vp =  viewport(x = 0.881, y = 0.095, width = 0.2,
              height = 0.2),
            unit = "in", dpi = 300, height = 4, width = 4)

          # Clean up objects
          rm(sim_k_map_all_final, sim_k_map_air_final,
            sim_k_map_stationary_final)
        }
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
          ggsave(file = "C:/TEMP/TEMP_Maps/tex_str.png", plot = gg_tex,
            width = 5, height = .75)
          tex_m <- image_trim(image_read("C:/TEMP/TEMP_Maps/tex_str.png"))
          file.remove("C:/TEMP/TEMP_Maps/tex_str.png")
          assign(paste0("tex_", tex_name_m), tex_m)
        }
        rm(m, tex_df, tex_name_m, tex_str_m, tex_m, tex_head)

        # Compose final figure
        sim_k_map_all_files <- list.files(path = "C:/TEMP/TEMP_Maps",
          pattern = "^sim.*_all.png", full.names = TRUE)
        sim_k_map1_all_inset <- image_read(sim_k_map_all_files[1], density =300)
        sim_k_map2_all_inset <- image_read(sim_k_map_all_files[2], density =300)
        baea_id_map_all_inset <- image_read(baea_j_map_all_file, density = 300)

        sim_k_map_air_files <- list.files(path = "C:/TEMP/TEMP_Maps",
          pattern = "^sim.*_air.png", full.names = TRUE)
        sim_k_map1_air_inset <- image_read(sim_k_map_air_files[1], density =300)
        sim_k_map2_air_inset <- image_read(sim_k_map_air_files[2], density =300)
        baea_id_map_air_inset <- image_read(baea_j_map_air_file, density = 300)

        sim_k_map_stationary_files <- list.files(path = "C:/TEMP/TEMP_Maps",
          pattern = "^sim.*_stationary.png", full.names = TRUE)
        sim_k_map1_stationary_inset <- image_read(sim_k_map_stationary_files[1],
          density =300)
        sim_k_map2_stationary_inset <- image_read(sim_k_map_stationary_files[2],
          density =300)
        baea_id_map_stationary_inset <- image_read(baea_j_map_stationary_file,
          density = 300)

        # Image background
        backgrd <- image_blank(3600, 1300, color = "white")
        sim_baea_map_all_fig <- backgrd %>%
          image_composite(., baea_id_map_all_inset, offset = "+0+100") %>%
          image_composite(., sim_k_map1_all_inset, offset ="+1200+100") %>%
          image_composite(., sim_k_map2_all_inset, offset ="+2400+100") %>%
          image_composite(., tex_baea_label, offset = "+500+50") %>%
          image_composite(., tex_sim_label, offset = "+1650+50") %>%
          image_composite(., tex_sim_label, offset = "+2850+50")
        sim_baea_map_air_fig <- backgrd %>%
          image_composite(., baea_id_map_air_inset, offset = "+0+100") %>%
          image_composite(., sim_k_map1_air_inset, offset ="+1200+100") %>%
          image_composite(., sim_k_map2_air_inset, offset ="+2400+100") %>%
          image_composite(., tex_baea_label, offset = "+500+50") %>%
          image_composite(., tex_sim_label, offset = "+1650+50") %>%
          image_composite(., tex_sim_label, offset = "+2850+50")
        sim_baea_map_stationary_fig <- backgrd %>%
          image_composite(., baea_id_map_stationary_inset, offset = "+0+100")%>%
          image_composite(., sim_k_map1_stationary_inset,offset ="+1200+100")%>%
          image_composite(., sim_k_map2_stationary_inset,offset ="+2400+100")%>%
          image_composite(., tex_baea_label, offset = "+500+50") %>%
          image_composite(., tex_sim_label, offset = "+1650+50") %>%
          image_composite(., tex_sim_label, offset = "+2850+50")

        # Export to sim akde folder
        figure_base_file <- file.path(akde_dir,
            paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"), "-",
              str_pad(j, 2, side = "left", "0")))
        if(match_baea){
          sim_baea_map_all_fig_file <- paste0(figure_base_file,
            "_all_matched.png")
          sim_baea_map_air_fig_file <- paste0(figure_base_file,
            "_air_matched.png")
          sim_baea_map_stationary_fig_file <- paste0(figure_base_file,
            "_stationary_matched.png")
        } else {
          sim_baea_map_all_fig_file <- paste0(figure_base_file,
            "_all.png")
          sim_baea_map_air_fig_file <- paste0(figure_base_file,
            "_air.png")
          sim_baea_map_stationary_fig_file <- paste0(figure_base_file,
            "_stationary.png")
        }
        if(exists(sim_baea_map_all_fig_file)){
          file.remove(sim_baea_map_all_fig_file)
        }
        if(exists(sim_baea_map_air_fig_file)){
          file.remove(sim_baea_map_air_fig_file)
        }
        if(exists(sim_baea_map_stationary_fig_file)){
          file.remove(sim_baea_map_stationary_fig_file)
        }
        image_write(sim_baea_map_all_fig, path = sim_baea_map_all_fig_file,
          format = ".png")
        image_write(sim_baea_map_air_fig, path = sim_baea_map_air_fig_file,
          format = ".png")
        image_write(sim_baea_map_stationary_fig,
          path = sim_baea_map_stationary_fig_file, format = ".png")

        # Delete temp maps
        file.remove(baea_j_map_all_file)
        file.remove(sim_k_map_all_files)
        file.remove(baea_j_map_air_file)
        file.remove(sim_k_map_air_files)
        file.remove(baea_j_map_stationary_file)
        file.remove(sim_k_map_stationary_files)
      }
    }
  }
}

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#
        # Image background (ORIGINAL VERSION for full PowerPoint slide)
        # backgrd <- image_blank(2400, 1800, color = "white")
        # sim_baea_map_fig <- backgrd %>%
        #   image_composite(., baea_id_map_inset, offset = "+0+300") %>%
        #   image_composite(., sim_k_map_inset, offset ="+1200+300") %>%
        #   image_composite(., tex_baea_label, offset = "+500+250") %>%
        #   image_composite(., tex_sim_label, offset = "+1650+250")

          # # Home range, points, and flight paths
          # sim_k_map <-
          #   tm_layout(asp = 1) +
          #   tm_shape(combined_om, raster.downsample = FALSE) +
          #   tm_rgb() +
          #   tm_shape(sim_lines_k) +
          #   tm_lines("#ffffff", lwd = 2, alpha = .35) +
          #   tm_shape(sim_step_k, bbox = bb(combined_bb1_sfc, ext = 1.1),
          #     is.master = TRUE) +
          #   tm_dots(size = 0.075, col = "#700074", alpha = .5) +
          #   tm_shape(ridge_poly_clip) +
          #   tm_borders(col = "wheat4", alpha = .6) +
          #   tm_shape(ridge_poly_clip) +
          #   tm_fill("forestgreen", alpha = .4) +
          #   tm_shape(me_turbines_buff_clip, title = "Wind Turbines") +
          #   tm_polygons(col = turbine_color,
          #     border.col = "black",  lwd = 1) +
          #   tm_shape(sim_ud_95_k) +
          #   tm_polygons(col = "yellow", alpha = .15) +
          #   tm_shape(sim_ud_95_k) +
          #   tm_borders(col= "yellow", lwd = 2) +
          #   tm_shape(sim_ud_50_k) +
          #   tm_polygons(col = "red", alpha = .15) +
          #   tm_shape(sim_ud_50_k) +
          #   tm_borders(col = "red", lwd = 2) +
          #   tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
          #     main.title.position = "center",
          #     main.title.size = 1.15,
          #     title.snap.to.legend = TRUE) +
          #   tm_legend(title.size = 1, text.size = .85,
          #     outside = TRUE, position = c("right", "bottom")) +
          #   tm_scale_bar(text.size = .65,
          #     breaks = combined_x_breaks,
          #     position = c(.05, .01)) +
          #   tm_compass(type = "4star",  show.labels = 1, size = 2.2,
          #     position = c(.875, .86)) +
          #   tm_credits(paste0(sim_id, "\n(n = ", nrow(sim_step_k), ")"),
          #     bg.color = "white", position = c("LEFT", "TOP")) +
          #   tm_xlab("") + tm_ylab("")
          #
          # # Export to TEMP Folder
          # sim_k_map_file <- file.path("C:/TEMP/TEMP_Maps",
          #   paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"), "-",
          #     str_pad(k, 2, side = "left", "0"), ".png"))
          # tmap_save(tm = sim_k_map, filename = sim_k_map_file,
          #   insets_tm = maine_overview,
          #   insets_vp =  viewport(x = 0.881, y = 0.095, width = 0.2,
          #     height = 0.2),
          #   unit = "in", dpi = 300, height = 4, width = 4)

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

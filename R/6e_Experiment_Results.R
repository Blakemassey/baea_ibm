# Results
#
# - Calculate portion of flight paths through the wind area polygons and turbine
# polygons.
# - Use Cruise and Flight percentage of flights that pass through those areas
# - Calculate "events" to determine risk

#--------------------------- Experiment Results -------------------------------#
# This script checks the experiment data and assesses the results
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------

# Load packages
pacman::p_load(cartography, ctmm, dplyr, fasterize, forcats, gplots, ggplot2,
  ggthemes, ggpubr, grid, leaflet, lubridate, magick, mapview, move,
  OpenStreetMap, patchwork, plotly, prettymapr, purrr, raster, readr, rosm,
  rsvg, sf, s2, stringr, tidyr, tmap, tmaptools, viridis, units, webshot, zoo)
pacman::p_load(baear, gisr, ibmr)
suppressMessages(extrafont::loadfonts(device = "win"))
set_thin_PROJ6_warnings(TRUE)

# Set options
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE, chunksize = 1e9,
  memfrac = .9)

# Experiment id
exp_ids <- 1:15

# Variable
mapping <- FALSE

# Directories
exp_dir <- "C:/TEMP"
exp_step_data_dir <- "Step_Data"
exp_results_dir <- "Results"
exp_output_dir <- "Output/Experiment"
ridge_file_dir <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines"
wind_output_dir <- "Output/Analysis/Wind"
exp_turbines_dir <- "C:/ArcGIS/Data/R_Input/EXP"

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

# Coordinate systems
wgs84 <- 4326 # WGS84 Lat/Long
wgs84n19 <- 32619 # WGS84 UTM 19N

# Functions
MakeLines <- function(x, y, x_end, y_end) {
  st_linestring(matrix(c(x, x_end, y, y_end), 2, 2))
}

# Behavior colors
behavior_colors <- CreateColorsByMetadata(file = file.path("Data/Assets",
  "behavior_colors.csv"), metadata_id = "behavior")
sex_colors <- tibble(#female = col2hex("yellow"), male = col2hex("tomato"),
  Female = col2hex("yellow"), Male = col2hex("tomato"))

## Import Data -----------------------------------------------------------------

ridge_poly <- read_sf(ridge_poly_file) %>%
  st_transform(., crs = CRS(SRS_string = paste0("EPSG:", wgs84n19))) %>%
  st_set_crs(wgs84n19)

wilson_n_area <- readRDS(file.path(wind_output_dir, "wilson_n_area.rds"))
wilson_s_area <- readRDS(file.path(wind_output_dir, "wilson_s_area.rds"))

wilson_wt_n = readRDS(file.path(wind_output_dir, "wilson_n_turbines.rds"))
wilson_wt_s = readRDS(file.path(wind_output_dir, "wilson_s_turbines.rds"))

wilson_n_area_buff <- wilson_n_area %>% st_buffer(150)
wilson_s_area_buff <- wilson_s_area %>% st_buffer(150)

wilson_wt_n_buff <- wilson_wt_n %>% st_buffer(56)
wilson_wt_s_buff <- wilson_wt_s %>% st_buffer(56)

if(mapping) mapview(wilson_n_area_buff) + mapview(wilson_wt_n_buff)
if(mapping) mapview(wilson_s_area_buff) + mapview(wilson_wt_s_buff)

for (i in exp_ids){
  exp_id <- i
  # Experiment simulation files in TEMP directory
  exp_id_vec <- list.dirs("C:/TEMP", recursive = FALSE, full.names = FALSE) %>%
    str_subset(., paste0("Wilson_[:alpha:]{1,}-", str_pad(exp_id, width = 2,
      side = "left", pad = "0")))

  for(j in seq_len(length(exp_id_vec))){
    exp_id_j <- exp_id_vec %>% .[j]
    exp_id_j_rds <- exp_id_j %>% paste0(., ".rds")
    scenario <- exp_id_j %>%
      str_remove(., "sim_20210831_Wilson_") %>%
      str_remove_all(., paste0("-",  str_pad(exp_id, width = 2,
        side = "left", pad = "0")))

    # Create Folders
    if(!dir.exists(file.path(exp_dir, exp_id_j, exp_results_dir))){
      dir.create(file.path(exp_dir, exp_id_j, exp_results_dir))
    }

    if(!dir.exists(file.path(exp_dir, exp_id_j, exp_step_data_dir))){
      dir.create(file.path(exp_dir, exp_id_j, exp_step_data_dir))
    }

    exp_runs <- readRDS(file.path(exp_dir, exp_id_j, exp_id_j_rds))

    exp_step_data <- exp_runs %>%
      pluck(., "run_1") %>%
      pluck(., "agents") %>%
      pluck(., "all") %>%
      pluck(., "agent") %>%
      pluck(., "step_data") %>%
      mutate(exp_id = exp_id) %>%
      mutate(scenario = scenario) %>%
      dplyr::select(exp_id, scenario, everything(.))

    exp_steps <- exp_step_data %>%
      mutate(behavior = fct_recode(as.character(behavior), "Cruise" = "1",
         "Flight" = "2", "Nest" = "3", "Perch" = "4", "Roost" = "5")) %>%
      group_by(id) %>%
        mutate(x_end = lead(x),
               y_end = lead(y)) %>%
        mutate(behavior_next = lead(behavior)) %>%
        mutate(behavior_behavior = paste(behavior, "->", behavior_next)) %>%
        mutate(step_time2 = lead(datetime) - datetime) %>%
      ungroup(.) %>%
      filter(step_time2 <= 20)  %>%
      filter(step_length > 42.43) %>% # ORIGINAL VALUE = 42.43
      filter(behavior_behavior != "Nest -> Nest",
        behavior_behavior != "Roost -> Roost",
        behavior_behavior != "Cruise -> Roost",
        behavior_behavior != "Roost -> Cruise") %>%
      filter(!is.na(x_end)) %>%
      dplyr::select(., -c(step_time2)) %>%
      mutate(behavior_line =
        if_else(behavior == "Cruise" | behavior_next == "Cruise", "Cruise",
          "Flight"))

    # Create Lines and Points
    exp_lines_sfc <- exp_steps %>%
      dplyr::select(x, y, x_end, y_end) %>%
      pmap(.f = MakeLines) %>%
      st_as_sfc(crs = wgs84n19)
    exp_lines_sf <- exp_steps %>%
      st_as_sf(., geom = exp_lines_sfc) %>%
      tibble::rowid_to_column("row_id") %>%
      st_set_crs(.,  wgs84n19)

    if(mapping) mapview(exp_lines_sf, zcol = "behavior_line")

    saveRDS(exp_step_data, file.path(exp_dir, exp_id_j, exp_step_data_dir,
        paste0(exp_id_j, "_step_data.rds")))
    saveRDS(exp_steps, file.path(exp_dir, exp_id_j, exp_step_data_dir,
        paste0(exp_id_j, "_steps.rds")))
    saveRDS(exp_lines_sf, file.path(exp_dir, exp_id_j, exp_step_data_dir,
        paste0(exp_id_j, "_lines_sf.rds")))

    # Line intersections
    exp_lines_intersects_n_area <- st_intersects(exp_lines_sf,
        wilson_n_area, sparse = TRUE) %>%
      as.data.frame(.) %>%
      as_tibble(.) %>%
      rename(row_id = row.id,
        intersect_n_area = col.id)

    exp_lines_intersects_s_area <- st_intersects(exp_lines_sf,
        wilson_s_area, sparse = TRUE) %>%
      as.data.frame(.) %>%
      as_tibble(.) %>%
      rename(row_id = row.id,
        intersect_s_area = col.id)

    exp_lines_intersects_n_turbines <- st_intersects(exp_lines_sf,
        wilson_wt_n_buff, sparse = TRUE) %>%
      as.data.frame(.) %>%
      as_tibble(.) %>%
      rename(row_id = row.id,
        intersect_n_turbines = col.id)

    exp_lines_intersects_s_turbines <- st_intersects(exp_lines_sf,
        wilson_wt_s_buff, sparse = TRUE) %>%
      as.data.frame(.) %>%
      as_tibble(.) %>%
      rename(row_id = row.id,
        intersect_s_turbines = col.id)

    if(mapping){
      mapview(exp_lines_sf, zcol = "behavior_line") +
      mapview(wilson_wt_n) + mapview(wilson_wt_s)
    }

    # Identify crossings
    wind_crossings_sum <- exp_lines_sf %>%
      left_join(., exp_lines_intersects_n_area, by = "row_id") %>%
      left_join(., exp_lines_intersects_s_area, by = "row_id") %>%
      left_join(., exp_lines_intersects_n_turbines, by = "row_id") %>%
      left_join(., exp_lines_intersects_s_turbines, by = "row_id") %>%
      dplyr::select(row_id, behavior_line,
        intersect_n_area, intersect_s_area, intersect_n_turbines,
        intersect_s_turbines) %>%
      st_drop_geometry(.) %>%
      group_by(row_id) %>%
      summarize(behavior_line = first(behavior_line),
        n_area_cross = any(!is.na(intersect_n_area)),
        s_area_cross = any(!is.na(intersect_s_area)),
        n_turbines_cross = any(!is.na(intersect_n_turbines)),
        s_turbines_cross = any(!is.na(intersect_s_turbines))) %>%
      ungroup(.) %>%
      group_by(behavior_line) %>%
      summarize(total_steps_n = n(),
        n_area_steps_n = sum(n_area_cross),
        s_area_steps_n = sum(s_area_cross),
        n_turbines_steps_n = sum(n_turbines_cross),
        s_turbines_steps_n = sum(s_turbines_cross)) %>%
      ungroup(.) %>%
      mutate(exp_id = exp_id) %>%
      mutate(scenario = scenario) %>%
      dplyr::select(exp_id, scenario, everything(.))

    # Save wind_crossing_sum file
    saveRDS(wind_crossings_sum, file.path(exp_dir, exp_id_j, exp_results_dir,
      paste0("wind_crossings_sum.rds")))
  }
}

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
  if(FALSE) sim_rds_vec <- sim_rds_vec[c(49:73)]
}

# Variables
plot_compare_graphs <- FALSE

# Coordinate systems
wgs84 <- 4326 # WGS84 Lat/Long
wgs84n19 <- 32619 # WGS84 UTM 19N
crs_wgs84n19 <- CRS(SRS_string = "EPSG:32619")

# Directories
input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
sim_dir <- "C:/TEMP"
sim_calibration_dir <- "Calibration"
sim_step_data_dir <- "Step_Data"
sim_step_compare_dir <- "Compare_Plots"
baea_hr_dir <- "Data/BAEA"

# Files
baea_hr_file <- file.path(baea_hr_dir, "baea_homerange.rds")

# Read Sim and Visualize -------------------------------------------------------

# Read BAEA homerange data
baea_hr <- readRDS(baea_hr_file)

# Matches for sim to baea data
sim_baea_matches <- tibble(
  nest_id = c("282A", "659A", "446R01", "423R01"),
  baea_id = c("Ellis", "Hebron", "Musquash", "Sandy"),
  baea_year = c(2016, 2016, 2016, 2017))

# Generate step_data and step_data_matched
for (m in seq_len(length(sim_rds_vec))){
  sim_rds <- sim_rds_vec[m]
  print(sim_rds)
  # Read in sim_out file
  sim_id <- tools::file_path_sans_ext(sim_rds)
  sim_runs <- readRDS(file.path(sim_dir, sim_id, sim_rds))
  for (i in seq_len(length(sim_runs))){
    print(paste0("Starting run ", i, " of ", length(sim_runs), " at ",
      GetDateTime()))
    sim_out <- sim_runs %>% pluck(i)
    sim_input <- sim_out$agents$input
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
    if(!dir.exists(file.path(sim_dir, sim_id, sim_step_data_dir))){
      dir.create(file.path(sim_dir, sim_id, sim_step_data_dir))
    }
    sim_step_data_nested <- sim_step_data %>%
      group_by(id) %>%
      nest(.) %>%
      rename(step_data = data) %>%
      ungroup(.) %>%
      left_join(sim_input, .) %>%
      left_join(., sim_baea_matches)
    for (j in unique(sim_step_data_nested$id)){
      print(paste0("Starting match for ", j, " at ", GetDateTime()))
      sim_step_data_nested_i <- sim_step_data_nested %>%
        filter(id == j)
      sim_hr_j <- sim_step_data_nested_i %>%
        pluck("step_data", 1) %>%
        arrange(datetime) %>%
        mutate(id = j) %>%
        select(id, everything(.))
      baea_id <- sim_step_data_nested_i %>% pull("baea_id")
      baea_year <- sim_step_data_nested_i %>% pull("baea_year")
      # Create Nest Point
      baea_step_id <- baea_hr %>%
        filter(id == baea_id) %>%
        arrange(datetime) %>%
        st_as_sf(., coords = c("long_utm","lat_utm"),
          crs = 32619, agr = "constant") %>%
        filter(year(date) == baea_year)
      # Get vector of baea datetime and match closest sim values so
      # the sim values are very close to the baea values
      baea_vec <- baea_step_id %>% pull(datetime) %>%
        with_tz(., tzone = "Etc/GMT+5")
      year(baea_vec) <- 2015
      sim_vec <- sim_hr_j %>%
        pull(datetime) %>%
        with_tz(., tzone = "Etc/GMT+5")
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
      sim_hr_j_matched <- sim_hr_j %>%
        filter(datetime %in% sim_vec_out) %>%
        arrange(datetime)
      # Compile matched data
      if(j == unique(sim_step_data_nested$id)[1]){
        sim_hr_matched <- sim_hr_j_matched
      } else {
        sim_hr_matched <- bind_rows(sim_hr_matched, sim_hr_j_matched)
      }
      # Compare datetime between baea and sim
      if(plot_compare_graphs){
        gg_baea_i <- baea_step_id %>%
          mutate(dt_day = date(datetime),
            dt_time = hour(datetime) + (minute(datetime)/60)) %>%
        ggplot(data = ., aes(x = dt_day, y = dt_time)) +
          geom_point(color = "darkgreen") +
          labs(x = "Date", y = "Hour", title = paste0("BAEA: ", baea_id))
        gg_sim_i <- sim_hr_j_matched %>%
          mutate(dt_day = date(datetime),
            dt_time = hour(datetime) + (minute(datetime)/60)) %>%
        ggplot(data = ., aes(x = dt_day, y = dt_time)) +
          geom_point(color = "navyblue") +
            labs(x = "Date", y = "Hour", title = paste0("Sim: ",
            unique(sim_hr_j$id)))
        gg_baea_sim_compare <- ggarrange(gg_baea_i + rremove("x.text") +
            rremove("xlab"), gg_sim_i, ncol = 1, nrow = 2)
        if(!dir.exists(file.path(sim_dir, sim_id, sim_step_data_dir,
          sim_step_compare_dir))){
            dir.create(file.path(sim_dir, sim_id, sim_step_data_dir,
              sim_step_compare_dir))
        }
        ggsave(filename = paste0("gg_baea_sim_compare", "_",
            str_pad(i, 2, side = "left", "0"), "_",
            str_pad(j, 2, side = "left", "0") ,".png"),
          plot = gg_baea_sim_compare,
          path = file.path(sim_dir, sim_id, sim_step_data_dir,
            sim_step_compare_dir),
          scale = 1, width = 6, height = 4, units = "in", dpi = 300)
      }
    }
    # Save step_data_matched
    sim_hr_matched_grouped <- sim_hr_matched %>%
      group_by(id) %>%
      nest() %>%
      rename(step_data_matched = data) %>%
      ungroup(.)
    sim_step_data_nested_matched <- sim_step_data_nested %>%
      left_join(., sim_hr_matched_grouped)
    saveRDS(sim_step_data_nested_matched,
      file.path(sim_dir, sim_id, sim_step_data_dir,
        paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"),
          "_step_data.rds")))
    rm(sim_hr_matched)
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

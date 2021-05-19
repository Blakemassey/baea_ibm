####################### VISUALIZE SIM ##########################################
### This script is used to create visual a 'sim' object

pacman::p_load(cartography,ctmm, dplyr, fasterize, forcats, ggplot2, ggthemes,
  grid, leaflet, lubridate, magick, mapview, move, OpenStreetMap, plotly,
  prettymapr, purrr, raster, rosm, rsvg, sf, stringr, s2, tmap, tmaptools,
  viridis, units, webshot, zoo)
pacman::p_load(baear, gisr, ibmr)
set_thin_PROJ6_warnings(TRUE)
theme_update(plot.title = element_text(hjust = 0.5))
#devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/ibmr")

# Sim file and code Boolean parameters
sim_rds <- "sim_20210505-01.rds"
create_kml = FALSE
calculate_akde = TRUE
map_akde = TRUE
view_maps = FALSE
save_maps = TRUE

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
maine_om = read_osm(maine_bb, zoom = 5, minNumTiles = 9, type = om_nat_geo)

# Directories
input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
sim_dir <- "C:/TEMP"
sim_calibration_dir <- "Calibration"
baea_calibration_dir <- "Output/Sim/Calibration"
sim_id <- tools::file_path_sans_ext(sim_rds)

##################### READ SIM AND VISUALIZE ###################################

# Read in sim_out file
# File Directory and ID
sim_runs <- readRDS(file.path(sim_dir, sim_id, sim_rds))

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

  # Subset data
  #sim_step_data <- sim_step_data %>%
  # filter(previous_step_type == "3_4")

  # Create kml -----------------------------------------------------------------
  if(create_kml){
    # KMLs of Points and Flights
    kml_dir = file.path(sim_dir, sim_id, "KMLs")

    if(!dir.exists(kml_dir)){
      dir.create(kml_dir)
    }
    for (j in unique(sim_step_data$id)){
      print(paste0("Starting kml for ", j, " at ", GetDateTime()))
      sim_step_data_j <- sim_step_data %>% filter(id == j)
      ExportKMLTelemetry(sim_step_data_j, lat = "lat", long = "long", alt =NULL,
        speed = NULL, file = paste0(sim_id, "_", str_pad(i, 2, side = "left",
        "0"),  "_", str_pad(j, 2, side = "left", "0"), ".kml"),
        icon_by_sex = TRUE, behavior = "behavior", point_color ="behavior",
        output_dir = kml_dir)
    }
  }

  # Calculate akde -------------------------------------------------------------
  if(calculate_akde){
    akde_dir = file.path(sim_dir, sim_id, "AKDEs")
    if(!dir.exists(akde_dir)){
      dir.create(akde_dir)
    }
    akde_file <- file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
      side = "left", "0"), ".rds"))
    # Calculate akde (if it doesn't already exist)
    if(!file.exists(akde_file)){
      for (j in unique(sim_step_data$id)){
        print(paste0("Starting adke for ", j, " at ", GetDateTime()))
        sim_hr_j <- sim_step_data %>% filter(id == j) %>% arrange(datetime)
        for (k in unique(year(sim_hr_j$datetime))){
          sim_hr_k <- sim_hr_j %>% filter(year(datetime) == k)
          print(paste0("Run:" ,i, ", ID:", j, ", Year:", k))
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
            print("Created hr_akde")
          }
          hr_akde <- bind_rows(hr_akde, tibble(id = j, year = k,
            hr_akde = list(akde_k)))
        }
        rm(sim_hr_j, move_k, telemetry_k, guess_k, fit_k, akde_k, j, k)
      }
      saveRDS(hr_akde, file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
        side = "left", "0"), ".rds")))
      rm(hr_akde)
    }
  }

  # Map akde -------------------------------------------------------------------
  if(map_akde){
    # Check for akde
    if(!file.exists(file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
      side = "left", "0"), ".rds")))){
      print(paste0("AKDE missing for: ", sim_id, "_", str_pad(i, 2,
        side = "left", "0")), " Map SKIPPED.")
    } else {
      print(paste0("AKDE found for: ", sim_id, "_", str_pad(i, 2, side = "left",
        "0")))
      hr_akde <- readRDS(file.path(akde_dir, paste0(sim_id, "_", str_pad(i, 2,
          side = "left", "0"), ".rds")))
      for (j in unique(sim_step_data$id)){
        sim_step_j <- sim_step_data %>% filter(id == j) %>% arrange(datetime)
        sim_akde_j <- hr_akde %>% filter(id == j)
        for (k in unique(year(sim_step_j$datetime))){
          print(paste0("ID:", j, "; ", "Year:", k))

          # Create Points and UDs
          sim_step_sf_k <- sim_step_j %>% filter(year(datetime) == k) %>%
            st_as_sf(., coords = c("x", "y"), crs = wgs84n19, agr = "constant")
                 #%>% #st_transform(., crs = wgs84)
          sim_akde_k <- sim_akde_j %>% filter(year == k)
          akde_k <- sim_akde_k %>% pull(hr_akde) %>% pluck(1)
          ud_95_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.95,
            level = 0.95) %>% st_as_sf(.) %>% slice(2)
          ud_50_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.5,
            level = 0.95) %>% st_as_sf(.) %>% slice(2)

          # Map Points and UDs
          if(view_maps){
            mapview(list(ud_95_k, ud_50_k, sim_step_k),
              zcol = list(NULL,NULL, NULL),
              legend = list(TRUE, FALSE, FALSE),
              homebutton = list(FALSE, TRUE, TRUE))
          }

          # Create Flightpaths
          sim_lines_k <- sim_step_sf_k %>%
            group_by(id) %>%
            arrange(datetime) %>%
            dplyr::summarize(m = mean(year(datetime)), do_union = FALSE,
              .groups = "drop") %>%
            st_cast("LINESTRING")

          # Map Points and Flightpaths
          if(view_maps){
            mapview(list(sim_step_sf_k, sim_lines_k),
              zcol = list("behavior", NULL),
              legend = list(TRUE, FALSE),
              homebutton = list(TRUE, FALSE))
          }

          # Create Nest Point
          nest_id_j <- sim_out$agents$input %>% filter(id == j) %>%
             pull(nest_id)
          nest_j <- sim_out$agents$input %>% filter(id == j) %>%
            st_as_sf(., coords = c("start_x", "start_y"), crs = 32619) %>%
            st_transform(., crs = 4326)
          # Create Raster of Locations
          base_k <- crop(base, as(st_as_sfc(st_bbox(sim_step_sf_k)) %>%
            st_transform(., crs = wgs84n19), "Spatial"), snap = "out")

          if(view_maps){
            mapview(base_k) +
              mapview(sim_step_k)
          }

          sim_raster_k <- rasterize(sim_step_sf_k, base_k, field = 1,
            fun = 'count', background = NA, mask = FALSE, update = FALSE,
            updateValue = 'all', na.rm = TRUE)

          # Map Nest and Point Location Raster
          if(view_maps){
          mapview(nest_j,  zcol = NULL) +
            mapview(sim_raster_k) +
            mapview(sim_lines_k)
          }

          # Get osm baselayer for sim_k
          sim_bb_sf_k <- st_as_sfc(bb(sim_step_sf_k, relative = TRUE,
            height = 4, width = 4))
          sim_k_om = read_osm(sim_bb_sf_k, minNumTiles = 21,
            type = om_nat_geo)  # may need to add and adjust 'zoom' arg
          sim_sf_dist <- st_as_sfc(bb(sim_step_sf_k, relative = TRUE,
            height = 1, width = 1))
          sim_k_x_dist <- as.numeric(approx_distances(bb(sim_sf_dist,
            ext = 1.15))[1])/1000/5
          sim_k_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0,
            sim_k_x_dist), scales::pretty_breaks(2)) [1]))
          print(sim_k_x_breaks)

          # Home range, points, and flight paths
          sim_k_hr_paths <-
            tm_layout(asp = 1) +
            tm_shape(sim_k_om, raster.downsample = FALSE) +
              tm_rgb() +
            tm_shape(sim_lines_k) +
              tm_lines("#ffffff", lwd = 2, alpha = .25) +
            tm_shape(sim_step_sf_k,
              bbox = bb(sim_step_sf_k, ext = 1.15), is.master = TRUE) +
              tm_dots(size = 0.075, col = "#700074", alpha = .5) +
            tm_shape(ud_95_k) +
              tm_polygons(col = "yellow", alpha = .15) +
            tm_shape(ud_95_k) +
              tm_borders(col= "yellow", lwd = 2) +
            tm_shape(ud_50_k) +
              tm_polygons(col = "red", alpha = .15) +
            tm_shape(ud_50_k) +
              tm_borders(col = "red", lwd = 2) +
            tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
              main.title.position = "center",
              main.title.size = 1.15,
              title.snap.to.legend = TRUE) +
            tm_legend(title.size = 1, text.size = .85,
              outside = TRUE, position = c("right", "bottom")) +
            tm_scale_bar(text.size = .75,
              breaks = sim_k_x_breaks,
              position = c(.05, .01)) +
            tm_compass(type = "4star",  show.labels = 1, size = 2.5,
              position = c(.875, .875)) +
            tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "black",
              alpha = 1, ticks = TRUE, lines = FALSE, labels.col = "grey25",
              labels.format = list(format = "f", big.mark = ""),
              labels.inside.frame = FALSE) +
            tm_xlab("") + tm_ylab("")

          sim_k_hr_paths

          if (save_maps){
          # Maine Overview Map
            baea_k_bb = gisr::CreateMapExtentBB(sim_step_sf_k, asp = 1,
              ext = 1.15)
            maine_overview <-
              tm_shape(maine_om, raster.downsample = FALSE) +
                tm_rgb() +
              tm_shape(maine) + # setting this as master sets lat/long
                tm_borders(col = "black") +
              tm_shape(baea_k_bb) +
                tm_borders(col = "red")

            # Export to TEMP Folder
            tmap_save(tm = sim_k_hr_paths, filename = file.path(akde_dir,
              paste0(sim_id, "_", str_pad(i, 2, side = "left", "0"), "-",
                str_pad(j, 2, side = "left", "0"), ".svg")),
              insets_tm = maine_overview,
              insets_vp =  viewport(x = 0.881, y = 0.147, width = 0.2,
                height = 0.2),
              unit = "in", dpi = 300, height = 6, width = 6)

            # Export to LaTeX Folder
            # tmap_save(tm = sim_k_hr_paths, filename = file.path(tex_dir,
            #   "Figures/Ch3/Sim_HR_Maps", paste0(i, "_", j, ".svg")),
            #   insets_tm = maine_overview,
            #   insets_vp =  viewport(x = 0.881, y = 0.147, width = 0.2,
            #     height = 0.2),
            #   unit = "in", dpi = 300, height = 6, width = 6)
          }
        }
      }
    }
  }
}

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

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

####################### VISUALIZE SIM ##########################################
### This script is used to create visual a 'sim' object

pacman::p_load(cartography,ctmm, dplyr, fasterize, ggplot2, ggthemes, grid,
  leaflet, lubridate, magick, mapview, move, OpenStreetMap, plotly, prettymapr,
  purrr, raster, rosm, rsvg, sf, s2, tmap, tmaptools, viridis, units, webshot,
  zoo)
pacman::p_load(baear, gisr, ibmr)
#devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/ibmr")

toc_msg <- function(tic, toc, msg, info){
  outmsg <- paste(seconds_to_period(round(toc - tic)))
}

theme_update(plot.title = element_text(hjust = 0.5))

# Coordinate systems
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

# Base
base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")

# Maine Outline
maine <- read_sf(file.path("C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp")) %>%
  st_transform(., crs = 4326) %>%
  mutate(state = "Maine")  %>%
  dplyr::select(state)

##################### READ SIM AND VISUALIZE ###################################

# Read in sim_out file

sim_out <- readRDS("C:/TEMP/sim_20200823-02.rds")

sim_out1 <- sim_out[[1]]
sim_step_data <- CompileAllAgentsStepData(sim=sim_out1) %>%
  mutate(behavior = as.factor(behavior)) %>%
  group_by(id) %>%
    mutate(step_type = paste0(behavior, "_", lead(behavior))) %>%
    mutate(previous_step_type = lag(step_type)) %>%
  ungroup() %>%
  filter(!is.na(datetime))
sim_step_data <- ConvertStepDataCoordinates(sim_step_data)
levels(sim_step_data$behavior) <- c("Cruise", "Flight", "Nest", "Perch","Roost")
sim_step_data$behavior <- as.character(sim_step_data$behavior)

# Subset data
#sim_step_data <- sim_step_data %>%
# filter(previous_step_type == "3_4")

# KMLs of Points and Flights
kml_dir = "C:/TEMP/Sim6"
for (i in unique(sim_step_data$id)){
  sim_step_data_i <- sim_step_data %>% filter(id == i)
  ExportKMLTelemetry(sim_step_data_i, lat = "lat", long = "long", alt = NULL,
    speed = NULL, file = paste0("Sim_", str_pad(i, 2, side = "left", "0"),
    ".kml"), icon_by_sex = TRUE, behavior = "behavior", point_color ="behavior",
    output_dir = kml_dir)
}

# Maps of Point Locations and Path Maps

# Calculate akde
for (i in unique(sim_step_data$id)){
  sim_hr_i <- sim_step_data %>% filter(id == i) %>% arrange(datetime)
  for (j in unique(year(sim_hr_i$datetime))){
    sim_hr_k <- sim_hr_i %>% filter(year(datetime) == j)
    print(paste0("ID:", i, "; ", "Year:", j))
    move_k <- move(x = sim_hr_k$x, y = sim_hr_k$y,
      time = as.POSIXct(sim_hr_k$datetime, format = "%Y-%m-%d %H:%M:%S",
      tz = "NYC"), data = sim_hr_k, proj = wgs84n19, animal = i,
      sensor = "GPS")
    # Compute movement models
    telemetry_k <- as.telemetry(move_k)
    guess_k <- ctmm.guess(telemetry_k, interactive = FALSE)
    fit_k <- ctmm.fit(telemetry_k, guess_k)
    # Compute akde object
    akde_k <- akde(telemetry_k, fit_k)
    if(i == unique(sim_step_data$id) %>% .[1] &&
        j == unique(year(sim_hr_i$datetime)) %>% .[1]) {
      hr_akde <- tibble(id = NA, year = NA, hr_akde = list(akde_k)) %>%
        slice(0)
      print("Created hr_land_metrics and hr_akde")
    }
    hr_akde <- bind_rows(hr_akde, tibble(id = i, year = j,
      hr_akde = list(akde_k)))
  }
}
#saveRDS(hr_akde, "C:/TEMP/hr_akde_20200823-02.rds")
hr_akde <- readRDS("C:/TEMP/hr_akde_20200823-02.rds")

projcrs <- raster::crs(sim_out1$spatial$base)
sim_step_sf <- st_as_sf(x = sim_step_data, coords = c("x", "y"),
  crs = projcrs)
stm_step_sf_1 <- sim_step_sf %>%
  filter(id == 1)

# Use "Tmap_baselayers.R" script to get other baselayers
maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1, width = 2))
maine_bb <- bb_poly(bb(maine_bb_sf, ext = 1.15))
maine_om = read_osm(maine_bb, zoom = 5, minNumTiles = 9, type = om_nat_geo)

# For mapping
for (i in unique(sim_step_data$id)){
  sim_hr_i <- sim_step_data %>% filter(id == i) %>% arrange(datetime)
  sim_akde_i <- hr_akde %>% filter(id == i)
  for (j in unique(baea_hr_i$year)){
    print(paste0("ID:", i, "; ", "Year:", j))

    # Create Points and UDs
    sim_hr_k <- sim_hr_i %>% filter(year(datetime) == j)
    sim_akde_k <- sim_akde_i %>% filter(year == j)
    akde_k <- sim_akde_k %>% pull(hr_akde) %>% pluck(1)
    ud_95_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.95,
      level = 0.95)
    ud_95_sf_k <- st_as_sf(ud_95_sp_k) %>% slice(2)
    ud_50_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.5,
      level = 0.95)
    ud_50_sf_k <- st_as_sf(ud_50_sp_k) %>% slice(2)
    sim_sf_k <- st_as_sf(sim_hr_k, coords = c("x", "y"),
      crs = 32619, agr = "constant")

    mapview(list(ud_95_sf_k, ud_50_sf_k, sim_sf_k),
      zcol = list(NULL,NULL, NULL),
      legend = list(TRUE, FALSE, FALSE),
      homebutton = list(FALSE, TRUE, TRUE))

    # Create Flightpaths
    sim_k <- sim_hr_k %>% st_as_sf(., coords = c("x", "y"),
      crs = 32619)  %>%
      st_transform(., crs = as.character(OpenStreetMap::osm()))
    sim_k_lines <- sim_k %>% group_by(id) %>% arrange(datetime) %>%
      summarize(m = mean(year(datetime)), do_union = FALSE) %>%
      st_cast("LINESTRING")

    mapview(list(sim_k, sim_k_lines),
      zcol = list(NULL,NULL),
      legend = list(TRUE, FALSE),
      homebutton = list(TRUE, FALSE))

    # Create Raster of Locations
    nest_id_i <- sim_out$run_1$agents$input %>% filter(id == i) %>%
      pull(nest_id)
    con_nest_dist_i <- sim_out$run_1$spatial$con_nest_dist[[nest_id_i]]
    sim_steps_raster_k <- trim(rasterize(sim_sf_k, con_nest_dist_i, field = 1,
      fun = 'sum', background = NA, mask = FALSE, update = FALSE,
      updateValue = 'all', filename = "", na.rm = TRUE))
    plot(sim_steps_raster_k)


    # Get osm baselayer for sim_k
    sim_k_bb_sf <- st_as_sfc(bb(sim_k, relative = TRUE, height = 4, width = 4))
    sim_k_om = read_osm(sim_k_bb_sf, minNumTiles = 21,
      type = om_nat_geo)  # may need to add and adjust 'zoom' arg
    sim_dist_sf <- st_as_sfc(bb(sim_k, relative = TRUE, height = 1, width = 1))
    sim_k_x_dist <- as.numeric(approx_distances(bb(sim_dist_sf,
      ext = 1.15))[1])/1000/5
    sim_k_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0, sim_k_x_dist),
      scales::pretty_breaks(2))[1]))
    print(sim_k_x_breaks)

    # Home range, points, and flight paths
    sim_k_hr_paths <-
      tm_layout(asp = 1) +
      tm_shape(sim_k_om, raster.downsample = FALSE) +
        tm_rgb() +
      tm_shape(sim_k_lines) +
        tm_lines("#ffffff", lwd = 2, alpha = .25) +
      tm_shape(sim_k,
        bbox = bb(sim_k, ext = 1.15), is.master = TRUE) +
        tm_dots(size = 0.075, col = "#700074", alpha = .5) +
      tm_shape(ud_95_sf_k) +
        tm_polygons(col = "yellow", alpha = .15) +
      tm_shape(ud_95_sf_k) +
        tm_borders(col= "yellow", lwd = 2) +
      tm_shape(ud_50_sf_k) +
        tm_polygons(col = "red", alpha = .15) +
      tm_shape(ud_50_sf_k) +
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
      tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "black", alpha = 1,
        ticks = TRUE, lines = FALSE, labels.col = "grey25",
        labels.format = list(format = "f", big.mark = ""),
        labels.inside.frame = FALSE) +
      tm_xlab("") + tm_ylab("")

    sim_k_hr_paths

    save_maps = FALSE
    if (save_maps){
    # Maine Overview Map
      baea_k_bb = gisr::CreateMapExtentBB(sim_k, asp = 1, ext = 1.15)
      maine_overview <-
        tm_shape(maine_om, raster.downsample = FALSE) +
          tm_rgb() +
        tm_shape(maine) + # setting this as master sets lat/long
          tm_borders(col = "black") +
        tm_shape(baea_k_bb) +
          tm_borders(col = "red")

      # Export to TEMP Folder
      tmap_save(tm = sim_k_hr_paths, filename = file.path("C:/Temp",
        paste0(i, "_", j, ".svg")),
        insets_tm = maine_overview,
        insets_vp =  viewport(x = 0.881, y = 0.147, width = 0.2, height = 0.2),
        unit = "in", dpi = 300, height = 6, width = 6)

      # Export to LaTeX Folder
      tmap_save(tm = sim_k_hr_paths, filename = file.path(tex_dir,
        "Figures/Ch3/Sim_HR_Maps", paste0(i, "_", j, ".svg")),
        insets_tm = maine_overview,
        insets_vp =  viewport(x = 0.881, y = 0.147, width = 0.2, height = 0.2),
        unit = "in", dpi = 300, height = 6, width = 6)
    }
  }
}

# Rasters of Location Density

for (i in unique(sim_step_data$id)){
  sim_steps_i <- sim_step_data %>% filter(id == i) %>% arrange(datetime)
  for (j in unique(baea_hr_i$year)){
    print(paste0("ID:", i, "; ", "Year:", j))
    sim_steps_k <- sim_hr_i %>% filter(year(datetime) == j)

    sim_sf_k <- st_as_sf(sim_steps_k, coords = c("x", "y"),
      crs = 32619, agr = "constant")

    nest_id_i <- sim_out$run_1$agents$input %>% filter(id == i) %>% pull(nest_id)
    con_nest_dist_i <- sim_out$run_1$spatial$con_nest_dist[[nest_id_i]]

    sim_steps_raster_k <- trim(rasterize(sim_sf_k, con_nest_dist_i, field = 1,
      fun = 'sum', background = NA, mask = FALSE, update = FALSE,
      updateValue = 'all', filename = "", na.rm = TRUE))
    plot(sim_steps_raster_k)
  }
}

# destination_raster <- rasterize(destination_xy, prob_raster, field = 1,
#   fun = 'sum', background = NA, mask = FALSE, update = FALSE,
#   updateValue = 'all', filename = "", na.rm = TRUE)
#
# plot(prob_raster)
# plot(destination_raster)
# Plot3DRaster(destination_raster, col = viridis::viridis(20),
#   main = "Probability Plot")






nest_locs <- sim_step_data %>% dplyr::filter(behavior == 3)



# Plots of Sim Daily Behavior
locs_dir = "Output/Sim/01_BehaviorMove/Plots/Daily_Locations"
for (i in unique(sim_step_data$id)){
  PlotLocationSunriseSunset(df = sim_step_data %>% filter(id == i),
    by = "id", color_factor = "behavior", individual = "", start = "", end = "",
    breaks = "14 days", tz = "Etc/GMT+5", addsolartimes = FALSE, wrap = TRUE)
  SaveGGPlot(file.path(locs_dir ,paste0("DailyLocs_", str_pad(i, 2,
    side = "left", "0"), ".png")))
}
title_sim = "Daily Behavior Distributions (simulated data)"
PlotBehaviorProportionBar(sim_step_data, title = title_sim)
SaveGGPlot("Results/Sim/01_BehaviorMove/Behavior/Proportion_Bar_SIM.png")


# Plots of Empirical Daily Behavior
baea_behavior <- readRDS(file="Data/Baea/baea_behavior.rds")
behave_dir <- "Results/Analysis/Plots/Behavior"

PlotLocationSunriseSunset(df=baea_behavior %>% as.data.frame() %>%
    filter(id == "Three"),
  by = "id", color_factor = "behavior", individual = "", start = "2015-03-20",
  end = "2015-09-20", breaks = "14 days", tz = "Etc/GMT+5",
  addsolartimes = FALSE, wrap = TRUE)
SaveGGPlot(file.path(behave_dir ,paste0("DailyLocs_Three.png")))

PlotLocationSunriseSunset(df = baea_behavior %>% as.data.frame() %>%
    filter(id == "Ellis"),
  by = "id", color_factor = "behavior", individual = "",
  start = "2016-03-20", end = "2016-09-20", breaks = "10 days",
  tz = "Etc/GMT+5", addsolartimes = FALSE, wrap = TRUE)
SaveGGPlot(file.path(behave_dir ,paste0("DailyLocs_Ellis.png")))

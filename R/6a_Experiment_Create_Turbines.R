###################### EXPERIMENT CREATE TURBINES ##############################

## Load packages, scripts, and input parameters --------------------------------
pacman::p_load(arcgisbinding, tidyverse, ggplot2, lubridate, mapview,
  matrixStats, nngeo, purrr, raster, reproducible, sf, stars, tmap, tmaptools,
  units, viridis, weathermetrics, whitebox, windfarmGA, xtable)
pacman::p_load(baear, gisr, ibmr)
whitebox::wbt_init() # required for WhiteboxTools to work
suppressMessages(extrafont::loadfonts(device="win"))
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE,
  memfrac = .85)
wbt_version() # check WhiteboxTools version
arc.check_product()

# Directories
nests_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Nests/Nests_rds")
wind_input_dir <- "C:/ARCGIS/Data/Wind"
wind_output_dir <- "Output/Analysis/Wind"
gis_exp_dir <- "C:/ArcGIS/Data/R_Input/EXP"

# Input Files
uswtdb_file <- file.path(wind_input_dir, "USGS_Wind_Turbine_2018-12-21",
  "uswtdb_v1_2_20181001.shp")
greenville_wind_file <- "Data/Wind/greenville_wind_data.csv"

# Rasters
base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"

# CRS
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N
ProjLAEA <- CRS(paste0("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 ",
  "+y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

## Create Maine Wind Turbines --------------------------------------------------

# Import Shapefile and Subset Data
uswtdb <- st_read(uswtdb_file)
me_wt <- uswtdb %>% filter(t_state == "ME") %>% st_transform(crs = wgs84n19)
if(FALSE) mapview(me_wt)

# Calculate wind-turbine nearest neighbors
me_wt_2010 <- me_wt %>%
  filter(p_year > 2010) %>%
  dplyr::select(case_id, faa_asn, t_hh, t_rd)
dist_matrix <- st_distance(me_wt_2010, me_wt_2010)  # creates distances matrix
diag(dist_matrix) <- NA

me_wt_2010$distance <- rowMins(dist_matrix, na.rm = TRUE) # dist nearest turbine
if(FALSE) mapview(wt_2010)

# Calculate mean and min nearest neighbor distance
me_wt_dist_mean <- round(mean(me_wt_2010$distance))
me_wt_dist_min <- round(min(me_wt_2010$distance))
saveRDS(me_wt_dist_mean, file.path(wind_output_dir,
  "maine_turbine_dist_mean.rds"))
saveRDS(me_wt_dist_min, file.path(wind_output_dir,
  "maine_turbine_dist_min.rds"))

# Calculate mean and min turbine
me_wt_hub_height_mode <- round(CalculateMode(me_wt_2010$t_hh))
me_wt_rotor_diameter_mode <- round(CalculateMode(me_wt_2010$t_rd))
saveRDS(me_wt_hub_height_mode, file.path(wind_output_dir,
  "maine_turbine_hub_height_mode.rds"))
saveRDS(me_wt_rotor_diameter_mode, file.path(wind_output_dir,
  "maine_turbine_rotor_diameter_mode.rds"))

# Clean up objects
rm(uswtdb, me_wt, me_wt_2010, dist_matrix, me_wt_dist_mean, me_wt_dist_min,
  me_wt_hub_height_mode, me_wt_rotor_diameter_mode)

# Create Wilson Windspeed Data -------------------------------------------------
greenville_wind_org <- read_csv(greenville_wind_file)

greenville_wind <- greenville_wind_org %>%
  mutate(Direction = if_else(Direction == "355-004", "0-0", Direction)) %>%
  separate(Direction, c("dir_min", "dir_max"), "-") %>%
  mutate(dir_min = as.integer(dir_min)) %>%
  mutate(dir_max = as.integer(dir_max)) %>%
  mutate(wd = ((dir_min + dir_max)/2)+.5) %>%
  mutate(wd = if_else(wd == .5, 0, wd)) %>% # wd "355-004" should be 0
  dplyr::select(-c(dir_min, dir_max)) %>%
  rename("0" = Calm, "3.5" = "2.0  4.9", "6" = "5.0  6.9", "8.5" = "7.0  9.9",
    "12.5" = "10.0 14.9", "17.5" = "15.0 19.9", "21" = "20.0+") %>%
  pivot_longer(!wd, names_to = "ws") %>%
  mutate(wd = as.numeric(wd)) %>%
  mutate(ws = as.numeric(ws)) %>%
  replace_na(., list(value = 0)) %>%
  rename(probab = value) %>%
  group_by(wd) %>%
  dplyr::select(ws, wd, probab) %>%
  ungroup()
glimpse(greenville_wind)

# Remove zero speeds
greenville_wind_no_zero <- greenville_wind %>% filter(ws != 0)

# Save greenville_wind objects
saveRDS(greenville_wind, file.path(wind_output_dir, "greenville_wind.rds"))
saveRDS(greenville_wind_no_zero, file.path(wind_output_dir,
  "greenville_wind_no_zero.rds"))

# Check windspeeds (not including zero speeds)
ggplot(data = greenville_wind_no_zero) +
  geom_col(aes(x = wd, y = probab, fill = ws)) +
  coord_polar(start = -((dirres/2)/360) *2 * pi) +
  scale_x_continuous(breaks = seq(0, 350, by = 10),
    minor_breaks = seq(0, 350, by = 10))

# Clean up objects
rm(greenville_wind_file, greenville_wind_org, greenville_wind,
  greenville_wind_no_zero)

## Create Wilson Turbines Areas ------------------------------------------------

# Nests
nests_study_org <- readRDS(file.path(nests_dir, "nests_study.rds"))
nests_study <- st_as_sf(x = nests_study_org, coords = c("long", "lat"),
  crs = "+proj=longlat +datum=WGS84") %>% filter(!name %in% c("Davis", "Upper"))

# Filter to Wilson
wilson_study_nest <- nests_study %>%
  filter(name == "Wilson") %>%
  st_transform(crs = 32619)

# Save wilson_map_center
saveRDS(wilson_study_nest, file.path(wind_output_dir, "wilson_study_nest.rds"))

wilson_map_center <- wilson_study_nest
wilson_sfc <- st_sfc(st_point(c(st_coordinates(wilson_study_nest)[1] + 300,
  st_coordinates(wilson_study_nest)[2] - 750)))
st_geometry(wilson_map_center) <- wilson_sfc
st_crs(wilson_map_center) <- 32619

# Save wilson_map_center
saveRDS(wilson_map_center, file.path(wind_output_dir, "wilson_map_center.rds"))

# Create 5km buffer for Wilson wind areas
wilson_bb_5km <- st_buffer(wilson_map_center, 5000)
saveRDS(wilson_bb_5km, file.path(wind_output_dir, "wilson_bb_5km.rds"))
mapview(wilson_bb_5km)

rm(nests_study_org, nests_study, wilson_study_nest, wilson_map_center,
  wilson_sfc, wilson_bb_5km)

## Create wind turbine areas

# Wilson bounding box (5km)
wilson_bb_5km <- readRDS(file.path(wind_output_dir, "wilson_bb_5km.rds"))

# Wind
wind_class <- read_sf(file.path(wind_input_dir, "Maine_Wind_High_Resolution",
  "maine_50mwind.shp")) %>%
  st_transform(crs = 32619)

# Create wind array to north of Wilson
wilson_n_area <- st_crop(wind_class, wilson_bb_5km) %>% filter(WPC >= 3) %>%
  filter(ID %in% c(7136, 7138:7139, 7170:7173, 7199:7201, 7233:7237, 7256:7263,
    7290:7293, 7322:7325, 7360:7361)) %>%
  st_union(.) %>%
  st_as_sf(.)

# Create wind array to south of Wilson
wilson_s_area_full <- st_crop(wind_class, wilson_bb_5km) %>% filter(WPC >= 3)%>%
  filter(ID %in% c(7512, 7553:7556, 7590:7593, 7626:7665, 7700:7705, 7735, 7761,
    7762, 7777, 7778, 7816)) %>%
  st_union(.) %>%
  st_as_sf(.)

# Two ponds that need to be removed from the Wilon south area
cranberry_sfc <- st_polygon(list(cbind(c(463000, 463600, 463600, 463000,463000),
    c(5036000, 5036000, 5035600, 5035600, 5036000)))) %>%
  st_sfc(., crs = st_crs(wgs84n19))
notch_sfc <- st_polygon(list(cbind(c(464200, 464800, 464800, 464200, 464200),
    c(5037000, 5037000, 5036600, 5036600, 5037000)))) %>%
  st_sfc(., crs = st_crs(wgs84n19))
cranberry_sf <- st_sf(tibble(name = "canberry"), geometry = cranberry_sfc)
notch_sf <- st_sf(tibble(name = "notch"), geometry = notch_sfc)

# Remove ponds
wilson_s_area <- wilson_s_area_full %>%
  st_difference(., cranberry_sf) %>%
  st_difference(., notch_sf) %>%
  st_union(.) %>%
  st_as_sf(.)

# View maps
mapview(wilson_n_area) +
mapview(wilson_s_area)

# Save Wilson areas
saveRDS(wilson_n_area, file.path(wind_output_dir, "wilson_n_area.rds"))
saveRDS(wilson_s_area, file.path(wind_output_dir, "wilson_s_area.rds"))

# Clean up objects
rm(wilson_bb_5km, wind_class, wilson_n_area, wilson_s_area_full, cranberry_sfc,
  cranberry_sf, notch_sfc, notch_sf, wilson_s_area, wilson_bb_5km)

## Create Wind Turbine Scenarios -----------------------------------------------

# Import base
base <- raster(base_file)

# Import Wilson nest
wilson_nest <- readRDS(file.path(wind_output_dir, "wilson_study_nest.rds"))

# Create 50km buffer for Wilson wind areas
wilson_bb_50km <- st_buffer(wilson_nest, 50000) %>% st_bbox(.) %>%
  st_as_sfc(.) %>% st_as_sf(tibble(id = 1), geometry = .)
if(FALSE) mapview(wilson_bb_50km)

# Save wilson_base_50km
wilson_base_50km <- crop(base, wilson_bb_50km)
writeRaster(wilson_base_50km, file.path(gis_exp_dir,
  "wilson_base_50km.tif"), progress = "text", datatype = 'INT2U',
  overwrite = TRUE)

# Clean up objects
rm(base, wilson_nest, wilson_bb_50km, wilson_base_50km)

# Update code to fix problems with windfarmGA
source("R/6h_Experiment_Fix_WindfarmGA.R")

# Import turbine specs
wt_hub_height_mode <- readRDS(file.path(wind_output_dir,
  "maine_turbine_hub_height_mode.rds"))
wt_rotor_diameter_mode <- readRDS(file.path(wind_output_dir,
  "maine_turbine_rotor_diameter_mode.rds"))

# Set wind turbine parameters
hub_height <- wt_hub_height_mode
rotor_radius <- wt_rotor_diameter_mode/2
grid_spacing <- 7
area_proportion <- .75
turbines_n <- 15

# Clean up objects
rm(wt_hub_height_mode, wt_rotor_diameter_mode)

# Create wind data for region
greenville_wind_formatted <- readRDS(file.path(wind_output_dir,
  "greenville_wind_no_zero.rds")) %>%
  mutate(ws = convert_wind_speed(ws, old_metric = "mph", new_metric = "mps"))%>%
  windata_format(.) %>%
  pluck(1)

## Wilson North Area Turbines  -------------------------------------------------

# Create buffered Wilson wind areas (to ensure sufficient space for turbines)
wilson_n_area <- readRDS(file.path(wind_output_dir, "wilson_n_area.rds"))
wilson_n_area_buffer_150m <- wilson_n_area %>% st_buffer(., dist = 150)

# Check grid and run genetric algorithm for Wilson North
wilson_n_grid <- grid_area_fixed(shape = wilson_n_area_buffer_150m,
  size = (rotor_radius*grid_spacing), prop = area_proportion, plotGrid = TRUE)
# Running optimization without topographic effects
wilson_n_result <- genetic_algorithm_fixed(Polygon1 = wilson_n_area_buffer_150m,
  n = turbines_n, Rotor = rotor_radius, fcrR = grid_spacing,
  referenceHeight = 10, RotorHeight = hub_height, iteration = 50,
  Proportionality = area_proportion, Projection = "EPSG:32619",
  vdirspe = greenville_wind_formatted)
# Running optimization with topographic effects
wilson_n_result_topo <- genetic_algorithm_fixed(
  Polygon1 = wilson_n_area_buffer_150m,
  n = turbines_n, Rotor = rotor_radius, fcrR = grid_spacing,
  referenceHeight = 10, RotorHeight = hub_height, iteration = 50,
  topograp = TRUE, sourceCCL = file.path(gis_exp_dir, "wilson_base_50km.tif"),
  Proportionality = area_proportion, Projection = "EPSG:32619",
  vdirspe = greenville_wind_formatted)

# Extract "best" turbine arrangement based on "EnergyOverall"
wilson_n_turbines_sf <- ConvertBestResultsToSF(wilson_n_result)
wilson_n_turbines_topo_sf <- ConvertBestResultsToSF(wilson_n_result_topo)

# Clean up objects
rm(wilson_n_grid, wilson_n_result, wilson_n_result_topo)

# Create distance matrix
wilson_n_dist_matrix <- st_distance(wilson_n_turbines_sf, wilson_n_turbines_sf)
diag(wilson_n_dist_matrix) <- NA

# Calculate mean and min nearest neighbor distance
wilson_n_turbines_sf$nn_distance <- rowMins(wilson_n_dist_matrix, na.rm = TRUE)
wilson_n_turbines_sf$nn_nearest <- rowMins(wilson_n_dist_matrix, na.rm = TRUE,
  value = T)

# Save sf and shapefile results without topographic effects
saveRDS(wilson_n_turbines_sf, file.path(wind_output_dir,
  "wilson_n_turbines.rds"))
st_write(wilson_n_turbines_sf, file.path(gis_exp_dir,
  "wilson_n_turbines.shp"), append = FALSE)

# Save sf and shapefile results with topographic effects
saveRDS(wilson_n_turbines_topo_sf, file.path(wind_output_dir,
  "wilson_n_turbines_topo.rds"))
st_write(wilson_n_turbines_topo_sf, file.path(gis_exp_dir,
  "wilson_n_turbines_topo.shp"), append = FALSE)

# Compare results
mapview::mapview(wilson_n_area_buffer_150m) +
mapview::mapview(wilson_n_turbines_sf, color = "red") +
mapview::mapview(wilson_n_turbines_topo_sf, color = "yellow")

wilson_n_dist_mean <- round(CalculateMode(wilson_n_turbines_sf$nn_distance))
wilson_n_dist_min <- round(min(wilson_n_turbines_sf$nn_distance))

maine_turbine_dist_mean <- readRDS(file.path(wind_output_dir,
  "maine_turbine_dist_mean.rds"))
maine_turbine_dist_min <- readRDS(file.path(wind_output_dir,
  "maine_turbine_dist_min.rds"))

print(paste0("Mean dist values: Wilson N (", wilson_n_dist_mean, "), Maine (",
  maine_turbine_dist_mean, ")"))
print(paste0("Minimum dist values: Wilson N (", wilson_n_dist_min, "), Maine (",
  maine_turbine_dist_min, ")"))

# Clean up objects for Wilson nouth
rm(wilson_n_area, wilson_n_area_buffer_150m, wilson_n_turbines_sf,
  wilson_n_turbines_topo_sf, wilson_n_dist_matrix, maine_turbine_dist_mean,
  maine_turbine_dist_min)

## Wilson South Area Turbines  -------------------------------------------------

# Create buffered Wilson wind areas (to ensure sufficient space for turbines)
wilson_s_area <- readRDS(file.path(wind_output_dir, "wilson_s_area.rds"))
wilson_s_area_buffer_150m <- wilson_s_area %>% st_buffer(., dist = 150)

# Check grid and run genetric algorithm for Wilson North
wilson_s_grid <- grid_area_fixed(shape = wilson_s_area_buffer_150m,
  size = (rotor_radius*grid_spacing), prop = area_proportion, plotGrid = TRUE)
# Running optimization without topographic effects
wilson_s_result <- genetic_algorithm_fixed(Polygon1 = wilson_s_area_buffer_150m,
  n = turbines_n, Rotor = rotor_radius, fcrR = grid_spacing,
  referenceHeight = 10, RotorHeight = hub_height, iteration = 50,
  Proportionality = area_proportion, Projection = "EPSG:32619",
  vdirspe = greenville_wind_formatted)
# Running optimization with topographic effects
wilson_s_result_topo <- genetic_algorithm_fixed(
  Polygon1 = wilson_s_area_buffer_150m,
  n = turbines_n, Rotor = rotor_radius, fcrR = grid_spacing,
  referenceHeight = 10, RotorHeight = hub_height, iteration = 50,
  topograp = TRUE, sourceCCL = file.path(gis_exp_dir, "wilson_base_50km.tif"),
  Proportionality = area_proportion, Projection = "EPSG:32619",
  vdirspe = greenville_wind_formatted)

# Extract "best" turbine arrangement based on "EnergyOverall"
wilson_s_turbines_sf <- ConvertBestResultsToSF(wilson_s_result)
wilson_s_turbines_topo_sf <- ConvertBestResultsToSF(wilson_s_result_topo)

# Clean up objects
rm(wilson_s_grid, wilson_s_result, wilson_s_result_topo)

# Create distance matrix
wilson_s_dist_matrix <- st_distance(wilson_s_turbines_sf, wilson_s_turbines_sf)
diag(wilson_s_dist_matrix) <- NA

# Calculate mean and min nearest neighbor distance
wilson_s_turbines_sf$nn_distance <- rowMins(wilson_s_dist_matrix, na.rm = TRUE)
wilson_s_turbines_sf$nn_nearest <- rowMins(wilson_s_dist_matrix, na.rm = TRUE,
  value = T)

# Save sf and shapefile results without topographic effects
saveRDS(wilson_s_turbines_sf, file.path(wind_output_dir,
  "wilson_s_turbines.rds"))
st_write(wilson_s_turbines_sf, file.path(gis_exp_dir,
  "wilson_s_turbines.shp"), append = FALSE)

# Save sf and shapefile results with topographic effects
saveRDS(wilson_s_turbines_topo_sf, file.path(wind_output_dir,
  "wilson_s_turbines_topo.rds"))
st_write(wilson_s_turbines_topo_sf, file.path(gis_exp_dir,
  "wilson_s_turbines_topo.shp"), append = FALSE)

# Compare results
mapview::mapview(wilson_s_area_buffer_150m) +
mapview::mapview(wilson_s_turbines_sf, color = "red") +
mapview::mapview(wilson_s_turbines_topo_sf, color = "yellow")

wilson_s_dist_mean <- round(mean(wilson_s_turbines_sf$nn_distance))
wilson_s_dist_min <- round(min(wilson_s_turbines_sf$nn_distance))

maine_turbine_dist_mean <- readRDS(file.path(wind_output_dir,
  "maine_turbine_dist_mean.rds"))
maine_turbine_dist_min <- readRDS(file.path(wind_output_dir,
  "maine_turbine_dist_min.rds"))

print(paste0("Mean dist values: Wilson N (", wilson_s_dist_mean, "), Maine (",
  maine_turbine_dist_mean, ")"))
print(paste0("Minimum dist values: Wilson N (", wilson_s_dist_min, "), Maine (",
  maine_turbine_dist_min, ")"))

# Clean up objects for Wilson south
rm(wilson_s_area, wilson_s_area_buffer_150m, wilson_s_turbines_sf,
  wilson_s_turbines_topo_sf, wilson_s_dist_matrix, maine_turbine_dist_mean,
  maine_turbine_dist_min)

# Clean up objects for wind turbine scenarios
rm(greenville_wind_formatted, hub_height, rotor_radius, grid_spacing,
  area_proportion, turbines_n)

## Create Turbines Distance Rasters --------------------------------------------

# Import base_50km
wilson_base_50km <- raster(file.path(gis_exp_dir, "wilson_base_50km.tif"))

# Import wind turbine data
wilson_n_turbines_sf <- readRDS(file.path(wind_output_dir,
  "wilson_n_turbines.rds"))
wilson_s_turbines_sf <- readRDS(file.path(wind_output_dir,
  "wilson_s_turbines.rds"))

wilson_n_turbines_30mc <- rasterize(as_Spatial(wilson_n_turbines_sf),
  y = wilson_base_50km, field = 'id', small = TRUE)
wilson_s_turbines_30mc <- rasterize(as_Spatial(wilson_s_turbines_sf),
  y = wilson_base_50km, field = 'id', small = TRUE)

wilson_ns_turbines_30mc <- sum(wilson_n_turbines_30mc,
  wilson_s_turbines_30mc, na.rm=TRUE)
wilson_ns_turbines_30mc[wilson_ns_turbines_30mc == 0] <- NA

wilson_n_dist_30mc <- distance(wilson_n_turbines_30mc, doEdge = TRUE)
wilson_s_dist_30mc <- distance(wilson_s_turbines_30mc, doEdge = TRUE)
wilson_ns_dist_30mc <- distance(wilson_ns_turbines_30mc, doEdge = TRUE)

# Rescale max distance to 20km (New step added in 2021-06)
wilson_n_dist_30mc[wilson_n_dist_30mc > 20000] <- 20000
wilson_n_dist_30mc[is.na(wilson_n_dist_30mc[])] <- 20000
wilson_s_dist_30mc[wilson_s_dist_30mc > 20000] <- 20000
wilson_s_dist_30mc[is.na(wilson_s_dist_30mc[])] <- 20000
wilson_ns_dist_30mc[wilson_ns_dist_30mc > 20000] <- 20000
wilson_ns_dist_30mc[is.na(wilson_ns_dist_30mc[])] <- 20000

writeRaster(wilson_n_dist_30mc, file.path(gis_exp_dir,
  "dist_turbines_wilson_n.tif"), progress = "text", datatype = 'INT2U',
  overwrite = TRUE)
writeRaster(wilson_s_dist_30mc, file.path(gis_exp_dir,
  "dist_turbines_wilson_s.tif"), progress = "text", datatype = 'INT2U',
  overwrite = TRUE)
writeRaster(wilson_ns_dist_30mc, file.path(gis_exp_dir,
  "dist_turbines_wilson_ns.tif"), progress = "text", datatype = 'INT2U',
  overwrite = TRUE)

# Create 'control' layer with all turbine_dist = 20km
wilson_c_dist_30mc <- wilson_ns_dist_30mc
wilson_c_dist_30mc[wilson_c_dist_30mc >= 0] <- 20000
plot(wilson_c_dist_30mc)

writeRaster(wilson_c_dist_30mc, file.path(gis_exp_dir,
  "dist_turbines_wilson_c.tif"), progress = "text", datatype = 'INT2U',
  overwrite = TRUE)

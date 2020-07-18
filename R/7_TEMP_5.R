# This script checks the sim run output and assesses the calibration metrics
# ---------------------------------------------------------------------------- #
# Load packages
pacman::p_load(plyr, dplyr, future, furrr, ggplot2, ggthemes, raster,
  stringr, tibble, tictoc, tidyr, whitebox, GmAMisc)
whitebox::wbt_init() # required for WhiteboxTools to work
wbt_version() # check WhiteboxTools version
library(gisr)

# Set options
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE, chunksize=1e9,
  memfrac=.9)

############################ IMPORT RASTERS ####################################

# Source data directories
file_dir <- "C:/ArcGIS/Data/R_Input/BAEA"

# Raster files

# Elevation names are elev_aggregate_(aggregation size)
elev_file <- file.path(file_dir, "elev_30mc.tif")
# Terrain class
elev_org <- raster(elev_file) # all other layers' extent are set to this layer

ext <- extent(elev_org) # all of Maine
ext <- extent(c(510000, 560000, 5025000, 5065000)) # middle of ME

elev <- crop(elev_org, ext, snap = "near")

slopes_1_5_50 <- ClassifyLandform(elev, elev_agg = 1, fine_scale = 5,
  course_scale = 50, wd = getwd(), remove_files = FALSE)
writeRaster(slopes_1_5_50, "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/slopes_1_5_50.tif")


slopes_1_10_25 <- ClassifyLandform(elev, elev_agg = 1, fine_scale = 10,
  course_scale = 25, wd = getwd(), remove_files = FALSE)
writeRaster(slopes_1_10_25, "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/slopes_1_10_25.tif")


slopes_3_10_50 <- ClassifyLandform(elev, elev_agg = 3, fine_scale = 10,
  course_scale = 50, wd = getwd(), remove_files = FALSE)
writeRaster(slopes_3_10_50, "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/slopes_3_10_50.tif")

slopes_5_3_10 <- ClassifyLandform(elev, elev_agg = 5, fine_scale = 3,
  course_scale = 10, wd = getwd(), remove_files = FALSE)
writeRaster(slopes_5_3_10, "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/slopes_5_3_10.tif")

slopes_5_3_20 <- ClassifyLandform(elev, elev_agg = 5, fine_scale = 3,
  course_scale = 20, wd = getwd(), remove_files = FALSE)
writeRaster(slopes_5_3_20, "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/slopes_5_3_20.tif")



ClassifyLandform <- function(elev,
                             elev_agg = 1,
                             fine_scale = 300/30,
                             course_scale = 1500/30,
                             wd = getwd(),
                             remove_files = FALSE) {

  # Check arguments
  is_odd <- function(x) {
    assertthat::assert_that(is.numeric(x), length(x) == 1)
    x %% 2 == 1
  }

  assertthat::assert_that(is_odd(elev_agg))
  assertthat::assert_that(fine_scale %% 1 == 0)
  assertthat::assert_that(course_scale %% 1 == 0)

  # Set and create function's working directory
  fun_wd <- file.path(wd, "ClassifyLandformFiles")
  suppressWarnings(if(!exists(fun_wd)) dir.create(fun_wd))

  # File Names
  elev_file <- file.path(fun_wd, paste0("ElevOrg.tif"))
  elev_agg_file <- file.path(fun_wd, paste0("ElevAgg_", elev_agg, ".tif"))
  elev_std_file <- file.path(fun_wd, paste0("ElevStand_", elev_agg, ".tif"))
  slope_file <- file.path(fun_wd, "Slope.tiff")
  fine_file <- file.path(fun_wd, paste0("RelTopoPos_", fine_scale, ".tif"))
  course_file <- file.path(fun_wd, paste0("RelTopoPos_", course_scale, ".tif"))
  fine_file2 <- file.path(fun_wd, paste0("RelTopoPos_", fine_scale, "a.tif"))
  course_file2 <- file.path(fun_wd, paste0("RelTopoPos_", course_scale, "a.tif"))

  # Elev raster aggregation
  if (elev_agg == 1){
    raster::writeRaster(elev, elev_agg_file, format="GTiff", overwrite = TRUE)
  } else {
    raster::writeRaster(elev, elev_file, format="GTiff", overwrite = TRUE)
    wbt_aggregate_raster(elev_file, elev_agg_file, agg_factor = elev_agg,
      type = "mean", verbose_mode = TRUE)
  }

  file.exists(elev_agg_file)
  # Standardize elevation
  wbt_z_scores(input = elev_agg_file, output = elev_std_file)

  # RTP calculations
  wbt_relative_topographic_position(elev_std_file, fine_file,
    filterx = fine_scale, filtery = fine_scale)
  wbt_relative_topographic_position(elev_std_file, course_file,
    filterx = course_scale, filtery = course_scale)

  wbt_dev_from_mean_elev(elev_agg_file, fine_file2,
    filterx = fine_scale, filtery = fine_scale)
  wbt_dev_from_mean_elev(elev_agg_file, course_file2,
    filterx = course_scale, filtery = course_scale)

  # Slope Calculations
  wbt_slope(elev_agg_file, slope_file, zfactor=1.0, units="degrees")

  # Extract Rasters
  fs <- raster::raster(fine_file)
  cs <- raster::raster(course_file)
  slp <- raster::raster(slope_file)

  # Define the ten classes on the basis of thresholds of sn, sl, and slope
  canyons <- (fs <= -1) & (cs <= -1)
  #canyons[na.omit(canyons)] <- 1

  midslope.dr <- (fs <= -1) & (cs> -1 & cs < 1)
  #midslope.dr[na.omit(midslope.dr)] <- 2

  upland.dr <-  (fs <= -1) & (cs >= 1)
  #upland.dr[na.omit(upland.dr)] <- 3

  us.valley <-  (fs > -1 & fs< 1) & (cs <=-1)
  #us.valley[na.omit(us.valley)] <- 4

  plains <- (fs > -1 & fs < 1) & (cs > -1 & cs < 1) & (slp <= 5)
  #plains[na.omit(plains)] <- 5

  open.slp <-  (fs > -1 & fs < 1) & (cs > -1 & cs < 1) & (slp > 5)
  #open.slp[na.omit(open.slp)] <- 6

  upper.slp <- (fs > -1 & fs < 1) & (cs >= 1)
  #upper.slp[na.omit(upper.slp)] <- 7

  local.rdg <- (fs >= 1) & (cs <= -1)
  #local.rdg[na.omit(local.rdg)] <- 8

  midslp.rdg <- (fs >= 1) & (cs > -1 & cs < 1)
  #midslp.rdg[na.omit(midslp.rdg)] <- 9

  mount.top <- (fs >= 1) & (cs >=1)
  #mount.top[na.omit(mount.top)] <- 10

  slope_brick <- brick(upper.slp, midslp.rdg, mount.top)
  slope_sum <- calc(slope_brick, sum)
  return(slope_sum)

}

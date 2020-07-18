# This script checks the sim run output and assesses the calibration metrics
# ---------------------------------------------------------------------------- #
# Load packages
pacman::p_load(plyr, dplyr, future, furrr, ggplot2, ggthemes, raster,
  stringr, tibble, tictoc, tidyr, whitebox)
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
elev_agg_02 <- "C:/ArcGIS/Data/R_Input/BAEA/elev_aggregate_02.tiff"
elev_agg_05 <- "C:/ArcGIS/Data/R_Input/BAEA/elev_aggregate_05.tiff"
elev_agg_10 <- "C:/ArcGIS/Data/R_Input/BAEA/elev_aggregate_10.tiff"

# Elev raster aggregation
wbt_aggregate_raster(elev_file, elev_agg_02, agg_factor = 2, type = "mean",
  verbose_mode = TRUE)
wbt_aggregate_raster(elev_file, elev_agg_05, agg_factor = 5, type = "mean",
  verbose_mode = TRUE)
wbt_aggregate_raster(elev_file, elev_agg_10, agg_factor = 10, type = "mean",
  verbose_mode = TRUE)

# Relative Topographic Position names are ridge_(aggregation size)_(filter size)
rtp_02_50 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_02_50.tiff"
rtp_05_30 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_05_30.tiff"
rtp_05_50 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_05_50.tiff"
rtp_10_30 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_10_30.tiff"
rtp_10_50 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_10_50.tiff"
rtp_10_75 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_10_75.tiff"
rtp_02_50_rc1 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_02_50_rc1.tiff"
rtp_02_50_rc1_thin <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_02_50_rc1_thin.tiff"

rtp_05_50_rc1 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_05_50_rc1.tiff"
rtp_05_50_rc2 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_05_50_rc2.tiff"

rtp_10_50_rc1 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_10_50_rc1.tiff"
rtp_10_50_rc2 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_10_50_rc2.tiff"
rtp_10_75_rc1 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_10_75_rc1.tiff"
rtp_10_75_rc2 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_10_75_rc2.tiff"

# RTP calculations
wbt_relative_topographic_position(elev_agg_02, rtp_02_50,
  filterx = 51, filtery = 51, verbose_mode = TRUE)
wbt_relative_topographic_position(elev_agg_05, rtp_05_30,
  filterx = 31, filtery = 31, verbose_mode = TRUE)
wbt_relative_topographic_position(elev_agg_05, rtp_05_50,
  filterx = 51, filtery = 51, verbose_mode = TRUE)
wbt_relative_topographic_position(elev_agg_10, rtp_10_30,
  filterx = 31, filtery = 31, verbose_mode = TRUE)
wbt_relative_topographic_position(elev_agg_10, rtp_10_50,
  filterx = 51, filtery = 51, verbose_mode = TRUE)
wbt_relative_topographic_position(elev_agg_10, rtp_10_75,
  filterx = 76, filtery = 76, verbose_mode = TRUE)

wbt_reclass(rtp_02_50, rtp_02_50_rc1, '0.0;-1.0;.35;1;.35;1.0',
  assign_mode = FALSE, verbose_mode = TRUE)

wbt_reclass(rtp_05_50, rtp_05_50_rc1, '0.0;-1.0;.15;1;.15;1.0',
  assign_mode = FALSE, verbose_mode = TRUE)
wbt_reclass(rtp_05_50, rtp_05_50_rc2, '0.0;-1.0;.35;1;.35;1.0',
  assign_mode = FALSE, verbose_mode = TRUE)

wbt_reclass(rtp_10_50, rtp_10_50_rc1, '0.0;-1.0;.15;1;.15;1.0',
  assign_mode = FALSE, verbose_mode = TRUE)
wbt_reclass(rtp_10_50, rtp_10_50_rc2, '0.0;-1.0;.35;1;.35;1.0',
  assign_mode = FALSE, verbose_mode = TRUE)
wbt_reclass(rtp_10_75, rtp_10_75_rc1, '0.0;-1.0;.15;1;.15;1.0',
  assign_mode = FALSE, verbose_mode = TRUE)
wbt_reclass(rtp_10_75, rtp_10_75_rc2, '0.0;-1.0;.35;1;.35;1.0',
  assign_mode = FALSE, verbose_mode = TRUE)

wbt_line_thinning(rtp_02_50_rc1,rtp_02_50_rc1_thin, verbose_mode = TRUE)


r <- raster(rtp_05_50_rc2)
r[r==0]<-NA
plot(r)

# extend r with a number of rows and columns (at each side)
# to isolate clumps adjacent to plot axes
r2 <- extend(r, c(1,1))
rc <- clump(r2, directions = 8)

# get frequency table
f <- freq(rc)
# save frequency table as data frame
f <- as.data.frame(f)

# which rows of the data.frame are only represented by clumps under 9pixels?
str(which(f$count <= 3))
# which values do these correspond to?
str(f$value[which(f$count <= 3)])
# put these into a vector of clump ID's to be removed
excludeID <- f$value[which(f$count <= 3)]

# make a new raster to be sieved
formaskSieve <- rc
# assign NA to all clumps whose IDs are found in excludeID
formaskSieve[rc %in% excludeID] <- NA

writeRaster(formaskSieve, "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/rtp_05_50_rc2_clump3.tiff",
            overwrite = TRUE)

# Elevation Percentile names are ridge_(aggregation size)_(filter size)
ep_05_30 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/ep_05_30.tiff"
ep_05_50 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/ep_05_50.tiff"
ep_10_30 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/ep_10_30.tiff"
ep_10_50 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/ep_10_50.tiff"
ep_10_75 <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines/ep_10_75.tiff"

# EP calculations
wbt_elev_percentile(elev_agg_05, ep_05_30,
  filterx = 31, filtery = 31, verbose_mode = TRUE)
wbt_elev_percentile(elev_agg_05, ep_05_50,
  filterx = 51, filtery = 51, verbose_mode = TRUE)
wbt_elev_percentile(elev_agg_10, ep_10_30,
  filterx = 31, filtery = 31, verbose_mode = TRUE)
wbt_elev_percentile(elev_agg_10, ep_10_50,
  filterx = 51, filtery = 51, verbose_mode = TRUE)
wbt_elev_percentile(elev_agg_10, ep_10_75,
  filterx = 76, filtery = 76, verbose_mode = TRUE)

plot(raster(rtp_10_75_rc2))
hist(raster(rtp_10_75_rc2))

library(whitebox)
dem <- system.file("extdata", "DEM.tif", package="whitebox")
output <- file.path(getwd(), "output.tif")
wbt_find_ridges(dem, "C:/ArcGIS/Data/R_Input/BAEA/RidgeLines_Test.tiff",
  line_thin = FALSE, verbose_mode = TRUE)

wbt_find_ridges(dem, "C:/ArcGIS/Data/R_Input/BAEA/RidgeLinesThin_Test.tiff",
  line_thin = TRUE, verbose_mode = TRUE)

plot(raster(dem))
ridges <- raster("C:/ArcGIS/Data/R_Input/BAEA/RidgeLinesThin_Test.tiff")
plot(ridges)

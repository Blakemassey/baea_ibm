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


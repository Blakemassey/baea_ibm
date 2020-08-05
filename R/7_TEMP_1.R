## THIS IS A TEST FOR CONVERTING THE SSF MODELS TO SSF SURFACES

# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, plyr, dplyr, future, furrr, optimx, ggplot2,
  ggthemes, glmulti, lubridate, optimx, purrr, raster, reproducible, rgenoud,
  stringr, survival, surveybootstrap, tibble, tictoc, tidyr, whitebox)
whitebox::wbt_init() # required for WhiteboxTools to work
wbt_version() # check WhiteboxTools version
library(gisr)
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE,
  memfrac = .75)
############################ IMPORT RASTERS ####################################

############################ CREATE RASTERS FOR MAINE ##########################

preds_tbl <- readRDS("Output/Analysis/SSF/Models/best_fits/preds_tbl.rds")

# Start of Terrain Rasters -----------------------------------------------------

# Source data directories
file_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
elev_file <- file.path(file_dir, "elev_30mc.tif")

elev_org <- raster(elev_file) # all other layers' extent are set to this layer

x_min <- xmin(elev_org)
x_max <- xmax(elev_org)
y_min <- ymin(elev_org)
y_max <- ymax(elev_org)
y_mid <- (ymax(elev_org) + ymin(elev_org))/2
y_half <- (ymax(elev_org) - ymin(elev_org))*(1/2)

ext_top <- extent(x_min, x_max, y_min + y_half, y_max)
ext_mid <- extent(x_min, x_max, y_mid - 100000, y_mid + 100000)
ext_mid_crop <- extent(x_min, x_max, y_mid - 50000, y_mid + 50000)
ext_bot <- extent(x_min, x_max, y_min, y_max - y_half)

removeTmpFiles(h=0)

CalculateTerrainMetricWithSigma <- function(sigma, metric, ext, elev = elev_org){
  print(paste0("Starting: ", metric, sigma))
  if(sigma == 0) sigma <- 1
  size <- (sigma*2) + 1
  x <- crop(elev, ext)
  weight_matrix <- matrix(1, nrow = size, ncol = size)
  center <- ceiling(0.5 * length(weight_matrix))
  window <- length(weight_matrix)-1
  if (metric == "tri"){
    tri <- focal(x, w = weight_matrix,
      fun = function(x, ...) sum(abs(x[-center] - x[center]))/window,
      pad = TRUE, padValue = NA)
      out_matrix <- tri
  } else if (metric == "tpi"){
    tpi <- focal(x, w = weight_matrix,
      fun = function(x, ...) x[center] - mean(x[-center]),
      pad = TRUE, padValue = NA)
    out_matrix <- tpi
  } else if (metric == "roughness"){
    rough <- focal(x, w = weight_matrix,
      fun = function(x, ...) max(x) - min(x),
      pad = TRUE, padValue = NA, na.rm = TRUE)
    out_matrix <- rough
  } else {
    stop("'metric' must equal 'tpi', 'tri', or 'roughness'", call. = FALSE)
  }
  return(out_matrix)
}

preds_terrain <- preds_tbl %>%
  filter(raster_class == "terrain_class")

for (i in 6:nrow(preds_terrain)){
  preds_terrain_i <- preds_terrain %>%
    slice(i)
  sigma <- preds_terrain_i %>% pull(sigma)
  covar <- preds_terrain_i %>% pull(covar)
  print(paste0("Starting: ", covar, sigma, " (", i, " of ", nrow(preds_terrain),
    ") at: ", lubridate::now()))

  top_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Terrain",
    paste0(covar, sigma, "_top.grd"))
  mid_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Terrain",
    paste0(covar, sigma, "_mid.grd"))
  bot_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Terrain",
    paste0(covar, sigma, "_bot.grd"))
  out_file <- file.path(file_dir, "SSF_Rasters", paste0(covar, sigma, ".tif"))

  # Covar TOP
  print(paste0("Starting top at: ", lubridate::now()))
  covar_i_top <- CalculateTerrainMetricWithSigma(sigma, covar, ext_top, elev_org)
  writeRaster(covar_i_top, top_file, format = "raster", overwrite = TRUE)
  rm(covar_i_top)
  gc()

  # Covar MID
  print(paste0("Starting mid at: ", lubridate::now()))
  print("mid")
  covar_i_mid <- CalculateTerrainMetricWithSigma(sigma, covar, ext_mid, elev_org)
  covar_i_mid_crop <- crop(covar_i_mid, ext_mid_crop)
  writeRaster(covar_i_mid_crop, mid_file, format = "raster", overwrite = TRUE)
  rm(covar_i_mid)
  gc()

  # Covar BOT
  print(paste0("Starting bot at: ", lubridate::now()))
  covar_i_bot <- CalculateTerrainMetricWithSigma(sigma, covar, ext_bot, elev_org)
  writeRaster(covar_i_bot, bot_file, format = "raster", overwrite = TRUE)
  rm(covar_i_bot)
  gc()

   # Merge together rasters
  top_raster <- raster(top_file)
  mid_raster <- raster(mid_file)
  bot_raster <- raster(bot_file)
  out_raster <- raster::merge(mid_raster, top_raster, bot_raster)

  # Write out merged raster
  writeRaster(out_raster, out_file, format = "GTiff", overwrite = TRUE)

  rm(top_raster, mid_raster, bot_raster, top_file, mid_file, bot_file)
}

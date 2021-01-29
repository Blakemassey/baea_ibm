########## This script converts SSF Models to SSF Input Surfaces #############-#

# Load libraries, scripts, and input parameters
pacman::p_load(tidyverse, optimx, ggplot2, ggthemes, glmulti, lubridate,
  optimx, raster, reproducible, stringr, tictoc, whitebox)
library(gisr)

whitebox::wbt_init() # required for WhiteboxTools to work
wbt_version() # check WhiteboxTools version
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE,
  memfrac = .75)

# Directory of fits
mod_fit_dir = "Output/Analysis/SSF/Models"

############################ IMPORT RASTERS ####################################

## Get SSF FITS ----------------------------------------------------------------

best_ssf_fits_org <- readRDS(file.path(mod_fit_dir, "model_fits_compiled_best",
  "model_fits_compiled_best.rds")) %>% slice(c(3,6,10,14)) #c(2, 5, 13))
best_ssf_fits_org %>% dplyr::select(step_type, fit_covars_clean, model_full)
best_ssf_fits <- best_ssf_fits_org

best_ssf_fits %>% pluck("model_full") %>% unlist()

# Determine all the raster_sigma layers
preds_all <- unlist(best_ssf_fits$fit_covars_clean) %>% str_remove_all("\\^2")
preds_unique <- unique(preds_all)
preds_rasters <- unique(str_replace_all(preds_unique, "[0-9]", ""))

# Raster classes (used in next step)
raster_classes <- c(developed = "kernel_class",
  forest = "kernel_class",
  open_water = "kernel_class",
  pasture = "kernel_class",
  shrub_herb = "kernel_class",
  wetland = "kernel_class",
  road = "kernel_class",
  eastness = "kernel_class",
  northness = "kernel_class",
  wind_class = "kernel_class",
  dist_developed = "extract_class",
  dist_hydro = "extract_class",
  dist_turbine = "extract_class",
  road_dist = "extract_class",
  tpi = "terrain_class",
  tri = "terrain_class",
  roughness = "terrain_class")

preds_tbl <- tibble(preds_unique) %>%
  mutate(sigma = as.integer(str_extract(preds_unique, "[0-9]{1,3}"))) %>%
  mutate(covar = str_replace_all(preds_unique, "[0-9]", "")) %>%
  dplyr::select(covar, sigma) %>%
  mutate(covar_sigma = paste0(covar, sigma)) %>%
  mutate(raster_class = recode(covar, !!!raster_classes)) %>%
  mutate(raster_layer = vector(mode = "list", length = nrow(.))) %>%
  arrange(covar, sigma)

saveRDS(preds_tbl, file.path(mod_fit_dir, "model_fits_compiled_best",
  "preds_tbl.rds"))

################ CREATE COVARIATE RASTERS FOR MAINE ############################

preds_tbl <- readRDS(file.path(mod_fit_dir, "model_fits_compiled_best",
  "preds_tbl.rds"))

# Source data directories
input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
elev_file <- file.path(input_dir, "elev_30mc.tif")
kernel_dir <- file.path(input_dir, "SSF_Rasters/Processed_Sections/Kernel")
covars_full_dir <- file.path(input_dir, "SSF_Rasters/Covars_Full")

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

# Start of Kernel Rasters ------------------------------------------------------
preds_kernel <- preds_tbl %>%
  filter(raster_class == "kernel_class")

for (i in 1:nrow(preds_kernel)){
  preds_kernel_i <- preds_kernel %>% slice(i)
  sigma <- preds_kernel_i %>% pull(sigma)
  covar <- preds_kernel_i %>% pull(covar)
  print(paste0("Checking: ", covar, sigma))
  out_file <- file.path(covars_full_dir, paste0(covar, sigma, ".tif"))
  if(!file.exists(out_file)){
    raster_file <- file.path(input_dir, paste0(covar, "_30mc.tif"))
    print(paste0("Needed: ", covar, sigma))
  }
}

for (i in 1:nrow(preds_kernel)){
  preds_kernel_i <- preds_kernel %>% slice(i)
  sigma <- preds_kernel_i %>% pull(sigma)
  covar <- preds_kernel_i %>% pull(covar)
  print(paste0("Checking: ", covar, sigma))
  out_file <- file.path(covars_full_dir, paste0(covar, sigma, ".tif"))
  if(!file.exists(out_file)){
    raster_file <- file.path(input_dir, paste0(covar, "_30mc.tif"))
    print(paste0("Starting: ", covar, sigma, " (", i, " of ",
      nrow(preds_kernel), ") at: ", lubridate::now()))
    if(sigma == 0){
      out_raster <- raster(raster_file)
      # Write out raster
      writeRaster(out_raster, out_file, format = "GTiff", overwrite = TRUE)
    } else {
      top_file <- file.path(kernel_dir, paste0(covar, sigma, "_top.grd"))
      mid_file <- file.path(kernel_dir, paste0(covar, sigma, "_mid.grd"))
      bot_file <- file.path(kernel_dir, paste0(covar, sigma, "_bot.grd"))
      # Covar TOP
      print("top")
      covar_top <- crop(raster(raster_file), ext_top, snap = "near")
      covar_top_smooth <- raster::raster(covar_top) # creates blank raster
      values(covar_top_smooth) <-
        smoothie::gauss2dsmooth(raster::as.matrix(covar_top),
        lambda = sigma, nx = DescTools::RoundTo(nrow(covar_top), 2),
        ny = DescTools::RoundTo(ncol(covar_top), 2))
      writeRaster(covar_top_smooth, top_file, format = "raster",
        overwrite = TRUE)
      rm(covar_top, covar_top_smooth)
      gc()
      # Covar MID
      print("mid")
      covar_mid <- crop(raster(raster_file), ext_mid, snap = "near")
      covar_mid_smooth <- raster::raster(covar_mid) # creates blank raster
      values(covar_mid_smooth) <-
        smoothie::gauss2dsmooth(raster::as.matrix(covar_mid),
        lambda = sigma, nx = DescTools::RoundTo(nrow(covar_mid), 2),
        ny = DescTools::RoundTo(ncol(covar_mid), 2))
      covar_mid_smooth <- crop(covar_mid_smooth, ext_mid_crop)
      writeRaster(covar_mid_smooth, mid_file, format = "raster",
        overwrite = TRUE)
      rm(covar_mid, covar_mid_smooth)
      gc()
      # Covar BOTTOM
      print("bottom")
      covar_bot <- crop(raster(raster_file), ext_bot, snap = "near")
      covar_bot_smooth <- raster::raster(covar_bot) # creates blank raster
      values(covar_bot_smooth) <-
        smoothie::gauss2dsmooth(raster::as.matrix(covar_bot),
        lambda = sigma, nx = DescTools::RoundTo(nrow(covar_bot), 2),
        ny = DescTools::RoundTo(ncol(covar_bot), 2))
      writeRaster(covar_bot_smooth, bot_file, format = "raster",
        overwrite = TRUE)
      rm(covar_bot, covar_bot_smooth)
      gc()
      # Merge three rasters
      top_raster <- raster(top_file)
      mid_raster <- raster(mid_file)
      bot_raster <- raster(bot_file)
      out_raster <- raster::merge(mid_raster, top_raster, bot_raster)
      # Write out merged raster
      writeRaster(out_raster, out_file, format = "GTiff", overwrite = TRUE)
      rm(top_file, mid_file, bot_file, top_raster, mid_raster, bot_raster)
    }
  }
}

# Start of Terrain Rasters -----------------------------------------------------

# Source data directories

input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
elev_file <- file.path(input_dir, "elev_30mc.tif")
terrain_dir <- file.path(input_dir, "SSF_Rasters/Processed_Sections/Terrain")
covars_full_dir = file.path(input_dir, "SSF_Rasters/Covars_Full")

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

CalculateTerrainMetricWithSigma <- function(sigma, metric, ext, elev =elev_org){
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

for (i in seq_len(nrow(preds_terrain))){
  preds_terrain_i <- preds_terrain %>% slice(i)
  sigma <- preds_terrain_i %>% pull(sigma)
  covar <- preds_terrain_i %>% pull(covar)
  print(paste0("Checking: ", covar, sigma))
  out_file <- file.path(covars_full_dir, paste0(covar, sigma, ".tif"))

  if(!file.exists(out_file)){
    print(paste0("Needed: ", covar, sigma))
  }
}

for (i in seq_len(nrow(preds_terrain))){
  preds_terrain_i <- preds_terrain %>% slice(i)
  sigma <- preds_terrain_i %>% pull(sigma)
  covar <- preds_terrain_i %>% pull(covar)
  print(paste0("Checking: ", covar, sigma))
  out_file <- file.path(covars_full_dir, paste0(covar, sigma, ".tif"))

  if(!file.exists(out_file)){
    print(paste0("Starting: ", covar, sigma, " (", i, " of ",
      nrow(preds_terrain), ") at: ", lubridate::now()))

    top_file <- file.path(terrain_dir, paste0(covar, sigma, "_top.grd"))
    mid_file <- file.path(terrain_dir, paste0(covar, sigma, "_mid.grd"))
    bot_file <- file.path(terrain_dir, paste0(covar, sigma, "_bot.grd"))

    # Covar TOP
    print(paste0("Starting top at: ", lubridate::now()))
    covar_i_top <- CalculateTerrainMetricWithSigma(sigma, covar, ext_top,
      elev_org)
    writeRaster(covar_i_top, top_file, format = "raster", overwrite = TRUE)
    rm(covar_i_top)
    gc()

    # Covar MID
    print(paste0("Starting mid at: ", lubridate::now()))
    covar_i_mid <- CalculateTerrainMetricWithSigma(sigma, covar, ext_mid,
      elev_org)
    covar_i_mid_crop <- crop(covar_i_mid, ext_mid_crop)
    writeRaster(covar_i_mid_crop, mid_file, format = "raster", overwrite = TRUE)
    rm(covar_i_mid)
    gc()

    # Covar BOT
    print(paste0("Starting bot at: ", lubridate::now()))
    covar_i_bot <- CalculateTerrainMetricWithSigma(sigma, covar, ext_bot,
      elev_org)
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
}

# Start of Extract Rasters -----------------------------------------------------

input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
covars_full_dir = file.path(input_dir, "SSF_Rasters/Covars_Full")

dist_developed_file <- file.path(input_dir, "dist_turbine_30mc.tif")
dist_hydro_file <- file.path(input_dir, "dist_hydro_30mc.tif")
dist_turbine_file <- file.path(input_dir, "dist_turbine_30mc.tif")
road_dist_file <- file.path(input_dir, "road_dist_30mc.tif")

dist_developed_out_file <- file.path(covars_full_dir, "dist_developed0.tif")
dist_hydro_out_file <- file.path(covars_full_dir, "dist_hydro0.tif")
dist_turbine_out_file <- file.path(covars_full_dir, "dist_turbine0.tif")
road_dist_out_file <- file.path(covars_full_dir, "dist_road0.tif")

if (!file.exists(dist_developed_out_file)){
writeRaster(raster(dist_developed_file), dist_developed_out_file,
  format = "GTiff", overwrite = TRUE)
}
if (!file.exists(dist_hydro_out_file)){
writeRaster(raster(dist_hydro_file), dist_hydro_out_file, format = "GTiff",
  overwrite = TRUE)
}
if (!file.exists(dist_turbine_out_file)){
  # Change turbine dist raster cells > 20000 or NA to 20000 (max dist in ua_data)
  dist_turbine_org <- raster(dist_turbine_file)
  dist_turbine_max <- cellStats(dist_turbine_org, stat = "max")
  dist_turbine <- dist_turbine_org
  dist_turbine[dist_turbine > 20000] <- 20000 # max dist allowed in ua_data
  dist_turbine[is.na(dist_turbine)] <- 20000 # max dist allowed in ua_data
  plot(dist_turbine)
  hist(dist_turbine)
  writeRaster(dist_turbine, dist_turbine_out_file, format = "GTiff",
    overwrite = TRUE)
}
if (!file.exists(road_dist_out_file)){
writeRaster(raster(road_dist_file), road_dist_out_file, format = "GTiff",
  overwrite = TRUE)
}


################### MASK COVARIATE RASTERS TO MAINE ONLY #######################

# Crop all the Covariate Rasters -----------------------------------------------
preds_tbl_file = "Output/Analysis/SSF/Models/best_simpler_fits/preds_tbl.rds"
covars_full_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters/Covars_Full"
covars_crop_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters/Covars_Crop"
maine_raster_trim_file = "C:/ArcGIS/Data/BlankRaster/maine_trim.tif"

preds_tbl <- readRDS(file.path("Output/Analysis/SSF/Models",
  "model_fits_compiled_best/preds_tbl.rds"))
maine_raster_trim <- raster(maine_raster_trim_file)

for (i in seq_len(nrow(preds_tbl))){
  preds_tbl_i <- preds_tbl %>% slice(i)
  covar_sigma <- preds_tbl_i %>% pull(covar_sigma)
  print(paste0("Starting: ", covar_sigma, " (", i, " of ", nrow(preds_tbl),
    ") at: ", lubridate::now()))

  covar_file <- file.path(covars_full_dir, paste0(covar_sigma, ".tif"))
  out_file <- file.path(covars_crop_dir, paste0(covar_sigma, ".tif"))

  if(!file.exists(out_file)){
    print(paste0("Writing: ", covar_sigma, " at: ", lubridate::now()))
    covar_raster <- raster(covar_file)
    covar_raster_crop <- crop(covar_raster, maine_raster_trim)
    covar_raster_mask <- mask(covar_raster_crop, maine_raster_trim)
    writeRaster(covar_raster_mask, out_file, format = "GTiff", overwrite = TRUE)
  }
}

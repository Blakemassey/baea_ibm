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

## Get SSF FITS ----------------------------------------------------------------

# Directory of fits
mod_fit_dir = "Output/Analysis/SSF/Models"

best_ssf_fits_org <- readRDS(file.path(mod_fit_dir, "best_fits",
  "best_ssf_fit_all.rds"))

best_ssf_fits_org %>% dplyr::select(step_type, preds)
best_ssf_fits <- best_ssf_fits_org

# Determine all the raster_sigma layers
preds_all <- paste0(best_ssf_fits$preds, collapse = " + ")
preds_unique <- unique(str_split(preds_all, " \\+ ") %>% pluck(1))
preds_rasters <- unique(str_replace_all(preds_unique, "[0-9]", ""))

# Raster classes (used in next step)
raster_classes <- c(developed = "kernel_class",
  forest = "kernel_class",
  open_water = "kernel_class",
  pasture = "kernel_class",
  shrub_herb = "kernel_class",
  wetland = "kernel_class",
  eastness = "kernel_class",
  northness = "kernel_class",
  wind_class = "kernel_class",
  developed_dist = "extract_class",
  hydro_dist = "extract_class",
  turbine_dist = "extract_class",
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

saveRDS(preds_tbl, "Output/Analysis/SSF/Models/best_fits/preds_tbl.rds")

################ CREATE COVARIATE RASTERS FOR MAINE ############################

preds_tbl <- readRDS("Output/Analysis/SSF/Models/best_fits/preds_tbl.rds")

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

# Start of Kernel Rasters ------------------------------------------------------
preds_kernel <- preds_tbl %>%
  filter(raster_class == "kernel_class")
for (i in 1:nrow(preds_kernel)){
  preds_kernel_i <- preds_kernel %>%
    slice(i)
  sigma <- preds_kernel_i %>% pull(sigma)
  covar <- preds_kernel_i %>% pull(covar)
  raster_file <- file.path(file_dir, paste0(covar, "_30mc.tif"))
  print(paste0("Starting: ", covar, sigma, " (", i, " of ", nrow(preds_kernel),
    ") at: ", lubridate::now()))

  top_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Kernel",
    paste0(covar, sigma, "_top.grd"))
  mid_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Kernel",
    paste0(covar, sigma, "_mid.grd"))
  bot_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Kernel",
    paste0(covar, sigma, "_bot.grd"))
  out_file <- file.path(file_dir, "SSF_Rasters", paste0(covar, sigma, ".tif"))

  # Covar TOP
  print("top")
  covar_top <- crop(raster(raster_file), ext_top, snap = "near")
  covar_top_smooth <- raster::raster(covar_top) # creates blank raster
  values(covar_top_smooth) <-
    smoothie::gauss2dsmooth(raster::as.matrix(covar_top),
    lambda = sigma, nx = DescTools::RoundTo(nrow(covar_top), 2),
    ny = DescTools::RoundTo(ncol(covar_top), 2))
  writeRaster(covar_top_smooth, top_file, format = "raster", overwrite = TRUE)
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
  writeRaster(covar_mid_smooth, mid_file, format = "raster", overwrite = TRUE)
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
  writeRaster(covar_bot_smooth, bot_file, format = "raster", overwrite = TRUE)
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

for (i in seq_along(nrow(preds_terrain))){
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

# Start of Extract Rasters -----------------------------------------------------

file_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"

developed_dist_file <- file.path(file_dir, "developed_dist_30mc.tif")
hydro_dist_file <- file.path(file_dir, "hydro_dist_30mc.tif")
turbine_dist_file <- file.path(file_dir, "turbine_dist_30mc.tif")

writeRaster(raster(developed_dist_file), file.path(ssf_raster_dir,
  "developed_dist0.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster(hydro_dist_file), file.path(ssf_raster_dir,
  "hydro_dist0.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster(developed_dist_file), file.path(ssf_raster_dir,
  "turbine_dist0.tif"), format = "GTiff", overwrite = TRUE)

# ONE-OFF FILE MOVE
# tri50_file <- file.path(ssf_raster_dir, "Terrain/Final_Raster",
#   "tri50.tif")
# writeRaster(raster(tri50_file), file.path(ssf_raster_dir,
#   "tri50.tif"), format = "GTiff", overwrite = TRUE)

################### GENERATE SSF_LAYERS FOR MAINE ##############################

# Directory of fits
mod_fit_dir = "Output/Analysis/SSF/Models"
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"
maine_raster_trim_file = "C:/ArcGIS/Data/BlankRaster/maine_trim.tif"

best_ssf_fits <- readRDS(file.path(mod_fit_dir, "best_fits",
  "best_ssf_fit_all.rds"))
maine_raster_trim <- raster(maine_raster_trim_file)

# Generate layer for each ssf
for (i in 1:nrow(best_ssf_fits)){
  print(i)
  clogit_fit_i <- best_ssf_fits %>% slice(i) %>% pull(clogit_fit) %>% pluck(1)
  step_type_i <- best_ssf_fits %>% slice(i) %>% pull(step_type)
  # Extract terms(not including 'strata(step_id)')
  terms_i <- clogit_fit_i %>% pluck(terms, attr_getter("term.labels")) %>%
    .[!. %in% c("strata(step_id)")]
  # Extract coefficients
  coefs_i <- clogit_fit_i %>% pluck(coef)

  # Create Raster_Brick
  covars_list <- vector(mode = "list", length = length(terms_i))
  for (j in seq_along(terms_i)){
    terms_i_j <- terms_i[j]
    print(terms_i_j)
    raster_file <- file.path(ssf_raster_dir, paste0(terms_i_j, ".tif"))
    covars_list[[j] <- raster(raster_file)
  }
  covars_brick <- raster::brick(covars_list)
  names(covars_brick)

  # Generate formulas
  ssf_formula <- paste0("(", paste(paste0("covars_brick[['", names(coefs_i),
    "']]"), paste0("coefs_i['", names(coefs_i),"']"), sep = "*",
    collapse = ") + ("), ")")

  # Create raster, then crop and mask
  ssf_raster <- eval(parse(text = ssf_formula))
  ssf_raster_crop <- crop(ssf_raster, maine_raster_trim)
  ssf_raster_mask <- mask(ssf_raster_crop, maine_raster_trim)

  # Write Raster to output dir
  step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
    "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
  writeRaster(ssf_raster_mask, file.path(ssf_raster_dir, "Step_Types",
    step_type_i_numeric), format = "GTiff", overwrite = TRUE)
  rm(ssf_formula, ssf_raster, ssf_raster_crop, ssf_raster_mask,
    step_type_i_numeric)
  removeTmpFiles(h = 0)
  gc()
}

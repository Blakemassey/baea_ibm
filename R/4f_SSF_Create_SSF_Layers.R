## THIS IS A TEST FOR CONVERTING THE SSF MODELS TO SSF SURFACES

# Load libraries, scripts, and input parameters
pacman::p_load(plyr, dplyr, fasterize, future, furrr, optimx, ggplot2, ggthemes,
  glmulti, lubridate, optimx, purrr, raster, reproducible, rgenoud, stringr,
  survival, surveybootstrap, tibble, tictoc, tidyr, whitebox)
library(gisr)

whitebox::wbt_init() # required for WhiteboxTools to work
wbt_version() # check WhiteboxTools version
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

saveRDS(preds_tbl, file.path(mod_fit_dir, "best_fits/preds_tbl.rds"))

################ CREATE COVARIATE RASTERS FOR MAINE ############################

preds_tbl <- readRDS(file.path(mod_fit_dir, "best_fits/preds_tbl.rds"))

# Source data directories
input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
elev_file <- file.path(input_dir, "elev_30mc.tif")
kernel_dir <- file.path(input_dir, "SSF_Rasters/Processed_Sections/Kernel")
covar_full_dir <- file.path(input_dir, "SSF_Rasters/Covars_Full")

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
  raster_file <- file.path(input_dir, paste0(covar, "_30mc.tif"))
  print(paste0("Starting: ", covar, sigma, " (", i, " of ", nrow(preds_kernel),
    ") at: ", lubridate::now()))

  top_file <- file.path(kernel_dir, paste0(covar, sigma, "_top.grd"))
  mid_file <- file.path(kernel_dir, paste0(covar, sigma, "_mid.grd"))
  bot_file <- file.path(kernel_dir, paste0(covar, sigma, "_bot.grd"))
  out_file <- file.path(covars_full, paste0(covar, sigma, ".tif"))

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
  preds_terrain_i <- preds_terrain %>% slice(i)
  sigma <- preds_terrain_i %>% pull(sigma)
  covar <- preds_terrain_i %>% pull(covar)
  print(paste0("Starting: ", covar, sigma, " (", i, " of ", nrow(preds_terrain),
    ") at: ", lubridate::now()))

  top_file <- file.path(terrain_dir, paste0(covar, sigma, "_top.grd"))
  mid_file <- file.path(terrain_dir, paste0(covar, sigma, "_mid.grd"))
  bot_file <- file.path(terrain_dir, paste0(covar, sigma, "_bot.grd"))
  out_file <- file.path(covars_full_dir, paste0(covar, sigma, ".tif"))

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

input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
covars_full_dir = file.path(input_dir, "SSF_Rasters/Covars_Full")

developed_dist_file <- file.path(input_dir, "developed_dist_30mc.tif")
hydro_dist_file <- file.path(input_dir, "hydro_dist_30mc.tif")
turbine_dist_file <- file.path(input_dir, "turbine_dist_30mc.tif")

# Change turbine dist raster cells with NA to 60000 (max dist calculated)
turbine_dist_org <- raster(turbine_dist_file)
turbine_dist_max <- cellStats(turbine_dist_org, stat = "max")
turbine_dist <- turbine_dist_org
turbine_dist[is.na(turbine_dist)] <- turbine_dist_max
plot(turbine_dist)

writeRaster(raster(developed_dist_file), file.path(covars_full_dir,
  "developed_dist0.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(raster(hydro_dist_file), file.path(covars_full_dir,
  "hydro_dist0.tif"), format = "GTiff", overwrite = TRUE)
writeRaster(turbine_dist, file.path(covars_full_dir,
  "turbine_dist0.tif"), format = "GTiff", overwrite = TRUE)

################### MASK COVARIATE RASTERS TO MAINE ONLY #######################

# Crop all the Covariate Rasters -----------------------------------------------
preds_tbl_file = "Output/Analysis/SSF/Models/best_fits/preds_tbl.rds"
covars_full_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters/Covars_Full"
covars_crop_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters/Covars_Crop"
maine_raster_trim_file = "C:/ArcGIS/Data/BlankRaster/maine_trim.tif"

preds_tbl <- readRDS("Output/Analysis/SSF/Models/best_fits/preds_tbl.rds")
maine_raster_trim <- raster(maine_raster_trim_file)

for (i in seq_len(nrow(preds_tbl))){
  preds_tbl_i <- preds_tbl %>% slice(i)
  covar_sigma <- preds_tbl_i %>% pull(covar_sigma)
  print(paste0("Starting: ", covar_sigma, " (", i, " of ", nrow(preds_tbl),
    ") at: ", lubridate::now()))

  covar_file <- file.path(covars_full_dir, paste0(covar_sigma, ".tif"))
  out_file <- file.path(covars_crop_dir, paste0(covar_sigma, ".tif"))

  covar_raster <- raster(covar_file)
  covar_raster_crop <- crop(covar_raster, maine_raster_trim)
  covar_raster_mask <- mask(covar_raster_crop, maine_raster_trim)
  writeRaster(covar_raster_mask, out_file, format = "GTiff", overwrite = TRUE)
}

################### GENERATE SSF_LAYERS FOR MAINE ##############################

# Directory of fits
mod_fit_dir = "Output/Analysis/SSF/Models"
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"
covars_crop_dir = file.path(ssf_raster_dir, "Covars_Crop")
step_type_dir = file.path(ssf_raster_dir, "Step_Type")
maine_raster_trim_file = "C:/ArcGIS/Data/BlankRaster/maine_trim.tif"

best_ssf_fits <- readRDS(file.path(mod_fit_dir, "best_fits",
  "best_ssf_fit_all.rds"))
maine_raster_trim <- raster(maine_raster_trim_file)

# Generate layer for each ssf based on original fits
for (i in 1:nrow(best_ssf_fits)){
  print(paste0("i:", i))
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
    print(paste0("covariates: ", terms_i_j))
    raster_file <- file.path(covars_crop_dir, paste0(terms_i_j, ".tif"))
    covars_list[[j]] <- raster(raster_file)
  }
  covars_brick <- raster::brick(covars_list)
  #plot(covars_brick)
  rm(clogit_fit_i, covars_list)

  # Generate formulas
  ssf_formula <- paste0("(", paste(paste0("covars_brick[['", names(coefs_i),
    "']]"), paste0("coefs_i['", names(coefs_i),"']"), sep = "*",
    collapse = ") + ("), ")")

  # Create raster, then crop and mask
  ssf_raster <- eval(parse(text = ssf_formula))
  plot(ssf_raster, main = step_type_i)

  # Write Raster to output dir
  step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
    "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
  writeRaster(ssf_raster, file.path(ssf_raster_dir, "Step_Types",
    step_type_i_numeric), format = "GTiff", overwrite = TRUE)
  rm(coefs_i, covars_brick, raster_file, ssf_formula, ssf_raster,
    step_type_i_numeric, terms_i, terms_i_j)
  gc()
}


# Generate UPDATE_01 layers based on PERCH using only hydro_dist and open_water
for (i in 1:nrow(best_ssf_fits)){
  print(paste0("i:", i))
  clogit_fit_i <- best_ssf_fits %>% slice(i) %>% pull(clogit_fit) %>% pluck(1)
  step_type_i <- best_ssf_fits %>% slice(i) %>% pull(step_type)
  step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
    "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
  start_behavior <- str_split(step_type_i_numeric, "_") %>% pluck(1, 1) %>%
    as.numeric(.)
  end_behavior <- str_split(step_type_i_numeric, "_") %>% pluck(1, 2) %>%
    as.numeric(.)

  if(end_behavior != 4){

    # Extract terms(not including 'strata(step_id)')
    terms_i <- clogit_fit_i %>% pluck(terms, attr_getter("term.labels")) %>%
      .[!. %in% c("strata(step_id)")]
    # Extract coefficients
    coefs_i <- clogit_fit_i %>% pluck(coef)

  } else {

    # Extract terms associated with water only
    terms_logical_i <- clogit_fit_i %>% pluck(terms,
      attr_getter("term.labels")) %>%str_detect(., "hydro_dist0|open_water")
    terms_i <- clogit_fit_i %>% pluck(terms, attr_getter("term.labels")) %>%
      .[terms_logical_i]
    # Extract coefficients
    coefs_i <- clogit_fit_i %>% pluck(coef) %>%
      .[terms_logical_i]

  }

  # Create Raster_Brick
  covars_list <- vector(mode = "list", length = length(terms_i))
  for (j in seq_along(terms_i)){
    terms_i_j <- terms_i[j]
    print(paste0("covariates: ", terms_i_j))
    raster_file <- file.path(covars_crop_dir, paste0(terms_i_j, ".tif"))
    covars_list[[j]] <- raster(raster_file)
  }
  covars_brick <- raster::brick(covars_list)
  #plot(covars_brick)
  rm(clogit_fit_i, covars_list)

  # Generate formulas
  ssf_formula <- paste0("(", paste(paste0("covars_brick[['", names(coefs_i),
    "']]"), paste0("coefs_i['", names(coefs_i),"']"), sep = "*",
    collapse = ") + ("), ")")

  # Create raster, then crop and mask
  ssf_raster <- eval(parse(text = ssf_formula))
  plot(ssf_raster, main = step_type_i)

  # Write Raster to output dir

  writeRaster(ssf_raster, file.path(ssf_raster_dir, "Step_Types_Update_01",
    step_type_i_numeric), format = "GTiff", overwrite = TRUE)
  rm(coefs_i, covars_brick, raster_file, ssf_formula, ssf_raster,
    step_type_i_numeric, terms_i, terms_i_j)
  gc()
}


### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###

# # Generate probability surface for each ssf_layer
#
# ssf_layers <- list.files(file.path(ssf_raster_dir, "Step_Types"),
#   pattern = ".tif$")
# for (i in seq_along(ssf_layers)){
#   ssf_layer_i <- ssf_layers[i]
#   print(ssf_layer_i)
#   ssf_raster <- raster(file.path(ssf_raster_dir, "Step_Types", ssf_layer_i))
#   ssf_raster_inv_logit <- calc(ssf_raster, fun = boot::inv.logit)
#   writeRaster(ssf_raster_inv_logit, file.path(ssf_raster_dir, "Step_Types_Prob",
#     ssf_layer_i), format = "GTiff", overwrite = TRUE)
#   rm(ssf_layer_i, ssf_raster, ssf_raster_inv_logit)
# }
#
# # Create histogram of the probability surfaces
# ssf_prob_layers <- list.files(file.path(ssf_raster_dir, "Step_Types_Prob"),
#   pattern = ".tif$")
# for (i in seq_along(ssf_prob_layers)){
#   ssf_prob_layer_i <- ssf_prob_layers[i]
#   print(ssf_prob_layer_i)
#   ssf_raster <- raster(file.path(ssf_raster_dir, "Step_Types_Prob",
#     ssf_prob_layer_i))
#
#   f <- hist(ssf_raster, breaks=30)
#   dat <- data.frame(counts= f$counts,breaks = f$mids)
#   ggplot(dat, aes(x = breaks, y = counts)) +
#   geom_bar(stat = "identity", fill='blue',alpha = 0.8)+
#   xlab("Logit")+ ylab("Value")+
#   scale_x_continuous(breaks = seq(-1,1,0.25),
#     labels = seq(-1,1,0.25))
#
#   writeRaster(ssf_raster_inv_logit, file.path(ssf_raster_dir, "Step_Types_Prob",
#     ssf_layer_i), format = "GTiff", overwrite = TRUE)
#   rm(ssf_layer_i, ssf_raster, ssf_raster_inv_logit)
# }
#
# # Rescale surfaces to new range (-10 - 10)
# ssf_layers <- list.files(file.path(ssf_raster_dir, "Step_Types"),
#   pattern = ".tif$", full.names = TRUE)
# for (i in seq_along(ssf_layers)){
#   ssf_layer_i <- basename(ssf_layers[i])
#   print(ssf_layer_i)
#   ssf_layer_i_file <- ssf_layers[i]
#   ssf_layer_rescale_i_file <- file.path(ssf_raster_dir, "Step_Types_Rescale",
#     ssf_layer_i)
#   wbt_rescale_value_range(ssf_layer_i_file, ssf_layer_rescale_i_file,
#     -10, 10, clip_min = NULL, clip_max = NULL, wd = NULL, verbose_mode = TRUE
#   )
#   ssf_raster <- raster(file.path(ssf_raster_dir, "Step_Types", ssf_layer_i))
#   ssf_raster_inv_logit <- calc(ssf_raster, fun = boot::inv.logit)
#   writeRaster(ssf_raster_inv_logit, file.path(ssf_raster_dir, "Step_Types_Prob",
#     ssf_layer_i), format = "GTiff", overwrite = TRUE)
#   rm(ssf_layer_i, ssf_raster, ssf_raster_inv_logit)
# }
#
# # Generate probability surface for each rescaled ssf_layer
# ssf_layers <- list.files(file.path(ssf_raster_dir, "Step_Types_Rescale"),
#   pattern = ".tif$")
# for (i in seq_along(ssf_layers)){
#   ssf_layer_i <- ssf_layers[i]
#   print(ssf_layer_i)
#   ssf_raster <- raster(file.path(ssf_raster_dir, "Step_Types_Rescale",
#     ssf_layer_i))
#   ssf_raster_inv_logit <- calc(ssf_raster, fun = boot::inv.logit)
#   ssf_raster_mask <- mask(ssf_raster_inv_logit, mask, updatevalue=NA)
#   writeRaster(ssf_raster_mask, file.path(ssf_raster_dir,
#     "Step_Types_Rescale_Prob", ssf_layer_i), format = "GTiff", overwrite = TRUE)
#   rm(ssf_layer_i, ssf_raster, ssf_raster_inv_logit)
# }

#   ssf_raster_inv_log <- calc(ssf_raster, fun = boot::inv.logit)
#   # plot histogram
#   hist(ssf_raster_inv_log, col = "springgreen", breaks = seq(0,1, by = .05))
#   hist(ssf_raster, col = "springgreen")
#
#   boot::inv.logit(10)
#
#   ssf_raster_list[[i]] <- ssf_raster_inv_log
#   step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
#     "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
#   # Names are "step_#_#" because raster stack names can't start with a number
#   names(ssf_raster_list[[i]]) <- paste0("step_", step_type_i_numeric)
#
#
# ssf_stack <- raster::stack(ssf_raster_list)
# names(ssf_stack)
# ssf_brick <- writeRaster(ssf_stack, file.path(file_dir,
#   "ssf_stack_raw"), format = "raster", overwrite = TRUE)
#
# for (i in seq_len(nlayers(ssf_brick))){
#   plot(ssf_brick[[i]], main = names(ssf_brick[[i]]))
#   invisible(readline(prompt = "Press [enter] to continue; [esc] to exit"))
# }
#
# # Empty list
# ssf_raster_list <- vector(mode = "list", length = nrow(best_ssf_fits))
#
# inverse_logit <- TRUE
# # Generate layer for each ssf

#
# ssf_stack <- raster::stack(ssf_raster_list)
# names(ssf_stack)
# ssf_brick <- writeRaster(ssf_stack, file.path(file_dir,
#   "ssf_stack_inverse_logit"), format = "raster", overwrite = TRUE)
#
# for (i in seq_len(nlayers(ssf_brick))){
#   plot(ssf_brick[[i]], main = names(ssf_brick[[i]]))
#   invisible(readline(prompt = "Press [enter] to continue; [esc] to exit"))
# }
#
#
#
#
#
#
# # Source data directories
# base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
# file_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
#
# # Import Covariate Rasters ---
#
# # Base
# base <- raster(base_file)
#
# # Elevation
# elev_file <- file.path(file_dir, "elev_30mc.tif")
# elev_org <- raster(elev_file) # all other layers' extent are set to this layer
#
#
#
#
# # Terrain class
# elev_file <- file.path(file_dir, "elev_30mc.tif")
#
# # Extract class
# developed_dist_file <- file.path(file_dir, "developed_dist_30mc.tif")
# hydro_dist_file <- file.path(file_dir, "hydro_dist_30mc.tif")
# turbine_dist_file <- file.path(file_dir, "turbine_dist_30mc.tif")
#
# # Kernel class
# developed_file <- file.path(file_dir, "developed_30mc.tif")
# forest_file <- file.path(file_dir, "forest_30mc.tif")
# open_water_file <- file.path(file_dir, "open_water_30mc.tif")
# pasture_file <- file.path(file_dir, "pasture_30mc.tif")
# shrub_herb_file <- file.path(file_dir, "shrub_herb_30mc.tif")
# wetland_file <- file.path(file_dir, "wetland_30mc.tif")
# eastness_file <- file.path(file_dir, "eastness_30mc.tif")
# northness_file <- file.path(file_dir, "northness_30mc.tif")
# wind_class_file <- file.path(file_dir, "wind_class_30mc.tif")
#
#
#
# # Extract class
# developed_dist_org <- crop(raster(developed_dist_file), elev_org)
# hydro_dist_org <- crop(raster(hydro_dist_file), elev_org)
# turbine_dist_org <- crop(raster(turbine_dist_file), elev_org)
#
# # Kernel class
# developed_org <- crop(raster(developed_file), elev_org)
# forest_org <- crop(raster(forest_file), elev_org)
# open_water_org <- crop(raster(open_water_file), elev_org)
# pasture_org <- crop(raster(pasture_file), elev_org)
# shrub_herb_org <- crop(raster(shrub_herb_file), elev_org)
# wetland_org <- crop(raster(wetland_file), elev_org)
# eastness_org <- crop(raster(eastness_file), elev_org)
# northness_org <- crop(raster(northness_file), elev_org)
# wind_class_org <- crop(raster(wind_class_file), elev_org)
#
# # plot(developed$as.RasterLayer(band = 1))
# rm(base_file,
#   developed_dist_file, hydro_dist_file, turbine_dist_file,
#   developed_file, forest_file, open_water_file, pasture_file,
#   shrub_herb_file, wetland_file, eastness_file, northness_file, wind_class_file,
#   elev_file)
#
# ext <- extent(elev_org) # all of Maine
# #ext <- extent(c(510000, 560000, 5025000, 5065000)) # middle of ME
# #ext <- extent(c(322640, 412670, 4901590, 4991620)) # Around Ellis
#
# developed <- crop(developed_org, ext, snap = "near")
# forest <- crop(forest_org, ext, snap = "near")
# open_water <- crop(open_water_org, ext, snap = "near")
# pasture <- crop(pasture_org, ext, snap = "near")
# shrub_herb <- crop(shrub_herb_org, ext, snap = "near")
# wetland <- crop(wetland_org, ext, snap = "near")
# eastness <- crop(eastness_org, ext, snap = "near")
# northness <- crop(northness_org, ext, snap = "near")
# wind_class <- crop(wind_class_org, ext, snap = "near")
# developed_dist <- crop(developed_dist_org, ext, snap = "near")
# hydro_dist <- crop(hydro_dist_org, ext, snap = "near")
# turbine_dist <- crop(turbine_dist_org, ext, snap = "near")
# elev <- crop(elev_org, ext, snap = "near")
# rm(elev_org, developed_org, forest_org, open_water_org, pasture_org,
#   shrub_herb_org, wetland_org, developed_dist_org, hydro_dist_org,
#   turbine_dist_org, eastness_org, northness_org, wind_class_org)
# removeTmpFiles(h = 0)
#
#
#
#
#
#
# # Had to split out types b/c I couldn't get case_when() to work inside mutate()
#
# preds_terrain <- preds_tbl %>%
#   filter(raster_class == "terrain_class")
# for (i in 1:nrow(preds_terrain)){
#   preds_terrain_i <- preds_terrain %>%
#     slice(i)
#   raster_terrain_i <- preds_terrain_i %>%
#     mutate(raster_layer = map2(.x = sigma, .y = covar,
#       .f = CalculateTerrainMetricWithSigma)) %>%
#     pluck("raster_layer", 1)
#   raster_terrain_i_name <- preds_terrain_i %>% pull(covar_sigma)
#   names(raster_terrain_i) <- raster_terrain_i_name
#   writeRaster(raster_terrain_i, file.path(file_dir, "SSF_Rasters", paste0(
#     raster_terrain_i_name, ".tif")), format = "raster", overwrite = TRUE)
#   rm(raster_terrain_i)
# }
#
# preds_tbl %>% dplyr::select(covar, sigma)
#
#
#
# preds_kernel <- preds_tbl %>%
#   filter(raster_class == "kernel_class")
# for (i in 1:nrow(preds_kernel)){
#   preds_kernel_i <- preds_kernel %>%
#     slice(i)
#   raster_kernel_i <- preds_kernel_i %>%
#     mutate(raster_layer = map2(.x = sigma, .y = covar, .f = SmoothRaster)) %>%
#     pluck("raster_layer", 1)
#
#   covar <- preds_kernel_i %>% pull(covar)
#   sigma <- preds_kernel_i %>% pull(sigma)
#
#     sigma <- 17
#     covar_smooth <- raster::raster(developed) # creates blank raster
#     values(covar_smooth) <- smoothie::gauss2dsmooth(raster::as.matrix(developed),
#       lambda = sigma, nx = DescTools::RoundTo(nrow(developed), 2),
#       ny = DescTools::RoundTo(ncol(developed), 2))
#
#
#   print(paste0("Starting: ", covar, ", sigma = ", sigma))
#   covar <- get(covar)
#   if (sigma >= 1) {
#     covar_smooth <- raster::raster(covar) # creates blank raster
#     values(covar_smooth) <- smoothie::gauss2dsmooth(raster::as.matrix(covar),
#       lambda = sigma, nx = DescTools::RoundTo(nrow(covar), 2),
#       ny = DescTools::RoundTo(ncol(covar), 2))
#   } else {
#     covar_smooth <- covar
#   }
#   names(covar_smooth) <- paste0(names(covar), as.character(sigma))
#   return(covar_smooth)
#
#
#   raster_kernel_i_name <- preds_kernel_i %>% pull(covar_sigma)
#   names(raster_kernel_i) <- raster_kernel_i_name
#   writeRaster(raster_kernel_i, file.path(file_dir, "SSF_Rasters", paste0(
#     raster_kernel_i_name, ".tif")), format = "raster", overwrite = TRUE)
#   rm(raster_kernel_i)
# }
#
# preds_rasters_extract <- preds_tbl %>%
#   filter(raster_class == "extract_class") %>% #slice(1) %>%
#   mutate(raster_layer = map(.x = covar, ~ get(.x)))
#
# preds_rasters <- bind_rows(preds_rasters_terrain, preds_rasters_extract,
#   preds_rasters_kernel) %>%
#   mutate(covar_sigma = paste0(covar, sigma)) %>%
#   dplyr::arrange(raster_class, covar, sigma, covar_sigma) %>%
#   as_tibble(.)
#
# #rm(preds_rasters_terrain, preds_rasters_extract, preds_rasters_kernel)
#
# glimpse(preds_rasters)
#
# # Compile covars into a RasterBrick
# covars_stack <- raster::stack(preds_rasters %>% pull("raster_layer"))
# names(covars_stack) <- preds_rasters %>% pull(covar_sigma)
# covars_brick <- writeRaster(covars_stack, file.path(file_dir,
#   "ssf_covars_stack"), format = "raster", overwrite = TRUE)
#
# for (i in seq_len(nlayers(covars_brick))){
#   plot(covars_brick[[i]], main = names(covars_brick[[i]]))
#   invisible(readline(prompt="Press [enter] to continue; [esc] to exit"))
# }

# # Extract model fit
# i <- 2
# clogit_fit_i <- best_ssf_fits %>% slice(i) %>% pull(clogit_fit) %>% pluck(1)
#
# # Extract terms(not including 'strata(step_id)')
# terms_i <- clogit_fit_i %>% pluck(terms, attr_getter("term.labels")) %>%
#   .[!. %in% c("strata(step_id)")]
#
# # Extract coefficients
# coefs_i <- clogit_fit_i %>% pluck(coef)
#
# rsf <- (covars_brick[['forest100']]*coefs_i["forest100"] +
#     covars_brick[['open_water100']]*coefs_i["open_water100"] +
#     covars_brick[['eastness57']]*coefs_i["eastness57"] +
#     covars_brick[['northness100']]*coefs_i["northness100"] +
#     covars_brick[['wind_class100']]*coefs_i["wind_class100"] +
#     covars_brick[["tri50"]]*coefs_i["tri50"] +
#     covars_brick[["roughness14"]]*coefs_i["roughness14"] +
#     covars_brick[["turbine_dist0"]]*coefs_i["turbine_dist0"])
# plot(rsf)
# mapview::mapview(rsf)
#
# rsf_rescale <- rescale0to1(rsf)
# plot(rsf_rescale)
# mapview::mapview(rsf_rescale)
#
# # Extract model fit
# i <- 3
# clogit_fit_i <- best_ssf_fits %>% slice(i) %>% pull(clogit_fit) %>% pluck(1)
#
# # Extract terms(not including 'strata(step_id)')
# terms_i <- clogit_fit_i %>% pluck(terms, attr_getter("term.labels")) %>%
#   .[!. %in% c("strata(step_id)")]
#
# # Extract coefficients
# coefs_i <- clogit_fit_i %>% pluck(coef)
#
# # CRITICAL STEP - KEEP
# ssf_formula <- paste0("exp((", paste(names(coefs_i), coefs_i, sep = "*",
#   collapse = ") + ("), "))")
#
# rsf <- (covars_brick[['developed61']]*coefs_i["developed61"] +
#     covars_brick[['forest76']]*coefs_i["forest76"] +
#     covars_brick[['open_water37']]*coefs_i["open_water37"] +
#     covars_brick[['eastness94']]*coefs_i["eastness94"] +
#     covars_brick[['northness0']]*coefs_i["northness0"] +
#     covars_brick[["wind_class18"]]*coefs_i["wind_class18"] +
#     covars_brick[["tpi50"]]*coefs_i["tpi50"] +
#     covars_brick[["roughness8"]]*coefs_i["roughness8"] +
#     covars_brick[["developed_dist0"]]*coefs_i["developed_dist0"] +
#     covars_brick[["hydro_dist0"]]*coefs_i["hydro_dist0"])
# plot(rsf)
#
# library(climateStability)
# rsf_rescale <- rescale0to1(rsf)
# plot(rsf_rescale)
#
# # FOR ROOST_FLIGHT
# open_water8 <- preds_rasters %>% slice(1) %>% pull("raster_layer") %>%
#   pluck(1)
# roost_flight_overlay <- overlay(open_water8,
#   fun=function(x){return(
#     exp(x*382.617685605717))
#     }
#   )
# plot(roost_flight_overlay)
# cellStats(roost_flight_overlay, stat = "mean")
# cellStats(roost_flight_overlay, stat = "min")
# cellStats(roost_flight_overlay, stat = "max")
#
# roost_flight_overlay_rescale <- rescale0to1(roost_flight_overlay)
# plot(roost_flight_overlay_rescale)
# cellStats(roost_flight_overlay_rescale, stat = "mean")
# cellStats(roost_flight_overlay_rescale, stat = "min")
# cellStats(roost_flight_overlay_rescale, stat = "max")
#
# # FOR FLIGHT_ROOST
# northness3 <- preds_rasters %>% slice(1) %>% pull("raster_layer") %>% pluck(1)
# wind_class100 <- preds_rasters %>% slice(2) %>% pull("raster_layer") %>%
#   pluck(1)
# hydro_dist0 <- preds_rasters %>% slice(3) %>% pull("raster_layer") %>%
#   pluck(1)
# flight_roost_overlay <- overlay(northness3, wind_class100, hydro_dist0,
#   fun=function(x,y,z){return(
#     exp((x*9.32) + (y*-707.95) + (z*-0.013)))
#     }
#   )
# plot(flight_roost_overlay)
# cellStats(flight_roost_overlay, stat = "mean")
# cellStats(flight_roost_overlay, stat = "min")
# cellStats(flight_roost_overlay, stat = "max")
#
# library(climateStability)
# flight_roost_overlay_rescale <- rescale0to1(flight_roost_overlay)
# plot(flight_roost_overlay_rescale)
# cellStats(flight_roost_overlay_rescale, stat = "mean")
# cellStats(flight_roost_overlay_rescale, stat = "min")
# cellStats(flight_roost_overlay_rescale, stat = "max")
#
# r <- raster(ncol=10, nrow=10)
# r1 <- init(r, fun=runif)
# r2 <- init(r, fun=runif)
# r3 <- overlay(r1, r2, fun=function(x,y){return(x+y)})
# plot(r1)
# plot(r2)
# plot(r3)
#
# load_covars <- TRUE
# if (isTRUE(load_covars)){
#   covar1 <- raster("Data/covar1.tif")
#   covar2 <- raster("Data/covar2.tif")
#   covar3 <- raster("Data/covar3.tif")
#   names(covar1) <- "elev"
#   names(covar2) <- "develop"
#   names(covar3) <- "gauss"
# }
#
# ############################ ModelFit_SSF ################################ ###
# # Load libraries, scripts, and input parameters -------------------------- ---
# pacman::p_load(AICcmodavg, plyr, dplyr, ggplot2, ggthemes, optimx, raster,
#   reproducible, rgdal, smoothie, stringr, survival, tictoc) #spatialfil
# pacman::p_load(baear, gisr, ibmr)
# options(stringsAsFactors=FALSE)
# #setwd("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm")
#
# # Output paths
# ua_data_dir <- "Output/Analysis/SSF/UA_Data"
# ua_data_files <- list.files(ua_data_dir, full.names = TRUE)
#
# for (i in seq_along(ua_data_files)){
#   ua_data_i <- readRDS(ua_data_files[i])
#   unique(ua_data_i$behavior_behavior)
#   identical(head(ua_data_i$tpi0), head(ua_data_i$tri0),
#     head(ua_data_i$roughness0))
#   identical(ua_data_i$tpi0, ua_data_i$tpi30)
#   identical(ua_data_i$tri0, ua_data_i$tri30)
#   identical(ua_data_i$roughness0, ua_data_i$roughness30)
#   ua_data_i_out <- ua_data_i %>%
#     mutate(tpi0 = tpi30,
#            tri0 = tri30,
#            roughness0 = roughness30)
#   updated <- all(identical(ua_data_i_out$tpi0, ua_data_i_out$tpi30),
#       identical(ua_data_i_out$tri0, ua_data_i_out$tri30),
#       identical(ua_data_i_out$roughness0, ua_data_i_out$roughness30))
#   if(updated) saveRDS(ua_data_i_out, ua_data_files[i])
# }
#
# saveRDS(ua_steps_i, file.path(ua_data_dir, paste0("ua_steps_",
#   step_type_i_name, ".rds")))
#
# # # Rename files
# # fit_files <- list.files(file.path(mod_fit_dir, "cruise_perch"),
# #   full.names = TRUE)
# # for (i in seq_along(fit_files)){
# #   fit_file_i <- fit_files[i]
# #   fit_file_i_new <- str_replace(fit_file_i, "models", "ssf")
# #   file.rename(fit_file_i, fit_file_i_new)
# # }

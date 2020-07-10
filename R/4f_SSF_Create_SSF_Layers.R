## THIS IS A TEST FOR CONVERTING THE SSF MODELS TO SSF SURFACES

# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, plyr, dplyr, future, furrr, optimx, ggplot2,
  ggthemes, glmulti, lubridate, optimx, purrr, raster, reproducible, rgenoud,
  stringr, survival, surveybootstrap, tibble, tictoc, tidyr)
library(gisr)
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE, chunksize=1e9,
  memfrac=.9)
############################ IMPORT RASTERS ####################################

# Source data directories
base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
file_dir <- "C:/ArcGIS/Data/R_Input/BAEA"

# Terrain class
elev_file <- file.path(file_dir, "elev_30mc.tif")

# Extract class
developed_dist_file <- file.path(file_dir, "developed_dist_30mc.tif")
hydro_dist_file <- file.path(file_dir, "hydro_dist_30mc.tif")
turbine_dist_file <- file.path(file_dir, "turbine_dist_30mc.tif")

# Kernel class
developed_file <- file.path(file_dir, "developed_30mc.tif")
forest_file <- file.path(file_dir, "forest_30mc.tif")
open_water_file <- file.path(file_dir, "open_water_30mc.tif")
pasture_file <- file.path(file_dir, "pasture_30mc.tif")
shrub_herb_file <- file.path(file_dir, "shrub_herb_30mc.tif")
wetland_file <- file.path(file_dir, "wetland_30mc.tif")
eastness_file <- file.path(file_dir, "eastness_30mc.tif")
northness_file <- file.path(file_dir, "northness_30mc.tif")
wind_class_file <- file.path(file_dir, "wind_class_30mc.tif")

## Import Covariate Rasters ----------------------------------------------------

# Base
base <- raster(base_file)

# Terrain class
elev_org <- raster(elev_file) # all other layers' extent are set to this layer

# Extract class
developed_dist_org <- crop(raster(developed_dist_file), elev_org)
hydro_dist_org <- crop(raster(hydro_dist_file), elev_org)
turbine_dist_org <- crop(raster(turbine_dist_file), elev_org)

# Kernel class
developed_org <- crop(raster(developed_file), elev_org)
forest_org <- crop(raster(forest_file), elev_org)
open_water_org <- crop(raster(open_water_file), elev_org)
pasture_org <- crop(raster(pasture_file), elev_org)
shrub_herb_org <- crop(raster(shrub_herb_file), elev_org)
wetland_org <- crop(raster(wetland_file), elev_org)
eastness_org <- crop(raster(eastness_file), elev_org)
northness_org <- crop(raster(northness_file), elev_org)
wind_class_org <- crop(raster(wind_class_file), elev_org)

# plot(developed$as.RasterLayer(band = 1))
rm(base_file,
  developed_dist_file, hydro_dist_file, turbine_dist_file,
  developed_file, forest_file, open_water_file, pasture_file,
  shrub_herb_file, wetland_file, eastness_file, northness_file, wind_class_file,
  elev_file)

ext <- extent(elev_org) # all of Maine
#ext <- extent(c(510000, 560000, 5025000, 5065000)) # middle of ME
#ext <- extent(c(322640, 412670, 4901590, 4991620)) # Around Ellis

developed <- crop(developed_org, ext, snap = "near")
forest <- crop(forest_org, ext, snap = "near")
open_water <- crop(open_water_org, ext, snap = "near")
pasture <- crop(pasture_org, ext, snap = "near")
shrub_herb <- crop(shrub_herb_org, ext, snap = "near")
wetland <- crop(wetland_org, ext, snap = "near")
eastness <- crop(eastness_org, ext, snap = "near")
northness <- crop(northness_org, ext, snap = "near")
wind_class <- crop(wind_class_org, ext, snap = "near")
developed_dist <- crop(developed_dist_org, ext, snap = "near")
hydro_dist <- crop(hydro_dist_org, ext, snap = "near")
turbine_dist <- crop(turbine_dist_org, ext, snap = "near")
elev <- crop(elev_org, ext, snap = "near")
rm(elev_org, developed_org, forest_org, open_water_org, pasture_org,
  shrub_herb_org, wetland_org, developed_dist_org, hydro_dist_org,
  turbine_dist_org, eastness_org, northness_org, wind_class_org)
removeTmpFiles(h=0)

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

## Get SSF FITS ----------------------------------------------------------------

# Directory of fits
mod_fit_dir = "Output/Analysis/SSF/Models"

best_ssf_fits_org <- readRDS(file.path(mod_fit_dir, "best_fits",
  "best_ssf_fit_all.rds"))

best_ssf_fits_org %>% dplyr::select(step_type, preds)

best_ssf_fits <- best_ssf_fits_org # all
#best_ssf_fits <- best_ssf_fits_org %>% slice(c(1,4,8,12)) # all '-> Cruise'
#best_ssf_fits <- best_ssf_fits_org %>% slice(c(2,5,9,13,16)) # all '-> Flight'
#best_ssf_fits <- best_ssf_fits_org %>% slice(c(3,6,10,14,17)) # all '-> Perch'
#best_ssf_fits <- best_ssf_fits_org %>% slice(c(7,11,15)) # all '-> Roost'

# Determine all the raster_sigma layers
preds_all <- paste0(best_ssf_fits$preds, collapse = " + ")
preds_unique <- unique(str_split(preds_all, " \\+ ") %>% pluck(1))
preds_rasters <- unique(str_replace_all(preds_unique, "[0-9]", ""))

preds_tbl <- tibble(preds_unique) %>%
  mutate(sigma = as.integer(str_extract(preds_unique, "[0-9]{1,3}"))) %>%
  mutate(covar = str_replace_all(preds_unique, "[0-9]", "")) %>%
  dplyr::select(covar, sigma) %>%
  mutate(covar_sigma = paste0(covar, sigma)) %>%
  mutate(raster_class = recode(covar, !!!raster_classes)) %>%
  mutate(raster_layer = vector(mode = "list", length = nrow(.))) %>%
  arrange(covar, sigma)

CalculateTerrainMetricWithSigma <- function(sigma, metric){
  print(paste0("Starting: ", metric, sigma))
  if(sigma == 0) sigma <- 1
  size <- (sigma*2) + 1
  x <- elev
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

# Had to split out types b/c I couldn't get case_when() to work inside mutate()

preds_terrain <- preds_tbl %>%
  filter(raster_class == "terrain_class") %>%
  slice(c(7,10,11))
for (i in 1:nrow(preds_terrain)){
  preds_terrain_i <- preds_terrain %>%
    slice(i)
  raster_terrain_i <- preds_terrain_i %>%
    mutate(raster_layer = map2(.x = sigma, .y = covar,
      .f = CalculateTerrainMetricWithSigma)) %>%
    pluck("raster_layer", 1)
  raster_terrain_i_name <- preds_terrain_i %>% pull(covar_sigma)
  names(raster_terrain_i) <- raster_terrain_i_name
  writeRaster(raster_terrain_i, file.path(file_dir, "SSF_Rasters", paste0(
    raster_terrain_i_name, ".tif")), format = "raster", overwrite = TRUE)
  rm(raster_terrain_i)
}

preds_kernel <- preds_tbl %>%
  filter(raster_class == "kernel_class")
for (i in 1:nrow(preds_kernel)){
  preds_kernel_i <- preds_kernel %>%
    slice(i)
  raster_kernel_i <- preds_kernel_i %>%
    mutate(raster_layer = map2(.x = sigma, .y = covar, .f = SmoothRaster)) %>%
    pluck("raster_layer", 1)
  raster_kernel_i_name <- preds_kernel_i %>% pull(covar_sigma)
  names(raster_kernel_i) <- raster_kernel_i_name
  writeRaster(raster_kernel_i, file.path(file_dir, "SSF_Rasters", paste0(
    raster_kernel_i_name, ".tif")), format = "raster", overwrite = TRUE)
  rm(raster_kernel_i)
}

preds_rasters_extract <- preds_tbl %>%
  filter(raster_class == "extract_class") %>% #slice(1) %>%
  mutate(raster_layer = map(.x = covar, ~ get(.x)))

preds_rasters <- bind_rows(preds_rasters_terrain, preds_rasters_extract,
  preds_rasters_kernel) %>%
  mutate(covar_sigma = paste0(covar, sigma)) %>%
  dplyr::arrange(raster_class, covar, sigma, covar_sigma) %>%
  as_tibble(.)

#rm(preds_rasters_terrain, preds_rasters_extract, preds_rasters_kernel)

glimpse(preds_rasters)

# Compile covars into a RasterBrick
covars_stack <- raster::stack(preds_rasters %>% pull("raster_layer"))
names(covars_stack) <- preds_rasters %>% pull(covar_sigma)
covars_brick <- writeRaster(covars_stack, file.path(file_dir,
  "ssf_covars_stack"), format = "raster", overwrite = TRUE)

for (i in seq_len(nlayers(covars_brick))){
  plot(covars_brick[[i]], main = names(covars_brick[[i]]))
  invisible(readline(prompt="Press [enter] to continue; [esc] to exit"))
}

# Generate ssf_layers ----------------------------------------------------------

# Empty list
ssf_raster_list <- vector(mode = "list", length = nrow(best_ssf_fits))

# Generate layer for each ssf
for (i in seq_len(nrow(best_ssf_fits))){
  print(i)
  clogit_fit_i <- best_ssf_fits %>% slice(i) %>% pull(clogit_fit) %>% pluck(1)
  step_type_i <- best_ssf_fits %>% slice(i) %>% pull(step_type)
  # Extract terms(not including 'strata(step_id)')
  terms_i <- clogit_fit_i %>% pluck(terms, attr_getter("term.labels")) %>%
    .[!. %in% c("strata(step_id)")]
  # Extract coefficients
  coefs_i <- clogit_fit_i %>% pluck(coef)
  # Generate formulas
  ssf_formula <- paste0("(", paste(paste0("covars_brick[['", names(coefs_i),
    "']]"), paste0("coefs_i['", names(coefs_i),"']"), sep = "*",
    collapse = ") + ("), ")")
  ssf_raster <- eval(parse(text = ssf_formula))
  ssf_raster_inv_log <- calc(ssf_raster, fun = boot::inv.logit)
  # plot histogram
  hist(ssf_raster_inv_log, col = "springgreen", breaks = seq(0,1, by = .05))

  ssf_raster_list[[i]] <- ssf_raster_inv_log
  step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
    "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
  # Names are "step_#_#" because raster stack names can't start with a number
  names(ssf_raster_list[[i]]) <- paste0("step_", step_type_i_numeric)
}

ssf_stack <- raster::stack(ssf_raster_list)
names(ssf_stack)
ssf_brick <- writeRaster(ssf_stack, file.path(file_dir,
  "ssf_stack_raw"), format = "raster", overwrite = TRUE)

for (i in seq_len(nlayers(ssf_brick))){
  plot(ssf_brick[[i]], main = names(ssf_brick[[i]]))
  invisible(readline(prompt = "Press [enter] to continue; [esc] to exit"))
}

# Empty list
ssf_raster_list <- vector(mode = "list", length = nrow(best_ssf_fits))

inverse_logit <- TRUE
# Generate layer for each ssf
for (i in seq_len(nrow(best_ssf_fits))){
  print(i)
  clogit_fit_i <- best_ssf_fits %>% slice(i) %>% pull(clogit_fit) %>% pluck(1)
  step_type_i <- best_ssf_fits %>% slice(i) %>% pull(step_type)
  # Extract terms(not including 'strata(step_id)')
  terms_i <- clogit_fit_i %>% pluck(terms, attr_getter("term.labels")) %>%
    .[!. %in% c("strata(step_id)")]
  # Extract coefficients
  coefs_i <- clogit_fit_i %>% pluck(coef)
  # Generate formulas
  ssf_formula <- paste0("(", paste(paste0("covars_brick[['", names(coefs_i),
    "']]"), paste0("coefs_i['", names(coefs_i),"']"), sep = "*",
    collapse = ") + ("), ")")
  ssf_raster <- eval(parse(text=ssf_formula))
  if (isTRUE(inverse_logit)){
    ssf_raster_final <- calc(ssf_raster, fun = boot::inv.logit)
    } else {
    ssf_raster_final <- ssf_raster
  }
  ssf_raster_list[[i]] <- ssf_raster_final
  step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
    "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
  # Names are "step_#_#" because raster stack names can't start with a number
  names(ssf_raster_list[[i]]) <- paste0("step_", step_type_i_numeric)
}

ssf_stack <- raster::stack(ssf_raster_list)
names(ssf_stack)
ssf_brick <- writeRaster(ssf_stack, file.path(file_dir,
  "ssf_stack_inverse_logit"), format = "raster", overwrite = TRUE)

for (i in seq_len(nlayers(ssf_brick))){
  plot(ssf_brick[[i]], main = names(ssf_brick[[i]]))
  invisible(readline(prompt = "Press [enter] to continue; [esc] to exit"))
}

boot::inv.logit(10)
gtools::inv.logit(10)

boot::inv.logit(0)
gtools::inv.logit(0)

boot::inv.logit(-10)
gtools::inv.logit(-10)



### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###


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

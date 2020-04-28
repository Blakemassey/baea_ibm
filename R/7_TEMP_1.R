<<<<<<< HEAD
## THIS IS A TEST FOR CONVERTING THE SSF MODELS TO SSF SURFACES

# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, plyr, dplyr, future, furrr, optimx, ggplot2,
  ggthemes, glmulti, lubridate, optimx, purrr, reproducible, rgenoud, stringr,
  survival, surveybootstrap, tibble, tictoc, tidyr)
library(gisr)
options(stringsAsFactors = FALSE)

############################ IMPORT RASTERS ####################################

library(raster)
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
rm(base_file, file_dir,
  developed_dist_file, hydro_dist_file, turbine_dist_file,
  developed_file, forest_file, open_water_file, pasture_file,
  shrub_herb_file, wetland_file, eastness_file, northness_file, wind_class_file,
  elev_file)

############################## DONE IMPORT RASTERS #############################

ext <- extent(c(510000, 560000, 5025000, 5065000))
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

# Output Directory
mod_fit_dir = "Output/Analysis/SSF/Models"
best_ssf_fits <- readRDS(file.path(mod_fit_dir, "best_ssf_fits_all.rds"))

# Determine all the raster_sigma layers
preds_all <- paste0(best_ssf_fits$preds, collapse = " + ")
preds_unique <- unique(str_split(preds_all, " \\+ ") %>% pluck(1))
preds_rasters <- unique(str_replace_all(preds_unique, "[0-9]", ""))

preds_tbl <- tibble(preds_unique) %>%
  mutate(sigma = as.integer(str_extract(preds_unique, "[0-9]{1,3}"))) %>%
  mutate(covar = str_replace_all(preds_unique, "[0-9]", "")) %>%
  dplyr::select(covar, sigma) %>%
  mutate(raster_class = recode(covar, !!!raster_classes)) %>%
  mutate(raster_layer = vector(mode = "list", length = n())) %>%
  arrange(covar, sigma)

CalculateTerrainMetricWithSigma <- function(sigma, metric){
  print(paste0("Starting: ", metric))
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

# Split btwn groups because I couldn't get case_when() to work inside mutate()
preds_rasters_terrain <- preds_tbl %>%
  filter(raster_class == "terrain_class") %>%
  mutate(raster_layer = map2(.x = sigma, .y = covar,
    .f = CalculateTerrainMetricWithSigma))

preds_rasters_extract <- preds_tbl %>%
  filter(raster_class == "extract_class") %>%
  mutate(raster_layer = map(.x = covar, ~ get(.x)))

preds_rasters_kernel <- preds_tbl %>%
  filter(raster_class == "kernel_class") %>%
  mutate(raster_layer = map2(.x = sigma, .y = covar, .f = SmoothRaster))

preds_rasters <- bind_rows(preds_rasters_terrain, preds_rasters_extract,
  preds_rasters_kernel) %>%
  dplyr::arrange(raster_class, covar, sigma) %>%
  as_tibble(.)

#rm(preds_rasters_terrain, preds_rasters_extract, preds_rasters_kernel)

glimpse(preds_rasters)

preds_rasters[1,"covar"]
preds_rasters[2,"covar"]
preds_rasters[3,"covar"]
preds_rasters[4,"covar"]
preds_rasters[5,"covar"]
preds_rasters[6,"covar"]

plot(preds_rasters %>% slice(1) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(2) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(3) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(4) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(5) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(6) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(7) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(8) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(9) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(10) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(11) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(12) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(13) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters %>% slice(14) %>% pull("raster_layer") %>% pluck(1))

plot(preds_rasters_extract %>% slice(1) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters_extract %>% slice(2) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters_extract %>% slice(3) %>% pull("raster_layer") %>% pluck(1))

plot(preds_rasters_kernel %>% slice(1) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters_kernel %>% slice(2) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters_kernel %>% slice(3) %>% pull("raster_layer") %>% pluck(1))
plot(preds_rasters_kernel %>% slice(4) %>% pull("raster_layer") %>% pluck(1))

plot(preds_rasters %>% slice(3) %>% pull("raster_layer") %>% pluck(1))

plot(covar_brick[[1]])
plot(covar_brick[[2]])

# Extract model fit
i <- 1
clogit_fit_i <- best_ssf_fits %>% slice(i) %>% pull(clogit_fit) %>% pluck(1)

# Extract terms(not including 'strata(step_id)')
terms_i <- clogit_fit_i %>% pluck(terms, attr_getter("term.labels")) %>%
  .[!. %in% c("strata(step_id)")]

# Extract coefficients
coefs_i <- clogit_fit_i %>% pluck(coef)

# CRITICAL KEEP
ssf_formula <- paste0("exp((", paste(names(coefs_i), coefs_i, sep = "*",
  collapse = ") + ("), "))")

load_covars <- TRUE
if (isTRUE(load_covars)){
  covar1 <- raster("Data/covar1.tif")
  covar2 <- raster("Data/covar2.tif")
  covar3 <- raster("Data/covar3.tif")
  names(covar1) <- "elev"
  names(covar2) <- "develop"
  names(covar3) <- "gauss"
}
=======

############################# ModelFit_SSF #####################################
# Load libraries, scripts, and input parameters --------------------------------
pacman::p_load(AICcmodavg, plyr, dplyr, ggplot2, ggthemes, optimx, raster,
  reproducible, rgdal, smoothie, stringr, survival, tictoc) #spatialfil
pacman::p_load(baear, gisr, ibmr)
options(stringsAsFactors=FALSE)
#setwd("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm")


# Output paths
ua_data_dir <- "Output/Analysis/SSF/UA_Data"

ua_data_files <- list.files(ua_data_dir, full.names = TRUE)

for (i in seq_along(ua_data_files)){
  ua_data_i <- readRDS(ua_data_files[i])
  unique(ua_data_i$behavior_behavior)
  identical(head(ua_data_i$tpi0), head(ua_data_i$tri0), head(ua_data_i$roughness0))
  identical(ua_data_i$tpi0, ua_data_i$tpi30)
  identical(ua_data_i$tri0, ua_data_i$tri30)
  identical(ua_data_i$roughness0, ua_data_i$roughness30)
  ua_data_i_out <- ua_data_i %>%
    mutate(tpi0 = tpi30,
           tri0 = tri30,
           roughness0 = roughness30)
  updated <- all(identical(ua_data_i_out$tpi0, ua_data_i_out$tpi30),
      identical(ua_data_i_out$tri0, ua_data_i_out$tri30),
      identical(ua_data_i_out$roughness0, ua_data_i_out$roughness30))
  if(updated) saveRDS(ua_data_i_out, ua_data_files[i])
}

  saveRDS(ua_steps_i, file.path(ua_data_dir, paste0("ua_steps_",
    step_type_i_name, ".rds")))



# # Rename files
# fit_files <- list.files(file.path(mod_fit_dir, "cruise_perch"),
#   full.names = TRUE)
# for (i in seq_along(fit_files)){
#   fit_file_i <- fit_files[i]
#   fit_file_i_new <- str_replace(fit_file_i, "models", "ssf")
#   file.rename(fit_file_i, fit_file_i_new)
# }
>>>>>>> 4735db33143c2e471e8ae0ae76b6d00a01c296c8

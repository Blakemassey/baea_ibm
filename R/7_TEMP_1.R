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

## Import Covariate Rasters ----------------------------------------------------

# Base
base <- raster(base_file)

# Terrain class
elev_org <- raster(elev_file) # all other layers' extent are set to this layer

rm(base_file)

x_min <- xmin(elev_org)
x_max <- xmax(elev_org)
y_min <- ymin(elev_org)
y_max <- ymax(elev_org)
(y_half <- (ymax(elev_org) - ymin(elev_org))*(1/2))
(y_third <- (ymax(elev_org) - ymin(elev_org))*(1/3))

#elev_1 <- crop(elev_org, extent(x_min, x_max, y_min, y_max - y_half))
#elev_2 <- crop(elev_org, extent(x_min, x_max, y_min + y_third,
#   y_min + y_third + y_half))
elev_3 <- crop(elev_org, extent(x_min, x_max, y_min + y_half, y_max))

elev <- elev_3

rm(elev_org, elev_3)
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
best_ssf_fits <- best_ssf_fits_org

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
    raster_terrain_i_name, "_3")), format = "raster", overwrite = TRUE)
  rm(raster_terrain_i)
}

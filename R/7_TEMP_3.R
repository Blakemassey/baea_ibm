############################# ModelFit_SSF #####################################
# Load libraries, scripts, and input parameters --------------------------------
pacman::p_load(AICcmodavg, plyr, dplyr, ggplot2, ggthemes, optimx, raster,
  reproducible, rgdal, smoothie, stringr, survival, tictoc) #spatialfil
#setwd("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm")

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
elev <- raster(elev_file) # all other layers' extent are set to this layer

# Extract class
developed_dist <- crop(raster(developed_dist_file), elev)
hydro_dist <- crop(raster(hydro_dist_file), elev)
turbine_dist <- crop(raster(turbine_dist_file), elev)

# Kernel class
developed <- crop(raster(developed_file), elev)
forest <- crop(raster(forest_file), elev)
open_water <- crop(raster(open_water_file), elev)
pasture <- crop(raster(pasture_file), elev)
shrub_herb <- crop(raster(shrub_herb_file), elev)
wetland <- crop(raster(wetland_file), elev)
eastness <- crop(raster(eastness_file), elev)
northness <- crop(raster(northness_file), elev)
wind_class <- crop(raster(wind_class_file), elev)

# plot(developed$as.RasterLayer(band = 1))
rm(base_file, file_dir,
  developed_dist_file, hydro_dist_file, turbine_dist_file,
  developed_file, forest_file, open_water_file, pasture_file,
  shrub_herb_file, wetland_file, eastness_file, northness_file, wind_class_file,
  elev_file)

library(gisr)

# For testing purposes - clip to smaller area
ext <- extent(c(510000, 560000, 5025000, 5065000))
covar_brick <- crop(covar_stack, ext, snap='near')

names(covar_brick)

  sigma1 <- c(0, 10, 20)
  sigma2 <- c(0, 10, 20)
  sigma3 <- c(0, 10, 20)

# Create Parameter Combination Dataframe ---------------------------------------

df_pars <- tibble(sigma1 = as.numeric(sigma1),
    sigma2 = as.numeric(sigma2),
    sigma3 = as.numeric(sigma3)) %>%
  mutate(number = 1:n()) %>%
  dplyr::select(number, sigma1, sigma2, sigma3)

# NEED TO GET ALL OF THE RASTERS OUT AS INDIVIDUAL RASTERS AND NAMED!


covar_brick <- brick(c(
  tibble(sigma = unique(df_pars$sigma1), covar = "developed") %>%
    pmap(., SmoothRaster),
  tibble(sigma = unique(df_pars$sigma2), covar = "eastness") %>%
    pmap(., SmoothRaster),
  tibble(sigma = unique(df_pars$sigma3), covar = "wind_class") %>%
    pmap(., SmoothRaster)))


covar_matrix <- raster::as.matrix(covar_stack_clip)
covar_cols <- setNames(seq_len(ncol(covar_matrix)), colnames(covar_matrix))
covar_names <- c(names(covar1), names(covar2), names(covar3))
rm(covar_brick)

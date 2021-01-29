####### This script converts SSF Models to SSF Probability Surfaces ##########-#

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

################### GENERATE SSF_LAYERS FOR MAINE ##############################

# Directory of fits
mod_fit_dir = "Output/Analysis/SSF/Models"
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"
covars_crop_dir = file.path(ssf_raster_dir, "Covars_Crop")
step_type_dir = file.path(ssf_raster_dir, "Step_Types_Simpler")
maine_raster_trim_file = "C:/ArcGIS/Data/BlankRaster/maine_trim.tif"

best_ssf_fits <- readRDS(file.path(mod_fit_dir, "model_fits_compiled_best",
  "model_fits_compiled_best.rds")) %>% slice(c(3,6,10,14)) #slice(c(2,5,13))

best_ssf_fits %>% pluck("model_full") %>% unlist()

maine_raster_trim <- raster(maine_raster_trim_file)

# Original
# Generate layer for each ssf based on original fits
for (i in 1:nrow(best_ssf_fits)){
  print(paste0("i:", i))
  step_type_i <- best_ssf_fits %>% slice(i) %>% pull(step_type)

  covars_i <- best_ssf_fits %>% slice(i) %>% pluck("covar_fitted", 1) %>%
    pull("covar_clean") %>% str_remove_all("\\^2") %>% unique()

  # Create Raster_Brick
  covars_list <- vector(mode = "list", length = length(covars_i))
  for (j in seq_along(covars_i)){
    covars_i_j <- covars_i[j]
    print(paste0("covariates: ", covars_i_j))
    raster_file <- file.path(covars_crop_dir, paste0(covars_i_j, ".tif"))
    covars_list[[j]] <- raster(raster_file)
  }
  covars_brick <- raster::brick(covars_list)
  rm(covars_list)

  # Generate formula

  covars_clean_i <- best_ssf_fits %>% slice(i) %>% pluck("covar_fitted", 1) %>%
    pull("covar_clean")

  covars_i <- ifelse(str_detect(covars_clean_i, "\\^2"),
    paste0("covars_brick[['", str_remove_all(covars_clean_i, "\\^2"), "']]^2"),
    paste0("covars_brick[['", covars_clean_i, "']]"))

  coefs_i <- best_ssf_fits %>% slice(i) %>% pluck("covar_fitted", 1) %>%
    pull("coef_signif")

  # Generate formulas
  ssf_formula <- paste0("(", paste0(paste0(coefs_i, "*",covars_i),
    collapse = ") + ("), ")")
  print(ssf_formula)

  # Create value raster, then crop and mask
  ssf_value_raster <- eval(parse(text = ssf_formula))
  plot(ssf_value_raster, main = step_type_i)

  # Calculate probability
  ssf_prob_raster <- raster::calc(ssf_value_raster, fun = boot::inv.logit)
  plot(ssf_prob_raster, main = step_type_i)
  hist(ssf_prob_raster)

  # Write Rasters to output dir
  step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
    "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
  writeRaster(ssf_value_raster, file.path(ssf_raster_dir, "Step_Types",
    step_type_i_numeric), format = "GTiff", overwrite = TRUE)
  writeRaster(ssf_prob_raster, file.path(ssf_raster_dir,
    "Step_Types_Prob", step_type_i_numeric), format = "GTiff",
    overwrite = TRUE)
  rm(coefs_i, covars_brick, raster_file, ssf_formula, ssf_value_raster,
    ssf_prob_raster, step_type_i_numeric)
  gc()

}










### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###

## Demonstrate raster evaluation with quadratic terms

# r1 <- raster(ncol=5, nrow=5, xmn=0, xmx=5, ymn=0, ymx=5)
# r1[] <- 0
# r1[c(1:5)] <- c(5)
# r1[c(21:25)] <- c(5)
# crs(r1) <- "+proj=ortho"  # Needed for mapping with tmap
# names(r1) <-'r1'
# ncell(r1)
# plot(r1)
#
# r2 <- raster(ncol=5, nrow=5, xmn=0, xmx=5, ymn=0, ymx=5)
# r2[] <- 0
# r2[c(6:10)] <- c(2)
# r2[c(16:20)] <- c(2)
# crs(r2) <- "+proj=ortho" # Needed for mapping with tmap
# names(r2) <-'r2'
# ncell(r2)
# plot(r2)
#
# covars_list <- vector(mode = "list", length = 2)
# covars_list[[1]] <- r1
# covars_list[[2]] <- r2
# covars_brick <- raster::brick(covars_list)
#
# plot(covars_brick[[1]], main = "R1")
# plot(covars_brick[[2]], main = "R2")
#
# # Generate formula
# ssf_formula <- "covars_brick[['r1']] + covars_brick[['r1']]^2 +
#   covars_brick[['r2']]"
#
# # Create value raster, then crop and mask
# ssf_value_raster <- eval(parse(text = ssf_formula))
#
# plot(ssf_value_raster, main = step_type_i, col = viridis::viridis(30))
# text(ssf_value_raster)

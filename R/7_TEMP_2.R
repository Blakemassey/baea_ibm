## THIS IS A TEST FOR CONVERTING THE SSF MODELS TO SSF SURFACES

# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, plyr, dplyr, future, furrr, optimx, ggplot2,
  ggthemes, glmulti, lubridate, optimx, purrr, reproducible, rgenoud, stringr,
  survival, surveybootstrap, tibble, tictoc, tidyr)
options(stringsAsFactors = FALSE)

# Output Directory
mod_fit_dir = "Output/Analysis/SSF/Models"
best_ssf_fits <- readRDS(file.path(mod_fit_dir, "best_ssf_fits_all.rds"))

# Determine all the raster_sigma layers
preds_all <- paste0(best_ssf_fits$preds, collapse = " + ")
preds_unique <- unique(str_split(preds_all, " \\+ ") %>% pluck(1))
preds_rasters <- unique(str_replace_all(preds_unique, "[0-9]", ""))

tibble(preds_unique) %>%
  mutate(sigma = str_extract_all(preds_unique, "[0-9]")) %>%
  mutate(covar = str_replace_all(preds_unique, "[0-9]", ""))

ext <- extent(c(510000, 560000, 5025000, 5065000))
developed <- crop(developed, ext, snap = "near")
eastness <- crop(eastness, ext, snap = "near")
forest <- crop(forest, ext, snap = "near")
open_water <- crop(open_water, ext, snap = "near")
northness <- crop(northness, ext, snap = "near")
wind_class <- crop(wind_class, ext, snap = "near")
elev <- crop(elev, ext, snap = "near")

extent(developed)
extent(eastness)

library(gisr)
covar_brick <- brick(c(
  tibble(sigma = c(100), covar = "developed") %>%
    pmap(., SmoothRaster))
  )

cell_size <- 30
kernel_bandwidths <- c(seq(0, 3000, by = 30))  # radius (meters)
terrain_bandwidths <- c(seq(0, 1500, by = 30))
terrain_bandwidths[1]

#CalculateTerrainMetric
  elev = elev
  size = (terrain_bandwidths[2]/cell_size)*2 + 1
  metric = "tri"

  ((terrain_bandwidths[1]/cell_size)*2 + 1)

  x <- elev
  weight_matrix <- matrix(1, nrow=size, ncol=size)
  center <- ceiling(0.5 * length(weight_matrix))
  window <- length(weight_matrix)-1
  if (metric == "tri"){
    tri <- focal(x, w=weight_matrix,
      fun=function(x, ...) sum(abs(x[-center]-x[center]))/window,
      pad=TRUE, padValue=NA)
    out_matrix <- tri
  } else if (metric == "tpi"){
    tpi <- focal(x, w = weight_matrix,
      fun=function(x, ...) x[center] - mean(x[-center]),
      pad=TRUE, padValue=NA)
    out_matrix <- tpi
  } else if (metric == "roughness"){
    rough <- focal(x, w = weight_matrix,
      fun = function(x, ...) max(x) - min(x),
      pad = TRUE, padValue =NA, na.rm=TRUE)
    out_matrix <- rough
  } else {
    stop("'metric' must equal 'tpi', 'tri', or 'roughness'", call. = FALSE)
  }

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

# Set Opt Sigma Seq ------------------------------------------------------------

opt_sigmas <- seq(0, 40, by = 1)
length(opt_sigmas)^3 # Number of sigma combinations

# Create Covar Sigma Rasters Brick ---------------------------------------------

covar_brick <- brick(c(
  tibble(sigma = opt_sigmas, covar = "covar1") %>% pmap(., SmoothRaster),
  tibble(sigma = opt_sigmas, covar = "covar2") %>% pmap(., SmoothRaster),
  tibble(sigma = opt_sigmas, covar = "covar3") %>% pmap(., SmoothRaster)
  ))

covar_matrix <- raster::as.matrix(covar_brick)
covar_cols <-  setNames(seq_len(ncol(covar_matrix)), colnames(covar_matrix))
covar_names <- c(names(covar1), names(covar2), names(covar3))
rm(covar_brick)








# TEMPLATES
preds <- paste(c(covars_scale_sigmas, covars_fixed_0), collapse = " + ")
clogit_model_formula <- as.formula(paste('case', preds, sep = " ~ "))

ssf <- exp(elevation*coefs["elevation"] + roads*coefs["roads"] +
    wells*coefs["wells"] + shrub*coefs["shrub"] + barren*coefs["barren"])
eval(parse(text="5+5"))

# # REPLACE THIS WITH THE FUNCTION TO GET THE RASTER MATRIX BELOW
# developed100 = .5
# forest100 = .5
# open_water100 = .1
# northness84 = .1
# wind_class24 = .1
# tpi50 = .1
# tri0 = .1
# roughness50 = .1
# developed_dist0 = 30
# turbine_dist0 = 3000

#setwd("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm")


# Output paths
# ua_data_dir <- "Output/Analysis/SSF/UA_Data"
#
# ua_data_files <- list.files(ua_data_dir, full.names = TRUE)
#
# for (i in seq_along(ua_data_files)){
#   ua_data_i <- readRDS(ua_data_files[i])
#   unique(ua_data_i$behavior_behavior)
#   identical(head(ua_data_i$tpi0), head(ua_data_i$tri0), head(ua_data_i$roughness0))
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
#   saveRDS(ua_steps_i, file.path(ua_data_dir, paste0("ua_steps_",
#     step_type_i_name, ".rds")))
#


# # Rename files
# fit_files <- list.files(file.path(mod_fit_dir, "cruise_perch"),
#   full.names = TRUE)
# for (i in seq_along(fit_files)){
#   fit_file_i <- fit_files[i]
#   fit_file_i_new <- str_replace(fit_file_i, "models", "ssf")
#   file.rename(fit_file_i, fit_file_i_new)
# }


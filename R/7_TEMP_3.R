## THIS IS A TEST FOR CONVERTING THE SSF MODELS TO SSF SURFACES

# Load libraries, scripts, and input parameters
pacman::p_load(plyr, dplyr, optimx, purrr, raster, tibble, tidyr, whitebox)
whitebox::wbt_init() # required for WhiteboxTools to work
wbt_version() # check WhiteboxTools version
library(gisr)
rasterOptions(progress = "text", timer = TRUE, memfrac = .6)

############################ IMPORT RASTERS ####################################

preds_tbl <- readRDS("Output/Analysis/SSF/Models/best_fits/preds_tbl.rds")

# Source data directories
file_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
elev_file <- file.path(file_dir, "elev_30mc.tif")
developed_file <- file.path(file_dir, "developed_30mc.tif")

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

  # Merge together rasters
  top_raster <- raster(top_file)
  mid_raster <- raster(mid_file)
  bot_raster <- raster(bot_file)
  out_raster <- raster::merge(mid_raster, top_raster, bot_raster)

  # Write out merged raster
  writeRaster(out_raster, out_file, format = "GTiff", overwrite = TRUE)

}














#
# for (i in 1:nrow(preds_kernel)){
#   preds_kernel_i <- preds_kernel %>%
#     slice(i)
#   sigma <- preds_kernel_i %>% pull(sigma)
#   covar <- preds_kernel_i %>% pull(covar)
#   print(paste0("Starting: ", covar, sigma, " (", i, " of ", nrow(preds_kernel),
#     ") at: ", lubridate::now()))
#
#   top_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Kernel", paste0(covar, sigma, "_top.grd"))
#   mid_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Kernel", paste0(covar, sigma, "_mid.grd"))
#   bot_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Kernel", paste0(covar, sigma, "_bot.grd"))
#   out_file <- file.path(file_dir, "SSF_Rasters", paste0(covar, sigma, ".tif"))
#   out_file2 <- file.path(file_dir, "SSF_Rasters", paste0(covar, sigma, "_2.tif"))
#
#   top_raster <- raster(top_file)
#   mid_raster <- raster(mid_file)
#   bot_raster <- raster(bot_file)
#
#   out_raster <- raster::merge(mid_raster, top_raster, bot_raster)
#
#   writeRaster(out_raster, out_file, format = "GTiff", overwrite = TRUE)
#   rm(top_file, mid_file, bot_file, top_raster, mid_raster, bot_raster)
#
# }
#
#


#
#
#
#
#
#   raster_file <- file.path(file_dir, paste0(covar, "_30mc.tif"))
#   print(paste0("Starting: ", covar, sigma, " (", i, " of ", nrow(preds_kernel),
#     ") at: ", lubridate::now()))
#
#   # Covar TOP
#   print("top")
#   covar_top <- crop(raster(raster_file), ext_top, snap = "near")
#   covar_top_smooth <- raster::raster(covar_top) # creates blank raster
#   values(covar_top_smooth) <-
#     smoothie::gauss2dsmooth(raster::as.matrix(covar_top),
#     lambda = sigma, nx = DescTools::RoundTo(nrow(covar_top), 2),
#     ny = DescTools::RoundTo(ncol(covar_top), 2))
#   writeRaster(covar_top_smooth, file.path(file_dir, "SSF_Rasters", paste0(
#     covar, sigma, "_top")), format = "raster", overwrite = TRUE)
#   rm(covar_top, covar_top_smooth)
#   gc()
#
#   # Covar MID
#   print("mid")
#   covar_mid <- crop(raster(raster_file), ext_mid, snap = "near")
#   covar_mid_smooth <- raster::raster(covar_mid) # creates blank raster
#   values(covar_mid_smooth) <-
#     smoothie::gauss2dsmooth(raster::as.matrix(covar_mid),
#     lambda = sigma, nx = DescTools::RoundTo(nrow(covar_mid), 2),
#     ny = DescTools::RoundTo(ncol(covar_mid), 2))
#   covar_mid_smooth <- crop(covar_mid_smooth, ext_mid_crop)
#   writeRaster(covar_mid_smooth, file.path(file_dir, "SSF_Rasters", paste0(
#     covar, sigma, "_mid")), format = "raster", overwrite = TRUE)
#   rm(covar_mid, covar_mid_smooth)
#   gc()
#
#   # Covar BOTTOM
#   print("bottom")
#   covar_bot <- crop(raster(raster_file), ext_bot, snap = "near")
#   covar_bot_smooth <- raster::raster(covar_bot) # creates blank raster
#   values(covar_bot_smooth) <-
#     smoothie::gauss2dsmooth(raster::as.matrix(covar_bot),
#     lambda = sigma, nx = DescTools::RoundTo(nrow(covar_bot), 2),
#     ny = DescTools::RoundTo(ncol(covar_bot), 2))
#   writeRaster(covar_bot_smooth, file.path(file_dir, "SSF_Rasters", paste0(
#     covar, sigma, "_bot")), format = "raster", overwrite = TRUE)
#   rm(covar_bot, covar_bot_smooth)
#   gc()
#
# }



  # raster_kernel_i <- preds_kernel_i %>%
  #   mutate(raster_layer = map2(.x = sigma, .y = covar, .f = SmoothRaster)) %>%
  #   pluck("raster_layer", 1)
  #
  #
  #
  #
  # sigma <-
  # raster_name <- "developed"
  # raster_file <- file.path(file_dir, "developed_30mc.tif")
  #
  #
  #
  # writeRaster(covar_smooth, file.path(file_dir, "SSF_Rasters", paste0(
  #   raster_name, "_3")), format = "raster", overwrite = TRUE)
  #
  # rm(covar_smooth)

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
# raster_classes <- c(developed = "kernel_class",
#   forest = "kernel_class",
#   open_water = "kernel_class",
#   pasture = "kernel_class",
#   shrub_herb = "kernel_class",
#   wetland = "kernel_class",
#   eastness = "kernel_class",
#   northness = "kernel_class",
#   wind_class = "kernel_class",
#   developed_dist = "extract_class",
#   hydro_dist = "extract_class",
#   turbine_dist = "extract_class",
#   tpi = "terrain_class",
#   tri = "terrain_class",
#   roughness = "terrain_class")
#
# ## Get SSF FITS ----------------------------------------------------------------
#
# # Directory of fits
# mod_fit_dir = "Output/Analysis/SSF/Models"
#
# best_ssf_fits_org <- readRDS(file.path(mod_fit_dir, "best_fits",
#   "best_ssf_fit_all.rds"))
#
# best_ssf_fits_org %>% dplyr::select(step_type, preds)
#
# best_ssf_fits <- best_ssf_fits_org # all
# #best_ssf_fits <- best_ssf_fits_org %>% slice(c(1,4,8,12)) # all '-> Cruise'
# #best_ssf_fits <- best_ssf_fits_org %>% slice(c(2,5,9,13,16)) # all '-> Flight'
# #best_ssf_fits <- best_ssf_fits_org %>% slice(c(3,6,10,14,17)) # all '-> Perch'
# #best_ssf_fits <- best_ssf_fits_org %>% slice(c(7,11,15)) # all '-> Roost'
#
# # Determine all the raster_sigma layers
# preds_all <- paste0(best_ssf_fits$preds, collapse = " + ")
# preds_unique <- unique(str_split(preds_all, " \\+ ") %>% pluck(1))
# preds_rasters <- unique(str_replace_all(preds_unique, "[0-9]", ""))
#
# preds_tbl <- tibble(preds_unique) %>%
#   mutate(sigma = as.integer(str_extract(preds_unique, "[0-9]{1,3}"))) %>%
#   mutate(covar = str_replace_all(preds_unique, "[0-9]", "")) %>%
#   dplyr::select(covar, sigma) %>%
#   mutate(covar_sigma = paste0(covar, sigma)) %>%
#   mutate(raster_class = recode(covar, !!!raster_classes)) %>%
#   mutate(raster_layer = vector(mode = "list", length = nrow(.))) %>%
#   arrange(covar, sigma)
#
# saveRDS(preds_tbl, "Output/Analysis/SSF/Models/best_fits/preds_tbl.rds")
#
# CalculateTerrainMetricWithSigma <- function(sigma, metric){
#   print(paste0("Starting: ", metric, sigma))
#   if(sigma == 0) sigma <- 1
#   size <- (sigma*2) + 1
#   x <- elev
#   weight_matrix <- matrix(1, nrow = size, ncol = size)
#   center <- ceiling(0.5 * length(weight_matrix))
#   window <- length(weight_matrix)-1
#   if (metric == "tri"){
#     tri <- focal(x, w = weight_matrix,
#       fun = function(x, ...) sum(abs(x[-center] - x[center]))/window,
#       pad = TRUE, padValue = NA)
#       out_matrix <- tri
#   } else if (metric == "tpi"){
#     tpi <- focal(x, w = weight_matrix,
#       fun = function(x, ...) x[center] - mean(x[-center]),
#       pad = TRUE, padValue = NA)
#     out_matrix <- tpi
#   } else if (metric == "roughness"){
#     rough <- focal(x, w = weight_matrix,
#       fun = function(x, ...) max(x) - min(x),
#       pad = TRUE, padValue = NA, na.rm = TRUE)
#     out_matrix <- rough
#   } else {
#     stop("'metric' must equal 'tpi', 'tri', or 'roughness'", call. = FALSE)
#   }
#   return(out_matrix)
# }
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
#
# # Generate ssf_layers ----------------------------------------------------------
#
# # Base all of the calculations on the con_nest_dist rasters???
# CreateSimLandscapeRasters <- function(con_nest_dist){
#   return(landscape)
# }
#
# # Empty list
# ssf_raster_list <- vector(mode = "list", length = nrow(best_ssf_fits))
#
# # Generate layer for each ssf
# for (i in seq_len(nrow(best_ssf_fits))){
#   print(i)
#   clogit_fit_i <- best_ssf_fits %>% slice(i) %>% pull(clogit_fit) %>% pluck(1)
#   step_type_i <- best_ssf_fits %>% slice(i) %>% pull(step_type)
#   # Extract terms(not including 'strata(step_id)')
#   terms_i <- clogit_fit_i %>% pluck(terms, attr_getter("term.labels")) %>%
#     .[!. %in% c("strata(step_id)")]
#   # Extract coefficients
#   coefs_i <- clogit_fit_i %>% pluck(coef)
#   # Generate formulas
#   ssf_formula <- paste0("(", paste(paste0("covars_brick[['", names(coefs_i),
#     "']]"), paste0("coefs_i['", names(coefs_i),"']"), sep = "*",
#     collapse = ") + ("), ")")
#   ssf_raster <- eval(parse(text = ssf_formula))
#   ssf_raster_inv_log <- calc(ssf_raster, fun = boot::inv.logit)
#   # plot histogram
#   hist(ssf_raster_inv_log, col = "springgreen", breaks = seq(0,1, by = .05))
#
#   ssf_raster_list[[i]] <- ssf_raster_inv_log
#   step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
#     "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
#   # Names are "step_#_#" because raster stack names can't start with a number
#   names(ssf_raster_list[[i]]) <- paste0("step_", step_type_i_numeric)
# }
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
# for (i in seq_len(nrow(best_ssf_fits))){
#   print(i)
#   clogit_fit_i <- best_ssf_fits %>% slice(i) %>% pull(clogit_fit) %>% pluck(1)
#   step_type_i <- best_ssf_fits %>% slice(i) %>% pull(step_type)
#   # Extract terms(not including 'strata(step_id)')
#   terms_i <- clogit_fit_i %>% pluck(terms, attr_getter("term.labels")) %>%
#     .[!. %in% c("strata(step_id)")]
#   # Extract coefficients
#   coefs_i <- clogit_fit_i %>% pluck(coef)
#   # Generate formulas
#   ssf_formula <- paste0("(", paste(paste0("covars_brick[['", names(coefs_i),
#     "']]"), paste0("coefs_i['", names(coefs_i),"']"), sep = "*",
#     collapse = ") + ("), ")")
#   ssf_raster <- eval(parse(text=ssf_formula))
#   if (isTRUE(inverse_logit)){
#     ssf_raster_final <- calc(ssf_raster, fun = boot::inv.logit)
#     } else {
#     ssf_raster_final <- ssf_raster
#   }
#   ssf_raster_list[[i]] <- ssf_raster_final
#   step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
#     "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
#   # Names are "step_#_#" because raster stack names can't start with a number
#   names(ssf_raster_list[[i]]) <- paste0("step_", step_type_i_numeric)
# }
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
# boot::inv.logit(10)
# gtools::inv.logit(10)
#
# boot::inv.logit(0)
# gtools::inv.logit(0)
#
# boot::inv.logit(-10)
# gtools::inv.logit(-10)
#

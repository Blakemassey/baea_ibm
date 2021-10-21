#--------------------- SSF Calculates Covariates ------------------------------#
# Calculate landscape covariates for each of the steps
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------

# Load libraries
pacman::p_load(AICcmodavg, plyr, dplyr, ggplot2, ggthemes, optimx, raster,
  reproducible, rgdal, smoothie, stringr, survival, tictoc) #spatialfil
pacman::p_load(baear, gisr, ibmr)
#setwd("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm")

# Output paths
ua_data_dir <- "Output/Analysis/SSF/UA_Data"
ssf_timing_dir <- "Output/Analysis/SSF/Timing"

# Source data directories
base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
file_dir <- "C:/ArcGIS/Data/R_Input/BAEA"

# Terrain class
elev_file <- file.path(file_dir, "elev_30mc.tif")

# Extract class
dist_developed_file <- file.path(file_dir, "dist_developed_30mc.tif")
dist_hydro_file <- file.path(file_dir, "dist_hydro_30mc.tif")
dist_turbine_file <- file.path(file_dir, "dist_turbine_30mc.tif")
dist_road_file <- file.path(file_dir, "dist_road_30mc.tif")

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
road_file <- file.path(file_dir, "road_30mc.tif")

# BAEA and Movement Parameters
baea_steps_file <- "Data/BAEA/baea_steps.rds"
move_pars_file <- "Output/Analysis/Movements/move_pars.rds"

# Subsetting Variables
subsetting_bandwidths <- FALSE
subsetting_covars <- TRUE
subsetting_ids <- FALSE
subsetting_step_types <- FALSE

# Import Base Raster, Steps Data, and Movement Parameters ----------------------

base <- raster(base_file)
baea_steps <- readRDS(file = baea_steps_file)
move_pars <- readRDS(file = move_pars_file)
rm(baea_steps_file, move_pars_file) #base_file

# Import Covariate Rasters -----------------------------------------------------

# Import rasters

# Terrain class
elev <- raster(elev_file) # all other layers' extent are set to this layer

# Extract class
dist_developed <- crop(raster(dist_developed_file), elev)
dist_hydro <- crop(raster(dist_hydro_file), elev)
dist_turbine <- crop(raster(dist_turbine_file), elev)
dist_road <- crop(raster(dist_road_file), elev)

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
road <- crop(raster(road_file), elev)

# Map developed to test code
if(FALSE) plot(developed$as.RasterLayer(band = 1))

# Clean up objects
rm(base_file, file_dir,
  dist_developed_file, dist_hydro_file, dist_turbine_file, dist_road_file,
  developed_file, forest_file, open_water_file, pasture_file,
  shrub_herb_file, wetland_file, eastness_file, northness_file, wind_class_file,
  road_file, elev_file)

# Specify Landscape Covariates and Bandwidths ---------------------------------

# Set parameters for analysis
cell_size <- 30
kernel_bandwidths <- c(seq(0, 3000, by = 30))  # radius (meters)
terrain_bandwidths <- c(seq(0, 1500, by = 30))

extract_class <- c("dist_developed", "dist_hydro", "dist_turbine", "dist_road")
kernel_class <- c("developed", "forest", "open_water", "pasture", "shrub_herb",
  "wetland", "eastness", "northness", "wind_class")
terrain_class <- c("tpi", "tri", "roughness")

covar_stack <- stack(dist_developed, dist_hydro, dist_turbine, dist_road,
  developed, forest, open_water, pasture, shrub_herb, wetland, eastness,
  northness, wind_class, road, elev)
names(covar_stack) <- str_replace_all(names(covar_stack), "_30mc", "")
covar_types <- c(extract_class, kernel_class, terrain_class)
covariate_cols <- c(paste0(rep(extract_class, each=1), 0),
  paste0(rep(kernel_class, each=length(kernel_bandwidths)),
    rep(kernel_bandwidths, times=length(kernel_class))),
  paste0(rep(terrain_class, each=length(terrain_bandwidths)),
    rep(terrain_bandwidths, times=length(terrain_class))))

if (subsetting_bandwidths == TRUE){  # subset data for testing
  rm(kernel_bandwidths, terrain_bandwidths)
  kernel_bandwidths <- c(seq(0, 300, by = 30))
  terrain_bandwidths <- c(seq(0, 150, by = 30))
}

if (subsetting_covars == TRUE){  # subset data for testing
  extract_class <- c("dist_developed", "dist_hydro", "dist_turbine", "dist_road")
  kernel_class <- c()
  terrain_class <- c()
  covar_stack <- stack(dist_developed, dist_hydro, dist_turbine, dist_road)
  names(covar_stack) <- str_replace_all(names(covar_stack), "_30mc", "")
  covar_types <- c(extract_class, kernel_class, terrain_class)
  covariate_cols <- c(paste0(rep(extract_class, each = 1), 0),
    paste0(rep(kernel_class, each = length(kernel_bandwidths)),
      rep(kernel_bandwidths, times = length(kernel_class))),
    paste0(rep(terrain_class, each = length(terrain_bandwidths)),
      rep(terrain_bandwidths, times = length(terrain_class))))
}

table(baea_steps$behavior_behavior)
if (subsetting_step_types == TRUE){  # subset data for testing
  baea_steps_file <- "Data/BAEA/baea_steps.rds"
  baea_steps <- readRDS(file = baea_steps_file)
  start = "cruise"
  end = "perch"
  baea_steps_all <- baea_steps
  sort(unique(baea_steps_all$behavior_behavior))
  baea_steps <- baea_steps_all %>%
    filter(!behavior_behavior %in%
     c("Cruise -> Cruise", "Cruise -> Flight", "Cruise -> Perch",
       "Nest -> Perch", "Roost -> Flight"))
  sort(unique(baea_steps$behavior_behavior))
}

if (subsetting_ids == TRUE){  # subset data for testing
  baea_steps <- baea_steps %>% filter(id %in% c("Sandy", "Norway"))
  unique(baea_steps$id)
  i <- j <- k <- m <- 1
}

rm(dist_developed, dist_hydro, dist_turbine, dist_road,
  developed, forest, open_water, pasture, shrub_herb, wetland, eastness,
  northness, wind_class, road, elev)

plot_stack <- FALSE
if (isTRUE(plot_stack)){
  plot(covar_stack, 1)
  plot(covar_stack, 2)
  plot(covar_stack, 3)
  plot(covar_stack, 4)
  plot(covar_stack, 5)
  plot(covar_stack, 6)
  plot(covar_stack, 7)
  plot(covar_stack, 8)
  plot(covar_stack, 9)
  plot(covar_stack, 10)
  plot(covar_stack, 11)
  plot(covar_stack, 12)
  plot(covar_stack, 13)
}

covar_matrix <- raster::as.matrix(covar_stack)
covar_cols <- setNames(seq_len(ncol(covar_matrix)), colnames(covar_matrix))
rm(covar_matrix)

# Calculate Kernel-Weighted Covariate Values For Used And Available ------------

# Start of large 'for loop' for each 'step_type' (i.e. behavior_behavior)

for (i in seq_along(unique(baea_steps$behavior_behavior))){
  tic.clearlog()

  # Subset 'baea_steps' to step yype
  step_type_i <- unique(baea_steps$behavior_behavior)[i]
  step_type_i_name <- str_to_lower(str_replace_all(step_type_i, " -> ", "_"))
  tic(paste0(step_type_i_name, "-NA-NA-NA"), quiet = FALSE,
    func.tic = TicMsg)
  steps_i <- baea_steps %>%
    filter(behavior_behavior == step_type_i) %>%
    tibble::rowid_to_column(., "step_id")

  # Create movement kernel for 'step_type'
  move_pars_i <- move_pars %>% filter(behavior_behavior == step_type_i)
  ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise", "Flight"),
    FALSE, TRUE)
  kernel_i <- CreateMoveKernelWeibullVonMises(max_r = NULL, cellsize =cell_size,
    pars = move_pars_i, ignore_von_mises = ignore_von_mises)
  r <- (cell_size*((nrow(kernel_i)-1)/2))+(cell_size/2)
  kernel_raster <- raster::raster(kernel_i, xmn=-r, xmx=r, ymn=-r, ymx=r)
  prob_raster <- kernel_raster/raster::cellStats(kernel_raster, stat = "sum")
  prob_raster[prob_raster <= .000001] <- 0
  rm(kernel_i, kernel_raster, move_pars_i, ignore_von_mises, r)

  # Create movement probability rasters for each step
  move_prob_rasters <- list(rep(NA, nrow(steps_i)))
  for (step_i in seq_len(nrow(steps_i))){
    cat(paste0("Calculating: '", step_type_i_name, "' Move_Prob Raster: ",
      step_i, " of ", nrow(steps_i),"\n"))
    exp_angle <- steps_i[step_i,] %>% pull(exp_angle)
    prob_raster_rotated <- RotateRaster(prob_raster, Rad2Deg(exp_angle))
    prob_raster_resampled <- resample(prob_raster_rotated, prob_raster,
      method="bilinear")
    prob_raster_resampled[is.na(prob_raster_resampled)] <- 0
    move_prob_rasters[[step_i]] <- trim(prob_raster_resampled, values = 0)
    rm(exp_angle, prob_raster_rotated, prob_raster_resampled, step_i)
  }
  names(move_prob_rasters) <- paste0("step_", steps_i$step_id)

  # Create dataframes for available and used values
  covariate_df <- setNames(data.frame(matrix(ncol = length(covariate_cols),
    nrow = nrow(steps_i), NA)), covariate_cols)
  used_steps_i <- cbind(case = 1, steps_i, covariate_df)
  avail_steps_i <- cbind(case = 0, steps_i, covariate_df)
  n_total <- length(unique(steps_i$id))*length(covariate_cols)
  counter <- 0

  # Calculate kernel-weighted covariate values
  for (j in seq_along(unique(steps_i$id))){
    id_j <- unique(steps_i$id)[j]
    tic(paste0(step_type_i_name, "-", id_j, "-NA-NA"), quiet = TRUE,
      func.tic = TicMsg)
    avail_steps_ij <- avail_steps_i %>% filter(id == id_j)
    used_steps_ij <- used_steps_i %>% filter(id == id_j)
    steps_ij <- steps_i %>% filter(id == id_j)
    steps_ij_range <- c(range(steps_ij$long_utm), range(steps_ij$lat_utm))
    extent_ij <- extend(extent(alignExtent(extent(steps_ij_range), base,
      snap = 'out')), 12000)
    for (k in seq_along(covar_types)){
      covar_type_k <- covar_types[k]
      tic(paste0(step_type_i_name, "-", id_j, "-", covar_type_k, "-NA"),
        quiet = TRUE, func.tic = TicMsg)
      if (covar_type_k %in% kernel_class){
        covar_layer_k <- which(names(covar_stack) == covar_type_k)
        covar_raster_k <- crop(raster(covar_stack, layer=covar_layer_k),
          extent_ij)
        covar_matrix_k <- raster::as.matrix(covar_raster_k)
        bandwidths_k <- kernel_bandwidths
      } else if (covar_type_k %in% extract_class){
        covar_layer_k <- which(names(covar_stack) == covar_type_k)
        covar_raster_k <- crop(raster(covar_stack, layer=covar_layer_k),
          extent_ij)
        covar_matrix_k <- raster::as.matrix(covar_raster_k)
        bandwidths_k <- 0
      } else if (covar_type_k %in% terrain_class){
        covar_layer_k <- which(names(covar_stack) == "elev")
        covar_raster_k <- crop(raster(covar_stack, layer=covar_layer_k),
          extent_ij)
        bandwidths_k <- terrain_bandwidths
      } else {
        stop(paste0("The covar_type: '", covar_type_k, "' is not ",
              "associated with an extract, kernel, or terrain class."))
      }
      for(m in seq_along(bandwidths_k)){
        bw_meters <- bandwidths_k[m]
        str_ijkm <- paste0(step_type_i_name, "-", id_j, "-", covar_type_k, "-",
          bw_meters)
        cat(paste0("Starting: (", (counter <- counter + 1), " of ", n_total,
          ") ", str_ijkm,"\n"))
        tic(str_ijkm, quiet = TRUE, func.tic = TicMsg)
        col_name <- paste0(covar_type_k, bw_meters)
        col_num_used <- which(colnames(used_steps_i) == col_name)
        col_num_avail <- which(colnames(used_steps_i) == col_name)
        if(bw_meters == 0){
          if (covar_type_k %in% kernel_class){
			covar_raster_calc_m <- covar_raster_k
          } else if (covar_type_k %in% terrain_class){
            covar_raster_calc_m <- CalculateTerrainMetric(elev = covar_raster_k,
              size = 1, metric = covar_type_k)
          } else if (covar_type_k %in% extract_class){
            covar_raster_calc_m <- covar_raster_k
          }
        } else {
          if (covar_type_k %in% kernel_class){
            covar_matrix_smooth_m <- kernel2dsmooth(covar_matrix_k,
              kernel.type="gauss", nx=nrow(covar_matrix_k),
              ny=ncol(covar_matrix_k), sigma = bandwidths_k[m]/cell_size)
            covar_raster_calc_m <- raster(covar_matrix_smooth_m,
              template = covar_raster_k)
            rm(covar_matrix_smooth_m)
          } else if (covar_type_k %in% terrain_class){
            covar_raster_calc_m <- CalculateTerrainMetric(elev = covar_raster_k,
              size = ((bandwidths_k[m]/cell_size)*2 + 1), metric = covar_type_k)
          } else if (covar_type_k %in% extract_class){
            covar_raster_calc_m <- covar_raster_k
          }
        }
        for (p in seq_len(nrow(avail_steps_ij))){
          avail_steps_ijp_xy <- c(avail_steps_ij[p,] %>% pull(long_utm),
            avail_steps_ij[p,] %>% pull(lat_utm))
          step_id_p <- paste0("step_", avail_steps_ij[p, "step_id"])
          move_prob_raster_p <- raster::shift(move_prob_rasters[[step_id_p]],
            dx = avail_steps_ijp_xy[1], dy = avail_steps_ijp_xy[2])
          covar_raster_smooth_p <- crop(covar_raster_calc_m,
            move_prob_raster_p)
          covar_kernel_sum <- sum(raster::as.matrix(covar_raster_smooth_p) *
            raster::as.matrix(move_prob_raster_p))
          row_num <- which(avail_steps_i$step_id == avail_steps_ij[p,"step_id"])
          avail_steps_i[row_num, col_num_avail] <- covar_kernel_sum
          rm(avail_steps_ijp_xy, covar_raster_smooth_p, covar_kernel_sum,
            move_prob_raster_p, p, row_num, step_id_p)
        }
        for (q in seq_len(nrow(used_steps_ij))){
          used_steps_ijq_xy <- data.frame(x = used_steps_ij[q,] %>%
            pull(long_utm_end), y = avail_steps_ij[q,] %>% pull(lat_utm_end))
          covar_value <- extract(covar_raster_calc_m, used_steps_ijq_xy)
          row_num <- which(used_steps_i$step_id == used_steps_ij[q, "step_id"])
          used_steps_i[row_num, col_num_used] <- covar_value
          rm(used_steps_ijq_xy, covar_value, row_num)
        }
        rm(bw_meters, str_ijkm, col_num_avail, col_num_used,
          covar_raster_calc_m)
        toc(quiet = FALSE, log = TRUE, func.toc = TocMsg, info = "Finished")
      }
      if (covar_type_k %in% kernel_class) rm(covar_matrix_k)
      rm(covar_type_k, covar_layer_k, covar_raster_k)
      toc(quiet = FALSE, log = TRUE, func.toc = TocMsg, info = "Finished")
    }
    rm(id_j, avail_steps_ij, used_steps_ij, steps_ij, steps_ij_range, extent_ij)
    toc(quiet = FALSE, log = TRUE, func.toc = TocMsg, info = "Finished")
  }
  rm(counter, n_total,  covariate_df, steps_i, move_prob_rasters)
  toc(quiet = FALSE, log = TRUE, func.toc = TocMsg, info = "Finished")
  ua_steps_i <- rbind(avail_steps_i, used_steps_i) %>% arrange(step_id, case)
  saveRDS(ua_steps_i, file.path(ua_data_dir, paste0("ua_steps_",
    step_type_i_name, ".rds")))
  steps_i_tictoc_df <- data.frame(stringr::str_split(unlist(tic.log(format =
    TRUE)), "\\-|\\:", simplify = TRUE))
  steps_i_tictoc_df[steps_i_tictoc_df == "NA"] <- NA
  saveRDS(steps_i_tictoc_df, file.path(ssf_timing_dir, paste0("tictoc_",
    step_type_i_name, ".rds")))
  rm(avail_steps_i, used_steps_i, step_type_i_name)
  tic.clearlog()
}

### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###
#
# # Set directories
# ua_data_dir <- "Output/Analysis/SSF/UA_Data"
# ua_data_dist_dir <- "Output/Analysis/SSF/Archive/UA_Data_Dist"
# ua_data_notrescaled_dir <- "Output/Analysis/SSF/Archive/UA_Data_NotRescaled"
#
# # Import rescaled ua_steps distance covariates
# ua_dist <- map(list.files(ua_data_dist_dir, full.names = TRUE), readRDS) %>%
#   bind_rows()
#
# # Subset to only the needed columns
# ua_dist_sub <- ua_dist %>%
#   select(case, step_id, id, datetime, behavior_behavior,
#     dist_developed0:dist_road0)
# colnames(ua_dist_sub)
#
# # Import *not* rescaled ua_steps distance covariates
# ua_notrescaled <- map(list.files(ua_data_notrescaled_dir, full.names = TRUE),
#   readRDS) %>% bind_rows()
#
# # Subset to only the needed columns
# ua_notrescaled_sub <- ua_notrescaled %>%
#   select(-c("developed_dist0", "hydro_dist0", "turbine_dist0", "road_dist0"))
#
# # Join data together
# ua_steps <- right_join(ua_notrescaled_sub, ua_dist_sub, by = c("case",
#   "step_id", "id", "datetime", "behavior_behavior"))
#
# # Check that rescaled distance values look correct
# ua_steps_check <- ua_steps %>%
#   select(case, step_id, id, datetime, behavior_behavior, dist_developed0,
#     dist_hydro0, dist_turbine0, dist_road0)
#
# # Split up into behaviors and save RDS
# for (i in unique(ua_steps$behavior_behavior)){
#   ua_steps_i <- ua_steps %>% filter(behavior_behavior == i)
#   step_type_numeric <- i %>%
#     fct_drop() %>%
#     str_to_lower() %>%
#     str_replace_all(" -> ", "_")
#   saveRDS(ua_steps_i, file.path(ua_data_dir, paste0("ua_steps_",
#     step_type_numeric, ".rds")))
# }
# FIT UNIVARIATE MODELS AT FIXED BANDWIDTHS
#
# start = "flight"
# end = "roost"
#
# step_type <- paste0(start, "_", end)
# ua_data <- readRDS(file.path("Output/Analysis/Movements/SSF/UA_Data",
#   paste0("ua_steps_", step_type, ".rds"))) %>%
#   dplyr::select(-c(datetime)) %>%
#   select_if(function(x) !(all(is.na(x)) | all(x=="")))
#
# # Fit univariate models to find the "approximate" AIC-best bandwidth
# # (i.e., stage-one of a two-stage analysis)
#
# start_covar_cols  <- which(colnames(ua_data) == "lat_utm_end") + 1
# covar_cols <- colnames(ua_data)[start_covar_cols:length(ua_data)]
# covar_cols_types_vec <- str_replace_all(covar_cols, "[:digit:]", "")
# covar_cols_bw_vec <- str_replace_all(covar_cols, "[:^digit:]", "")
# all_models <- data.frame(covar_type = covar_cols_types_vec,
#   bw = covar_cols_bw_vec, aic = NA, coef = NA, opt_bw = NA)
# opt_models <- cbind(data.frame(covar_type = unique(covar_types)), bw = NA)
# covar_types <- unique(covar_cols_types_vec)
#
# for (i in seq_along(covar_types)){
#   covar_type_i <- covar_types[i]
#   colnames_i <- str_subset(colnames(ua_data), covar_type_i)
#   bandwidths_i <- unique(str_replace_all(colnames_i, "[:^digit:]", ""))
#   covar_models_list <- vector("list", length(bandwidths_i))
#   names(covar_models_list) <- as.character(bandwidths_i)
#   for (j in seq_along(bandwidths_i)){
#     covar_bw_name <- paste0(covar_type_i, bandwidths_i[j])
#     model_formula <- as.formula(paste("case ~ ", covar_bw_name,
#       " + strata(step_id)"))
#     covar_model <- survival::clogit(model_formula, data = ua_data,
#       iter.max = 100000)
#     covar_models_list[[j]] <- covar_model
#     row_num = which(all_models$covar_type == covar_type_i &
#       all_models$bw == bandwidths_i[j])
#     print(row_num)
#     covar_mod_aic <- AIC(covar_model) #$aic
#     covar_mod_coef <- as.numeric(coef(covar_model)[1])
#     all_models[row_num, "aic"] <- ifelse(is.null(covar_mod_aic), NA,
#       covar_mod_aic)
#     all_models[row_num, "coef"] <- ifelse(is.null(covar_mod_coef), NA,
#       covar_mod_coef)
#   }
#   #lapply(covar_models_list, summary)
#   bandwidths_x <- seq(0, 3000, by=300)
#   aic_table <- aictab(covar_models_list, second.ord = FALSE) %>% arrange(AIC)
#   aic_table$covar_type <- covar_type_i
#   aic_table$opt_bw <- as.numeric(as.character(aic_table[1,1]))
#   opt_bw <- as.numeric(as.character(aic_table[1,1]))
#   all_models[all_models$covar_type == covar_type_i, "opt_bw"] <- opt_bw
#   opt_models[opt_models$covar_type == covar_type_i, "bw"] <- opt_bw
#   g <- ggplot(aic_table, aes(x = as.numeric(as.character(Modnames)), y = AIC)) +
#     #geom_line(color="red") +
#     geom_point() + xlab("Bandwidth") +
#     ggtitle(covar_type_i) +
#     theme_no_legend
#   if (nrow(aic_table) > 1){
#     g <- g +
#       scale_x_continuous(breaks = as.numeric(bandwidths_x)) +
#       geom_vline(xintercept = opt_bw, color="blue", linetype='dashed') +
#       geom_line(color="red") +
#       annotate("text",
#       x = opt_bw + (diff(range(as.numeric(aic_table$Modnames)))*.025),
#       y = max(aic_table$AIC), label = as.character(opt_bw), color = "blue")
#   }
#   print(g)
#   #SaveGGPlot(file = file.path("Results/Analysis/Plots/Step_Selection/AIC",
#   #  paste0("AIC_BW_", covar_type_i, ".svg")), bg = "transparent")
# }
#
# saveRDS(all_models, file=file.path("Output/Analysis/Movements/SSF/Models",
#   paste0("all_models_", step_type, ".rds")))
# saveRDS(opt_models, file=file.path("Output/Analysis/Movements/SSF/Models",
#   paste0("opt_models_", step_type, ".rds")))
#
# all_models_sort <- all_models %>% arrange(covar_type, aic)
# all_models_top5 <- all_models_sort %>% group_by(covar_type) %>% slice(1:5)
# saveRDS(all_models_top5, "Output/Analysis/Movements/SSF/Models",
#   paste0("top5_models_", step_type, ".rds")))
#
# # Plot all AIC by bandwidths in facet_wrap
# g <- ggplot(all_models, aes(x=bw, y = aic)) +
#   geom_line(color="red") + geom_point() + xlab("Bandwidth") + theme_no_legend +
#   facet_wrap(~covar_type, scales = "free_y") +
#   geom_vline(data = ddply(all_models, "covar_type", summarize,
#     opt_bw_i = min(opt_bw)), aes(xintercept=opt_bw_i), color= "blue") +
#   geom_text(data = ddply(all_models, "covar_type", summarize,
#     opt_bw_i = min(opt_bw), min_aic = min(aic)),
#     mapping=aes(x = opt_bw_i + 300, y = min_aic, label = opt_bw_i),
#     size = 4, color = "blue") +
#   theme(axis.title=element_text(size=12, face="bold")) +
#   theme(axis.text.x=element_text(size=8, angle=50, vjust=0.5)) +
#   theme(axis.text.y=element_text(size=8, vjust=0.5))
# print(g)
# SaveGGPlot(file = file.path("Results/Analysis/Plots/Step_Selection/AIC",
#     paste0("AIC_BW_ALL.svg")), bg = "transparent")

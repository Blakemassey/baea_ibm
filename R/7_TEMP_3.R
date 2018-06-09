# Start of Large 'for loop' for Each 'step_type' (i.e. behavior_behavior)
tic.clearlog()
for (i in seq_along(unique(baea_steps$behavior_behavior))){
  # Subset 'baea_steps' to Step Type -------------------------------------------
  step_type_i <- unique(baea_steps$behavior_behavior)[i]
  step_type_i_name <- str_to_lower(str_replace_all(step_type_i, " -> ", "_"))
  tic(paste0(step_type_i_name, "-NA-NA-NA"), quiet = FALSE,
    func.tic = tic_msg)
  steps_i <- baea_steps %>%
    filter(behavior_behavior == step_type_i) %>%
    tibble::rowid_to_column(., "step_id")
  # Create Movement Kernel for 'step_type' -------------------------------------
  move_pars_i <- move_pars %>% filter(behavior_behavior == step_type_i)
  ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise", "Flight"),
    FALSE, TRUE)
  kernel_i <- CreateMoveKernelWeibullVonMises(
    max_r = NULL,
    cellsize = cell_size,
    mu1 = move_pars_i$mvm_mu1[1],
    mu2 = move_pars_i$mvm_mu2[1],
    kappa1 = move_pars_i$mvm_kappa1[1],
    kappa2 = move_pars_i$mvm_kappa2[1],
    mix = move_pars_i$mvm_prop[1],
    shape = move_pars_i$weibull_shape[1],
    scale = move_pars_i$weibull_scale[1],
    ignore_von_mises = ignore_von_mises)
  r <- (cell_size*((nrow(kernel_i)-1)/2))+(cell_size/2)
  kernel_raster <- raster::raster(kernel_i, xmn=-r, xmx=r, ymn=-r, ymx=r)
  prob_raster <- kernel_raster/raster::cellStats(kernel_raster, stat = "sum")
  prob_raster[prob_raster <= .000001] <- 0
  rm(kernel_i, kernel_raster, move_pars_i, ignore_von_mises, r)

  # Create Movement Probability Rasters for Each Step --------------------------
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

  # Create Dataframes for Available and Used Values ----------------------------
  covariate_df <- setNames(data.frame(matrix(ncol = length(covariate_cols),
    nrow = nrow(steps_i), NA)), covariate_cols)
  used_steps_i <- cbind(case = 1, steps_i, covariate_df)
  avail_steps_i <- cbind(case = 0, steps_i, covariate_df)
  n_total <- length(unique(steps_i$id))*length(covar_types)*length(bandwidths)
  counter <- 0

  # Calculate Kernel-Weighted Covariate Values For Available -------------------

  for (j in seq_along(unique(steps_i$id))){
    id_j <- unique(steps_i$id)[j]
    tic(paste0(step_type_i_name, "-", id_j, "-NA-NA"), quiet = TRUE,
      func.tic = tic_msg)
    avail_steps_ij <- avail_steps_i %>% filter(id == id_j)
    used_steps_ij <- used_steps_i %>% filter(id == id_j)
    steps_ij <- steps_i %>% filter(id == id_j)
    steps_ij_range <- c(range(steps_ij$long_utm), range(steps_ij$lat_utm))
    extent_ij <- extend(extent(alignExtent(extent(steps_ij_range), base,
      snap='out')), 9990)
    for (k in seq_along(covar_types)){
      covar_type_k <- covar_types[k]
      tic(paste0(step_type_i_name, "-", id_j, "-", covar_type_k, "-NA"),
        quiet = TRUE, func.tic = tic_msg)
      covar_layer_k <- which(names(covar_stack) == covar_type_k)
      covar_raster_k <- crop(raster(covar_stack, layer=covar_layer_k),extent_ij)
      covar_matrix_k <- as.matrix(covar_raster_k)
      for(m in seq_along(bandwidths)){
        bw_meters <- bandwidths[m]
        str_ijkm <- paste0(step_type_i_name, "-", id_j, "-", covar_type_k, "-",
          bw_meters)
        cat(paste0("Starting: (", (counter <- counter + 1), " of ", n_total,
          ") ", str_ijkm,"\n"))
        tic(str_ijkm, quiet = TRUE, func.tic = tic_msg)
        bw_pixel <- bandwidths[m]/cell_radius # smoothie needs units in pixels
        col_name <- paste0(covar_type_k, bw_meters)
        col_num_used <- which(colnames(used_steps_i) == col_name)
        col_num_avail <- which(colnames(used_steps_i) == col_name)
        if(bw_meters == 0){
          covar_raster_smooth_m <- covar_raster_k
        } else {
          covar_matrix_smooth_m <- kernel2dsmooth(covar_matrix_k,
            kernel.type="gauss", nx=nrow(covar_matrix_k),
            ny=ncol(covar_matrix_k), sigma = bw_pixel)
          covar_raster_smooth_m <- raster(covar_matrix_smooth_m,
            template = covar_raster_k)
          rm(covar_matrix_smooth_m)
        }
        for (p in seq_len(nrow(avail_steps_ij))){
          avail_steps_ijp_xy <- c(avail_steps_ij[p,] %>% pull(long_utm),
            avail_steps_ij[p,] %>% pull(lat_utm))
          step_id_p <- paste0("step_", avail_steps_ij[p, "step_id"])
          move_prob_raster_p <- shift(move_prob_rasters[[step_id_p]],
            x = avail_steps_ijp_xy[1], y = avail_steps_ijp_xy[2])
          covar_raster_smooth_p <- crop(covar_raster_smooth_m,
            move_prob_raster_p)
          covar_kernel_sum <- sum(as.matrix(covar_raster_smooth_p) *
              as.matrix(move_prob_raster_p))
          row_num <- which(avail_steps_i$step_id == avail_steps_ij[p,"step_id"])
          avail_steps_i[row_num, col_num_avail] <- covar_kernel_sum
          rm(avail_steps_ijp_xy, covar_raster_smooth_p, covar_kernel_sum,
            move_prob_raster_p, p, row_num, step_id_p)
        }
        for (q in seq_len(nrow(used_steps_ij))){
          used_steps_ijq_xy <- data.frame(x = used_steps_ij[q,] %>%
            pull(long_utm_end), y = avail_steps_ij[q,] %>% pull(lat_utm_end))
          covar_value <- extract(covar_raster_smooth_m, used_steps_ijq_xy)
          row_num <- which(used_steps_i$step_id == used_steps_ij[q, "step_id"])
          used_steps_i[row_num, col_num_used] <- covar_value
          rm(used_steps_ijq_xy, covar_value, row_num)
        }
        rm(bw_meters, str_ijkm, bw_pixel, col_num_avail, col_num_used,
          covar_raster_smooth_m)
        toc(quiet = FALSE, log = TRUE, func.toc = toc_msg, info = "Finished")
      }
      rm(covar_type_k, covar_layer_k, covar_raster_k, covar_matrix_k)
      toc(quiet = FALSE, log = TRUE, func.toc = toc_msg, info = "Finished")
    }
    rm(id_j, avail_steps_ij, used_steps_ij, steps_ij, steps_ij_range, extent_ij)
    toc(quiet = FALSE, log = TRUE, func.toc = toc_msg, info = "Finished")
  }

  rm(counter, n_total,  covariate_df, steps_i, move_prob_rasters)
  toc(quiet = FALSE, log = TRUE, func.toc = toc_msg, info = "Finished")
  saveRDS(avail_steps_i, paste0("Results/Analysis/Tables/SSF/avail_steps_",
    step_type_i_name, ".rds"))
  saveRDS(used_steps_i, paste0("Results/Analysis/Tables/SSF/used_steps_",
    step_type_i_name, ".rds"))
  #rm(avail_steps_i, used_steps_i, step_type_i_name)
}
avail_tictoc_txt <- unlist(tic.log(format = TRUE))
avail_tictoc_df <- data.frame(stringr::str_split(avail_tictoc_txt, "\\-|\\:",
  simplify = TRUE))
avail_tictoc_df[avail_tictoc_df == "NA"] <- NA
View(avail_tictoc_df)
#saveRDS(avail_tictoc_df, paste0("Results/Analysis/Tables/avail_steps_",
#  "tictoc_1.rds"))
#1 = cruise->perch, nest->roost
tic.clearlog()






## Using custom callbacks in tic/toc
tic_msg <- function(tic, msg) {
  if (is.null(msg) || is.na(msg) || length(msg) == 0){
    outmsg <- paste(lubridate::duration(round(toc - tic)))
  } else {
    outmsg <- paste0("Starting ", msg)
  }
}
toc_msg <- function(tic, toc, msg, info) {
  tt_duration <- lubridate::duration(round(toc - tic))
  if (is.null(msg) || is.na(msg) || length(msg) == 0) {
    outmsg <- paste0(tt_duration)
  } else {
    outmsg <- paste0(info,"- ", tt_duration)
  }
}

vec_i <- 1:2
vec_j <- 1:3

tic.clearlog()
for (i in seq_along(vec_i)){
  tic(paste0(i, "- "), quiet = FALSE, func.tic = tic_msg)
  Sys.sleep(2)
  cat(paste0(i, "\n"))
  for (j in seq_along(vec_j)){
    tic(paste0(i, "-", j), quiet = FALSE, func.tic = tic_msg)
    Sys.sleep(1)
    toc(quiet = FALSE, log = TRUE, func.toc = toc_msg, info = "FINISHED")
  }
  toc(quiet = FALSE, log = TRUE, func.toc = toc_msg, info = "FINISHED")
}

(log.txt <- unlist(tic.log(format = TRUE)))

str_split(log.txt, c("\\-","\\:"), simplify = TRUE)

tictoc_df <- data.frame(stringr::str_split(log.txt, "\\-|\\:", simplify = TRUE))
View(tictoc_df)
tic.clearlog()

df <- data.frame(x = 1:10, y=LETTERS[1:10])
seq_len(nrow(df))



tic.clearlog()
for (x in 1:10) {
  tic(x)
  Sys.sleep(1)
  toc(log = TRUE, quiet = TRUE)
}
(log.txt <- unlist(tic.log(format = TRUE)))
tic.clearlog()







 "(", (counter <- counter + 1), " of ", n_total, ") ",






## Measure the time it takes to dispatch a simple function call
## compared to simply evaluating the constant \code{NULL}
library(microbenchmark)
f1 <- function(){
  x <- 100/3
  return(x)
  }

res <- microbenchmark(f1(), times=1000L)

## Print results:
print(res)

## Plot results:
boxplot(res)


sum1 <- function(){
  sum(as.matrix(covar_raster_smooth_p) * as.matrix(move_prob_raster_p_shift))
}

sum2 <- function(){
  cellStats(covar_raster_smooth_p * move_prob_raster_p_shift, "sum")
}


res <- microbenchmark(sum1(), sum2(), times = 1000L)


install.packages("microbenchmark")
        sum(as.matrix(covar_raster_smooth_p) * as.matrix(move_prob_raster_p_shift))
        covar_kernel_weighted_product_sum <- cellStats(covar_raster_smooth_p * move_prob_raster_p_shift, "sum")

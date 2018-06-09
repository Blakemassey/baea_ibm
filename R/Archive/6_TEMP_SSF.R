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

  # Calculate Kernel-Weighted Covariate Values For Available and Used ----------
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

## Using custom callbacks in tic/toc -------------------------------------------
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


# Create "Control" destination cells based on Movement Kernel probability
n_cc <- 10 # number of FALSE cases per TRUE case

cc_df <- data.frame(id = rep(baea_steps_type$id, each = n_cc),
  datetime = rep(baea_steps_type$datetime, each = n_cc),
  step_id = rep(baea_steps_type$step_id, each = n_cc),
  long_utm = rep(baea_steps_type$long_utm, each = n_cc),
  lat_utm = rep(baea_steps_type$lat_utm, each = n_cc),
  exp_angle = rep(baea_steps_type$exp_angle, each = n_cc),
  dx = NA, dy = NA, long_utm_rot = NA, lat_utm_rot = NA,
    case=FALSE)

tic()
for (i in 1:nrow(cc_df)){
  long_utm <- cc_df[i, "long_utm"]
  lat_utm <- cc_df[i, "lat_utm"]
  exp_angle <- cc_df[i, "exp_angle"]
  destination_cell <- suppressWarnings(sampling::strata(data=
    data.frame(cell=1:raster::ncell(prob_raster)), stratanames=NULL, size=1,
    method="systematic", pik=prob_raster@data@values))
  xy <- raster::xyFromCell(prob_raster, destination_cell[1,1])
  xy_rot <- as.data.frame(Rotation(xy, exp_angle))
  colnames(xy_rot) <- c("x", "y")
  xy_rot_centered <- CenterXYWithBase(xy_rot, base)
  cc_df[i, "dx"] <- xy[1]
  cc_df[i, "dy"] <- xy[2]
  cc_df[i, "dx_rot"] <- xy_rot[1]
  cc_df[i, "dy_rot"] <- xy_rot[2]
  cc_df[i, "long_utm_end"] <- long_utm + xy_rot_centered$x[1]
  cc_df[i, "lat_utm_end"] <- lat_utm + xy_rot_centered$y[1]
}
toc()

# Plot destination_xy
xy_raster <- rasterize(as.matrix(cc_df[, c("dx", "dy")]), prob_raster,
  fun='count', background=0)
plot(xy_raster)

# Join step data and case control data
baea_steps_type_cc <- rbind.fill(baea_steps_type, cc_df) %>% arrange(step_id)

# Plot destination_xy with 2D density
options(stringsAsFactors = TRUE)
origin <- data.frame(x = 0, y = 0)
ggplot(baea_steps_type_cc %>% filter(case == FALSE), aes(x=dx, y=dy)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", contour=TRUE) +
  scale_fill_distiller(type = "seq", palette = "YlOrBr", direction = 1,
    guide = FALSE) +
  geom_point(size = .75, color = "blue2") +
  geom_point(data = origin, aes(x, y), stroke = 1.5, size = 4, colour="black",
    shape = 1) +
  geom_point(data = origin, aes(x, y), stroke = 1.5, colour="black", shape = 4)+
  ggtitle(paste(step_type, "Control Points")) +  xlab("Long") + ylab("Lat") +
  coord_fixed(ratio = 1) + xlim(-8000, 8000) + ylim(-8000, 8000) + xy_theme
SaveGGPlot(paste(str_replace_all(step_type, " ->", ""),
  "Control Points All.png"), "Results/Analysis/Plots/Step_Selection")
options(stringsAsFactors = FALSE)

origin <- data.frame(x = 0, y = 0)

ConvertUnitRadianToStandard <- function(x){
  x <- x
  if (x >=0 && x <= .5*pi) {
    x <- (.5*pi)-x
  } else if (x > .5*pi && x <= pi) {
    x <- (2*pi)-(x-(.5*pi))
  } else if (x > pi && x <= 1.5*pi) {
    x <- (1.5*pi)-pi-x
  } else if (x > 1.5*pi && x <= 2*pi) {
    x <- pi-(x-1.5*pi)
  }
  return(x)
}

baea_steps_type_cc_ex <- baea_steps_type_cc %>% filter(step_id == 2)

baea_steps_type_cc_ex_f <- baea_steps_type_cc_ex %>%
  filter(case == FALSE) %>%
  filter(step_id == 2) %>%
  mutate(
    rad_start1 = atan2(dy, dx),
    r = sqrt((dx^2) + (dy^2)),
    rad_end1 = (atan2(dy_rot, dx_rot))) %>%
  mutate(rad_start2 = if_else(rad_start1 <= 0, (2*pi) + rad_start1, rad_start1),
    rad_end2 = if_else(rad_end1 <= 0,(2*pi) + rad_end1, rad_end1)) %>%
  mutate(rad_start3 = ConvertUnitRadianToStandard(rad_start2),
    rad_end3 = ConvertUnitRadianToStandard(rad_end2)) %>%
  mutate(rad_start = if_else(rad_start3 > 2*pi, rad_start3 - (2*pi) , rad_start3),
    rad_end = if_else(rad_end3 > 2*pi,rad_end3 - (2*pi), rad_end3))


baea_steps_type_cc_ex_f <- baea_steps_type_cc_ex %>%
  filter(case == FALSE) %>%
  filter(step_id == 2) %>%
  mutate(
    rad_start1 = atan2(dy, dx),
    r = sqrt((dx^2) + (dy^2)),
    rad_end1 = rad_start1 + exp_angle) %>%
  mutate(rad_start2 = if_else(rad_start1 <= 0, (2*pi) + rad_start1, rad_start1),
    rad_end2 = if_else(rad_end1 <= 0,(2*pi) + rad_end1, rad_end1)) %>%
  mutate(rad_start3 = ConvertUnitRadianToStandard(rad_start2),
    rad_end3 = ConvertUnitRadianToStandard(rad_end2)) %>%
  mutate(rad_start = if_else(rad_start3 > 2*pi, rad_start3 - (2*pi) , rad_start3),
    rad_end = if_else(rad_end3 > 2*pi,rad_end3 - (2*pi), rad_end3))


# Plot Control Points Example
ggplot(baea_steps_type_cc_ex, aes(dx,dy)) +
  geom_point(size = 2, color = "blue2") +
  geom_point(data = origin, aes(x, y), stroke = 1.5, size = 4, colour="black",
    shape = 1) +
  geom_point(data = origin, aes(x, y), stroke = 1.5, colour="black", shape = 4)+
  geom_point(data = baea_steps_type_cc_ex %>% filter(case == TRUE),
    aes(dx, dy), stroke = 1.5, colour="orange", shape = 16) +
  ggtitle(paste(step_type, "Control Points")) +
  coord_fixed(ratio = 1) + xlim(-8000, 8000) + ylim(-8000, 8000) + xy_theme
SaveGGPlot(paste(str_replace_all(step_type, " ->", ""),
  "Control Points Example.png"), "Results/Analysis/Plots/Step_Selection")


# Plot Control Points Example Rotated
ggplot(baea_steps_type_cc_ex_f) +
  geom_point(aes(dx, dy), size = 2, color = "blue2") +
  geom_point(aes(dx_rot, dy_rot), size = 2, color = "darkgreen") +
  geom_arc(aes(x0 = 0, y0 = 0, r = r, start = rad_start, end = rad_end)) +
  geom_point(data = origin, aes(x, y), stroke = 1.5, size = 4,
    colour="black", shape = 1) +
  geom_point(data = origin, aes(x, y), stroke = 1.5, colour="black",
    shape = 4) + xlab("dx") + ylab("dy") +
  geom_point(data = baea_steps_type_cc_ex %>% filter(case == TRUE),
    aes(dx, dy), stroke = 1.5, colour="orange", shape = 16) +
  ggtitle(paste(step_type, "Control Points")) +
  coord_fixed(ratio = 1) + xlim(-8000, 8000) + ylim(-8000, 8000) + xy_theme
SaveGGPlot(paste(str_replace_all(step_type, " ->", ""),
  "Control Points Example Rotated.png"),"Results/Analysis/Plots/Step_Selection")

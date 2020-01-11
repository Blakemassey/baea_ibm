testing = TRUE
if (isTRUE(testing)){
  i <- 1
  move_pars_i <- move_pars[i, ]
  ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise",
    "Flight"), FALSE, TRUE)
  max_r = NULL
  cellsize = 30
  pars = NULL
  mu1 = move_pars_i$mvm_mu1[1]
  mu2 = move_pars_i$mvm_mu2[1]
  kappa1 = move_pars_i$mvm_kappa1[1]
  kappa2 = move_pars_i$mvm_kappa2[1]
  mix = move_pars_i$mvm_prop[1]
  shape = move_pars_i$weibull_shape[1]
  scale = move_pars_i$weibull_scale[1]
  ignore_von_mises = ignore_von_mises
}

CreateMoveKernelWeibullVonMises2 <- function(max_r = 300,
                                            cellsize = 30,
                                            pars = NULL,
                                            mu1,
                                            mu2,
                                            kappa1,
                                            kappa2,
                                            mix,
                                            shape,
                                            scale,
                                            ignore_von_mises = FALSE,
                                            ignore_weibull = FALSE) {


  if(!is.null(pars)){
    mu1 = pars$mvm_mu1[1]
    mu2 = pars$mvm_mu2[1]
    kappa1 = pars$mvm_kappa1[1]
    kappa2 = pars$mvm_kappa2[1]
    mix = pars$mvm_prop[1]
    shape = pars$weibull_shape[1]
    scale = pars$weibull_scale[1]
  }
  if(is.null(max_r)) max_r <- qweibull(.99, shape, scale) #* 1000
  max_r_cells <- ceiling(max_r/cellsize)
  size <- max_r_cells * 2 + 1
  center <- max_r_cells + 1
  angle_matrix <- new("matrix", 0, size, size)
  row_matrix <- row(angle_matrix)
  col_matrix <- col(angle_matrix)
  distance_matrix <- new("matrix", 0, size, size)
  weibull_kernel <- new("matrix", 0, size, size)
  dx <- row_matrix - center
  dy <- col_matrix - center
  abs_angle <- atan2(dx, dy)
  angle_matrix <- ifelse(abs_angle < 0, (2*pi) + abs_angle, abs_angle)
  mvm_kernel <- suppressWarnings(CircStats::dmixedvm(angle_matrix,
    mu1=mu1, mu2=mu2, kappa1=kappa1, kappa2=kappa2, p=mix))
  mvm_kernel <- apply(mvm_kernel, 2, rev)
  distance_matrix <- (sqrt((row_matrix - center)^2 + (col_matrix - center)^2) *
      cellsize) #/ 1000
  weibull_kernel[] <- dweibull(as.vector(distance_matrix), shape=shape,
    scale=scale)
  weibull_kernel[center, center] <- 0 # probabilty at cell cell = Inf
  #sum(distance_matrix > (max_r))
  absolute_distance_matrix <- distance_matrix
  distance_matrix[distance_matrix > (max_r)] <- NA # (max_r/1000)] <- NA
  distance_matrix[!is.na(distance_matrix)] <- 1
  distance_matrix[is.na(distance_matrix)] <- 0
  distance_matrix[center, center] <- 0 # forces agent to move from center cell
  mvm_kernel <- distance_matrix*mvm_kernel
  weibull_kernel <- distance_matrix*weibull_kernel
  # This last part deletes the cells at the edge if they are all zero
  # if (all(mvm_kernel[1, ] == 0, mvm_kernel[, 1] == 0,
  #   mvm_kernel[nrow(mvm_kernel),] == 0, mvm_kernel[, ncol(mvm_kernel)]==0)){
  #   mvm_kernel <- mvm_kernel[2:(nrow(mvm_kernel) - 1), 2:(ncol(mvm_kernel)
  #     - 1)]
  # }
  # if (all(weibull_kernel[1, ] == 0, weibull_kernel[, 1] == 0,
  #   weibull_kernel[nrow(weibull_kernel),] == 0, weibull_kernel[,
  #     ncol(weibull_kernel)] == 0)){
  #   weibull_kernel <- weibull_kernel[2:(nrow(weibull_kernel) - 1),
  #     2:(ncol(weibull_kernel) - 1)]
  # }
  # Multiply the two kernels together and re-normalize
  if (ignore_von_mises) mvm_kernel <- 1
  if (ignore_weibull) weibull_kernel <- 1
  move_kernel <- weibull_kernel*mvm_kernel
  move_kernel <- move_kernel/sum(move_kernel)
  move_kernel[absolute_distance_matrix > (max_r)] <- NA
  return(move_kernel)
}

if (isTRUE(testing)){
  r <- (30*((nrow(move_kernel)-1)/2))+(30/2)
  kernel_raster <- raster::raster(move_kernel, xmn=-r, xmx=r, ymn=-r, ymx=r)
  raster::plot(kernel_raster, colNA = "yellow")
}

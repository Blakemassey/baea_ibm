
#' UpdateSpatialBAEA <- function(sim = sim,
#'                           init = TRUE) {
#'   sim <- sim
#'   if (init == TRUE) {
#'     spatial <- sim$spatial
#'     base <- spatial$base
#'     nests <- spatial$nests
#'     # male and female same for now
#'     move_pars <- sim$pars$classes$male$constant$fixed$move_pars %>%
#'       mutate(behavior_num = as.numeric(factor(behavior)),
#'              behavior_next_num = as.numeric(factor(behavior_next))) %>%
#'       mutate(ids = paste0(behavior_num, "_", behavior_next_num))
#'     move_pars_ids <- move_pars$ids
#'     move_kernels <- as.list(setNames(rep(NA, nrow(move_pars)),
#'       move_pars_ids), move_pars_ids)
#'     for (i in 1:nrow(move_pars)){
#'       move_pars_i <- move_pars[i, ]
#'       ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise",
#'         "Flight"), FALSE, TRUE)
#'       kernel_i <- CreateMoveKernelWeibullVonMises(
#'           max_r = NULL,
#'           cellsize = 30,
#'           mu1 = move_pars_i$mvm_mu1[1],
#'           mu2 = move_pars_i$mvm_mu2[1],
#'           kappa1 = move_pars_i$mvm_kappa1[1],
#'           kappa2 = move_pars_i$mvm_kappa2[1],
#'           mix = move_pars_i$mvm_prop[1],
#'           shape = move_pars_i$weibull_shape[1],
#'           scale = move_pars_i$weibull_scale[1],
#'           ignore_von_mises = ignore_von_mises)
#'       r <- (30*((nrow(kernel_i)-1)/2))+(30/2)
#'       kernel_raster <- raster::raster(kernel_i, xmn=-r, xmx=r, ymn=-r, ymx=r)
#'       move_kernels[[i]] <- kernel_raster
#'       names(move_kernels[[i]]) <- paste0(move_pars_i$behavior_behavior[1])
#'     }
#'     male <- NamedList(move_kernels)
#'     female <- NamedList(move_kernels)
#'     classes <- NamedList(male, female)
#'
#'     spatial <- NamedList(base, nests, classes)
#'     sim$spatial <- spatial
#'     return(sim)
#'   } else {
#' #   spatial_timer <- UpdateSpatialTimer(spatial_timer)
#' #   if (spatial_timer == timer_number) UpdateSpatial(); rm(spatial_timer)
#' #   spatial <- append()
#'     #spatial <- sim$spatial
#'     #sim$spatial <- spatial
#'     return(sim)
#'   }
#' }
#'
#'
#' #' CreateMoveKernelWeibullVonMises
#' #'
#' #' Create a movement kernel matrix based on a mixed von Mises distribution
#' #'   for direction and a Weibull distribution for distance.
#' #'
#' #' @usage CreateMoveKernelWeibullVonMises(max_r, cellsize, mu, rho, shape,
#' #'    scale, ignore_cauchy, ignore_weibull)
#' #'
#' #' @param max_r maximum radius of kernel in meters, default = 300
#' #' @param cellsize cell size in meters, default = 30
#' #' @param mu1 mu1 parameter of mixed von Mises distribution, 0 radians is due
#' #'   East because everything is based on the Unit Circle
#' #' @param mu2 mu2 parameter of mixed von Mises distribution, 0 radians is due
#' #'   East because everything is based on the Unit Circle
#' #' @param kappa1 kappa1 parameter of mixed von Mises distribution
#' #' @param kappa2 kappa2 parameter of mixed von Mises distribution
#' #' @param mix mixture (p) parameter of mixed von Mises distribution
#' #' @param shape shape parameter of Weibull distribution
#' #' @param scale scale parameter of Weibull distribution
#' #' @param ignore_von_mises logical, removes mixed von Mises kernel's
#' #'   contribution to output raster. Default is FALSE.
#' #' @param ignore_weibull logical, removes Weibull kernel's contribution to
#' #'   output raster. Default is FALSE.
#' #'
#' #' @return matrix
#' #' @details the Weibull parameters need to be based on distance in meters
#' #'
#' #' @export
#' #'
#'
#' CreateMoveKernelWeibullVonMises <- function(max_r = 300,
#'                                               cellsize = 30,
#'                                               mu1,
#'                                               mu2,
#'                                               kappa1,
#'                                               kappa2,
#'                                               mix,
#'                                               shape,
#'                                               scale,
#'                                               ignore_von_mises = FALSE,
#'                                               ignore_weibull = FALSE) {
#'   if (is.null(max_r)) max_r <- qweibull(.99, shape, scale) #* 1000
#'   max_r_cells <- ceiling(max_r/cellsize)
#'   size <- max_r_cells * 2 + 1
#'   center <- max_r_cells + 1
#'   angle_matrix <- new("matrix", 0, size, size)
#'   row_matrix <- row(angle_matrix)
#'   col_matrix <- col(angle_matrix)
#'   distance_matrix <- new("matrix", 0, size, size)
#'   weibull_kernel <- new("matrix", 0, size, size)
#'   dx <- row_matrix - center
#'   dy <- col_matrix - center
#'   abs_angle <- atan2(dx, dy)
#'   angle_matrix <- ifelse(abs_angle < 0, (2*pi) + abs_angle, abs_angle)
#'   mvm_kernel <- suppressWarnings(CircStats::dmixedvm(angle_matrix,
#'     mu1=mu1, mu2=mu2, kappa1=kappa1, kappa2=kappa2, p=mix))
#'   mvm_kernel <- apply(mvm_kernel, 2, rev)
#'   distance_matrix <- (sqrt((row_matrix - center)^2 + (col_matrix - center)^2) *
#'       cellsize) #/ 1000
#'   weibull_kernel[] <- dweibull(as.vector(distance_matrix), shape=shape,
#'     scale=scale)
#'   weibull_kernel[center, center] <- 0 # probabilty at cell cell = Inf
#'   distance_matrix[distance_matrix > (max_r)] <- NA # (max_r/1000)] <- NA
#'   distance_matrix[!is.na(distance_matrix)] <- 1
#'   distance_matrix[is.na(distance_matrix)] <- 0
#'   distance_matrix[center, center] <- 0 # forces agent to move from center cell
#'   mvm_kernel <- distance_matrix*mvm_kernel
#'   weibull_kernel <- distance_matrix*weibull_kernel
#'   # This last part deletes the cells at the edge if they are all zero
#'   if (all(mvm_kernel[1, ] == 0, mvm_kernel[, 1] == 0,
#'     mvm_kernel[nrow(mvm_kernel),] == 0, mvm_kernel[, ncol(mvm_kernel)]==0)){
#'     mvm_kernel <- mvm_kernel[2:(nrow(mvm_kernel) - 1), 2:(ncol(mvm_kernel)
#'       - 1)]
#'   }
#'   if (all(weibull_kernel[1, ] == 0, weibull_kernel[, 1] == 0,
#'     weibull_kernel[nrow(weibull_kernel),] == 0, weibull_kernel[,
#'       ncol(weibull_kernel)] == 0)){
#'     weibull_kernel <- weibull_kernel[2:(nrow(weibull_kernel) - 1),
#'       2:(ncol(weibull_kernel) - 1)]
#'   }
#'   # Multiply the two kernels together and re-normalize
#'   if (ignore_von_mises) mvm_kernel <- 1
#'   if (ignore_weibull) weibull_kernel <- 1
#'   move_kernel <- weibull_kernel*mvm_kernel
#'   move_kernel <- move_kernel/sum(move_kernel)
#'   return(move_kernel)
#' }
#'
#' library(tictoc)
#'
#' for (i in 1:nrow(move_pars)){
#'   move_pars_i <- move_pars[i, ]
#'   ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise",
#'     "Flight"), FALSE, TRUE)
#'   tic()
#'   kernel_i <- CreateMoveKernelWeibullVonMises(
#'       max_r = NULL,
#'       cellsize = 30,
#'       mu1 = move_pars_i$mvm_mu1[1],
#'       mu2 = move_pars_i$mvm_mu2[1],
#'       kappa1 = move_pars_i$mvm_kappa1[1],
#'       kappa2 = move_pars_i$mvm_kappa2[1],
#'       mix = move_pars_i$mvm_prop[1],
#'       shape = move_pars_i$weibull_shape[1],
#'       scale = move_pars_i$weibull_scale[1],
#'       ignore_von_mises = ignore_von_mises)
#'   r <- (30*((nrow(kernel_i)-1)/2))+(30/2)
#'   kernel_raster <- raster::raster(kernel_i, xmn=-r, xmx=r, ymn=-r, ymx=r)
#'   toc()
#'   df <- data.frame(raster::rasterToPoints(kernel_raster))
#'   names(df)[3] <- "dens"
#'   df$behavior_behavior <- move_pars_i$behavior_behavior
#'   move_dens <- rbind(move_dens, df)
#' }
#'
#' move_pars <- full_join(weibull_pars, von_mises_pars, by=c("behavior",
#'   "behavior_next", "behavior_behavior"))
#'
#' write.csv(move_pars, file="Output/Tables/move_pars.csv") # for Powerpoint
#' readRDS(move_pars, file="Output/Models/move_pars.RDS")
#'
#'

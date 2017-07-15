CreateRedistKernelWeibull1 <- function(max_r = 300,
                                      cellsize = 30,
                                      mu,
                                      rho,
                                      shape,
                                      scale) {
  max_r_cells <- ceiling(max_r/cellsize)
  size <- max_r_cells * 2 + 1
  center <- max_r_cells + 1
  wrpc_kernel <- new("matrix", 0, size, size)
  weibull_kernel <- new("matrix", 0, size, size)
  for (i in 1:size) {
    for (j in 1:size) {
      r = sqrt((i - center)^2 + (j - center)^2) * cellsize
      b = AngleToPoint(center, center, j, i)
      if(r <= max_r){
        wrpc_kernel[i, j] <- suppressWarnings(circular::dwrappedcauchy(b,
          mu=mu, rho=rho))
        weibull_kernel[i, j] <- dweibull(r/1000, shape=shape, scale=scale,
          log=FALSE)
      }
    }
  }
  wrpc_kernel <- apply(wrpc_kernel, 2, rev)
  weibull_kernel[center, center] <- 0
  # This last part deletes the cells at the edge if they are all zero
  if (all(wrpc_kernel[1, ] == 0, wrpc_kernel[, 1] == 0,
    wrpc_kernel[nrow(wrpc_kernel),] == 0, wrpc_kernel[, ncol(wrpc_kernel)] ==0))
    wrpc_kernel <- wrpc_kernel[2:(nrow(wrpc_kernel) - 1), 2:(ncol(wrpc_kernel)
      - 1)]
  if (all(weibull_kernel[1, ] == 0, weibull_kernel[, 1] == 0,
    weibull_kernel[nrow(weibull_kernel),] == 0, weibull_kernel[,
      ncol(weibull_kernel)] == 0))
    weibull_kernel <- weibull_kernel[2:(nrow(weibull_kernel) - 1),
      2:(ncol(weibull_kernel) - 1)]
  # Multiply the two kernels together and re-normalize
  redist_kernel <- weibull_kernel*wrpc_kernel
  redist_kernel <- redist_kernel/sum(redist_kernel)
  return(redist_kernel)
}

CreateRedistKernelWeibull2 <- function(max_r = 300,
                                      cellsize = 30,
                                      mu,
                                      rho,
                                      shape,
                                      scale) {
  max_r_cells <- ceiling(max_r/cellsize)
  size <- max_r_cells * 2 + 1
  center <- max_r_cells + 1
  angle_matrix <- new("matrix", 0, size, size)

  # row_matrix <- row(angle_matrix)
  # col_matrix <- col(angle_matrix)
   row_matrix <- new("matrix", 0, size, size)
  col_matrix <- new("matrix", 0, size, size)
  i <- j <-  1:size
  row_matrix[] <- rep(i, times=max(j))
  col_matrix <- t(row_matrix)

  distance_matrix <- new("matrix", 0, size, size)
  weibull_kernel <- new("matrix", 0, size, size)

  dx <- row_matrix - center
  dy <- col_matrix - center
  abs_angle <- atan2(dx, dy)
  angle_matrix <- ifelse(abs_angle < 0, (2*pi) + abs_angle, abs_angle)
  wrpc_kernel <- suppressWarnings(circular::dwrappedcauchy(angle_matrix,
    mu=mu, rho=rho))
  wrpc_kernel <- apply(wrpc_kernel, 2, rev)
  distance_matrix <- (sqrt((row_matrix - center)^2 + (col_matrix - center)^2) *
      cellsize) / 1000
  max_r_limits <- distance_matrix*1000
  max_r_limits[max_r_limits <= max_r] <- 1
  max_r_limits[max_r_limits > max_r] <- 0
  weibull_kernel[] <- dweibull(as.vector(distance_matrix), shape=shape,
    scale=scale)
  weibull_kernel[center, center] <- 0  # Forces agent to move from current cell
  weibull_kernel <- weibull_kernel*max_r_limits
  wrpc_kernel <- wrpc_kernel*max_r_limits
  # This last part deletes the cells at the edge if they are all zero
  if (all(wrpc_kernel[1, ] == 0, wrpc_kernel[, 1] == 0,
    wrpc_kernel[nrow(wrpc_kernel),] == 0, wrpc_kernel[, ncol(wrpc_kernel)] ==0))
    wrpc_kernel <- wrpc_kernel[2:(nrow(wrpc_kernel) - 1), 2:(ncol(wrpc_kernel)
      - 1)]
  if (all(weibull_kernel[1, ] == 0, weibull_kernel[, 1] == 0,
    weibull_kernel[nrow(weibull_kernel),] == 0, weibull_kernel[,
      ncol(weibull_kernel)] == 0))
    weibull_kernel <- weibull_kernel[2:(nrow(weibull_kernel) - 1),
      2:(ncol(weibull_kernel) - 1)]
  # Multiply the two kernels together and re-normalize
  redist_kernel <- weibull_kernel*wrpc_kernel
  redist_kernel <- redist_kernel/sum(redist_kernel)
  return(redist_kernel)
}

CreateRedistKernelWeibull <- function(max_r = 300,
                                      cellsize = 30,
                                      mu,
                                      rho,
                                      shape,
                                      scale) {
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
  wrpc_kernel <- suppressWarnings(circular::dwrappedcauchy(angle_matrix,
    mu=mu, rho=rho))
  wrpc_kernel <- apply(wrpc_kernel, 2, rev)
  distance_matrix <- (sqrt((row_matrix - center)^2 + (col_matrix - center)^2) *
      cellsize) / 1000
  max_r_limits <- distance_matrix*1000
  max_r_limits[max_r_limits <= max_r] <- 1
  max_r_limits[max_r_limits > max_r] <- 0
  weibull_kernel[] <- dweibull(as.vector(distance_matrix), shape=shape,
    scale=scale)
  weibull_kernel[center, center] <- 0  # Forces agent to move from current cell
  weibull_kernel <- weibull_kernel*max_r_limits
  wrpc_kernel <- wrpc_kernel*max_r_limits
  # This last part deletes the cells at the edge if they are all zero
  if (all(wrpc_kernel[1, ] == 0, wrpc_kernel[, 1] == 0,
    wrpc_kernel[nrow(wrpc_kernel),] == 0, wrpc_kernel[, ncol(wrpc_kernel)] ==0))
    wrpc_kernel <- wrpc_kernel[2:(nrow(wrpc_kernel) - 1), 2:(ncol(wrpc_kernel)
      - 1)]
  if (all(weibull_kernel[1, ] == 0, weibull_kernel[, 1] == 0,
    weibull_kernel[nrow(weibull_kernel),] == 0, weibull_kernel[,
      ncol(weibull_kernel)] == 0))
    weibull_kernel <- weibull_kernel[2:(nrow(weibull_kernel) - 1),
      2:(ncol(weibull_kernel) - 1)]
  # Multiply the two kernels together and re-normalize
  redist_kernel <- weibull_kernel*wrpc_kernel
  redist_kernel <- redist_kernel/sum(redist_kernel)
  return(redist_kernel)
}

library(ibmr)
library(raster)
library(tictoc)

max_r = 12000
cellsize = 30
mu = .33*pi
rho = .6
scale = 1.142086
shape = 0.5081443

tic.clearlog()
#tic("redist1")
#redist1 <- CreateRedistKernelWeibull1(max_r=max_r, cellsize=cellsize, mu=mu,
#  rho=rho, shape=shape, scale=scale)
#toc(log = TRUE, quiet = TRUE)
tic("redist2")
redist2 <- CreateRedistKernelWeibull2(max_r=max_r,
  cellsize=cellsize, mu=mu, rho=rho, shape=shape, scale=scale)
toc(log = TRUE, quiet = TRUE)
tic("redist2a")
redist2 <- CreateRedistKernelWeibull2(max_r=max_r,
  cellsize=cellsize, mu=mu, rho=rho, shape=shape, scale=scale)
toc(log = TRUE, quiet = TRUE)

writeLines(unlist(tic.log(format = TRUE)))
tic.clearlog()

identical(redist1, redist2)

r <- (cellsize*((nrow(redist1)-1)/2))+(cellsize/2)
redist1_raster <- raster::raster(redist1, xmn=-r, xmx=r, ymn=-r, ymx=r)
plot(redist1_raster, main = "redist1")
redist2_raster <- raster::raster(redist2, xmn=-r, xmx=r, ymn=-r, ymx=r)
plot(redist2_raster, main = "redist2")

redist_shift <- raster::shift(redist_raster, x=step_data$x[i],
  y=step_data$y[i])

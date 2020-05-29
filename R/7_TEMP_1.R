# con_nest_raster = con_nest_raster
# raster_extent = extent(move_kernel_final)
# pars_gamma = pars_gamma
# pars_rescale = pars_rescale
# x = step_data$x[i]
# y = step_data$y[i]
# x = 370000
# y = 4948000
# base = base

CreateRasterConNestDistProb <- function(con_nest_raster,
                                        raster_extent,
                                        pars_gamma,
                                        pars_rescale,
                                        x,
                                        y,
                                        base){
  gamma_shape <- as.numeric(pars_gamma$shape)
  gamma_rate <- as.numeric(pars_gamma$rate)
  y_min <- pars_rescale$y_min
  y_max <- pars_rescale$y_max
  y_min_new <- pars_rescale$y_min_new
  y_max_new <- pars_rescale$y_max_new

  cellsize <- raster::res(base)[1]

  #plot(con_nest_raster)
  con_nest_crop <- raster::crop(con_nest_raster, raster_extent, snap = 'in')
  #plot(con_nest_crop)

  xy <- CenterXYInCell(x, y, raster::xmin(base), raster::ymin(base),
    raster::res(base)[1])  # May be unnecessary
  xy_pt <- data.frame(x = xy[1], y = xy[2])
  xy_con_nest <- raster::extract(con_nest_crop, xy_pt)

  con_nest_centered <- raster::calc(con_nest_crop,
    fun = function(x){(x - xy_con_nest)/1000})
  #plot(con_nest_centered)
  y_diff_new <- y_max_new - y_min_new

  print(paste("xy_con_nest:", xy_con_nest))

  y_pgamma <- pgamma(xy_con_nest/1000, shape = gamma_shape, rate = gamma_rate)
  y_log_scale <- y_min_new + (((y_pgamma-y_min)/(y_max-y_min)) * (y_diff_new))

#  curve(LogisticByInflection(x, inflection=0, scale=xy_log_scale), -15, 15)
  LogisticByInflection2 <- function(x){
    x <- LogisticByInflection(x, inflection = 0, scale = y_log_scale)
  }
  con_nest_prob <- raster::calc(con_nest_centered, fun = LogisticByInflection2)
  return(con_nest_prob)
}

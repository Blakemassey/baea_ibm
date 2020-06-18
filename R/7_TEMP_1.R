CreateSimLandscapeRasters <- function(con_nest_dist){


  return(landscape)
}


































# UpdateAgentStates <- function(agent_states = NULL,
#                               sim = sim,
#                               init = FALSE) {
#   if (init == TRUE) {
#     input <- sim$agents$input
#     input <- CreateBirthDate(input)
#     input_columns <- colnames(input)
#     na_columns <- c("start_datetime", "died")
#     all <- list()
#     for (i in 1:nrow(input)) {
#       states <- list()
#       for (j in input_columns) states <- append(states, input[i, j])
#       for (k in 1:length(na_columns)) states <- append(states, NA)
#       states <- setNames(states, c(input_columns, na_columns))
#       agent <- NamedList(states)
#       all <- append(all, NamedList(agent))
#     }
#     sim$agents <- append(sim$agents, NamedList(all))
#     return(sim)
#   } else {
#     agent_states <- agent_states
#     return(agent_states)
#   }
# }
#
#
# library(raster)
# x <- raster(matrix(1:(15*25), nrow = 15), xmn = -1000, xmx = 1000,
# ymn = -1000, ymx = 1000)
# crs(x) <-
# plot(x, main="Original")
#  plot(RotateRaster(x, 30, 10), main = paste("Rotated by 30 degrees"))
#  plot(RotateRaster(x, 75, 10), main = paste("Rotated by 75 degrees"))
#  plot(RotateRaster(x, 180, 10), main = paste("Rotated by 180 degrees"))
#  plot(RotateRaster(x, 300, 10), main = paste("Rotated by 300 degrees"))

# con_nest_raster = con_nest_raster
# raster_extent = extent(move_kernel_final)
# pars_gamma = pars_gamma
# pars_rescale = pars_rescale
# x = step_data$x[i]
# y = step_data$y[i]
# x = 370000
# y = 4948000
# base = base

# CreateRasterConNestDistProb <- function(con_nest_raster,
#                                         raster_extent,
#                                         pars_gamma,
#                                         pars_rescale,
#                                         x,
#                                         y,
#                                         base){
#   gamma_shape <- as.numeric(pars_gamma$shape)
#   gamma_rate <- as.numeric(pars_gamma$rate)
#   y_min <- pars_rescale$y_min
#   y_max <- pars_rescale$y_max
#   y_min_new <- pars_rescale$y_min_new
#   y_max_new <- pars_rescale$y_max_new
#
#   cellsize <- raster::res(base)[1]
#
#   #plot(con_nest_raster)
#   con_nest_crop <- raster::crop(con_nest_raster, raster_extent, snap = 'in')
#   #plot(con_nest_crop)
#
#   xy <- CenterXYInCell(x, y, raster::xmin(base), raster::ymin(base),
#     raster::res(base)[1])  # May be unnecessary
#   xy_pt <- data.frame(x = xy[1], y = xy[2])
#   xy_con_nest <- raster::extract(con_nest_crop, xy_pt)
#
#   con_nest_centered <- raster::calc(con_nest_crop,
#     fun = function(x){(x - xy_con_nest)/1000})
#   #plot(con_nest_centered)
#   y_diff_new <- y_max_new - y_min_new
#
#   print(paste("xy_con_nest:", xy_con_nest))
#
#   y_pgamma <- pgamma(xy_con_nest/1000, shape = gamma_shape, rate = gamma_rate)
#   y_log_scale <- y_min_new + (((y_pgamma-y_min)/(y_max-y_min)) * (y_diff_new))
#
# #  curve(LogisticByInflection(x, inflection=0, scale=xy_log_scale), -15, 15)
#   LogisticByInflection2 <- function(x){
#     x <- LogisticByInflection(x, inflection = 0, scale = y_log_scale)
#   }
#   con_nest_prob <- raster::calc(con_nest_centered, fun = LogisticByInflection2)
#   return(con_nest_prob)
# }



y_diff_new <- y_max_new - y_min_new
y_pgamma <- pgamma(xy_con_nest/1000, shape = gamma_shape, rate = gamma_rate)
y_log_scale <- y_min_new + (((y_pgamma-y_min)/(y_max-y_min)) * (y_diff_new))

y_log_scale <- -5

LogisticByInflection2 <- function(x){
  x <- LogisticByInflection(x, inflection = 0, scale = y_log_scale)
}
con_nest_prob <- raster::calc(con_nest_centered, fun = LogisticByInflection2)
mapview::mapview(con_nest_prob)

MovementSubModelBAEA <- function(sim = sim,
                                 agent_states = agent_states,
                                 step_data = step_data,
                                 step = step) {
  sim <- sim
  base <- sim$spatial$base
  cellsize <- raster::res(sim$spatial$base)[1]
  step_start <- lubridate::int_start(step)
  step_end <- lubridate::int_end(step)
  sex <- agent_states$sex
  season <- FindSeasonFromDatetime(datetime = step_start,
    seasons = sim$pars$global$sim_seasons)
  home_return <-
    sim$pars$classes[[sex]]$julian[lubridate::yday(lubridate::int_start(step)),
    2]
  step_max_r <- sim$pars$classes[[sex]]$season[[season]]$step_max_r
  step_cauchy_mu <- sim$pars$classes[[sex]]$constant$fixed$step_cauchy_mu
  step_cauchy_rho <- sim$pars$classes[[sex]]$constant$fixed$step_cauchy_rho
  step_weibull_shape <-
    sim$pars$classes[[sex]]$season[[season]]$step_weibull_shape
  step_weibull_scale <-
    sim$pars$classes[[sex]]$season[[season]]$step_weibull_scale

  connest_gamma_shape <-
    sim$pars$classes[[sex]]$constant$fixed$nestcon_gamma_shape
  connest_gamma_rate <-
    sim$pars$classes[[sex]]$constant$fixed$nestcon_gamma_rate

  con_nest_raster <- sim$spatial$con_nest_raster[[agent_states$nest_id]]

#  homerange_kernel <- sim$spatial$homerange_kernel[[agent_states$nest_id]]
#  landcover <- sim$spatial$landcover
#  hydro_dist <- sim$spatial$hydro_dist

  if (nrow(step_data) == 1) {
    i <- 1
    step_data[i+1, "datetime"] <- lubridate::int_end(step)
    step_data[1, "exp_angle"] <- sample(x=seq(from=0, to=(2*pi), by=(2*pi/360)),
      size=1)
    go_home <- FALSE
  } else {
    i <- nrow(step_data)
    step_data[i+1, "datetime"] <- lubridate::int_end(step)
    step_data$exp_angle[i] <- step_data$abs_angle[i-1]
  #  go_home <- rbinom(1, 1, home_return)
    go_home <- FALSE
  }
  if (go_home == TRUE) {
    home_xy <- c(agent_states$start_x, agent_states$start_y)
    step_data$x[i+1] <- home_xy[[1]]
    step_data$y[i+1] <- home_xy[[2]]
    step_data$abs_angle[i] <- CalculateAngleToPoint(step_data$x[i],
      step_data$y[i], step_data$x[i+1], step_data$y[i+1])
    step_data$step_length[i] <- as.integer(sqrt((step_data[i, "x"] -
      step_data[i+1, "x"])^2 + (step_data[i, "y"]-step_data[i+1, "y"])^2))
  } else {
    redist <- CreateRedistKernelWeibull(max_r=step_max_r, cellsize=cellsize,
      mu=step_data$exp_angle[i], rho=step_cauchy_rho, shape=step_weibull_shape,
      scale=step_weibull_scale)
    r <- (cellsize*((nrow(redist)-1)/2))+(cellsize/2)
    redist_raster <- raster::raster(redist, xmn=-r, xmx=r, ymn=-r, ymx=r)
    redist_shift <- raster::shift(redist_raster, x=step_data$x[i],
      y=step_data$y[i])
    ### PLACE TO ADD IN OTHER PROBABILITY LAYERS

    print(paste("x:", step_data$x[i], " y:", step_data$y[i]))

    redist_shift <- raster::crop(redist_shift, base, snap="in")
    ### NEW ConNestProb Raster
    con_nest <- CreateConNestProb(con_nest_raster,
      gamma_shape=connest_gamma_shape, gamma_rate=connest_gamma_rate,
      x=step_data$x[i], y=step_data$y[i], max_r=step_max_r, cellsize=cellsize,
      base=base)
    print(paste0("con_nest:", as.vector(raster::extent(con_nest)),
      "redist_shift:", as.vector(raster::extent(redist_shift))))
    con_nest_crop <- raster::crop(con_nest, redist_shift, snap="out")
#    landcover_crop <- crop(landcover, redist_shift, snap="out")
#    hydro_dist_crop <- crop(hydro_dist, redist_shift, snap="out")
#    homerange_crop <- crop(homerange_kernel, redist_shift, snap="out")
#    prob_raster <- overlay(redist_shift, landcover_crop, hydro_dist_crop,
#      homerange_crop, fun=function(a,b,c,d) {return(a*b*c*d)}, recycle=FALSE)
#    prob_raster <- prob_raster/cellStats(prob_raster, stat="sum")
#    prob_raster <- redist_shift
#    prob_raster <- prob_raster/cellStats(prob_raster, stat="sum")
    prob_raster <- raster::overlay(redist_shift, con_nest_crop,
      fun=function(a,b){return(a*b)}, recycle=FALSE)
    prob_raster <- prob_raster/raster::cellStats(prob_raster, stat="sum")
    print("prob_min:", raster::minValue(prob_raster))
  #  prob_raster[prob_raster <= .000001] <- 0

    raster::crs(prob_raster) <- raster::crs(sim$spatial$base)
#    ExportKMLRasterOverlayWithTime(raster = prob_raster, time = step,
#      alpha = .8, color_pal= jet2.col(20),
#      outfile = paste0(agent_states$id, "_", i),
#      output_dir= file.path(getwd(), "Prob_Rasters"))

    ### END OF OTHER PROBABILITY LAYERS

#    plot(prob_raster)

    destination_cell <- suppressWarnings(sampling::strata(data=data.frame(cell=
      1:ncell(prob_raster)), stratanames=NULL, size=1, method="systematic",
      pik=prob_raster@data@values))

    while(is.na(destination_cell[1,1])) {
      destination_cell <- suppressWarnings(sampling::strata(data=data.frame(
        cell=1:ncell(prob_raster)), stratanames=NULL, size=1,
        method="systematic", pik=prob_raster@data@values))
    }

    print(paste("destination_cell:", destination_cell[1,1]))

    destination_xy <- raster::xyFromCell(prob_raster, destination_cell[1,1])

    print(paste("x:", destination_xy[1], " y:", destination_xy[2]))

    step_data[i+1, "x"] <- destination_xy[1]
    step_data[i+1, "y"] <- destination_xy[2]
    step_data$abs_angle[i] <- CalculateAngleToPoint(step_data$x[i],
      step_data$y[i], step_data$x[i+1], step_data$y[i+1])
    step_data$step_length[i] <- as.integer(sqrt((step_data[i, "x"] -
      step_data[i+1, "x"])^2 + (step_data[i, "y"] - step_data[i+1, "y"])^2))
  }
  step_data[i+1, "id"] <- step_data[i, "id"]
  return(step_data)
}
<environment: namespace:ibmr>

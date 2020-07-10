# sim
# agent_states
# step_data[2, "behavior"] <- 2
# step


MovementSubModelBAEA2 <- function(sim = sim,
                                  agent_states = agent_states,
                                  step_data = step_data,
                                  step = step) {
  sim <- sim
  base <- sim$spatial$base
  cellsize <- raster::res(sim$spatial$base)[1]
  i <- which(step_data$datetime == step)
  current_behavior <- as.numeric(step_data[i, "behavior"])
  next_behavior <- as.numeric(step_data[i + 1, "behavior"])
  behavior_trans <- paste0(current_behavior, "_", next_behavior)
  sex <- agent_states$sex

  if (i == 1) {
    step_data[1, "exp_angle"] <- sample(x=seq(from=0, to=(2*pi), by=(2*pi/360)),
      size=1)
  } else {
    step_data$exp_angle[i] <- step_data$abs_angle[i-1]
  }
  print(paste("behavior_trans:", behavior_trans))

  if (i == nrow(step_data)){
    print(paste("Last step for :", agent_states$id))
    step_type <- "None (last step)"
  } else if (behavior_trans %in% c("3_3", "5_5")){
    step_type <- "None"
  } else if (next_behavior == 3){
    step_type <- "To Nest"
  } else if (behavior_trans == "4_4"){
    perch_perch_pars <- sim$pars$classes$male$constant$fixed$move_pars %>%
      filter(behavior_behavior == "Perch -> Perch")
    bern_p <- perch_perch_pars$bern_p[1]
    step_type <- ifelse(rbinom(1, 1, bern_p), "Move", "None")
  } else {
    step_type <- "Move"
  }
  print(paste("step_type:", step_type))
  if (i == nrow(step_data)){
    step_data$abs_angle[i] <- NA
    step_data$step_length[i] <- NA
  } else if (step_type == "None") { # no movement
    step_data$x[i+1] <- step_data$x[i]
    step_data$y[i+1] <- step_data$y[i]
    step_data$abs_angle[i] <- 0
    step_data$step_length[i] <- 0
  } else if (step_type == "To Nest" & i != nrow(step_data)) {
    home_xy <- c(agent_states$start_x, agent_states$start_y)
    step_data$x[i+1] <- home_xy[[1]]
    step_data$y[i+1] <- home_xy[[2]]
    step_data$abs_angle[i] <- CalculateAngleToPoint(step_data$x[i],
      step_data$y[i], step_data$x[i+1], step_data$y[i+1])
    step_data$step_length[i] <- as.integer(sqrt((step_data[i, "x"] -
      step_data[i+1, "x"])^2 + (step_data[i, "y"]-step_data[i+1, "y"])^2))
  } else if (step_type == "Move"){

    # x <- step_data$x[i] + 1500;y <- step_data$y[i] + 1500
    # step_data$x[i] <- x;step_data$y[i] <- y

    # Move Kernel
    move_org <- sim$spatial$classes[[sex]][["move_kernels"]][[behavior_trans]]
    move_rotated <- RotateRaster(move_org, Rad2Deg(step_data$exp_angle[i]),
      resolution=raster::res(base))
    move_crop <- raster::crop(move_rotated, move_org, snap = "near")
    move_resample <- raster::resample(move_rotated, move_org, method = "ngb")
    move_shift <- raster::shift(move_resample, dx = step_data$x[i],
      dy = step_data$y[i])
    raster::crs(move_shift) <- raster::crs(base)
    move_kernel <- raster::crop(move_shift, base, snap="in")

    # Con_Nest Kernel
    con_nest_raster <- sim$spatial$con_nest_dist[[agent_states$nest_id]]
    pars_gamma <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$gamma
    pars_rescale <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$rescale
    con_nest_prob <- CreateRasterConNestDistProb(con_nest_raster,
      raster_extent = raster::extent(move_kernel), pars_gamma = pars_gamma,
      pars_rescale = pars_rescale, x=step_data$x[i], y=step_data$y[i],
      base= base)
    raster::crs(con_nest_prob) <- raster::crs(base)
    con_nest_kernel <- raster::crop(con_nest_prob, move_kernel, snap = "in")
    con_nest_kernel <- raster::extend(con_nest_kernel, move_kernel, value = NA)

    # Maine_Outline Kernel
    maine_outline <- sim$spatial$landscape$maine_outline[[agent_states$nest_id]]
    maine_outline_kernel <- raster::crop(maine_outline, move_kernel,
      snap = "in")
    maine_outline_kernel <- raster::extend(maine_outline_kernel, move_kernel,
      value = 0)

    # SSF Layer
    ssf_org <- sim$spatial$ssf_layers[[behavior_trans]][[agent_states$nest_id]]
    ssf_kernel <- raster::crop(ssf_org, move_kernel, snap = "in")
    ssf_kernel <- raster::extend(ssf_crop, move_kernel, value = NA)

    # Restrictions for different next_behavior
    # 1 (cruise), 2 (flight) = no restrictions
    # 3 (nest) = not applicable: always flies to nest
    # 4 (perch), 5 (roost) = only on land

    if (next_behavior %in% c(4,5)){
      land <- sim$spatial$landscape$land[[agent_states$nest_id]]
    } else {
     land <- move_kernel
     land[land > 0] <- 1
    }
    land_kernel <- raster::crop(land, move_kernel, snap = "in")
    land_kernel <- raster::extend(land_kernel, move_kernel, value = 0)

    ### FINAL PROBABILITY LAYER (use geometric mean for final probability layer)

    move_kernel_log <- log(move_kernel)
    con_nest_kernel_log <- log(con_nest_kernel)
    land_log <- log(land_kernel)
    maine_outline_log <- log(maine_outline_kernel)
    ssf_kernel_log <- log(ssf_kernel)
    # print(paste0("move_kernel_log:", raster::extent(move_kernel_log)))
    # print(paste0("con_nest_kernel_log:", raster::extent(con_nest_kernel)))
    # print(paste0("land_log:", raster::extent(land_log)))
    # print(paste0("maine_outline_log:", raster::extent(maine_outline_log)))

    kernel_stack <- raster::stack(list(move_kernel_log, con_nest_kernel_log,
      land_log, maine_outline_log, ssf_kernel_log))
    kernel_stack_mean <- raster::calc(kernel_stack, fun = mean, na.rm = TRUE)
    prob_raster <- exp(kernel_stack_mean)
    prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
    prob_raster[prob_raster <= .000001] <- 0
    prob_raster[is.na(prob_raster)] <- 0

    raster::crs(prob_raster) <- raster::crs(sim$spatial$base)
#    ExportKMLRasterOverlayWithTime(raster = prob_raster, time = step,
#      alpha = .8, color_pal= jet2.col(20),
#      outfile = paste0(agent_states$id, "_", i),
#      output_dir= file.path(getwd(), "Prob_Rasters"))

    ### END OF OTHER PROBABILITY LAYERS

    destination_cell <- suppressWarnings(sampling::strata(data=data.frame(cell=
      1:raster::ncell(prob_raster)), stratanames=NULL, size=1,
      method="systematic", pik=prob_raster@data@values))

    while(is.na(destination_cell[1,1])) {
      destination_cell <- suppressWarnings(sampling::strata(data=data.frame(
        cell=1:ncell(prob_raster)), stratanames=NULL, size=1,
        method="systematic", pik=prob_raster@data@values))
    }
    destination_xy <- raster::xyFromCell(prob_raster, destination_cell[1,1])
    step_data[i+1, "x"] <- destination_xy[1]
    step_data[i+1, "y"] <- destination_xy[2]
    step_data$abs_angle[i] <- CalculateAngleToPoint(step_data$x[i],
      step_data$y[i], step_data$x[i+1], step_data$y[i+1])
    step_data$step_length[i] <- as.integer(sqrt((step_data[i, "x"] -
      step_data[i+1, "x"])^2 + (step_data[i, "y"] - step_data[i+1, "y"])^2))

  }
  return(step_data)
}

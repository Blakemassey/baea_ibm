  sex <- "female"
  beta <- as.matrix(sim$pars$classes[[sex]]$constant$fixed$behavior_betas)
  step_row <- 0
  current_behavior <- 4
  current_time_prop <- .9
  next_time_prop <- .95
  current_nest_dist <- 5

  print(paste0("Current behavior: ", current_behavior))
  print(paste0("nest_dist: ", step_data[step_row, "nest_dist"]))

  gamma <- diag(5)
  g <- beta[1, ]  #  g = state transition probabilities intercepts
  g <- g +
    beta[2, ] * current_nest_dist +
    beta[3, ] * cos(2*pi * current_time_prop) +
    beta[4, ] * sin(2*pi * current_time_prop)
  exp(g)
  gamma[!gamma] <- exp(g) # Beta values in non-diagonal locations in matrix
  gamma2 <- t(gamma) # probabilities for state transitions are now in rows
  gamma3 <- gamma2/apply(gamma2, 1, sum) # rows sum to 1
  if(current_time_prop <= .5 & current_behavior != 5){
    gamma3[, 5] <- 0
    gamma3 <- gamma3/apply(gamma3, 1, sum)
    gamma3[, 5] <- 0
  }
  # next control is new - it forces agent to leave roost after .4 time prop
  if(current_time_prop >= .4 & current_time_prop <= .5 & current_behavior == 5){
    gamma3[, 5] <- 0
    gamma3 <- gamma3/apply(gamma3, 1, sum)
    gamma3[, 5] <- 0
  }
  if(current_time_prop > .5 & current_behavior == 5){
    gamma3[, 1:4] <- 0
    gamma3[, 5] <- 1
    #gamma3 <- gamma3/apply(gamma3, 1, sum)
  }
  if(current_behavior == 1){ # prevents 1->5 (Cruise to Roost)
    gamma3[, 5] <- 0
    gamma3 <- gamma3/apply(gamma3, 1, sum)
    gamma3[, 5] <- 0
  }
  if(current_behavior == 5){ # prevents 5->1 (Roost to Cruise)
    gamma3[, 1] <- 0
    gamma3 <- gamma3/apply(gamma3, 1, sum)
    gamma3[, 1] <- 0
  }

  # trans prob. given current behavior
  #next_behavior <- sample(1:5, size = 1, prob = gamma3[current_behavior, ])
  table(sample(1:5, size = 5000, prob = gamma3[current_behavior, ], replace = TRUE))

  print(paste0("Next behavior: ", next_behavior))























# Checking for issues with the step type Nest -> Perch
# Currently, all of the simulated birds seem to file to the edges/corners of the
# redistribution_kernel

base <- sim$spatial$base

# Create Move Kernels for 'Nest -> Perch' around each nest
for (i in c(1, 2)){
  print(i)
  start_x <- sim$agents$input$start_x[i]
  start_y <- sim$agents$input$start_y[i]
  move_org <- sim_out$run_1$spatial$classes$male$move_kernels$`3_4`
  plot(move_org)
  move_rotated <- suppressWarnings(RotateRaster(move_org, Rad2Deg(0),
    resolution=raster::res(base)))
  plot(move_rotated)
  move_crop <- raster::crop(move_rotated, move_org, snap = "near")
  plot(move_crop)
  move_resample <- raster::resample(move_rotated, move_org, method = "ngb")
  plot(move_resample)
  move_shift <- raster::shift(move_resample, dx = start_x,  dy = start_y)
  raster::crs(move_shift) <- raster::crs(base)
  move_kernel <- raster::crop(move_shift, base, snap="in")
  raster::writeRaster(move_kernel, paste0("C:/TEMP/Sim3/move_kernel", i,
    ".tif"), overwrite = TRUE)
  land <- sim$spatial$landscape$land[[sim$agents$input$nest_id[i]]]
  land_kernel <- raster::crop(land, move_kernel, snap = "in")
  land_kernel <- raster::extend(land_kernel, move_kernel, value = 0)
  land_kernel <- raster::mask(land_kernel, move_kernel)
  raster::writeRaster(land_kernel, paste0("C:/TEMP/Sim3/land_kernel", i,
    ".tif"), overwrite = TRUE)
}

# Create SSF Kernels for 'Nest -> Perch' around each nest
for (i in c(1, 3, 5, 7)){
  print(i)
  start_x <- sim$agents$input$start_x[i]
  start_y <- sim$agents$input$start_y[i]
  move_org <- sim_out$
  plot(move_org)
  move_rotated <- suppressWarnings(RotateRaster(move_org, Rad2Deg(0),
    resolution=raster::res(base)))
  plot(move_rotated)
  move_crop <- raster::crop(move_rotated, move_org, snap = "near")
  plot(move_crop)
  move_resample <- raster::resample(move_rotated, move_org, method = "ngb")
  plot(move_resample)
  move_shift <- raster::shift(move_resample, dx = start_x,  dy = start_y)
  raster::crs(move_shift) <- raster::crs(base)
  move_kernel <- raster::crop(move_shift, base, snap="in")
  raster::writeRaster(move_kernel, paste0("C:/TEMP/Sim2/ssf_kernel", i, ".tif"))
}

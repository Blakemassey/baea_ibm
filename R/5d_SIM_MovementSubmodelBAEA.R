

MovementSubModelBAEA4 <- function(sim = sim,
                                  agent_states = agent_states,
                                  step_data = step_data,
                                  step = step) {
  verbose <- FALSE
  plotting <- FALSE
  base <- sim$spatial$base
  cellsize <- raster::res(sim$spatial$base)[1]
  i <- which(step_data$datetime == step)
  current_behavior <- as.numeric(step_data[i, "behavior"])
  next_behavior <- as.numeric(step_data[i + 1, "behavior"])
  behavior_trans <- paste0(current_behavior, "_", next_behavior)
  sex <- agent_states$sex
  home_xy <- c(agent_states$start_x, agent_states$start_y)

  if (i == 1) {
    step_data[1, "exp_angle"] <- sample(x = seq(from = 0, to = (2*pi),
      by = (2*pi/360)), size = 1)
  } else {
    step_data$exp_angle[i] <- step_data$abs_angle[i-1]
  }
  if(verbose) writeLines(paste0("Behavior Trans: ", behavior_trans))

  if (i == nrow(step_data)){
    if(verbose) writeLines(paste("Last step for:", agent_states$id))
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
  if(verbose) writeLines(paste0("step_type: ", step_type))
  if (i == nrow(step_data)){
    step_data$abs_angle[i] <- NA
    step_data$step_length[i] <- NA
  } else if (step_type == "None") { # no movement
    step_data[i+1, "x"] <- step_data[i, "x"]
    step_data[i+1, "y"] <- step_data[i, "y"]
    step_data[i, "abs_angle"] <- 0
    step_data[i, "step_length"] <- 0
    step_data[i+1, "nest_dist"] <- as.integer(sqrt((home_xy[[1]] -
      step_data[i+1, "x"])^2 + (home_xy[[2]] - step_data[i+1, "y"])^2))
  } else if (step_type == "To Nest" & i != nrow(step_data)) {
    step_data$x[i+1] <- home_xy[[1]]
    step_data$y[i+1] <- home_xy[[2]]
    step_data$abs_angle[i] <- CalculateAngleToPoint(step_data$x[i],
      step_data$y[i], step_data$x[i+1], step_data$y[i+1])
    step_data$step_length[i] <- as.integer(sqrt((step_data[i, "x"] -
      step_data[i+1, "x"])^2 + (step_data[i, "y"]-step_data[i+1, "y"])^2))
    step_data[i+1, "nest_dist"] <- as.integer(sqrt((home_xy[[1]] -
      step_data[i+1, "x"])^2 + (home_xy[[2]] - step_data[i+1, "y"])^2))
  } else if (step_type == "Move"){

    # For testing
    if(FALSE) {
      x <- step_data$x[i] + 1500; y <- step_data$y[i] + 1500
      step_data$x[i] <- x; step_data$y[i] <- y
    }
    if(FALSE){
      if(i == 1) step_data$x[i] <- 478445; step_data$y[i] <- 4972105
    }

    # Move Kernel
    move_org <- sim$spatial$classes[[sex]][["move_kernels"]][[behavior_trans]]
    move_org_shift <- raster::shift(move_org, dx = step_data$x[i],
      dy = step_data$y[i])
    move_rotated <- suppressWarnings(RotateRaster(move_org,
      Rad2Deg(step_data$exp_angle[i]), resolution = raster::res(base)))
    move_crop <- raster::crop(move_rotated, move_org, snap = "near")
    move_resample <- raster::resample(move_rotated, move_org, method = "ngb",
      progress = FALSE)
    move_shift <- raster::shift(move_resample, dx = step_data$x[i],
      dy = step_data$y[i])
    raster::crs(move_shift) <- raster::crs(base)
    move_shift_ext <- raster::extend(move_shift, move_org_shift, value = NA)
    move_shift_crop <- raster::crop(move_shift_ext, move_org_shift, snap = "in")
    move_kernel_mask <- raster::mask(move_shift_crop, move_org_shift, value =NA)
    move_kernel <- move_kernel_mask/raster::cellStats(move_kernel_mask,
      stat = "sum")
    move_kernel[move_kernel == 0] <- NA
    if(plotting) plot(move_kernel, colNA = "black")

    # Con_Nest Kernel
    con_nest_raster <- sim$spatial$con_nest_dist[[agent_states$nest_id]]
    if(plotting) plot(con_nest_raster, colNA = "black")
    pars_gamma <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$gamma
    pars_rescale <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$rescale
    con_nest_prob <- CreateRasterConNestDistProb(con_nest_raster,
      raster_extent = raster::extent(move_kernel), pars_gamma = pars_gamma,
      pars_rescale = pars_rescale, x = step_data$x[i], y = step_data$y[i],
      base = base)
    if(plotting) plot(con_nest_prob, colNA = "black")
    raster::crs(con_nest_prob) <- raster::crs(base)
    con_nest_ext <- raster::extend(con_nest_prob, move_kernel, value = NA)
    con_nest_crop <- raster::crop(con_nest_ext, move_kernel, snap = "in")
    con_nest_mask <- raster::mask(con_nest_crop, move_kernel, value = NA)
    con_nest_kernel <- con_nest_mask/raster::cellStats(con_nest_mask,
      stat = "sum")
    con_nest_kernel[con_nest_kernel == 0] <- NA
    if(plotting) plot(con_nest_kernel, colNA = "black")

    # SSF_Kernel
    ssf_org <-sim$spatial$ssf_layers[[`behavior_trans`]][[agent_states$nest_id]]
    ssf_ext <- raster::extend(ssf_org, move_kernel, value = NA)
    ssf_crop <- raster::crop(ssf_ext, move_kernel, snap = "in")
    ssf_mask <- raster::mask(ssf_crop, move_kernel, snap = "in")
    ssf_kernel_mask <- raster::extend(ssf_mask, move_kernel, value = NA)
    ssf_kernel <- ssf_kernel_mask/raster::cellStats(ssf_kernel_mask,
      stat = "sum")
    ssf_kernel[ssf_kernel == 0] <- NA
    if(plotting) plot(ssf_kernel, colNA = "black")

    # Remove NA in any layer from all other layers
    move_kernel <- raster::mask(move_kernel, ssf_kernel, snap = "in")
    move_kernel <- raster::mask(move_kernel, con_nest_kernel, snap = "in")
    ssf_kernel <- raster::mask(ssf_kernel, move_kernel, snap = "in")
    con_nest_kernel <- raster::mask(con_nest_kernel, move_kernel, snap = "in")

    # Plots for visualization checks
    if(plotting) plot(move_kernel, main = "move_kernel", colNA = "black")
    if(plotting) plot(ssf_kernel, main = "ssf_kernel", colNA = "black")
    if(plotting) plot(con_nest_kernel, main = "con_nest_kernel", colNA ="black")

    # Maine outline kernel
    maine_outline <- sim$spatial$landscape$maine_outline[[agent_states$nest_id]]
    maine_outline_ext <- raster::extend(maine_outline, move_kernel, value = NA)
    maine_outline_crop <- raster::crop(maine_outline_ext, move_kernel,
      snap = "in")
    maine_outline_kernel <- raster::mask(maine_outline_crop, move_kernel,
      value = NA)
    if(plotting) plot(maine_outline_kernel, colNA = "black")

    # Land kernel - for perch/roost, to exclude water by including land_kernel
    if (next_behavior %in% c(4,5)){
      land <- sim$spatial$landscape$land[[agent_states$nest_id]]
      land_ext <- raster::extend(land, move_kernel, value = 0)
      land_crop <- raster::crop(land_ext, move_kernel, snap = "in")
      land_kernel <- raster::mask(land_crop, move_kernel, snap = "in")
      land_kernel[is.na(land_kernel)] <- 0
      if(plotting) plot(land_kernel, colNA = "black")
    }

    # Calculate probabilty raster - air to cruise
    if (behavior_trans %in% c("1_1", "2_1")){
      kernel_weights <- c(3, 4, 1)
      destination_cells_n <- 25
    }
    # Calculate probabilty raster - air to flight
    if (behavior_trans %in% c("1_2", "2_2")){
      kernel_weights <- c(3, 3, 2)
      destination_cells_n <- 100
    }
    # Calculate probabilty raster - perch/roost to air
    if (behavior_trans %in% c("4_1", "4_2", "5_2")){
      kernel_weights <- c(3, 3, 2)
      destination_cells_n <- 25
    }
    # Calculate probabilty raster - nest to air
    if (behavior_trans %in% c("3_1", "3_2")){
      kernel_weights <- c(3, 4, 2)
      destination_cells_n <- 25
    }
    # Calculate probabilty raster - air to stationary
    if (behavior_trans %in% c("1_4", "2_4", "2_5")){
      kernel_weights <- c(5, 1, 3)
      destination_cells_n <- 1000
    }
    # Calculate probabilty raster - nest to stationary
    if (behavior_trans %in% c("3_4", "3_5")){
      kernel_weights <- c(5, 1, 3)
      destination_cells_n <- 1000
    }
    # Calculate probabilty raster - stationary to stationary
    if (behavior_trans %in% c("4_4", "5_4", "4_5")){
      kernel_weights <- c(5, 2, 3)
      destination_cells_n <- 1500
    }

    kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
      con_nest_kernel))
    WeightedGeoMean <- function(x){
      geomeans <- geometric_mean(x, w = kernel_weights, na.rm = TRUE)
      return(geomeans)
    }
    kernel_geomeans <- raster::calc(kernel_stack, fun = WeightedGeoMean)

    if (next_behavior %in% c(4,5)){
      prob_raster <- kernel_geomeans * maine_outline_kernel * land_kernel
    } else {
      prob_raster <- kernel_geomeans * maine_outline_kernel
    }
    prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
    if(plotting) plot(prob_raster, colNA = "black")
    prob_raster[is.na(prob_raster)] <- 0
    raster::crs(prob_raster) <- raster::crs(sim$spatial$base)
    if(plotting) plot(prob_raster, colNA = "black")
    if(plotting) raster::hist(prob_raster)
    if(FALSE) raster::writeRaster(prob_raster, "C:/Temp/prob_raster.tif")

    # Write out KML files
    if(FALSE){
      ExportKMLRasterOverlayWithTime(raster = prob_raster, time = step_interval,
        alpha = .8, color_pal= viridis::viridis(20),
        outfile = paste0(agent_states$id, "_", i),
        output_dir = file.path("C:/Temp/sim_20201016-03"))
    }

    # Section for checking on errors (probability raster is all zero)
    prob_raster_cells_gt0 <- raster::cellStats(prob_raster,
      function(i, ...) sum(i > 0))
    if(prob_raster_cells_gt0 == 0){
      saveRDS(step_data, paste0("C:/TEMP/step_data", GetDateTime(), ".rds"))
      prob_raster_cells <- raster::cellStats(prob_raster,
        function(i, ...) sum(!is.na(i)))
      writeLines(paste0("Cells total (n = ", prob_raster_cells,
        "); Cells > 0 (n = ", prob_raster_cells_gt0, ")"))
      writeLines("ZERO probability cells greater than 0")
      writeLines(paste0("Step: ", step))
      writeLines(paste0("Agent: ", agent_states$id))
      writeLines(paste0("Behavior Trans: ", behavior_trans))
      writeLines(paste0("Previous x: ", step_data$x[i-1]))
      writeLines(paste0("Previous y: ", step_data$y[i-1]))
      writeLines(paste0("Current x: ", step_data$x[i]))
      writeLines(paste0("Current y: ", step_data$y[i]))
      plot(ssf_org, main = "ssf_org")
      plot(ssf_ext, main = "ssf_ext")
      plot(ssf_crop, main = "ssf_crop")
      plot(ssf_mask, main = "ssf_mask")
      plot(con_nest_crop, main = 'con_nest_crop')
      plot(move_kernel_mask, main = 'move_kernel_mask')
      plot(con_nest_mask, main = 'con_nest_mask')
      plot(ssf_kernel_mask, main = 'ssf_kernel_mask')
      plot(move_kernel, main = 'move_kernel')
      plot(ssf_kernel, main = 'ssf_kernel')
      plot(con_nest_kernel, main = 'con_nest_kernel')
      plot(prob_raster, colNA = "black", main = 'prob_raster')
      stop()
    }

    # Select destination cell
    destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
        cell = 1:raster::ncell(prob_raster)), stratanames = NULL,
        size = destination_cells_n, method = "systematic",
        pik = prob_raster@data@values)) %>%
      mutate(prob_rank = rank(-Prob, na.last = NA, ties.method = "random"))%>%
      filter(prob_rank == 1) %>%
      pull(ID_unit)
      prob_raster_value <- raster::extract(prob_raster, destination_cell)

    if(verbose) writeLines(paste0("Destination cell: ", destination_cell))
    if(verbose) writeLines(paste0("Value prob_raster: ", prob_raster_value))

    while(is.na(destination_cell) || prob_raster_value == 0) {
      destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
        cell = 1:raster::ncell(prob_raster)), stratanames = NULL,
        size = destination_cells_n, method = "systematic",
        pik = prob_raster@data@values))  %>%
      mutate(prob_rank = rank(-Prob, na.last = NA, ties.method = "random"))%>%
      filter(prob_rank == 1) %>%
      pull(ID_unit)
      prob_raster_value <- raster::extract(prob_raster, destination_cell)
    }
    destination_xy <- raster::xyFromCell(prob_raster, destination_cell)
    step_data[i+1, "x"] <- destination_xy[1]
    step_data[i+1, "y"] <- destination_xy[2]
    step_data[i, "abs_angle"] <- CalculateAngleToPoint(step_data[i, "x"],
      step_data[i, "y"], step_data[i+1, "x"], step_data[i+1, "y"])
    step_data[i, "step_length"] <- as.integer(sqrt((step_data[i, "x"] -
      step_data[i+1, "x"])^2 + (step_data[i, "y"] - step_data[i+1, "y"])^2))
    step_data[i+1, "nest_dist"] <- as.integer(sqrt((home_xy[[1]] -
      step_data[i+1, "x"])^2 + (home_xy[[2]] - step_data[i+1, "y"])^2))
  }
  return(step_data)
}

# For testing
if(FALSE){
  if(TRUE) sim$agents$input <- sim$agents$input %>% slice(c(1,3))
  if(TRUE) sim$pars$global$sim_start <- as.POSIXct("2015-03-15", tz = "UTC")
  if(TRUE) sim$pars$global$sim_end <- as.POSIXct("2015-03-20", tz = "UTC")
  runs = 1
  write = FALSE
  output_dir = getwd()
  i <- j <- k <- m <- n <- o <- 1
  m <- 5
}

RunSimulationBAEA <- function(sim = sim,
                          runs = 1,
                          write = FALSE,
                          output_dir = getwd()) {
  runs <- CreateRunsList(runs)
  for (i in 1:length(runs)){
    rep_intervals <- CreateReportIntervals(sim)
    sim <- UpdateAgentStates(init = TRUE, sim = sim)
    sim <- UpdateAgentStepDataBAEA2(init = TRUE, sim = sim,
      rep_intervals = rep_intervals)
    sim <- UpdateAgentParsData(init = TRUE, sim = sim)
    sim <- UpdateSpatialBAEA(init = TRUE, sim = sim)
    for (j in 1:length(rep_intervals)) {
      step_intervals <- CreateStepIntervals(rep_intervals[[j]],
        step_period = sim$pars$global$step_period)
      for (k in 1:length(step_intervals)) {
        step_interval <- step_intervals[[k]]
        time_steps <- CreateTimeStepsInStepIntervalBAEA2(step_interval, sim=sim)
        for (m in 1:length(time_steps)){
          time_step <- time_steps[[m]]
          writeLines(paste0("Starting time_step: ", time_steps[[m]]))
          alive_seq <- ReturnAliveSeq(sim)
          sim$agents$all <- UpdateAgentParsData(sim$agents$all)
          for (n in alive_seq){
            agent_states <- sim$agents$all[[n]][["states"]]
            step_data <- sim$agents$all[[n]][["step_data"]]
            pars_data <- sim$agents$all[[n]][["pars_data"]]
            if(any(step_data$datetime %within% time_step, na.rm = TRUE)){
              steps <- which(step_data$datetime %within% time_step)
              for (o in steps){
                step <- step_data$datetime[o]
                step_data <- BehaviorSubModelBAEA2(sim, agent_states, step_data,
                  step)
                step_data <- MovementSubModelBAEA4(sim, agent_states, step_data,
                  step)
              }
              sim$agents$all[[n]][["states"]] <- UpdateAgentStates(agent_states)
              sim$agents$all[[n]][["step_data"]]<-UpdateAgentStepData(step_data)
            }
          }
          sim$spatial <- UpdateSpatial(sim$spatial)
        }
      }
    }
    runs[[i]] <- SimplifySimSpatialBAEA(sim)
    WriteSimList(write = write, run = names(runs[j]), sim = sim,
      output_dir = getwd(), components = "all")
  }
  return(runs)
}

BehaviorSubModelBAEA2 <- function(sim = sim,
                                 agent_states = agent_states,
                                 step_data = step_data,
                                 step = step){
  verbose <- FALSE
  sex <- agent_states$sex
  beta <- as.matrix(sim$pars$classes[[sex]]$constant$fixed$behavior_betas)
  step_row <- which(step_data$datetime == step)
  current_behavior <- as.numeric(step_data[step_row, "behavior"])
  current_time_prop <- as.numeric(step_data[step_row, "time_proportion"])
  next_time_prop <- as.numeric(step_data[step_row + 1, "time_proportion"])
  julian <- yday(step_data[step_row, "datetime"])
  step_data <- as.data.frame(step_data)
  time_prop_second_to_last <- step_data %>%
    mutate(day = date(datetime)) %>%
    filter(day == date(step)) %>%
    filter(row_number() == (n() - 1)) %>%
    pull(time_proportion)
  gamma <- diag(5)
  g <- beta[1, ]  #  g = state transition probabilities intercepts
  g <- g +
    beta[2, ] * cos(2*pi * (julian/365)) +
    beta[3, ] * sin(2*pi * (julian/365)) +
    beta[4, ] * cos(2*pi * (current_time_prop)) +
    beta[5, ] * sin(2*pi * (current_time_prop))
  gamma[!gamma] <- exp(g) # Beta values in non-diagonal locations in matrix
  gamma2 <- t(gamma) # probabilities for state transitions are now in rows
  gamma3 <- gamma2/apply(gamma2, 1, sum) # rows sum to 1
  if(current_time_prop <= .5 & current_behavior != 5){
    gamma3[, 5] <- 0
    gamma3 <- gamma3/apply(gamma3, 1, sum)
    gamma3[, 5] <- 0
  }
  # next control is new - it forces agent to leave roost after .3 time prop
  if(current_time_prop >= .3 & current_time_prop <= .5 & current_behavior == 5){
    gamma3[, 5] <- 0
    gamma3 <- gamma3/apply(gamma3, 1, sum)
    gamma3[, 5] <- 0
  }
  if(current_time_prop > .5 & current_behavior == 5){
    gamma3[, 1:4] <- 0
    gamma3[, 5] <- 1
  }
  if(current_behavior == 1){ # prevents 1 -> 5 (Cruise to Roost)
    gamma3[, 5] <- 0
    gamma3 <- gamma3/apply(gamma3, 1, sum)
    gamma3[, 5] <- 0
  }
  if(current_behavior == 5){ # prevents 5 -> 1 (Roost to Cruise)
    gamma3[, 1] <- 0
    gamma3 <- gamma3/apply(gamma3, 1, sum)
    gamma3[, 1] <- 0
  }

  # trans prob. given current behavior
  gamma4 <- gamma3
  next_behavior <- sample(1:5, size = 1, prob = gamma4[current_behavior, ])

  if(step_row != nrow(step_data)) { # prevent the creation of an "extra" step
    if(next_time_prop == time_prop_second_to_last){ # Second to last step
      if (next_behavior != 1){ # For next behavior to be not Cruise
        step_data[step_row + 1, "behavior"] <- next_behavior
      } else { # forces selection of non-Cruise behavior
        gamma4[, 1] <- 0
        gamma4 <- gamma4/apply(gamma4, 1, sum)
        gamma4[, 1] <- 0
        next_behavior <- sample(1:5, size = 1, prob = gamma4[current_behavior,])
        step_data[step_row + 1, "behavior"] <- next_behavior
      }
    }
    if(next_time_prop == 1){ # only Nest or Roost for last location of day
      if(verbose) writeLines("End of day")
      if (next_behavior %in% c(3,5)){
        step_data[step_row + 1, "behavior"] <- next_behavior
      } else { # forces selection of Nest or Roost
        gamma4[, c(1,2,4)] <- 0
        gamma4 <- gamma4/apply(gamma4, 1, sum)
        gamma4[, c(1,2,4)] <- 0
        next_behavior <- sample(1:5, size = 1, prob = gamma4[current_behavior,])
        step_data[step_row + 1, "behavior"] <- next_behavior
      }
    }
    if(current_time_prop == 1){
      step_data[step_row + 1, "behavior"] <- current_behavior
    }
    step_data[step_row + 1, "behavior"] <- next_behavior
  }
  return(step_data)
}

UpdateAgentStepDataBAEA2 <- function(step_data = NULL,
                                    sim = sim,
                                    init = FALSE,
                                    rep_intervals = rep_intervals) {
  if (init == TRUE){
    sim_start <- sim$pars$global$sim_start
    all <- sim$agents$all
    for (i in 1:length(all)) {
      agent <- all[[i]]
      writeLines(paste("Creating initial 'step_data' dataframe for", i, "of",
        length(all)))
      all_time_steps <- as.POSIXct(NA)
      for (j in 1:length(rep_intervals)){
        #j <- 1
        step_intervals <- CreateStepIntervals(rep_intervals[[j]],
          step_period = sim$pars$global$step_period)
        for (k in 1:length(step_intervals)){
          #k <- 1
          time_steps <- CreateTimeStepsBAEA(step_intervals[[k]], agent = agent,
            sim = sim)
          for (m in 1:length(time_steps)){
            all_time_steps <- append(all_time_steps, int_start(time_steps[[m]]))
            if (m == length(time_steps)) all_time_steps <- append(all_time_steps,
              int_end(time_steps[[m]]))
          }
        }
      }   # this is all about getting 'all_time_steps'
      if(is.na(all_time_steps[1])) all_time_steps <- all_time_steps[-1]
      time_steps_df <- data.frame(datetime = all_time_steps) %>%
        mutate(julian = yday(datetime)) %>%
        group_by(julian) %>%
        mutate(day_start = min(datetime),
          day_end = max(datetime),
          day_minutes = as.integer(difftime(day_end,day_start,units="mins"))) %>%
        ungroup() %>%
        mutate(time_after_start = as.integer(difftime(datetime, day_start,
          units="mins"))) %>%
        mutate(time_proportion = time_after_start/day_minutes) %>%
        dplyr::select(datetime, time_proportion)
      step_data <- time_steps_df %>%
        mutate(id = agent$states$id,
          behavior = NA_integer_,
          x = NA_real_,
          y = NA_real_,
          nest_dist = NA_real_,
          exp_angle = NA_real_,
          abs_angle = NA_real_) %>%
        dplyr::select(id, datetime, behavior, x, y, exp_angle, abs_angle,
          nest_dist, time_proportion) %>%
        as.data.frame() # when saved as a tibble, broke BehaviorSubModel
      step_data[1, "behavior"] <- 3
      step_data[1, "nest_dist"] <- 0
      step_data[1, "x"] <- agent$states$start_x
      step_data[1, "y"] <- agent$states$start_y
      agent <- append(agent, NamedList(step_data))
      all[[i]] <- agent
    }
    sim$agents$all <- all
    return(sim)
  } else {
    step_data <- step_data
    return(step_data)
  }
}

PlotProbabilityRaster <- function(prob_raster,
                                  sample_n = 5000){
  #IMPORTANT - Process to plot probability plot
  destination_xy <- tibble(x = vector(mode = "numeric", sample_n),
    y = vector(mode = "numeric", sample_n))
  for (i in seq_len(sample_n)){
    destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
        cell = 1:raster::ncell(prob_raster)), stratanames = NULL, size = 1,
      method = "systematic", pik = prob_raster@data@values))
      while(is.na(destination_cell[1,1])) {
        destination_cell <- suppressWarnings(sampling::strata(data=data.frame(
          cell = 1:raster::ncell(prob_raster)), stratanames = NULL, size = 1,
          method="systematic", pik = prob_raster@data@values))
      }
    destination_xy[i, ] <- raster::xyFromCell(prob_raster, destination_cell[,1])
  }
  destination_raster <- rasterize(destination_xy, prob_raster, field = 1,
    fun = 'sum', background = NA, mask = FALSE, update = FALSE,
    updateValue = 'all', filename = "", na.rm = TRUE)
  if(FALSE) plot(prob_raster)
  if(FALSE) plot(destination_raster)
  Plot3DRaster(destination_raster, col = viridis::viridis(20),
    main = "Probability Plot")
}

CreateTimeStepsInStepIntervalBAEA2 <- function(step_interval = step_interval,
                                              sim = sim) {
  time_step_period = sim$pars$global$time_step_period
  options(lubridate.verbose=FALSE)
  xy_coords <- matrix(c(-67.7778, 44.8012), nrow = 1)
  interval_start_East <- force_tz(lubridate::int_start(step_interval),
    tzone = "US/Eastern")
  interval_end_East <- force_tz(lubridate::int_end(step_interval)-minutes(1),
    tzone = "US/Eastern") #subtracting one minute to make it the same day
  sunrise <- maptools::sunriset(xy_coords, interval_start_East,
    proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
    POSIXct.out= TRUE)[1,2]
  sunset <- maptools::sunriset(xy_coords, interval_end_East,
    proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunset",
    POSIXct.out= TRUE)[1,2]
  step_interval_start <- round_date(sunrise, "minute") - hours(2)
  step_interval_end <- round_date(sunset, "minute") + hours(2)
  end_int <- lubridate::int_end(lubridate::as.interval(time_step_period,
    step_interval_start))
  steps <- tibble(start_time = step_interval_start,
    end_time = end_int - seconds(1)) # sub 1 sec prevents overlap
  while (end_int < step_interval_end) {
    start_int <- end_int
    end_int <- end_int + time_step_period
    steps <- steps %>%
      add_row(start_time = start_int,
               end_time = end_int - seconds(1)) # sub 1 sec prevents overlap
  }
  steps[nrow(steps), "end_time"] <- step_interval_end
  time_step_list <- steps %>%
    mutate(step_interval = lubridate::as.interval(start_time, end_time,
      tzone = "US/Eastern")) %>%
    pull(step_interval)
  return(time_step_list)
}

SimplifySimSpatialBAEA <- function(sim){
    ssf_source <- sim %>%
      pluck('spatial', 'ssf_layers') %>%
      keep(., is.character)
    sim$spatial <- NULL
    sim[["spatial"]][["ssf_layers"]] <- ssf_source
    return(sim)
}

CreateBirthDate <- function(sim = sim){
  # Only proceed if there is no birth_date column
  sim = sim
  input = sim$agents$input
  if(!"birth_date" %in% colnames(input)){
    # Loop through each row in the input
    input_age_period <- sim$pars$global$input_age_period
    birth_day <- sim$pars$global$birth_day
    input <- tibble::add_column(input, birth_date = NA)
    for(a in 1:nrow(input)){
      # Is the age_period a year?
      if(input_age_period == "year" || input_age_period == "years") {
        # Determine the first sim_start date after the birth_day
        one_year <- as.period(1, "year")
        s0 <- as.Date(sim$pars$global$sim_start - (one_year*input$age[a]))
        # Set the format of the birth_day
        birth_day_format <- tail(guess_formats(birth_day, orders ="dm"), 1)
        birth_day_format <- paste(birth_day_format,"%Y",sep="")
        # Determine the first birth_day after s0
        s1 <- as.Date(paste(birth_day,year(s0),sep=""), format=birth_day_format)
        if(s0 >= s1) {
          input$birth_date[a] <- as.character(s1)
        } else {
          input$birth_date[a] <- as.character(s1-one_year)
        }
      } else {
        # If age period is not a year
        age_period_unit <- as.period(1, input_age_period)
        input$birth_date[a] <- as.character(sim$pars$global$sim_start -
          (age_period_unit*input$age[a]))
      }
    }
  }
  return(input)
}

UpdateAgentStates <- function(agent_states = NULL,
                              sim = sim,
                              init = FALSE) {
  if (init == TRUE) {
    input <- sim$agents$input
    input <- CreateBirthDate(sim)
    input_columns <- colnames(input)
    na_columns <- c("start_datetime", "died")
    all <- list()
    for (i in 1:nrow(input)) {
      states <- list()
      for (j in input_columns) states <- append(states, input[i, j])
      for (k in 1:length(na_columns)) states <- append(states, NA)
      states <- setNames(states, c(input_columns, na_columns))
      agent <- NamedList(states)
      all <- append(all, NamedList(agent))
    }
    sim$agents <- append(sim$agents, NamedList(all))
    return(sim)
  } else {
    agent_states <- agent_states
    return(agent_states)
  }
}

UpdateAgentStepDataBAEA <- function(step_data = NULL,
                                    sim = sim,
                                    init = FALSE,
                                    rep_intervals = rep_intervals) {
  if (init == TRUE) {
    sim_start <- sim$pars$global$sim_start
    all <- sim$agents$all
    for (i in 1:length(all)) {
      agent <- all[[i]]
      writeLines(paste("Creating initial 'step_data' dataframe for", i, "of",
        length(all)))
      all_time_steps <- as.POSIXct(NA)
      for (j in 1:length(rep_intervals)){
        #j <- 1
        step_intervals <- CreateStepIntervals(rep_intervals[[j]],
          step_period = sim$pars$global$step_period)
        for (k in 1:length(step_intervals)){
          #k <- 1
          time_steps <- CreateTimeStepsBAEA(step_intervals[[k]], agent=agent,
            sim=sim)
          for (m in 1:length(time_steps)){
            all_time_steps <- append(all_time_steps, int_start(time_steps[[m]]))
            if (m == length(time_steps)) all_time_steps <-append(all_time_steps,
              int_end(time_steps[[m]]))
          }
        }
      }   # this is all about getting 'all_time_steps'
      if(is.na(all_time_steps[1])) all_time_steps <- all_time_steps[-1]
      time_steps_df <- data.frame(datetime = all_time_steps) %>%
        mutate(julian = yday(datetime)) %>%
        group_by(julian) %>%
        mutate(day_start = min(datetime),
          day_end = max(datetime),
          day_minutes = as.integer(difftime(day_end,day_start,units="mins")))%>%
        ungroup() %>%
        mutate(time_after_start = as.integer(difftime(datetime, day_start,
          units="mins"))) %>%
        mutate(time_proportion = time_after_start/day_minutes) %>%
        dplyr::select(datetime, julian, time_proportion)
      step_data <- time_steps_df %>%
        mutate(id=agent$states$id,
          behavior = NA,
          x = NA,
          y = NA,
          exp_angle = NA,
          abs_angle = NA) %>%
        dplyr::select(id, datetime, behavior, x, y, exp_angle, abs_angle,
          julian, time_proportion) %>%
        as.data.frame() # when saved as a tibble, broke BehaviorSubModel
      step_data[1, "behavior"] <- 3
      step_data[1, "x"] <- agent$states$start_x
      step_data[1, "y"] <- agent$states$start_y
      agent  <- append(agent, NamedList(step_data))
      all[[i]] <- agent
    }
    sim$agents$all <- all
    return(sim)
  } else {
    step_data <- step_data
    return(step_data)
  }
}

CreateStepIntervals <- function(rep_interval = rep_interval,
                                step_period = sim$pars$global$step_period) {
  step_period <- step_period
  step_intervals <- list()
  interval_counter <- 1
  current_start <- lubridate::int_start(rep_interval)
  current_end <- (current_start + step_period)
  stop_point <- lubridate::int_end(rep_interval)
  while(current_start < (stop_point)) {
    current_end <- (current_start+step_period)
    step_intervals[[interval_counter]] <- lubridate::interval(current_start,
      current_end)
    interval_counter <- interval_counter + 1
    current_start <- current_start + step_period
    }
  if (lubridate::int_end(step_intervals[[length(step_intervals)]])>stop_point){
    step_intervals[[length(step_intervals)]] <-
      interval(lubridate::int_start(step_intervals[[length(step_intervals)]]),
        stop_point)
    }
  return(step_intervals)
}

toc_msg <- function(tic, toc, msg, info){
  outmsg <- paste(seconds_to_period(round(toc - tic)))
}

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

# MovementSubModelBAEA3 <- function(sim = sim,
#                                   agent_states = agent_states,
#                                   step_data = step_data,
#                                   step = step) {
#   verbose <- FALSE
#   plotting <- FALSE
#   base <- sim$spatial$base
#   cellsize <- raster::res(sim$spatial$base)[1]
#   i <- which(step_data$datetime == step)
#   current_behavior <- as.numeric(step_data[i, "behavior"])
#   next_behavior <- as.numeric(step_data[i + 1, "behavior"])
#   behavior_trans <- paste0(current_behavior, "_", next_behavior)
#   sex <- agent_states$sex
#   home_xy <- c(agent_states$start_x, agent_states$start_y)
#
#   if (i == 1) {
#     step_data[1, "exp_angle"] <- sample(x = seq(from = 0, to = (2*pi),
#       by = (2*pi/360)), size = 1)
#   } else {
#     step_data$exp_angle[i] <- step_data$abs_angle[i-1]
#   }
#   if(verbose) writeLines(paste0("Behavior Trans: ", behavior_trans))
#
#   if (i == nrow(step_data)){
#     if(verbose) writeLines(paste("Last step for:", agent_states$id))
#     step_type <- "None (last step)"
#   } else if (behavior_trans %in% c("3_3", "5_5")){
#     step_type <- "None"
#   } else if (next_behavior == 3){
#     step_type <- "To Nest"
#   } else if (behavior_trans == "4_4"){
#     perch_perch_pars <- sim$pars$classes$male$constant$fixed$move_pars %>%
#       filter(behavior_behavior == "Perch -> Perch")
#     bern_p <- perch_perch_pars$bern_p[1]
#     step_type <- ifelse(rbinom(1, 1, bern_p), "Move", "None")
#   } else {
#     step_type <- "Move"
#   }
#   if(verbose) writeLines(paste0("step_type: ", step_type))
#   if (i == nrow(step_data)){
#     step_data$abs_angle[i] <- NA
#     step_data$step_length[i] <- NA
#   } else if (step_type == "None") { # no movement
#     step_data[i+1, "x"] <- step_data[i, "x"]
#     step_data[i+1, "y"] <- step_data[i, "y"]
#     step_data[i, "abs_angle"] <- 0
#     step_data[i, "step_length"] <- 0
#     step_data[i+1, "nest_dist"] <- as.integer(sqrt((home_xy[[1]] -
#       step_data[i+1, "x"])^2 + (home_xy[[2]] - step_data[i+1, "y"])^2))
#   } else if (step_type == "To Nest" & i != nrow(step_data)) {
#     step_data$x[i+1] <- home_xy[[1]]
#     step_data$y[i+1] <- home_xy[[2]]
#     step_data$abs_angle[i] <- CalculateAngleToPoint(step_data$x[i],
#       step_data$y[i], step_data$x[i+1], step_data$y[i+1])
#     step_data$step_length[i] <- as.integer(sqrt((step_data[i, "x"] -
#       step_data[i+1, "x"])^2 + (step_data[i, "y"]-step_data[i+1, "y"])^2))
#     step_data[i+1, "nest_dist"] <- as.integer(sqrt((home_xy[[1]] -
#       step_data[i+1, "x"])^2 + (home_xy[[2]] - step_data[i+1, "y"])^2))
#   } else if (step_type == "Move"){
#
#     # For testing
#     if(FALSE) {
#       x <- step_data$x[i] + 1500; y <- step_data$y[i] + 1500
#       step_data$x[i] <- x; step_data$y[i] <- y
#     }
#     if(FALSE){
#       if(i == 1) step_data$x[i] <- 478445; step_data$y[i] <- 4972105
#     }
#
#     # Move Kernel
#     move_org <- sim$spatial$classes[[sex]][["move_kernels"]][[behavior_trans]]
#     move_org_shift <- raster::shift(move_org, dx = step_data$x[i],
#       dy = step_data$y[i])
#     move_rotated <- suppressWarnings(RotateRaster(move_org,
#       Rad2Deg(step_data$exp_angle[i]), resolution = raster::res(base)))
#     move_crop <- raster::crop(move_rotated, move_org, snap = "near")
#     move_resample <- raster::resample(move_rotated, move_org, method = "ngb",
#       progress = FALSE)
#     move_shift <- raster::shift(move_resample, dx = step_data$x[i],
#       dy = step_data$y[i])
#     raster::crs(move_shift) <- raster::crs(base)
#     move_shift_ext <- raster::extend(move_shift, move_org_shift, value = NA)
#     move_shift_crop <- raster::crop(move_shift_ext, move_org_shift, snap = "in")
#     move_kernel_mask <- raster::mask(move_shift_crop, move_org_shift, value =NA)
#     move_kernel <- move_kernel_mask/raster::cellStats(move_kernel_mask,
#       stat = "sum")
#     move_kernel[move_kernel == 0] <- NA
#     if(plotting) plot(move_kernel, colNA = "black")
#
#     # Con_Nest Kernel
#     con_nest_raster <- sim$spatial$con_nest_dist[[agent_states$nest_id]]
#     if(plotting) plot(con_nest_raster, colNA = "black")
#     pars_gamma <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$gamma
#     pars_rescale <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$rescale
#     con_nest_prob <- CreateRasterConNestDistProb(con_nest_raster,
#       raster_extent = raster::extent(move_kernel), pars_gamma = pars_gamma,
#       pars_rescale = pars_rescale, x = step_data$x[i], y = step_data$y[i],
#       base = base)
#     if(plotting) plot(con_nest_prob, colNA = "black")
#     raster::crs(con_nest_prob) <- raster::crs(base)
#     con_nest_ext <- raster::extend(con_nest_prob, move_kernel, value = NA)
#     con_nest_crop <- raster::crop(con_nest_ext, move_kernel, snap = "in")
#     con_nest_mask <- raster::mask(con_nest_crop, move_kernel, value = NA)
#     con_nest_kernel <- con_nest_mask/raster::cellStats(con_nest_mask,
#       stat = "sum")
#     con_nest_kernel[con_nest_kernel == 0] <- NA
#     if(plotting) plot(con_nest_kernel, colNA = "black")
#
#     # SSF_Kernel
#     ssf_org <-sim$spatial$ssf_layers[[`behavior_trans`]][[agent_states$nest_id]]
#     ssf_ext <- raster::extend(ssf_org, move_kernel, value = NA)
#     ssf_crop <- raster::crop(ssf_ext, move_kernel, snap = "in")
#     ssf_mask <- raster::mask(ssf_crop, move_kernel, snap = "in")
#     ssf_kernel_mask <- raster::extend(ssf_mask, move_kernel, value = NA)
#     ssf_kernel <- ssf_kernel_mask/raster::cellStats(ssf_kernel_mask,
#       stat = "sum")
#     ssf_kernel[ssf_kernel == 0] <- NA
#     if(plotting) plot(ssf_kernel, colNA = "black")
#
#     # Remove NA in any layer from all other layers
#     move_kernel <- raster::mask(move_kernel, ssf_kernel, snap = "in")
#     move_kernel <- raster::mask(move_kernel, con_nest_kernel, snap = "in")
#     ssf_kernel <- raster::mask(ssf_kernel, move_kernel, snap = "in")
#     con_nest_kernel <- raster::mask(con_nest_kernel, move_kernel, snap = "in")
#
#     # Plots for visualization checks
#     if(plotting) plot(move_kernel, main = "move_kernel", colNA = "black")
#     if(plotting) plot(ssf_kernel, main = "ssf_kernel", colNA = "black")
#     if(plotting) plot(con_nest_kernel, main = "con_nest_kernel", colNA ="black")
#
#     # Maine outline kernel
#     maine_outline <- sim$spatial$landscape$maine_outline[[agent_states$nest_id]]
#     maine_outline_ext <- raster::extend(maine_outline, move_kernel, value = NA)
#     maine_outline_crop <- raster::crop(maine_outline_ext, move_kernel,
#       snap = "in")
#     maine_outline_kernel <- raster::mask(maine_outline_crop, move_kernel,
#       value = NA)
#     if(plotting) plot(maine_outline_kernel, colNA = "black")
#
#     # Land kernel - for perch/roost, to exclude water by including land_kernel
#     if (next_behavior %in% c(4,5)){
#       land <- sim$spatial$landscape$land[[agent_states$nest_id]]
#       land_ext <- raster::extend(land, move_kernel, value = 0)
#       land_crop <- raster::crop(land_ext, move_kernel, snap = "in")
#       land_kernel <- raster::mask(land_crop, move_kernel, snap = "in")
#       land_kernel[is.na(land_kernel)] <- 0
#       if(plotting) plot(land_kernel, colNA = "black")
#     }
#
#     # Calculate probabilty raster - air to air
#     if (behavior_trans %in% c("1_1", "2_1", "1_2", "2_2")){
#       kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
#         con_nest_kernel))
#       WeightedGeoMean <- function(x){
#         geomeans <- gpindex::geometric_mean(x, w = c(3, 1, 2), na.rm = TRUE)
#         return(geomeans)
#       }
#       kernel_geomeans <- raster::calc(kernel_stack, fun = WeightedGeoMean)
#       prob_raster <- kernel_geomeans * maine_outline_kernel
#       prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
#       destination_cells_n <- 25
#     }
#     # Calculate probabilty raster - stationary to air
#     if (behavior_trans %in% c("3_1", "4_1", "3_2", "4_2", "5_2")){
#       kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
#         con_nest_kernel))
#       WeightedGeoMean <- function(x){
#         geomeans <- geometric_mean(x, w = c(3, 1, 2), na.rm = TRUE)
#         return(geomeans)
#       }
#       kernel_geomeans <- raster::calc(kernel_stack, fun = WeightedGeoMean)
#       prob_raster <- kernel_geomeans * maine_outline_kernel
#       prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
#       if(plotting) plot(prob_raster)
#       prob_raster == 0
#       destination_cells_n <- 25
#     }
#     # Calculate probabilty raster - air to stationary
#     if (behavior_trans %in% c("1_4", "2_4", "2_5")){
#       kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
#         con_nest_kernel))
#       WeightedGeoMean <- function(x){
#         geomeans <- geometric_mean(x, w = c(5, 1, 2), na.rm = TRUE)
#         return(geomeans)
#       }
#       kernel_geomeans <- raster::calc(kernel_stack, fun = WeightedGeoMean)
#       prob_raster <- kernel_geomeans * maine_outline_kernel * land_kernel
#       prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
#       destination_cells_n <- 1000
#     }
#     # Calculate probabilty raster - stationary to stationary
#     if (behavior_trans %in% c("3_4", "4_4", "5_4", "3_5", "4_5")){
#       kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
#         con_nest_kernel))
#       WeightedGeoMean <- function(x){
#         geomeans <- geometric_mean(x, w = c(5, 1, 2), na.rm = TRUE)
#         return(geomeans)
#       }
#       kernel_geomeans <- raster::calc(kernel_stack, fun = WeightedGeoMean)
#       prob_raster <- kernel_geomeans * maine_outline_kernel * land_kernel
#       prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
#       if(plotting) plot(prob_raster, colNA = "black")
#       destination_cells_n <- 1000
#     }
#
#     prob_raster[is.na(prob_raster)] <- 0
#     raster::crs(prob_raster) <- raster::crs(sim$spatial$base)
#     if(plotting) plot(prob_raster, colNA = "black")
#     if(plotting) raster::hist(prob_raster)
#     if(FALSE) raster::writeRaster(prob_raster, "C:/Temp/prob_raster.tif")
#
#     # Write out KML files
#     if(FALSE){
#       ExportKMLRasterOverlayWithTime(raster = prob_raster, time = step_interval,
#         alpha = .8, color_pal= viridis::viridis(20),
#         outfile = paste0(agent_states$id, "_", i),
#         output_dir = file.path("C:/Temp/sim_20201016-03"))
#     }
#
#     # Section for checking on errors (probability raster is all zero)
#     prob_raster_cells_gt0 <- raster::cellStats(prob_raster,
#       function(i, ...) sum(i > 0))
#     if(prob_raster_cells_gt0 == 0){
#       saveRDS(step_data, paste0("C:/TEMP/step_data", GetDateTime(), ".rds"))
#       prob_raster_cells <- raster::cellStats(prob_raster,
#         function(i, ...) sum(!is.na(i)))
#       writeLines(paste0("Cells total (n = ", prob_raster_cells,
#         "); Cells > 0 (n = ", prob_raster_cells_gt0, ")"))
#       writeLines("ZERO probability cells greater than 0")
#       writeLines(paste0("Step: ", step))
#       writeLines(paste0("Agent: ", agent_states$id))
#       writeLines(paste0("Behavior Trans: ", behavior_trans))
#       writeLines(paste0("Previous x: ", step_data$x[i-1]))
#       writeLines(paste0("Previous y: ", step_data$y[i-1]))
#       writeLines(paste0("Current x: ", step_data$x[i]))
#       writeLines(paste0("Current y: ", step_data$y[i]))
#       plot(ssf_org, main = "ssf_org")
#       plot(ssf_ext, main = "ssf_ext")
#       plot(ssf_crop, main = "ssf_crop")
#       plot(ssf_mask, main = "ssf_mask")
#       plot(con_nest_crop, main = 'con_nest_crop')
#       plot(move_kernel_mask, main = 'move_kernel_mask')
#       plot(con_nest_mask, main = 'con_nest_mask')
#       plot(ssf_kernel_mask, main = 'ssf_kernel_mask')
#       plot(move_kernel, main = 'move_kernel')
#       plot(ssf_kernel, main = 'ssf_kernel')
#       plot(con_nest_kernel, main = 'con_nest_kernel')
#       plot(prob_raster, colNA = "black", main = 'prob_raster')
#       stop()
#     }
#
#     # Select destination cell
#     destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
#         cell = 1:raster::ncell(prob_raster)), stratanames = NULL,
#         size = destination_cells_n, method = "systematic",
#         pik = prob_raster@data@values)) %>%
#       mutate(prob_rank = rank(-Prob, na.last = NA, ties.method = "random"))%>%
#       filter(prob_rank == 1) %>%
#       pull(ID_unit)
#       prob_raster_value <- raster::extract(prob_raster, destination_cell)
#
#     if(verbose) writeLines(paste0("Destination cell: ", destination_cell))
#     if(verbose) writeLines(paste0("Value prob_raster: ", prob_raster_value))
#
#     while(is.na(destination_cell) || prob_raster_value == 0) {
#       destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
#         cell = 1:raster::ncell(prob_raster)), stratanames = NULL,
#         size = destination_cells_n, method = "systematic",
#         pik = prob_raster@data@values))  %>%
#       mutate(prob_rank = rank(-Prob, na.last = NA, ties.method = "random"))%>%
#       filter(prob_rank == 1) %>%
#       pull(ID_unit)
#       prob_raster_value <- raster::extract(prob_raster, destination_cell)
#     }
#     destination_xy <- raster::xyFromCell(prob_raster, destination_cell)
#     step_data[i+1, "x"] <- destination_xy[1]
#     step_data[i+1, "y"] <- destination_xy[2]
#     step_data[i, "abs_angle"] <- CalculateAngleToPoint(step_data[i, "x"],
#       step_data[i, "y"], step_data[i+1, "x"], step_data[i+1, "y"])
#     step_data[i, "step_length"] <- as.integer(sqrt((step_data[i, "x"] -
#       step_data[i+1, "x"])^2 + (step_data[i, "y"] - step_data[i+1, "y"])^2))
#     step_data[i+1, "nest_dist"] <- as.integer(sqrt((home_xy[[1]] -
#       step_data[i+1, "x"])^2 + (home_xy[[2]] - step_data[i+1, "y"])^2))
#   }
#   return(step_data)
# }
#
# MovementSubModelBAEA2 <- function(sim = sim,
#                                   agent_states = agent_states,
#                                   step_data = step_data,
#                                   step = step) {
#   verbose <- FALSE
#   plotting <- FALSE
#   base <- sim$spatial$base
#   cellsize <- raster::res(sim$spatial$base)[1]
#   i <- which(step_data$datetime == step)
#   current_behavior <- as.numeric(step_data[i, "behavior"])
#   next_behavior <- as.numeric(step_data[i + 1, "behavior"])
#   behavior_trans <- paste0(current_behavior, "_", next_behavior)
#   sex <- agent_states$sex
#   home_xy <- c(agent_states$start_x, agent_states$start_y)
#
#   if (i == 1) {
#     step_data[1, "exp_angle"] <- sample(x = seq(from = 0, to = (2*pi),
#       by = (2*pi/360)), size = 1)
#   } else {
#     step_data$exp_angle[i] <- step_data$abs_angle[i-1]
#   }
#   if(verbose) writeLines(paste0("Behavior Trans: ", behavior_trans))
#
#   if (i == nrow(step_data)){
#     if(verbose) writeLines(paste("Last step for:", agent_states$id))
#     step_type <- "None (last step)"
#   } else if (behavior_trans %in% c("3_3", "5_5")){
#     step_type <- "None"
#   } else if (next_behavior == 3){
#     step_type <- "To Nest"
#   } else if (behavior_trans == "4_4"){
#     perch_perch_pars <- sim$pars$classes$male$constant$fixed$move_pars %>%
#       filter(behavior_behavior == "Perch -> Perch")
#     bern_p <- perch_perch_pars$bern_p[1]
#     step_type <- ifelse(rbinom(1, 1, bern_p), "Move", "None")
#   } else {
#     step_type <- "Move"
#   }
#   if(verbose) writeLines(paste0("step_type: ", step_type))
#   if (i == nrow(step_data)){
#     step_data$abs_angle[i] <- NA
#     step_data$step_length[i] <- NA
#   } else if (step_type == "None") { # no movement
#     step_data[i+1, "x"] <- step_data[i, "x"]
#     step_data[i+1, "y"] <- step_data[i, "y"]
#     step_data[i, "abs_angle"] <- 0
#     step_data[i, "step_length"] <- 0
#     step_data[i+1, "nest_dist"] <- as.integer(sqrt((home_xy[[1]] -
#       step_data[i+1, "x"])^2 + (home_xy[[2]] - step_data[i+1, "y"])^2))
#   } else if (step_type == "To Nest" & i != nrow(step_data)) {
#     step_data$x[i+1] <- home_xy[[1]]
#     step_data$y[i+1] <- home_xy[[2]]
#     step_data$abs_angle[i] <- CalculateAngleToPoint(step_data$x[i],
#       step_data$y[i], step_data$x[i+1], step_data$y[i+1])
#     step_data$step_length[i] <- as.integer(sqrt((step_data[i, "x"] -
#       step_data[i+1, "x"])^2 + (step_data[i, "y"]-step_data[i+1, "y"])^2))
#     step_data[i+1, "nest_dist"] <- as.integer(sqrt((home_xy[[1]] -
#       step_data[i+1, "x"])^2 + (home_xy[[2]] - step_data[i+1, "y"])^2))
#   } else if (step_type == "Move"){
#
#     # For testing
#     if(FALSE) {
#       x <- step_data$x[i] + 1500; y <- step_data$y[i] + 1500
#       step_data$x[i] <- x; step_data$y[i] <- y
#     }
#     if(FALSE){
#       if(i == 1) step_data$x[i] <- 478445; step_data$y[i] <- 4972105
#     }
#
#     # Move Kernel
#     move_org <- sim$spatial$classes[[sex]][["move_kernels"]][[behavior_trans]]
#     move_org_shift <- raster::shift(move_org, dx = step_data$x[i],
#       dy = step_data$y[i])
#     move_rotated <- suppressWarnings(RotateRaster(move_org,
#       Rad2Deg(step_data$exp_angle[i]), resolution = raster::res(base)))
#     move_crop <- raster::crop(move_rotated, move_org, snap = "near")
#     move_resample <- raster::resample(move_rotated, move_org, method = "ngb",
#       progress = FALSE)
#     move_shift <- raster::shift(move_resample, dx = step_data$x[i],
#       dy = step_data$y[i])
#     raster::crs(move_shift) <- raster::crs(base)
#     move_shift_ext <- raster::extend(move_shift, move_org_shift, value = NA)
#     move_shift_crop <- raster::crop(move_shift_ext, move_org_shift, snap = "in")
#     move_kernel_mask <- raster::mask(move_shift_crop, move_org_shift, value =NA)
#     move_kernel <- move_kernel_mask/raster::cellStats(move_kernel_mask,
#       stat = "sum")
#     move_kernel[move_kernel == 0] <- NA
#     if(plotting) plot(move_kernel, colNA = "black")
#
#     # Con_Nest Kernel
#     con_nest_raster <- sim$spatial$con_nest_dist[[agent_states$nest_id]]
#     if(plotting) plot(con_nest_raster, colNA = "black")
#     pars_gamma <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$gamma
#     pars_rescale <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$rescale
#     con_nest_prob <- CreateRasterConNestDistProb(con_nest_raster,
#       raster_extent = raster::extent(move_kernel), pars_gamma = pars_gamma,
#       pars_rescale = pars_rescale, x = step_data$x[i], y = step_data$y[i],
#       base = base)
#     if(plotting) plot(con_nest_prob, colNA = "black")
#     raster::crs(con_nest_prob) <- raster::crs(base)
#     con_nest_ext <- raster::extend(con_nest_prob, move_kernel, value = NA)
#     con_nest_crop <- raster::crop(con_nest_ext, move_kernel, snap = "in")
#     con_nest_mask <- raster::mask(con_nest_crop, move_kernel, value = NA)
#     con_nest_kernel <- con_nest_mask/raster::cellStats(con_nest_mask,
#       stat = "sum")
#     con_nest_kernel[con_nest_kernel == 0] <- NA
#     if(plotting) plot(con_nest_kernel, colNA = "black")
#
#     # SSF_Kernel
#     ssf_org <-sim$spatial$ssf_layers[[`behavior_trans`]][[agent_states$nest_id]]
#     ssf_ext <- raster::extend(ssf_org, move_kernel, value = NA)
#     ssf_crop <- raster::crop(ssf_ext, move_kernel, snap = "in")
#     ssf_mask <- raster::mask(ssf_crop, move_kernel, snap = "in")
#     ssf_kernel_mask <- raster::extend(ssf_mask, move_kernel, value = NA)
#     ssf_kernel <- ssf_kernel_mask/raster::cellStats(ssf_kernel_mask,
#       stat = "sum")
#     ssf_kernel[ssf_kernel == 0] <- NA
#     if(plotting) plot(ssf_kernel, colNA = "black")
#
#     # Remove NA in any layer from all other layers
#     move_kernel <- raster::mask(move_kernel, ssf_kernel, snap = "in")
#     move_kernel <- raster::mask(move_kernel, con_nest_kernel, snap = "in")
#     ssf_kernel <- raster::mask(ssf_kernel, move_kernel, snap = "in")
#     con_nest_kernel <- raster::mask(con_nest_kernel, move_kernel, snap = "in")
#
#     # Plots for visualization checks
#     if(plotting) plot(move_kernel, main = "move_kernel", colNA = "black")
#     if(plotting) plot(ssf_kernel, main = "ssf_kernel", colNA = "black")
#     if(plotting) plot(con_nest_kernel, main = "con_nest_kernel", colNA ="black")
#
#     # Maine outline kernel
#     maine_outline <- sim$spatial$landscape$maine_outline[[agent_states$nest_id]]
#     maine_outline_ext <- raster::extend(maine_outline, move_kernel, value = NA)
#     maine_outline_crop <- raster::crop(maine_outline_ext, move_kernel,
#       snap = "in")
#     maine_outline_kernel <- raster::mask(maine_outline_crop, move_kernel,
#       value = NA)
#     if(plotting) plot(maine_outline_kernel, colNA = "black")
#
#     # Land kernel - for perch/roost, to exclude water by including land_kernel
#     if (next_behavior %in% c(4,5)){
#       land <- sim$spatial$landscape$land[[agent_states$nest_id]]
#       land_ext <- raster::extend(land, move_kernel, value = 0)
#       land_crop <- raster::crop(land_ext, move_kernel, snap = "in")
#       land_kernel <- raster::mask(land_crop, move_kernel, snap = "in")
#       land_kernel[is.na(land_kernel)] <- 0
#       if(plotting) plot(land_kernel, colNA = "black")
#     }
#
#     # Calculate probabilty raster - air to air
#     if (behavior_trans %in% c("1_1", "2_1", "1_2", "2_2")){
#       kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
#         con_nest_kernel))
#       WeightedGeoMean <- function(x){
#         geomeans <- gpindex::geometric_mean(x, w = c(5, 2, 1), na.rm = TRUE)
#         return(geomeans)
#       }
#       kernel_geomeans <- raster::calc(kernel_stack, fun = WeightedGeoMean)
#       prob_raster <- kernel_geomeans * maine_outline_kernel
#       prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
#       destination_cells_n <- 1000
#     }
#     # Calculate probabilty raster - stationary to air
#     if (behavior_trans %in% c("3_1", "4_1", "3_2", "4_2", "5_2")){
#       kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
#         con_nest_kernel))
#       WeightedGeoMean <- function(x){
#         geomeans <- geometric_mean(x, w = c(5, 2, 1), na.rm = TRUE)
#         return(geomeans)
#       }
#       kernel_geomeans <- raster::calc(kernel_stack, fun = WeightedGeoMean)
#       prob_raster <- kernel_geomeans * maine_outline_kernel
#       prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
#       if(plotting) plot(prob_raster)
#       prob_raster == 0
#       destination_cells_n <- 1000
#     }
#     # Calculate probabilty raster - air to stationary
#     if (behavior_trans %in% c("1_4", "2_4", "2_5")){
#       kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
#         con_nest_kernel))
#       WeightedGeoMean <- function(x){
#         geomeans <- geometric_mean(x, w = c(4, 1, 2), na.rm = TRUE)
#         return(geomeans)
#       }
#       kernel_geomeans <- raster::calc(kernel_stack, fun = WeightedGeoMean)
#       prob_raster <- kernel_geomeans * maine_outline_kernel * land_kernel
#       prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
#       destination_cells_n <- 1000
#     }
#     # Calculate probabilty raster - stationary to stationary
#     if (behavior_trans %in% c("3_4", "4_4", "5_4", "3_5", "4_5")){
#       kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
#         con_nest_kernel))
#       WeightedGeoMean <- function(x){
#         geomeans <- geometric_mean(x, w = c(5, 2, 1), na.rm = TRUE)
#         return(geomeans)
#       }
#       kernel_geomeans <- raster::calc(kernel_stack, fun = WeightedGeoMean)
#       prob_raster <- kernel_geomeans * maine_outline_kernel * land_kernel
#       prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
#       if(plotting) plot(prob_raster, colNA = "black")
#       destination_cells_n <- 1000
#     }
#
#     prob_raster[is.na(prob_raster)] <- 0
#     raster::crs(prob_raster) <- raster::crs(sim$spatial$base)
#     if(plotting) plot(prob_raster, colNA = "black")
#     if(plotting) raster::hist(prob_raster)
#     if(FALSE) raster::writeRaster(prob_raster, "C:/Temp/prob_raster.tif")
#
#     # Write out KML files
#     if(FALSE){
#       ExportKMLRasterOverlayWithTime(raster = prob_raster, time = step_interval,
#         alpha = .8, color_pal= viridis::viridis(20),
#         outfile = paste0(agent_states$id, "_", i),
#         output_dir = file.path("C:/Temp/sim_20201016-03"))
#     }
#
#     # Section for checking on errors (probability raster is all zero)
#     prob_raster_cells_gt0 <- raster::cellStats(prob_raster,
#       function(i, ...) sum(i > 0))
#     if(prob_raster_cells_gt0 == 0){
#       saveRDS(step_data, paste0("C:/TEMP/step_data", GetDateTime(), ".rds"))
#       prob_raster_cells <- raster::cellStats(prob_raster,
#         function(i, ...) sum(!is.na(i)))
#       writeLines(paste0("Cells total (n = ", prob_raster_cells,
#         "); Cells > 0 (n = ", prob_raster_cells_gt0, ")"))
#       writeLines("ZERO probability cells greater than 0")
#       writeLines(paste0("Step: ", step))
#       writeLines(paste0("Agent: ", agent_states$id))
#       writeLines(paste0("Behavior Trans: ", behavior_trans))
#       writeLines(paste0("Previous x: ", step_data$x[i-1]))
#       writeLines(paste0("Previous y: ", step_data$y[i-1]))
#       writeLines(paste0("Current x: ", step_data$x[i]))
#       writeLines(paste0("Current y: ", step_data$y[i]))
#       plot(ssf_org, main = "ssf_org")
#       plot(ssf_ext, main = "ssf_ext")
#       plot(ssf_crop, main = "ssf_crop")
#       plot(ssf_mask, main = "ssf_mask")
#       plot(con_nest_crop, main = 'con_nest_crop')
#       plot(move_kernel_mask, main = 'move_kernel_mask')
#       plot(con_nest_mask, main = 'con_nest_mask')
#       plot(ssf_kernel_mask, main = 'ssf_kernel_mask')
#       plot(move_kernel, main = 'move_kernel')
#       plot(ssf_kernel, main = 'ssf_kernel')
#       plot(con_nest_kernel, main = 'con_nest_kernel')
#       plot(prob_raster, colNA = "black", main = 'prob_raster')
#       stop()
#     }
#
#     # Select destination cell
#     # A/S->A selects cells based on probability, then 1 random cell
#     if (behavior_trans %in% c("1_1", "2_1", "1_2", "2_2", "3_1", "4_1", "3_2",
#       "4_2", "5_2")){
#       # Select destination cell
#       destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
#           cell = 1:raster::ncell(prob_raster)), stratanames = NULL,
#           size = destination_cells_n, method = "systematic",
#           pik = prob_raster@data@values)) %>%
#         mutate(prob_rank = rank(-Prob, na.last = NA, ties.method = "random"))%>%
#         filter(prob_rank > 26) %>%
#         slice_sample(., n = 1) %>%
#         pull(ID_unit)
#       prob_raster_value <- raster::extract(prob_raster, destination_cell)
#
#       if(verbose) writeLines(paste0("Destination cell: ", destination_cell))
#       if(verbose) writeLines(paste0("Value prob_raster: ", prob_raster_value))
#
#       while(is.na(destination_cell) || prob_raster_value == 0) {
#         destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
#           cell = 1:raster::ncell(prob_raster)), stratanames = NULL,
#           size = destination_cells_n, method = "systematic",
#           pik = prob_raster@data@values)) %>%
#         mutate(prob_rank = rank(-Prob, na.last = NA, ties.method = "random"))%>%
#         filter(prob_rank > 26) %>%
#         slice_sample(., n = 1) %>%
#         pull(ID_unit)
#         prob_raster_value <- raster::extract(prob_raster, destination_cell)
#       }
#     } else { # A/S->S selects cells based on probility, then highest prob cell
#       # Select destination cell
#       destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
#           cell = 1:raster::ncell(prob_raster)), stratanames = NULL,
#           size = destination_cells_n, method = "systematic",
#           pik = prob_raster@data@values)) %>%
#         mutate(prob_rank = rank(-Prob, na.last = NA, ties.method = "random"))%>%
#         filter(prob_rank == 1) %>%
#         pull(ID_unit)
#
#       prob_raster_value <- raster::extract(prob_raster, destination_cell)
#
#       if(verbose) writeLines(paste0("Destination cell: ", destination_cell))
#       if(verbose) writeLines(paste0("Value prob_raster: ", prob_raster_value))
#
#       while(is.na(destination_cell) || prob_raster_value == 0) {
#         destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
#           cell = 1:raster::ncell(prob_raster)), stratanames = NULL,
#           size = destination_cells_n, method = "systematic",
#           pik = prob_raster@data@values)) %>%
#         mutate(prob_rank = rank(-Prob, na.last = NA, ties.method = "random"))%>%
#         filter(prob_rank == 1) %>%
#         pull(ID_unit)
#         prob_raster_value <- raster::extract(prob_raster, destination_cell)
#       }
#     }
#     destination_xy <- raster::xyFromCell(prob_raster, destination_cell)
#     step_data[i+1, "x"] <- destination_xy[1]
#     step_data[i+1, "y"] <- destination_xy[2]
#     step_data[i, "abs_angle"] <- CalculateAngleToPoint(step_data[i, "x"],
#       step_data[i, "y"], step_data[i+1, "x"], step_data[i+1, "y"])
#     step_data[i, "step_length"] <- as.integer(sqrt((step_data[i, "x"] -
#       step_data[i+1, "x"])^2 + (step_data[i, "y"] - step_data[i+1, "y"])^2))
#     step_data[i+1, "nest_dist"] <- as.integer(sqrt((home_xy[[1]] -
#       step_data[i+1, "x"])^2 + (home_xy[[2]] - step_data[i+1, "y"])^2))
#   }
#   return(step_data)
# }


  # NEW STEP - Reduce the probability of perching behavior by 25%
  # gamma4 <- gamma3
  # if(current_time_prop >= .1 & current_time_prop <= .9){
  #   if(current_behavior == 1){
  #     gamma4[current_behavior, 1] <- gamma4[current_behavior, 1]*(1.5)
  #   }
  #   if(current_behavior == 2){
  #     gamma4[current_behavior, 2] <- gamma4[current_behavior, 2]*(1.5)
  #   }
  # }
  # if(current_time_prop >= .2 & current_time_prop <= .8){
  #   if(current_behavior == 4){
  #     gamma4[current_behavior, 4] <- gamma4[current_behavior, 4]*(.85)
  #   }
  # }
  # gamma4 <- gamma4/apply(gamma4, 1, sum)



# BehaviorSubModelBAEA2 <- function(sim = sim,
#                                  agent_states = agent_states,
#                                  step_data = step_data,
#                                  step = step){
#   sex <- agent_states$sex
#   beta <- as.matrix(sim$pars$classes[[sex]]$constant$fixed$behavior_betas)
#   step_row <- which(step_data$datetime == step)
#   current_behavior <- as.numeric(step_data[step_row, "behavior"])
#   current_time_prop <- as.numeric(step_data[step_row, "time_proportion"])
#   next_time_prop <- as.numeric(step_data[step_row + 1, "time_proportion"])
#   current_nest_dist <- round(step_data[step_row, "nest_dist"]/500)
#   step_data <- as.data.frame(step_data)
#
#   writeLines(paste0("Current behavior: ", current_behavior))
#   writeLines(paste0("nest_dist: ", current_nest_dist))
#
#   gamma <- diag(5)
#   g <- beta[1, ]  #  g = state transition probabilities intercepts
#   g <- g +
#     beta[2, ] * current_nest_dist +
#     beta[3, ] * cos(2*pi * current_time_prop) +
#     beta[4, ] * sin(2*pi * current_time_prop) +
#     exp(g)
#   gamma[!gamma] <- exp(g) # Beta values in non-diagonal locations in matrix
#   gamma2 <- t(gamma) # probabilities for state transitions are now in rows
#   gamma3 <- gamma2/apply(gamma2, 1, sum) # rows sum to 1
#   if(current_time_prop <= .5 & current_behavior != 5){
#     gamma3[, 5] <- 0
#     gamma3 <- gamma3/apply(gamma3, 1, sum)
#     gamma3[, 5] <- 0
#   }
#   # next control is new - it forces agent to leave roost after .3 time prop
#   if(current_time_prop >= .3 & current_time_prop <= .5 & current_behavior == 5){
#     gamma3[, 5] <- 0
#     gamma3 <- gamma3/apply(gamma3, 1, sum)
#     gamma3[, 5] <- 0
#   }
#   if(current_time_prop > .5 & current_behavior == 5){
#     gamma3[, 1:4] <- 0
#     gamma3[, 5] <- 1
#     #gamma3 <- gamma3/apply(gamma3, 1, sum)
#   }
#   if(current_behavior == 1){ # prevents 1->5 (Cruise to Roost)
#     gamma3[, 5] <- 0
#     gamma3 <- gamma3/apply(gamma3, 1, sum)
#     gamma3[, 5] <- 0
#   }
#   if(current_behavior == 5){ # prevents 5->1 (Roost to Cruise)
#     gamma3[, 1] <- 0
#     gamma3 <- gamma3/apply(gamma3, 1, sum)
#     gamma3[, 1] <- 0
#   }
#
#   # NEW STEP - Reduce the probability of nesting behavior by 20%
#   gamma4 <- gamma3
#   gamma4[current_behavior, 3] <- gamma4[current_behavior, 3]*(.80)
#   if(current_time_prop >= .25 & current_time_prop <= .85){
#     gamma4[current_behavior, 1] <- gamma4[current_behavior, 1]*(1.5)
#   }
#   if(current_time_prop >= .35 & current_time_prop <= .55){
#     gamma4[current_behavior, 2] <- gamma4[current_behavior, 2]*(1.5)
#   }
#   gamma4 <- gamma4/apply(gamma4, 1, sum)
#
#   # trans prob. given current behavior
#   next_behavior <- sample(1:5, size = 1, prob = gamma4[current_behavior, ])
#   writeLines(paste0("Next behavior: ", next_behavior))
#
#   if(step_row != nrow(step_data)) { # prevent the creation of an "extra" step
#     if(next_time_prop == 1){ # select Nest or Roost only for last location of day
#       if (next_behavior %in% c(3,5)){
#         step_data[step_row + 1, "behavior"] <- next_behavior
#       } else { # forces selection of Nest or Roost
#         gamma3[, c(1,2,4)] <- 0
#         gamma3 <- gamma3/apply(gamma3, 1, sum)
#         gamma3[, c(1,2,4)] <- 0
#         writeLines("End of day")
#         writeLines("prob: ", gamma3[current_behavior,])
#         next_behavior <- sample(1:5, size = 1, prob = gamma3[current_behavior,])
#         step_data[step_row + 1, "behavior"] <- next_behavior
#       }
#     }
#     if(current_time_prop == 1){
#       step_data[step_row + 1, "behavior"] <- current_behavior
#     }
#     step_data[step_row + 1, "behavior"] <- next_behavior
#   }
#   return(step_data)
# }


# # NEW SECTION THAT DOES A RESCALE AT EVERY STEP
# ssf_rescale <- ssf_mask
# ssf_rescale[] <- scales::rescale(ssf_mask[], to = c(-4, 4)) # Tried 4, 4
# if(plotting) plot(ssf_rescale)

# if(behavior_trans == "3_4"){
#   ssf_rescale <- ssf_mask
# }
# ssf_kernel <- raster::calc(ssf_rescale, fun = boot::inv.logit)
# if(plotting) plot(ssf_kernel)
#raster::writeRaster(ssf_kernel, "C:/Temp/Sim6/ssf_kernel.tif")

# destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
#   cell = 1:raster::ncell(prob_raster)), stratanames = NULL, size=1,
#   method = "systematic", pik = prob_raster@data@values))
# while(is.na(destination_cell[1,1])) {
#   destination_cell <- suppressWarnings(sampling::strata(data=data.frame(
#     cell = 1:raster::ncell(prob_raster)), stratanames = NULL, size = 1,
#     method = "systematic", pik = prob_raster@data@values))
# }

#writeLines(paste0("Sample proportion: 500 out of ", raster::ncell(prob_raster),
#  " (", 500/raster::ncell(prob_raster), ")"))





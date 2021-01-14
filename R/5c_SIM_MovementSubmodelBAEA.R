# sim
# agent_states
# step_data[i + 1, "behavior"] <- 4
# step

MovementSubModelBAEA2 <- function(sim = sim,
                                  agent_states = agent_states,
                                  step_data = step_data,
                                  step = step) {
  plotting <- FALSE #TRUE
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
    move_org_shift <- raster::shift(move_org, dx = step_data$x[i],
      dy = step_data$y[i])
    move_rotated <- suppressWarnings(RotateRaster(move_org,
      Rad2Deg(step_data$exp_angle[i]), resolution=raster::res(base)))
    move_crop <- raster::crop(move_rotated, move_org, snap = "near")
    move_resample <- raster::resample(move_rotated, move_org, method = "ngb")
    move_shift <- raster::shift(move_resample, dx = step_data$x[i],
      dy = step_data$y[i])
    raster::crs(move_shift) <- raster::crs(base)

    move_shift_ext <- raster::extend(move_shift, move_org_shift, value = NA)
    move_shift_crop <- raster::crop(move_shift_ext, move_org_shift, snap="in")
    move_kernel <- raster::mask(move_shift_crop, move_org_shift, value=NA)
    if(plotting) plot(move_kernel)

    # Con_Nest Kernel
    con_nest_raster <- sim$spatial$con_nest_dist[[agent_states$nest_id]]
    pars_gamma <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$gamma
    pars_rescale <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$rescale
    con_nest_prob <- CreateRasterConNestDistProb(con_nest_raster,
      raster_extent = raster::extent(move_kernel), pars_gamma = pars_gamma,
      pars_rescale = pars_rescale, x=step_data$x[i], y=step_data$y[i],
      base= base)
    raster::crs(con_nest_prob) <- raster::crs(base)
    con_nest_ext <- raster::extend(con_nest_prob, move_kernel, value = NA)
    con_nest_crop <- raster::crop(con_nest_ext, move_kernel, snap = "in")
    con_nest_kernel <- raster::mask(con_nest_crop, move_kernel, value=NA)
    if(plotting) plot(con_nest_kernel)

    # Maine_Outline Kernel
    maine_outline <- sim$spatial$landscape$maine_outline[[agent_states$nest_id]]
    maine_outline_ext <- raster::extend(maine_outline, move_kernel, value = 0)
    maine_outline_crop <- raster::crop(maine_outline_ext, move_kernel,
      snap = "in")
    maine_outline_kernel <- raster::mask(maine_outline_crop, move_kernel,
      value = NA)
    if(plotting) plot(maine_outline_kernel)

    #print(paste0("nest_id: ", agent_states$nest_id))

    # SSF Layer
    ssf_org <-sim$spatial$ssf_layers[[`behavior_trans`]][[agent_states$nest_id]]
    ssf_ext <- raster::extend(ssf_org, move_kernel, value = NA)
    ssf_crop <- raster::crop(ssf_ext, move_kernel, snap = "in")
    ssf_mask <- raster::mask(ssf_crop, move_kernel, snap = "in")
    if(plotting) plot(ssf_mask)

    # ORIGINAL WAY TO CALCULATE THE KERNEL
    ssf_kernel <- raster::extend(ssf_mask, move_kernel, value = NA)
    if(plotting) plot(ssf_kernel)

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
    land_ext <- raster::extend(land, move_kernel, value = 0)
    land_crop <- raster::crop(land_ext, move_kernel, snap = "in")
    land_kernel <- raster::mask(land_crop, move_kernel, snap = "in")
    if(plotting) plot(land_kernel)

    ### FINAL PROBABILITY LAYER (use geometric mean for final probability layer)

    move_kernel_log <- log(move_kernel)
    if(plotting) plot(move_kernel_log)
    con_nest_kernel_log <- log(con_nest_kernel)
    if(plotting) plot(con_nest_kernel_log)
    land_log <- log(land_kernel)
    if(plotting) plot(land_log)
    maine_outline_log <- log(maine_outline_kernel)
    if(plotting) plot(maine_outline_log)
    ssf_kernel_log <- log(ssf_kernel)
    if(plotting) plot(ssf_kernel_log)
    # print(paste0("move_kernel_log:", raster::extent(move_kernel_log)))
    # print(paste0("con_nest_kernel_log:", raster::extent(con_nest_kernel)))
    # print(paste0("land_log:", raster::extent(land_log)))
    # print(paste0("maine_outline_log:", raster::extent(maine_outline_log)))

    if (next_behavior %in% c(4,5)){
      # If end behavior is perch or roost - SSF, Maine, and Land
      kernel_stack <- raster::stack(list(land_log, maine_outline_log,
        con_nest_kernel_log, ssf_kernel_log))
    } else {
      # If end behavior is cruise or flight - SSF, Con_Nest, Maine, and Land
      kernel_stack <- raster::stack(list(move_kernel_log, con_nest_kernel_log,
        maine_outline_log, ssf_kernel_log))
    }

    kernel_stack_mean <- raster::calc(kernel_stack, fun = mean, na.rm = TRUE)

    if(plotting) plot(kernel_stack_mean)
    prob_raster <- exp(kernel_stack_mean)
    #prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
    prob_raster[is.na(prob_raster)] <- 0

    raster::crs(prob_raster) <- raster::crs(sim$spatial$base)
    if(plotting) plot(prob_raster)
    # ExportKMLRasterOverlayWithTime(raster = prob_raster, time = step_interval,
    #   alpha = .8, color_pal= viridis::viridis(20),
    #   outfile = paste0(agent_states$id, "_", i),
    #   output_dir = file.path("C:/Temp/sim_20201016-03"))

    if(plotting) raster::hist(prob_raster)
    ### END OF OTHER PROBABILITY LAYERS

    ## OLD SECTION ##########

    # destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
    #   cell = 1:raster::ncell(prob_raster)), stratanames = NULL, size=1,
    #   method = "systematic", pik = prob_raster@data@values))
    # while(is.na(destination_cell[1,1])) {
    #   destination_cell <- suppressWarnings(sampling::strata(data=data.frame(
    #     cell = 1:raster::ncell(prob_raster)), stratanames = NULL, size = 1,
    #     method = "systematic", pik = prob_raster@data@values))
    # }

    ## NEW SECTION ##########
    #print(paste0("Sample proportion: 500 out of ", raster::ncell(prob_raster),
    #  " (", 500/raster::ncell(prob_raster), ")"))

    destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
      cell = 1:raster::ncell(prob_raster)), stratanames = NULL, size = 50,
      method = "systematic", pik = prob_raster@data@values)) %>%
    mutate(prob_rank = rank(-Prob, na.last = NA, ties.method = "random")) %>%
    filter(prob_rank == 1)

    while(is.na(destination_cell[1,1])) {
      destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
        cell = 1:raster::ncell(prob_raster)), stratanames = NULL, size = 50,
        method = "systematic", pik = prob_raster@data@values)) %>%
      mutate(prob_rank = rank(-Prob, na.last = NA, ties.method = "random")) %>%
      filter(prob_rank == 1)
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


# IMPORTANT - Process to plot probability plot
# sample_n <- 50000
# destination_xy <- tibble(x = vector(mode = "numeric", sample_n),
#   y = vector(mode = "numeric", sample_n))
#
# for (i in seq_len(sample_n)){
#   destination_cell <- suppressWarnings(sampling::strata(data = data.frame(cell =
#       1:raster::ncell(prob_raster)), stratanames = NULL, size = 1,
#     method = "systematic", pik=prob_raster@data@values))
#     while(is.na(destination_cell[1,1])) {
#       destination_cell <- suppressWarnings(sampling::strata(data=data.frame(
#         cell = 1:raster::ncell(prob_raster)), stratanames = NULL, size = 1,
#         method="systematic", pik = prob_raster@data@values))
#     }
#   destination_xy[i, ] <- raster::xyFromCell(prob_raster, destination_cell[,1])
# }
# destination_raster <- rasterize(destination_xy, prob_raster, field = 1,
#   fun = 'sum', background = NA, mask = FALSE, update = FALSE,
#   updateValue = 'all', filename = "", na.rm = TRUE)
#
# plot(prob_raster)
# plot(destination_raster)
# Plot3DRaster(destination_raster, col = viridis::viridis(20),
#   main = "Probability Plot")

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
      print(paste("Creating initial 'step_data' dataframe for", i, "of",
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











GetDestinationCell <- function(prob_raster_sample_bin) {
  out <- tryCatch({
    destination_cell <- sample(raster::ncell(prob_raster_sample_bin),
      size = 1, replace = TRUE, prob = prob_raster_sample_bin@data@values)
    },
    error = function(cond) {
      saveRDS(prob_raster_sample_bin,
        paste0("C:/TEMP/prob_raster_sample_bin",
          GetDateTime(), ".rds"))
      saveRDS(prob_raster,
        paste0("C:/TEMP/prob_raster",
          GetDateTime(), ".rds"))
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
      plot(prob_raster, colNA = "black",
        main = 'prob_raster')
      plot(prob_raster_sample_bin, colNA = "black",
        main = 'prob_raster_sample_bin')
      stop()
      return(NA)
    },
    warning = function(cond) {
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    },
    finally = {
    }
  )
  return(out)
}


# THIS SUBMODEL WAS USED FOR THE EXPERIMENT
# IT IS BASED ON THE sim_20210725-77 CALIBRATION PARAMETERS
MovementSubModelBAEA <- function(sim = sim,
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
    #move_crop <- raster::crop(move_rotated, move_org, snap = "near") # Unused
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
      kernel_weights <- c(3, 2, 1)
      destination_cells_n <- 25
    }
    # Calculate probabilty raster - perch/roost to air
    if (behavior_trans %in% c("4_1", "4_2", "5_2")){
      kernel_weights <- c(3, 3, 2)
      destination_cells_n <- 25
    }
    # Calculate probabilty raster - nest to air
    if (behavior_trans %in% c("3_1", "3_2")){
      kernel_weights <- c(2, 3, 1)
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
      destination_cells_n <- 1000
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

    #### TEST SECTION START #####

    # prob_min <- raster::cellStats(prob_raster, "min", na.rm=TRUE)
    # prob_max <- raster::cellStats(prob_raster, "max", na.rm=TRUE)
    # prob_breaks <- seq(prob_min, prob_max, length.out = 11)
    # prob_raster_reclass <- raster::cut(prob_raster, breaks = prob_breaks)
    # # cut() removes zeroes because default argument: include.lowest = FALSE
    #
    # prob_bin <- sample(10, size = 1, replace = T, c(2^(1:10)/100))
    # prob_raster_sample_bin <- raster::mask(prob_raster, prob_raster_reclass,
    #   inverse = TRUE, maskvalue = prob_bin, updatevalue = 0)
    #
    # while(raster::cellStats(prob_raster_sample_bin, "sum") == 0) {
    #   if (!exists("resample_n")){
    #     resample_n <- 1
    #   } else {
    #     resample_n <- resample_n + 1
    #   }
    #   writeLines(paste0("Resampling probability bins (n=", resample_n), ")")
    #   prob_bin <- sample(10, size = 1, replace = T, c(2^(1:10)/100))
    #   prob_raster_sample_bin <- raster::mask(prob_raster, prob_raster_reclass,
    #     inverse = TRUE, maskvalue = prob_bin, updatevalue = 0)
    # }
    #
    # destination_cell <- sample(raster::ncell(prob_raster_sample_bin),
    #   size = 1, replace = TRUE, prob = prob_raster_sample_bin@data@values)
    # prob_raster_value <- prob_raster_sample_bin[destination_cell]
    #
    # # destination_cell <- sample(raster::ncell(prob_raster_sample_bin),
    # #   size = 1, replace = TRUE, prob = prob_raster_sample_bin@data@values)
    # # prob_raster_value <- prob_raster_sample_bin[destination_cell]
    #
    # if(verbose) writeLines(paste0("Destination cell: ", destination_cell))
    # if(verbose) writeLines(paste0("Value prob_raster: ", prob_raster_value))
    #
    # while(is.na(destination_cell) || prob_raster_value == 0) {
    #   destination_cell <- sample(raster::ncell(prob_raster_sample_bin),
    #     size = 1, replace = TRUE, prob = prob_raster_sample_bin@data@values)
    #   # destination_cell <- sample(raster::ncell(prob_raster_sample_bin),
    #   #   size = 1, replace = TRUE, prob = prob_raster_sample_bin@data@values)
    #   prob_raster_value <- prob_raster_sample_bin[destination_cell]
    # }
    #### TEST SECTION END #####

    #### ORIGINAL SECTION START #####
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
    ### ORIGINAL SECTION END #####

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

RunSimulationBAEA <- function(sim = sim,
                          runs = 1,
                          write = FALSE,
                          output_dir = getwd()){
  runs <- CreateRunsList(runs)
  for (i in 1:length(runs)){
    rep_intervals <- CreateReportIntervals(sim)
    sim <- UpdateAgentStatesBAEA(init = TRUE, sim = sim)
    sim <- UpdateAgentStepDataBAEA(init = TRUE, sim = sim,
      rep_intervals = rep_intervals)
    sim <- UpdateAgentParsData(init = TRUE, sim = sim)
    sim <- UpdateSpatialBAEA(init = TRUE, sim = sim)
    for (j in 1:length(rep_intervals)) {
      step_intervals <- CreateStepIntervals(rep_intervals[[j]],
        step_period = sim$pars$global$step_period)
      for (k in 1:length(step_intervals)) {
        step_interval <- step_intervals[[k]]
        time_steps <- CreateTimeStepsInStepIntervalBAEA(step_interval, sim=sim)
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
                step_data <- BehaviorSubModelBAEA(sim, agent_states, step_data,
                  step)
                step_data <- MovementSubModelBAEA(sim, agent_states, step_data,
                  step)
              }
              sim$agents$all[[n]][["states"]] <-
                UpdateAgentStatesBAEA(agent_states)
              sim$agents$all[[n]][["step_data"]] <-
                UpdateAgentStepDataBAEA(step_data)
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

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

# UpdateAgentStepDataBAEA <- function(step_data = NULL,
#                                     sim = sim,
#                                     init = FALSE,
#                                     rep_intervals = rep_intervals) {
#   if (init == TRUE){
#     sim_start <- sim$pars$global$sim_start
#     all <- sim$agents$all
#     for (i in 1:length(all)) {
#       agent <- all[[i]]
#       writeLines(paste("Creating initial 'step_data' dataframe for", i, "of",
#         length(all)))
#       all_time_steps <- as.POSIXct(NA)
#       for (j in 1:length(rep_intervals)){
#         #j <- 1
#         step_intervals <- CreateStepIntervals(rep_intervals[[j]],
#           step_period = sim$pars$global$step_period)
#         for (k in 1:length(step_intervals)){
#           #k <- 1
#           time_steps <- CreateTimeStepsBAEA(step_intervals[[k]], agent = agent,
#             sim = sim)
#           for (m in 1:length(time_steps)){
#             all_time_steps <- append(all_time_steps, int_start(time_steps[[m]]))
#             if (m == length(time_steps)) all_time_steps <- append(all_time_steps,
#               int_end(time_steps[[m]]))
#           }
#         }
#       }   # this is all about getting 'all_time_steps'
#       if(is.na(all_time_steps[1])) all_time_steps <- all_time_steps[-1]
#       time_steps_df <- data.frame(datetime = all_time_steps) %>%
#         mutate(julian = yday(datetime)) %>%
#         group_by(julian) %>%
#         mutate(day_start = min(datetime),
#           day_end = max(datetime),
#           day_minutes = as.integer(difftime(day_end,day_start,units="mins"))) %>%
#         ungroup() %>%
#         mutate(time_after_start = as.integer(difftime(datetime, day_start,
#           units="mins"))) %>%
#         mutate(time_proportion = time_after_start/day_minutes) %>%
#         dplyr::select(datetime, time_proportion)
#       step_data <- time_steps_df %>%
#         mutate(id = agent$states$id,
#           behavior = NA_integer_,
#           x = NA_real_,
#           y = NA_real_,
#           nest_dist = NA_real_,
#           exp_angle = NA_real_,
#           abs_angle = NA_real_) %>%
#         dplyr::select(id, datetime, behavior, x, y, exp_angle, abs_angle,
#           nest_dist, time_proportion) %>%
#         as.data.frame() # when saved as a tibble, broke BehaviorSubModel
#       step_data[1, "behavior"] <- 3
#       step_data[1, "nest_dist"] <- 0
#       step_data[1, "x"] <- agent$states$start_x
#       step_data[1, "y"] <- agent$states$start_y
#       agent <- append(agent, NamedList(step_data))
#       all[[i]] <- agent
#     }
#     sim$agents$all <- all
#     return(sim)
#   } else {
#     step_data <- step_data
#     return(step_data)
#   }
# }

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





# BehaviorSubModelBAEA2 <- function(sim = sim,
#                                  agent_states = agent_states,
#                                  step_data = step_data,
#                                  step = step){
#   verbose <- FALSE
#   sex <- agent_states$sex
#   beta <- as.matrix(sim$pars$classes[[sex]]$constant$fixed$behavior_betas)
#   step_row <- which(step_data$datetime == step)
#   current_behavior <- as.numeric(step_data[step_row, "behavior"])
#   current_time_prop <- as.numeric(step_data[step_row, "time_proportion"])
#   next_time_prop <- as.numeric(step_data[step_row + 1, "time_proportion"])
#   julian <- yday(step_data[step_row, "datetime"])
#   step_data <- as.data.frame(step_data)
#   time_prop_second_to_last <- step_data %>%
#     mutate(day = date(datetime)) %>%
#     filter(day == date(step)) %>%
#     filter(row_number() == (n() - 1)) %>%
#     pull(time_proportion)
#   gamma <- diag(5)
#   g <- beta[1, ]  #  g = state transition probabilities intercepts
#   g <- g +
#     beta[2, ] * cos(2*pi * (julian/365)) +
#     beta[3, ] * sin(2*pi * (julian/365)) +
#     beta[4, ] * cos(2*pi * (current_time_prop)) +
#     beta[5, ] * sin(2*pi * (current_time_prop))
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
#   }
#   if(current_behavior == 1){ # prevents 1 -> 5 (Cruise to Roost)
#     gamma3[, 5] <- 0
#     gamma3 <- gamma3/apply(gamma3, 1, sum)
#     gamma3[, 5] <- 0
#   }
#   if(current_behavior == 5){ # prevents 5 -> 1 (Roost to Cruise)
#     gamma3[, 1] <- 0
#     gamma3 <- gamma3/apply(gamma3, 1, sum)
#     gamma3[, 1] <- 0
#   }
#
#   # trans prob. given current behavior
#   gamma4 <- gamma3
#   next_behavior <- sample(1:5, size = 1, prob = gamma4[current_behavior, ])
#
#   if(step_row != nrow(step_data)) { # prevent the creation of an "extra" step
#     if(next_time_prop == time_prop_second_to_last){ # Second to last step
#       if (next_behavior != 1){ # For next behavior to be not Cruise
#         step_data[step_row + 1, "behavior"] <- next_behavior
#       } else { # forces selection of non-Cruise behavior
#         gamma4[, 1] <- 0
#         gamma4 <- gamma4/apply(gamma4, 1, sum)
#         gamma4[, 1] <- 0
#         next_behavior <- sample(1:5, size = 1, prob = gamma4[current_behavior,])
#         step_data[step_row + 1, "behavior"] <- next_behavior
#       }
#     }
#     if(next_time_prop == 1){ # only Nest or Roost for last location of day
#       if(verbose) writeLines("End of day")
#       if (next_behavior %in% c(3,5)){
#         step_data[step_row + 1, "behavior"] <- next_behavior
#       } else { # forces selection of Nest or Roost
#         gamma4[, c(1,2,4)] <- 0
#         gamma4 <- gamma4/apply(gamma4, 1, sum)
#         gamma4[, c(1,2,4)] <- 0
#         next_behavior <- sample(1:5, size = 1, prob = gamma4[current_behavior,])
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

#init=TRUE
#sim=sim

#sim$agents$all[[1]]
#test <- sim$agents$all[[1]][["step_data"]]
#behavior_levels <- c("Cruise", "Flight", "Nest", "Perch", "Roost")
#behavior_levels <- c("Cruise", "Flight", "Nest", "Perch", "Roost")


UpdateAgentStepDataBAEA <- function(step_data = NULL,
                                    sim = sim,
                                    init = FALSE,
                                    rep_intervals = rep_intervals) {
  if (init == TRUE) {
    sim_start <- sim$pars$global$sim_start
    all <- sim$agents$all
    for (i in 1:length(all)) {
      agent <- all[[i]]
      all_time_steps <- as.POSIXct(NA)
      for (j in 1:length(rep_intervals)){
        #j <- 1
        step_intervals <- CreateStepIntervals(rep_intervals[[j]])
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


#      step_data <- data.frame(id=agent$states$id, datetime=as.POSIXct(NA),
#        behavior=NA, x=agent$states$start_x, y=agent$states$start_y,

#sim$agents$all[[1]]$step_data

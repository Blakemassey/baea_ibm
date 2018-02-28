#init=TRUE
#sim=sim

#sim$agents$all[[1]]
#test <- sim$agents$all[[1]][["step_data"]]
#behavior_levels <- c("Cruise", "Flight", "Nest", "Perch", "Roost")
#behavior_levels <- c("Cruise", "Flight", "Nest", "Perch", "Roost")


UpdateAgentStepDataBAEA <- function(step_data = NULL,
                                    sim = sim,
                                    time_steps_df = time_steps_df,
                                    init = FALSE) {
  if (init == TRUE) {
    sim_start <- sim$pars$global$sim_start
    all <- sim$agents$all
    time_steps_df <- time_steps_df
    for (i in 1:length(all)) {
      agent <- all[[i]]
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

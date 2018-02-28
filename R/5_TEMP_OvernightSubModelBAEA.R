LastDailySubModelBAEA <- function(sim = sim,
                                  agent_states = agent_states,
                                  step_data = step_data,
                                  step = step) {
  step_row <- which(step_data$datetime == int_end(step))
  current_behavior <- as.numeric(step_data[step_row, "behavior"])
  if (current_behavior %in% c(3,5)){
    step_data[step_row + 1, "behavior"] <- current_behavior
  } else{
    overnight_behavior <- sample(c(3,5), 1)
    step_data[step_row, "behavior"] <- overnight_behavior
    step_data[step_row + 1, "behavior"] <- overnight_behavior
  }
  return(step_data)
}

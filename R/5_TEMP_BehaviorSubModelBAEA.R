BehaviorSubModelBAEA <- function(sim = sim,
                                 agent_states = agent_states,
                                 step_data = step_data,
                                 step = step) {
  sex <- agent_states$sex
  beta <- as.matrix(sim$pars$classes[[sex]]$constant$fixed$behavior_betas)
  step_row <- which(step_data$datetime == int_start(step))
  current_behavior <- as.numeric(step_data[step_row, "behavior"])
  step_data <- as.data.frame(step_data)
  (gamma <- diag(5))
  g <- beta[1, ]  #  g = state transition probabilities intercepts
  g <- g +
    beta[2, ] * cos(2*pi * (step_data[step_row, "julian"]/365)) +
    beta[3, ] * sin(2*pi * (step_data[step_row, "julian"]/365)) +
    beta[4, ] * cos(2*pi * (step_data[step_row, "time_proportion"])) +
    beta[5, ] * sin(2*pi * (step_data[step_row, "time_proportion"]))
  exp(g)
  gamma[!gamma] <- exp(g) # Beta values in non-diagonal locations in matrix
  gamma2 <- t(gamma) # probabilities for state transitions are now in rows
  gamma3 <- gamma2/apply(gamma2, 1, sum) # rows sum to 1
  step_data[step_row + 1, "behavior"] <- sample(1:5, size = 1,
    prob = gamma3[current_behavior, ])  # trans prob. given behavior
  return(step_data)
}

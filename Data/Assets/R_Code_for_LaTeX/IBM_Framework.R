## ---- myrcode1
# Create a sequence of numbers
X = 2:10

## ---- ibm_framework
# General function used to run an IBM simulation
RunSimulation <- function(sim = sim,
                          write = TRUE,
                          output_dir = getwd()){
  rep_intervals <- CreateReportIntervals(sim)
  sim <- UpdateAgentStates(init=TRUE, sim=sim)
  sim <- UpdateAgentStepData(init=TRUE, sim=sim)
  sim <- UpdateAgentParsData(init=TRUE, sim=sim)
  sim <- UpdateSpatial(init=TRUE, sim=sim)
  for (j in 1:length(rep_intervals)) {
    step_intervals <- CreateStepIntervals(rep_intervals[[j]])
    for (k in 1:length(step_intervals)) {
      time_steps <- CreateTimeSteps(step_intervals[[k]])
      for (m in 1:length(time_steps)) {
        step <- time_steps[[m]]
        alive_seq <- ReturnAliveSeq(sim)
        sim$agents$all <- UpdateAgentParsData(sim$agents$all)
        for (n in alive_seq) {
          agent_states <- sim$agents$all[[n]][["states"]]
          step_data <- sim$agents$all[[n]][["step_data"]]
          pars_data <- sim$agents$all[[n]][["pars_data"]]
          # START Submodels #
          agent_states <- AgingSubModel(agent_states, step_data, step)
          step_data <- MovementSubModel(sim, agent_states, step_data, step)
          # END Submodels #
          sim$agents$all[[n]][["step_data"]] <-
            UpdateAgentStepData(step_data)
          sim$agents$all[[n]][["states"]] <-
            UpdateAgentStates(agent_states)
        }
        sim$spatial <- UpdateSpatial(sim$spatial)
      } # end of time_steps[[m]]
    } # end of step_interval[[k]]
    sim$agents <- UpdateAgentsReport(sim, rep_intervals[[j]],
      step_intervals)
    sim$agents <- UpdatePopReport(sim, rep_intervals[[j]],
      step_intervals)
  } # end of rep_interval[[j]]
  WriteSimList(write = write, run = names(runs[j]), sim = sim,
    output_dir = getwd(), components = "all")
  return(sim)
}

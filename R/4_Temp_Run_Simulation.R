#save(sim, file="C:/Work/R/Data/Simulation/sim.RData")
load("Data/Simulation/sim.RData")
RemoveExcept(c("sim"))

# Set up simulation run
runs = 1
write = FALSE
output_dir = getwd()

RunSimulation <- function(sim = sim,
                          runs = 1,
                          write = FALSE,
                          output_dir = getwd()) {
  runs = 1
  runs <- CreateRunsList(runs)
  for (i in 1:length(runs)){
    # Calculate vector of all 'time_steps'
    all_time_steps <- as.POSIXct(NA)
    rep_intervals <- CreateReportIntervals(sim)
    library(tictoc)
    tic()
    for (j in 1:length(rep_intervals)){
      #j <- 1
      step_intervals <- CreateStepIntervals(rep_intervals[[j]])
      for (k in 1:length(step_intervals)){
        #k <- 1
        time_steps <- CreateTimeStepsBAEA(step_intervals[[k]], sim = sim)
        for (m in 1:length(time_steps)){
          all_time_steps <- append(all_time_steps, int_start(time_steps[[m]]))
          if (m == length(time_steps)) all_time_steps <- append(all_time_steps,
            int_end(time_steps[[m]]))
        }
      }
    }
    toc()
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

    sim <- UpdateAgentStates(init=TRUE, sim=sim)
    sim <- UpdateAgentStepDataBAEA(init=TRUE, sim=sim, time_steps_df=time_steps_df)
    sim <- UpdateAgentParsData(init=TRUE, sim=sim)
    sim <- UpdateSpatial(init=TRUE, sim=sim)
    tic()
    for (j in 1:length(rep_intervals)) {
      #j <- 1
      step_intervals <- CreateStepIntervals(rep_intervals[[j]])
      for (k in 1:length(step_intervals)) {
        #k <- 1
        time_steps <- CreateTimeStepsBAEA(step_intervals[[k]], sim = sim)
        for (m in 1:length(time_steps)){
          #time_steps[length(time_steps)]
          step <- time_steps[[m]]
          #print(paste("start of time_step:", time_steps[[m]]))
          alive_seq <- ReturnAliveSeq(sim)
          sim$agents$all <- UpdateAgentParsData(sim$agents$all)
          for (n in alive_seq){
            agent_states <- sim$agents$all[[n]][["states"]]
            step_data <- sim$agents$all[[n]][["step_data"]]
            pars_data <- sim$agents$all[[n]][["pars_data"]]
            # START Submodels #
            #agent_states <- AgingSubModel(agent_states, step_data, step)
            step_data <- BehaviorSubModelBAEA(sim, agent_states, step_data, step)
            #step_data <- MovementSubModelBAEA(sim, agent_states, step_data, step)
            #agent_states <- SurvivalSubModel(agent_states, step_data)
            #agent_states <- ReproductionSubModel(agent_states, step_data)
            if(m == length(time_steps)){
              step_data <- LastDailySubModelBAEA(sim, agent_states, step_data, step)
            }
            # END Submodels #
            sim$agents$all[[n]][["states"]] <- UpdateAgentStates(agent_states)
            sim$agents$all[[n]][["step_data"]] <- UpdateAgentStepData(step_data)
          } # end of alive_seq[[n]]
          #print(paste("end of time_step:", time_steps[[m]]))
          # Hatchling/Dispersal Submodel
          sim$spatial <- UpdateSpatial(sim$spatial)
        } # end of time_steps[[m]]
        print(paste("end of step_interval:", step_intervals[[k]]))
      } # end of step_interval[[k]]
      #sim$agents <- UpdateAgentsReport(sim, rep_intervals[[j]], step_intervals)
      #sim$agents <- UpdatePopReport(sim, rep_intervals[[j]], step_intervals)
      print(paste("end of rep_interval:", rep_intervals[[j]]))
    } # end of rep_interval[[j]]
    toc()
    runs[[i]] <- sim
    WriteSimList(write = write, run = names(runs[j]), sim = sim,
      output_dir = getwd(), components = "all")
  }
  return(runs)
}

saveRDS(sim, "Data/Models/sim_completed_20180227")

compiled_step_data <- compiled_step_data %>%
  mutate(behavior = as.factor(behavior))

levels(compiled_step_data$behavior) <- c("Cruise", "Flight", "Nest", "Perch", "Roost")


  CompileAllAgentsStepData(sim=sim)
compiled_step_data$behavior <- as.factor(compiled_step_data$behavior)

# HOW TO FACTOR AND UNLEVEL(?) BEHAVIOR???


PlotLocationSunriseSunset(df=compiled_step_data %>% filter(id == "2"),
  by = "id", color_factor = "behavior",
  individual = "", start = "", end = "", breaks = "14 days", tz = "Etc/GMT+5",
  addsolartimes = FALSE, wrap = TRUE)

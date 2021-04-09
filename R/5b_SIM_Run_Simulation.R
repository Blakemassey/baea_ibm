############################ RUN SIM ###########################################
### This script is used to create a 'sim' list object, consisting of 'agents',
### 'pars', and 'spatial' lists.

pacman::p_load(baear, gisr, ibmr)
pacman::p_load(rgdal, tictoc, tidyverse, lubridate)
#devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/ibmr")
toc_msg <- function(tic, toc, msg, info){
  outmsg <- paste(seconds_to_period(round(toc - tic)))
}

################# LOAD SIM AND RUN SIMULATION ##################################

source('R/5c_SIM_MovementSubmodelBAEA.R')

sim <- readRDS("C:/Work/R/Data/Simulation/sim_20201106.rds")

#sim$agents$input <- sim$agents$input %>% slice(c(1,3,5,7))

print(format(object.size(sim), units = "Mb"))

sim_out_file <- "sim_20200325-01.rds"

sim_out_dir <- "C:/TEMP"
sim_id <- tools::file_path_sans_ext(sim_out_file)

if(!dir.exists(file.path(sim_out_dir, sim_id))){
  dir.create(file.path(sim_out_dir, sim_id))
}

# Modify 'sim' (modify start/end dates, slice agents, etc.)
#sim$pars$global$sim_start <- as.POSIXct("2015-03-15", tz = "UTC")
#sim$pars$global$sim_end <- as.POSIXct("2015-05-15", tz = "UTC")
#sim$agents$input <- sim$agents$input %>% slice(c(1,3))

# Set up simulation run
runs = 3
write = FALSE
output_dir = getwd()

# i <- j <- k <- m <- n <- o <- 1
# m <- 5

RunSimulationBAEA <- function(sim = sim,
                          runs = 1,
                          write = FALSE,
                          output_dir = getwd()) {
  runs <- CreateRunsList(runs)
  for (i in 1:length(runs)){
    rep_intervals <- CreateReportIntervals(sim)
    sim <- UpdateAgentStates(init = TRUE, sim = sim)
    sim <- UpdateAgentStepDataBAEA(init = TRUE, sim = sim,
      rep_intervals = rep_intervals)
    sim <- UpdateAgentParsData(init = TRUE, sim = sim)
    sim <- UpdateSpatialBAEA(init = TRUE, sim = sim)
    for (j in 1:length(rep_intervals)) {
      step_intervals <- CreateStepIntervals(rep_intervals[[j]],
        step_period = sim$pars$global$step_period)
      for (k in 1:length(step_intervals)) {
        step_interval <- step_intervals[[k]]
        time_steps <- CreateTimeStepsInStepIntervalBAEA(step_interval, sim =sim)
        for (m in 1:length(time_steps)){
          time_step <- time_steps[[m]]
          #print(paste("start of time_step:", time_steps[[m]]))
          alive_seq <- ReturnAliveSeq(sim)
          sim$agents$all <- UpdateAgentParsData(sim$agents$all)
          for (n in alive_seq){
            agent_states <- sim$agents$all[[n]][["states"]]
            step_data <- sim$agents$all[[n]][["step_data"]]
            pars_data <- sim$agents$all[[n]][["pars_data"]]
            if (any(step_data$datetime %within% time_step, na.rm=TRUE)){
              steps <- which(step_data$datetime %within% time_step)
              for (o in steps){
                step <- step_data$datetime[o]
                #print(paste("j,k,m,n: ",j,k,m,n))
                print(paste("step:", step))
                # START Submodels #
                #agent_states <- AgingSubModel(agent_states, step_data, step)

                step_data <- BehaviorSubModelBAEA(sim, agent_states, step_data,
                  step)
                #step_data[o + 1, "behavior"] <- 4

                step_data <- MovementSubModelBAEA2(sim, agent_states, step_data,
                  step)
              }
              # END Submodels #
            sim$agents$all[[n]][["states"]] <- UpdateAgentStates(agent_states)
            sim$agents$all[[n]][["step_data"]] <- UpdateAgentStepData(step_data)
            }
          } # end of alive_seq[[n]]
          #print(paste("end of time_step:", time_steps[[m]]))
          sim$spatial <- UpdateSpatial(sim$spatial)
        } # end of time_steps[[m]]
        #print(paste("end of step_interval:", step_intervals[[k]]))
      } # end of step_interval[[k]]
      #sim$agents <- UpdateAgentsReport(sim, rep_intervals[[j]], step_intervals)
      #sim$agents <- UpdatePopReport(sim, rep_intervals[[j]], step_intervals)
      print(paste("end of rep_interval:", rep_intervals[[j]]))
    } # end of rep_interval[[j]]
    #toc()
    runs[[i]] <- SimplifySimSpatialBAEA(sim)
    WriteSimList(write = write, run = names(runs[j]), sim = sim,
      output_dir = getwd(), components = "all")
  }
  return(runs)
}

tic()
sim_out <- RunSimulationBAEA(sim = sim, runs = 3, write = FALSE,
  output_dir = getwd())
toc(func.toc = toc_msg)

# Check object size
#format(object.size(sim_out), units = "Mb")

saveRDS(sim_out, file.path(sim_out_dir, sim_id, sim_out_file))

sim_out1 <- sim_out[[1]]
sim_step_data <- CompileAllAgentsStepData(sim=sim_out1) %>%
  mutate(behavior = as.factor(behavior)) %>%
  group_by(id) %>%
    mutate(step_type = paste0(behavior, "_", lead(behavior))) %>%
    mutate(previous_step_type = lag(step_type)) %>%
  ungroup() %>%
  filter(!is.na(datetime))
sim_step_data <- ConvertStepDataCoordinates(sim_step_data)

nest_locs <- sim_step_data %>% dplyr::filter(behavior == 3)
sim_step_data$behavior <- fct_recode(sim_step_data$behavior, "Cruise" = "1",
  "Flight" = "2", "Nest" = "3", "Perch" = "4", "Roost" = "5")
sim_step_data$behavior <- as.character(sim_step_data$behavior)

# KMLs of Points and Flights
kml_dir = file.path(sim_out_dir, sim_id)
for (i in unique(sim_step_data$id)){
  sim_step_data_i <- sim_step_data %>% filter(id == i)
  ExportKMLTelemetry(sim_step_data_i, lat = "lat", long = "long", alt = NULL,
    speed = NULL, file = paste0("Sim_", str_pad(i, 2, side = "left", "0"),
    ".kml"), icon_by_sex = TRUE, behavior = "behavior", point_color ="behavior",
    output_dir = kml_dir)
}

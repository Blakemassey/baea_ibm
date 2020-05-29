# This script is based on 5_Sim_Run_Simulation

pacman::p_load(baear, gisr, ibmr)
pacman::p_load(rgdal, tictoc, tidyverse, lubridate)

source('R/7_TEMP_MovementSubmodelBAEA.R')
source('R/7_TEMP_1.R')

#devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/ibmr")

sim <- readRDS("C:/Work/R/Data/Simulation/sim_01.rds")
#RemoveExcept(c("sim"))
#sim$pars$global$sim_start <- as.POSIXct("2015-03-15", tz = "UTC")
#sim$pars$global$sim_end <- as.POSIXct("2015-05-20", tz = "UTC")
#sim$agents$input <- sim$agents$input[1:10,]

# Set up simulation run
runs = 1
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
      step_intervals <- CreateStepIntervals(rep_intervals[[j]])
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
                #print(paste("step:", step))
                # START Submodels #
                #agent_states <- AgingSubModel(agent_states, step_data, step)
                step_data <- BehaviorSubModelBAEA(sim, agent_states, step_data,
                  step)
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
        print(paste("end of step_interval:", step_intervals[[k]]))
      } # end of step_interval[[k]]
      #sim$agents <- UpdateAgentsReport(sim, rep_intervals[[j]], step_intervals)
      #sim$agents <- UpdatePopReport(sim, rep_intervals[[j]], step_intervals)
      print(paste("end of rep_interval:", rep_intervals[[j]]))
    } # end of rep_interval[[j]]
    #toc()
    runs[[i]] <- sim
    WriteSimList(write = write, run = names(runs[j]), sim = sim,
      output_dir = getwd(), components = "all")
  }
  return(runs)
}

tic()
sim_out <- RunSimulationBAEA(sim = sim, runs = 1, write = FALSE,
  output_dir = getwd())
toc()

saveRDS(sim_out, "Results/Sim/01_BehaviorMove/sim_completed_20200528_1537.rds")
#sim_out <- readRDS("Results/01_BehaviorMove/sim_completed_20180415.rds")

sim_out1 <- sim_out[[1]]
sim_step_data <- CompileAllAgentsStepData(sim=sim_out1) %>%
  mutate(behavior = as.factor(behavior)) %>%
  filter(!is.na(datetime))
sim_step_data <- ConvertStepDataCoordinates(sim_step_data)

nest_locs <- sim_step_data %>% dplyr::filter(behavior == 3)
levels(sim_step_data$behavior) <- c("Cruise", "Flight", "Nest", "Perch","Roost")
sim_step_data$behavior <- as.character(sim_step_data$behavior)

kml_dir = "Output/Sim/01_BehaviorMove/KML"
for (i in unique(sim_step_data$id)){
  sim_step_data_i <- sim_step_data %>% filter(id == i)
  ExportKMLTelemetry(sim_step_data_i, lat = "lat", long = "long", alt = NULL,
    speed = NULL, file = paste0("Sim_", str_pad(i, 2, side = "left", "0"),
    ".kml"), icon_by_sex = TRUE, behavior = "behavior", point_color ="behavior",
    output_dir = kml_dir)
}

locs_dir = "Output/Sim/01_BehaviorMove/Plots/Daily_Locations"
for (i in unique(sim_step_data$id)){
  PlotLocationSunriseSunset(df = sim_step_data %>% filter(id == i),
    by = "id", color_factor = "behavior", individual = "", start = "", end = "",
    breaks = "14 days", tz = "Etc/GMT+5", addsolartimes = FALSE, wrap = TRUE)
  SaveGGPlot(file.path(locs_dir ,paste0("DailyLocs_", str_pad(i, 2,
    side = "left", "0"), ".png")))
}

title_sim = "Daily Behavior Distributions (simulated data)"
PlotBehaviorProportionBar(sim_step_data, title = title_sim)
SaveGGPlot("Results/Sim/01_BehaviorMove/Behavior/Proportion_Bar_SIM.png")

# Plots of original behavior
baea_behavior <- readRDS(file="Data/Baea/baea_behavior.rds")
behave_dir <- "Results/Analysis/Plots/Behavior"

PlotLocationSunriseSunset(df=baea_behavior %>% as.data.frame() %>%
    filter(id == "Three"),
  by = "id", color_factor = "behavior", individual = "", start = "2015-03-20",
  end = "2015-09-20", breaks = "14 days", tz = "Etc/GMT+5",
  addsolartimes = FALSE, wrap = TRUE)
SaveGGPlot(file.path(behave_dir ,paste0("DailyLocs_Three.png")))

PlotLocationSunriseSunset(df = baea_behavior %>% as.data.frame() %>%
    filter(id == "Ellis"),
  by = "id", color_factor = "behavior", individual = "",
  start = "2016-03-20", end = "2016-09-20", breaks = "10 days",
  tz = "Etc/GMT+5", addsolartimes = FALSE, wrap = TRUE)
SaveGGPlot(file.path(behave_dir ,paste0("DailyLocs_Ellis.png")))

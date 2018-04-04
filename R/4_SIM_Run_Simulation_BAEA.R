library(baear)
library(gisr)
library(ibmr)
library(lubridate)
library(dplyr)
library(rgdal)


#save(sim, file="Data/Simulation/sim.RData")
load("Data/Simulation/sim.RData")
RemoveExcept(c("sim"))

sim$pars$global$sim_end <- as.POSIXct("2015-04-20", tz = "UTC")

# Set up simulation run
runs = 1
write = FALSE
output_dir = getwd()

RunSimulation <- function(sim = sim,
                          runs = 1,
                          write = FALSE,
                          output_dir = getwd()) {
  runs <- CreateRunsList(runs)
  for (i in 1:length(runs)){
    rep_intervals <- CreateReportIntervals(sim)
    sim <- UpdateAgentStates(init = TRUE, sim = sim)
    sim <- UpdateAgentStepDataBAEA(init = TRUE, sim = sim,
      rep_intervals = rep_intervals)
    #save(sim, file="Data/Simulation/sim_stepsTEST.RData")
    load("Data/Simulation/sim_stepsTEST.RData")
    sim <- UpdateAgentParsData(init = TRUE, sim = sim)
    sim <- UpdateSpatialBAEA(init = TRUE, sim = sim)
    for (j in 1:length(rep_intervals)) {
      #j <- 1
      step_intervals <- CreateStepIntervals(rep_intervals[[j]])
      for (k in 1:length(step_intervals)) {
        #k <- 1
        step_interval <- step_intervals[[k]]
        time_steps <- CreateTimeStepsInStepIntervalBAEA(step_interval, sim =sim)
        for (m in 1:length(time_steps)){
          #m <- 5
          time_step <- time_steps[[m]]
          print(paste("start of time_step:", time_steps[[m]]))
          alive_seq <- ReturnAliveSeq(sim)
          sim$agents$all <- UpdateAgentParsData(sim$agents$all)
          for (n in alive_seq){
            #n <- 1
            agent_states <- sim$agents$all[[n]][["states"]]
            step_data <- sim$agents$all[[n]][["step_data"]]
            pars_data <- sim$agents$all[[n]][["pars_data"]]
            if (any(step_data$datetime %within% time_step, na.rm=TRUE)){
              steps <- which(step_data$datetime %within% time_step)
              for (o in steps){
                #o <- 1
                step <- step_data$datetime[o]
                print(paste("j,k,m,n: ",j,k,m,n))
                print(paste("step:", step))
                # START Submodels #
                #agent_states <- AgingSubModel(agent_states, step_data, step)
                step_data <- BehaviorSubModelBAEA(sim, agent_states, step_data,
                  step)
                step_data <- MovementSubModelBAEA2(sim, agent_states, step_data,
                  step)
                #agent_states <- SurvivalSubModel(agent_states, step_data)
                #agent_states <- ReproductionSubModel(agent_states, step_data)
              }
              # END Submodels #
            sim$agents$all[[n]][["states"]] <- UpdateAgentStates(agent_states)
            sim$agents$all[[n]][["step_data"]] <- UpdateAgentStepData(step_data)
            }
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
    #toc()
    runs[[i]] <- sim
    WriteSimList(write = write, run = names(runs[j]), sim = sim,
      output_dir = getwd(), components = "all")
  }
  return(runs)
}

saveRDS(sim, "Data/Models/sim_completed_20180404")

sim_step_data <- CompileAllAgentsStepData(sim=sim) %>%
  mutate(behavior = as.factor(behavior)) %>%
  filter(!is.na(id))

levels(sim_step_data$behavior) <- c("Cruise", "Flight", "Nest", "Perch","Roost")

wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N
sim_xy <- SpatialPoints(sim_step_data[c("x", "y")], proj4string=wgs84n19)
sim_longlat <- spTransform(sim_xy, CRS("+proj=longlat +datum=WGS84"))
sim_step_data$long <- coordinates(sim_longlat)[,1]
sim_step_data$lat <- coordinates(sim_longlat)[,2]

for (i in unique(sim_step_data$id)){
  PlotLocationSunriseSunset(df = sim_step_data %>% filter(id == i),
    by = "id", color_factor = "behavior", individual = "", start = "", end = "",
    breaks = "14 days", tz = "Etc/GMT+5", addsolartimes = FALSE, wrap = TRUE)
  SaveGGPlot(paste0("Output/Plots/Behavior/Sim/DailyLocs_",i,".png"))
}

ExportKMLTelemetry(df = sim_step_data, id = "id", datetime = "datetime",
  lat = "lat", long = "long", behavior = "behavior", point_color = "behavior",
  file = "sim_2018-04-04.kml",
  output_dir = "C:/Users/blake/Desktop")

title_sim = "Daily Behavior Distributions (simulated data)"
PlotBehaviorProportionBar(compiled_step_data, title = title_sim)
SaveGGPlot("Output/Plots/Behavior/Proportion_Bar_SIM.png")


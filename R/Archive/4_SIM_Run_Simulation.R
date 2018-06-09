############################# RUN SIMULATION ###################################

library(baear)
library(gisr)
library(ibmr)

library(dplyr)
library(RColorBrewer)

cols <- brewer.pal(8, "Set1")

#save(sim, file="C:/Work/R/Data/Simulation/sim.RData")
load("Data/Simulation/sim.RData")
RemoveExcept(c("sim"))

# Examine 'sim' global parameters
sim$pars$global$sim_period <- period(20, "days")

# Set up simulation run

runs = 1
write = FALSE
output_dir = getwd()
#custom_agent_report = FALSE
#custom_pop_report = FALSE

devtools::reload("C:/Work/R/Packages/baear")
devtools::reload("C:/Work/R/Packages/gisr")
devtools::reload("C:/Work/R/Packages/ibmr")

output <- RunSimulation(sim=sim, runs=1, write=FALSE)
save(output, file="C:/Work/R/Data/Simulation/sim_output.RData")
#load("C:/Work/R/Data/Simulation/sim_output.RData")

RemoveExcept(c("sim","step_data"))

step_data <- CompileAllAgentsStepData(output$run_1)
step_data <- ConvertStepDataCoordinates(step_data)

output_dir = "C:/Users/blake/Desktop"

for (i in unique(step_data$id)){
  step_data1 <- step_data %>%
    filter(id == i)
  ExportKMLTelemetry(step_data1, lat="lat", long="long", alt=NULL, speed=NULL,
    file = paste0("Sim_", i, ".kmz"), icon_by_sex=TRUE, output_dir=output_dir)
}

con_nest_rasters <- sim$spatial$con_nest_raster
names(con_nest_rasters)
con_nest_raster1 <- sim$spatial$con_nest_raster[["nest_282A"]]
con_nest_raster3 <- sim$spatial$con_nest_raster[["nest_446R01"]]
con_nest_raster2 <- sim$spatial$con_nest_raster[["nest_659A"]]
con_nest_raster4 <- sim$spatial$con_nest_raster[["nest_423R01"]]


ExportKMLRasterOverlay(con_nest_raster1, color_pal=terrain.colors(10),
  outfile = "ConNest1", output_dir=output_dir)
ExportKMLRasterOverlay(con_nest_raster2, color_pal=terrain.colors(10),
  outfile = "ConNest2", output_dir=output_dir)
ExportKMLRasterOverlay(con_nest_raster3, color_pal=terrain.colors(10),
  outfile = "ConNest3", output_dir=output_dir)
ExportKMLRasterOverlay(con_nest_raster4, color_pal=terrain.colors(10),
  outfile = "ConNest4", output_dir=output_dir)




RunSimulation <- function(sim = sim,
                          runs = 1,
                          write = FALSE,
                          output_dir = getwd()) {
  runs = 1
  runs <- CreateRunsList(runs)
  for (i in 1:length(runs)){
    i <- 1
    rep_intervals <- CreateReportIntervals(sim)
    sim <- UpdateAgentStates(init=TRUE, sim=sim)
    sim <- UpdateAgentStepDataBAEA(init=TRUE, sim=sim)
    sim <- UpdateAgentParsData(init=TRUE, sim=sim)
    sim <- UpdateSpatial(init=TRUE, sim=sim)
    for (j in 1:length(rep_intervals)) {
      j <- 1
      step_intervals <- CreateStepIntervals(rep_intervals[[j]])
      for (k in 1:length(step_intervals)) {
        k <- 1
        time_steps <- CreateTimeStepsBAEA(step_intervals[[k]])
        for (m in 1:length(time_steps)){
          step <- time_steps[[m]]
          alive_seq <- ReturnAliveSeq(sim)
          sim$agents$all <- UpdateAgentParsData(sim$agents$all)
          for (n in alive_seq){
            n <- 1
            agent_states <- sim$agents$all[[n]][["states"]]
            step_data <- sim$agents$all[[n]][["step_data"]]
            pars_data <- sim$agents$all[[n]][["pars_data"]]
            # START Submodels #
            agent_states <- AgingSubModel(agent_states, step_data, step)
            step_data <- BehaviorSubModelBAEA(agent_states, step_data, step)
            step_data <- MovementSubModelBAEA(sim, agent_states, step_data, step)
            #agent_states <- SurvivalSubModel(agent_states, step_data)
            #agent_states <- ReproductionSubModel(agent_states, step_data)
            # END Submodels #
            sim$agents$all[[n]][["states"]] <- UpdateAgentStates(agent_states)
            sim$agents$all[[n]][["step_data"]] <- UpdateAgentStepData(step_data)
          } # end of alive_seq[[n]]
          print(paste("end of time_step:", time_steps[[m]]))
          # Hatchling/Dispersal Submodel
          sim$spatial <- UpdateSpatial(sim$spatial)
        } # end of time_steps[[m]]
        print(paste("end of step_interval:", step_intervals[[k]]))
      } # end of step_interval[[k]]
      sim$agents <- UpdateAgentsReport(sim, rep_intervals[[j]], step_intervals)
      sim$agents <- UpdatePopReport(sim, rep_intervals[[j]], step_intervals)
      print(paste("end of rep_interval:", rep_intervals[[j]]))
    } # end of rep_interval[[j]]
    runs[[i]] <- sim
    WriteSimList(write = write, run = names(runs[j]), sim = sim,
      output_dir = getwd(), components = "all")
  }
  return(runs)
}

# Setup anthill
library(anthill)
config()

# Create a directory to hold the results
output_dir <- "Z:/Users/Blake/Simulation/Output"
if(!file.exists(output_dir)) dir.create(output_dir, recursive=TRUE)
setwd("Z:/Users/Blake/Simulation")

# Create the parameters table
all_baea <- c("Three", "Hebron", "Sandy", "Onawa", "Branch")
all_hr <- c("Interpolate", "Pareto", "HalfNormal")

sim_pars <- expand.grid(baea=all_baea, homerange=all_hr)
sim_pars$id <- seq(1:nrow(sim_pars))
sim_pars$file_source <- as.character(file.path(getwd(), "Input"))
sim_pars$out_file <- as.character(file.path(getwd(), "Output"))

# Setting up call 
library(anthill)
config() # Will only work on cluster

call_sim <- "SimWrapper(par=sim_pams)"
subtaskarg_sim <- "id"
subtasks_sim <- paste("range!:", compact.range(sim_pars$id),  sep="")
priority.use <- 2
maxthreads.use <- 10

# Launch call
simple.launch(call = call_sim,
              subtaskarg = subtaskarg_sim, 
              subtask = subtasks_sim, 
              name = "BAEA_Simulation", 
              owner = "Blake_M", 
              priority = priority.use, 
              maxthreads = maxthreads.use)

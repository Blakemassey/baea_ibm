#------------------------ Run Simulation --------------------------------------#
# This script is used to run a full simulation using a 'sim' object
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------

# Load packages
pacman::p_load(baear, gisr, ibmr)
pacman::p_load(gpindex, rgdal, tictoc, tidyverse, lubridate)
source('R/5b_SIM_MovementSubmodel_RunSim.R')

# Directories and files
sim_file <- "C:/Work/Sim_Data/sim_20210725.rds"
sim_out_file <- "sim_20210725-81.rds"
sim_out_dir <- "C:/TEMP"
sim_id <- tools::file_path_sans_ext(sim_out_file)

# Load Sim and Run Simulation --------------------------------------------------

# Read 'sim' file
sim <- readRDS(sim_file)

if(!dir.exists(file.path(sim_out_dir, sim_id))){
  dir.create(file.path(sim_out_dir, sim_id))
}

tic()
sim_out <- RunSimulationBAEA(sim = sim, runs = 1, write = FALSE,
  output_dir = getwd())
saveRDS(sim_out, file.path(sim_out_dir, sim_id, sim_out_file))
toc(func.toc = TocMsg)

# Check object size
pryr::object_size(sim_out)

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

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
sim_out_file <- "sim_20210725-112.rds"
sim_out_dir <- "C:/TEMP"
sim_id <- tools::file_path_sans_ext(sim_out_file)

# Load Sim and Run Simulation --------------------------------------------------

# Read 'sim' file
sim <- readRDS(sim_file)

if(!dir.exists(file.path(sim_out_dir, sim_id))){
  dir.create(file.path(sim_out_dir, sim_id))
}

tic()
resample_n <- 0
sim_out <- RunSimulationBAEA(sim = sim, runs = 1, write = FALSE,
  output_dir = getwd())
saveRDS(sim_out, file.path(sim_out_dir, sim_id, sim_out_file))
toc(func.toc = TocMsg)

# Check object size
pryr::object_size(sim_out)

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

# For testing
# if(FALSE){
#   if(TRUE) sim$agents$input <- sim$agents$input %>% slice(c(1,3))
#   if(TRUE) sim$pars$global$sim_start <- as.POSIXct("2015-03-15", tz = "UTC")
#   if(TRUE) sim$pars$global$sim_end <- as.POSIXct("2015-04-15", tz = "UTC")
#   runs = 1
#   write = FALSE
#   output_dir = getwd()
#   i <- j <- k <- m <- n <- o <- 1
#   m <- 5
# }

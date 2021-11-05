#------------------------ Run Simulation --------------------------------------#
# This script is used to run a full simulation using a 'sim' object
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------

# Load packages
pacman::p_load(baear, gisr, ibmr)
pacman::p_load(gpindex, rgdal, tictoc, tidyverse, lubridate)
source('R/5d_SIM_MovementSubmodelBAEA.R')

# Directories and files
sim_file <- "C:/Work/Sim_Data/sim_20210725.rds"
sim_out_file <- "sim_20210725-63.rds"
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
toc(func.toc = toc_msg)

# Check object size
pryr::object_size(sim_out)

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

# sim_out1 <- sim_out[[1]]
# sim_step_data <- CompileAllAgentsStepData(sim=sim_out1) %>%
#   mutate(behavior = as.factor(behavior)) %>%
#   group_by(id) %>%
#     mutate(step_type = paste0(behavior, "_", lead(behavior))) %>%
#     mutate(previous_step_type = lag(step_type)) %>%
#   ungroup() %>%
#   filter(!is.na(datetime))
# sim_step_data <- ConvertStepDataCoordinates(sim_step_data)
#
# nest_locs <- sim_step_data %>% dplyr::filter(behavior == 3)
# sim_step_data$behavior <- fct_recode(sim_step_data$behavior, "Cruise" = "1",
#   "Flight" = "2", "Nest" = "3", "Perch" = "4", "Roost" = "5")
# sim_step_data$behavior <- as.character(sim_step_data$behavior)
#
# # KMLs of Points and Flights
# kml_dir = file.path(sim_out_dir, sim_id, "KMLs")
#
# if(!dir.exists(file.path(kml_dir))){
#   dir.create(file.path(kml_dir))
# }
#
# for (i in unique(sim_step_data$id)){
#   sim_step_data_i <- sim_step_data %>% filter(id == i)
#   ExportKMLTelemetry(sim_step_data_i, lat = "lat", long = "long", alt = NULL,
#     speed = NULL, file = paste0("Sim_", str_pad(i, 2, side = "left", "0"),
#     ".kml"), icon_by_sex = TRUE, behavior = "behavior",point_color="behavior",
#     output_dir = kml_dir)
# }

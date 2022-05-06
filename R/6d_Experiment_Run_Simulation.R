############################ RUN SIM ###########################################
### This script is used to create a 'sim' list object, consisting of 'agents',
### 'pars', and 'spatial' lists.

pacman::p_load(baear, gisr, ibmr)
pacman::p_load(gpindex, rgdal, tictoc, tidyverse, lubridate)

################# LOAD SIM AND RUN SIMULATION ##################################

source('R/5b_SIM_MovementSubmodel_RunSim.R')

site <- "Wilson"

for (j in c(16)) {

  exp_id <- as.character(j)

  for (i in c("C", "N", "S", "NS")){
    print(i)
    sim <- readRDS(paste0("C:/Work/Sim_Data/sim_20210831_", site, "_", i, ".rds"))
    format(object.size(sim), units = "Gb")

    sim_out_file <- paste0("sim_20210831_", site, "_", i, "-", exp_id, ".rds")
    sim_out_dir <- "C:/TEMP"
    sim_id <- tools::file_path_sans_ext(sim_out_file)

    if(!dir.exists(file.path(sim_out_dir, sim_id))){
      dir.create(file.path(sim_out_dir, sim_id))
    }

    # Run simulation!
    source('R/5b_SIM_MovementSubmodel_RunSim.R')
    tic()
    sim_out <- RunSimulationBAEA(sim = sim, runs = 1, write = FALSE,
      output_dir = getwd())
    toc(func.toc = TocMsg)

    # Save sim as .rds
    saveRDS(sim_out, file.path(sim_out_dir, sim_id, sim_out_file))

    # Check object size
    format(object.size(sim_out), units = "Gb")

    rm(sim, sim_out_file, sim_id, sim_out)
    gc()
  }
}

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

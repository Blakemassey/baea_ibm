# Source sim_log file
source(file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/R",
 "5c_SIM_Simulation_Log.R"))

# Variables
sim_max <- sim_log %>% pull(sim) %>% unique(.) %>% str_remove(., ".*-") %>%
  as.integer(.) %>% max(.)
sims_all <- c(1:5, 7:sim_max)
sims_subset_1 <- c(70:99)
sims_subset_2 <- c(88:sim_max)
sims_subset_3 <- c(103,104,108,109,110,111,112,77,78,105,100,107,101,106)
sims_subset_4 <- c(76, 77, 78)

sims_group_1 <- c(78, 81:84)
sims_group_2 <- c(77, 85:88)

sims_vec <- sims_subset_4
sims_group <- sims_group_1

nest_vec <- c("Ellis", "Hebron", "Musquash", "Sandy")
points_vec <- c("all", "air", "stationary")

# Directories
slide_dir <- "C:/Users/blake/Desktop/Slidy_Presentations"

# Results by Sim - shows results of each sim
for (j in points_vec){
  rmarkdown::render("R/Slidy/Sim_Results_By_Sim.Rmd",
    params = list(sims = sims_vec, points = j),
    output_file = file.path(slide_dir, paste0("Results by Sim - ",
      str_to_title(j), ".html")))
}

# Results by Nest - shows sims by nest
for (j in points_vec){
  rmarkdown::render("R/Slidy/Sim_Results_By_Nest.Rmd",
    params = list(sims = sims_vec, nests = nest_vec, points = j),
    output_file = file.path(slide_dir, paste0("Results by Nest - ",
      str_to_title(j), ".html")))
}

# Results by Sims Set - shows data from sims combined
rmarkdown::render("R/Slidy/Sim_Results_By_Sims_Set.Rmd",
  params = list(sims = sims_group),
  output_file = file.path(slide_dir, paste0("Results By Sim Set - ",
    paste0(sims_group, collapse = "_"), ".html")))
file.remove(list.files("C:/Temp/TEMP_Calibration", full.names = TRUE))
file.remove(list.files("C:/Temp/TEMP_Maps", full.names = TRUE))

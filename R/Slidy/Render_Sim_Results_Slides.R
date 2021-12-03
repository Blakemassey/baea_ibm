pacman::p_load(imager, knitr, tidyverse, kableExtra)
source(file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/R",
 "5c_SIM_Run_Simulation_Log.R"))

# Variables
sim_max <- sim_log %>% pull(sim) %>% unique(.) %>% str_remove(., ".*-") %>%
  max(.) %>% as.integer(.)
sims_all <- c(1:5, 7:sim_max)
sims_subset <- c(78, 81)
sims_vec <- list(sims_all, sims_subset)[[2]]
nest_vec <- c("Ellis", "Hebron", "Musquash", "Sandy")
points_vec <- c("all", "air", "stationary")

# Directories
slide_dir <- "C:/Users/blake/Desktop/Slidy_Presentations"

# Results by Sim
for (j in points_vec){
  rmarkdown::render("R/Slidy/Sim_Results_By_Sim.Rmd",
    params = list(sims = sims_vec, points = j),
    output_file = file.path(slide_dir, paste0("Sim_", str_to_title(j),
      ".html")))
}

# Results by Nest
for (j in points_vec){
  for (m in nest_vec){
    print(paste0("j: ", j, " ; m: ", m))
    rmarkdown::render("R/Slidy/Sim_Results_By_Nest.Rmd",
      params = list(sims = sims_vec, nest = m, points = j),
      output_file = file.path(slide_dir, paste0("Nest_", m, "_",
        str_to_title(j), ".html")))
  }
}



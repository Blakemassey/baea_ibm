# File Directory and ID

# File Directory and ID
sim_runs <- readRDS(file.path(sim_dir, sim_id, sim_rds))

# Sim data
for (i in seq_len(length(sim_runs))){
  sim_out <- sim_runs %>% pluck(i)
  print(paste0("Starting run: ", i, " of ", length(sim_runs)))
  sim_agents_input <- sim_out %>% pluck(1, "agents", "input")
  sim_behavior_i <- CompileAllAgentsStepData(sim = sim_out) %>%
    mutate(behavior = as.factor(behavior)) %>%
    group_by(id) %>%
    ungroup() %>%
    filter(!is.na(datetime)) %>%
    ConvertStepDataCoordinates(.)
  sim_behavior_i$behavior <- fct_recode(sim_behavior_i$behavior, "Cruise" = "1",
    "Flight" = "2", "Nest" = "3", "Perch" = "4", "Roost" = "5")
  sim_behavior_i$behavior <- as.character(sim_behavior_i$behavior)
  if(i == 1){
    sim_behavior <- sim_behavior_i
  } else {
    sim_behavior <- bind_rows(sim_behavior, sim_behavior_i)
  }
}

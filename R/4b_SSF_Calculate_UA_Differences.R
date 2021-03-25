################## ModelFit_SSF_Calculate_Covariates ###########################
# Load packages, scripts, and input parameters ---------------------------------
pacman::p_load(plyr, dplyr, forcats, ggplot2, ggthemes, purrr, stringr, tidyr,
  tibble)
pacman::p_load(baear, gisr, ibmr)

# Directories
ua_data_dir <- "Output/Analysis/SSF/UA_Data"
ua_data_diff_dir <- "Output/Analysis/SSF/UA_Data_Diff"

## Import Base Raster, Steps Data, and Movement Parameters ---------------------

ua_steps_org <- list.files(path = file.path(ua_data_dir),
    pattern = "^ua_steps_*") %>%
  map(~ readRDS(file.path(ua_data_dir, .))) %>%
  reduce(bind_rows) %>%
  mutate(case = factor(case)) %>%
  mutate(behavior_behavior = factor(behavior_behavior)) %>%
  dplyr::select(behavior_behavior, step_id, case, matches("[0-9]"))

ua_steps <- ua_steps_org

# Rename distance metric columns
colnames(ua_steps) <- colnames(ua_steps) %>%
  str_replace_all("developed_dist0", "dist_developed0") %>%
  str_replace_all("hydro_dist0", "dist_hydro0") %>%
  str_replace_all("road_dist0", "dist_road0") %>%
  str_replace_all("turbine_dist0", "dist_turbine0")

# Limits the dist_turbine to 20km
ua_steps_dist <- ua_steps %>%
  mutate(dist_turbine0 = if_else(dist_turbine0 < 20000, dist_turbine0, 20000))

# Find rows with missing data
ua_steps_na <- ua_steps_dist %>%
  filter_all(any_vars(is.na(.))) %>%
  select(behavior_behavior, step_id)

# Remove step_id pairs where any data is missing
ua_steps_all <- ua_steps_dist %>% anti_join(., ua_steps_na,
  by = c('behavior_behavior', 'step_id'))

# Check for behavior, step_id, case columns and covariates columns
unique(colnames(ua_steps_all %>% select(!matches("[0-9]"))))
unique(str_remove_all(colnames(ua_steps_all %>% select(matches("[0-9]"))),
  "[:digit:]"))

# Calculate differences for each of the behavior_behaviors
ua_steps_diff <- ua_steps_all %>%
  group_by(behavior_behavior, step_id) %>%
  arrange(behavior_behavior, step_id, case) %>%
  mutate(across(matches("[0-9]"), diff)) %>%
  ungroup() %>%
  filter(case == 1) %>%
  select(-c(step_id))

square <- function(x) {
  out <- x^2
  return(out)
}

ua_steps_squared_diff <- ua_steps_all %>%
  dplyr::select(behavior_behavior, step_id, case, developed0:developed30) %>%
  #group_by(behavior_behavior) %>%
  slice(1:100) %>%
  arrange(behavior_behavior, step_id, case) %>%
  ungroup() %>%
  mutate(across(matches("[0-9]"), square, .names = "add1_{col}")) %>%
  #filter(case == 1) %>%
  select(-c(step_id)) %>%
  head

ames_data %>%
  group_by(neighborhood) %>%
  summarize(across(where(is.numeric), mean, .names = "mean_{col}")) %>%
  head()

# Check size (>100 Mb is too large for GitHub)
format(object.size(ua_steps_diff), units = 'MB')

# Split up into behaviors and save RDS
for (i in unique(ua_steps_diff$behavior_behavior)){
  ua_steps_diff_i <- ua_steps_diff %>% filter(behavior_behavior == i)
  step_type_numeric <- i %>%
    fct_drop() %>%
    str_to_lower() %>%
    str_replace_all(" -> ", "_")
  saveRDS(ua_steps_diff_i, file.path(ua_data_diff_dir, paste0("ua_steps_diff_",
    step_type_numeric, ".rds")))
}



### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###

# ua_steps_all <- list.files(path = file.path(ua_data_dir),
#     pattern = "^ua_steps_*")

# for (i in seq_along(ua_steps_all)){
#   ua_steps_i <- readRDS(file.path(ua_data_dir, ua_steps_all[i]))
#   print(paste0(ua_steps_all[i], " Columns = ", dim(ua_steps_i)[2]))}


# ua_steps_i_rd <- readRDS(file.path(ua_data_dir, ua_steps_i[2]))
#
# ua_steps_i_combine <- left_join(ua_steps_i_org, ua_steps_i_rd)
#
# saveRDS(ua_steps_i_combine, file.path(ua_data_dir, ua_steps_i[1]))
# rm(ua_steps_i, ua_steps_i_org, ua_steps_i_rd, ua_steps_i_combine)

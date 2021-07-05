################## ModelFit_SSF_Calculate_Covariates ###########################
# Load packages, scripts, and input parameters ---------------------------------
pacman::p_load(plyr, dplyr, forcats, ggplot2, ggthemes, purrr, pryr, stringr,
  tidyr, tibble)
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

# Find rows with missing data
ua_steps_na <- ua_steps %>%
  filter(if_any(everything(), is.na))
  select(behavior_behavior, step_id)

# Remove step_id pairs where any data is missing
ua_steps_all <- ua_steps %>% anti_join(., ua_steps_na,
  by = c('behavior_behavior', 'step_id'))

# Check for behavior, step_id, case columns and covariates columns
unique(colnames(ua_steps_all %>% select(!matches("[0-9]"))))
unique(str_remove_all(colnames(ua_steps_all %>% select(matches("[0-9]"))),
  "[:digit:]"))

# Calculate squared values
square <- function(x) {
  out <- x^2
  return(out)
}

# Test procedure
ua_steps_squared <- ua_steps_all %>%
  mutate(across(matches("[0-9]"), square, .names = "{col}^2"))

# Calculate differences for each behavior_behavior
ua_steps_diff <- ua_steps_squared %>%
  group_by(behavior_behavior, step_id) %>%
  arrange(behavior_behavior, step_id, case) %>%
  mutate(across(matches("[0-9]"), diff)) %>%
  ungroup() %>%
  filter(case == 1) %>%
  select(-c(step_id))

# Check object sizes (>100 Mb is too large for GitHub)
for(i in unique(ua_steps_diff$behavior_behavior)){
  ua_steps_diff_i <- ua_steps_diff %>%
    filter(behavior_behavior == i)
  print(paste0(i))
  print(object_size(ua_steps_diff_i))
}

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

# # Rename distance metric columns (Not longer needed)
# colnames(ua_steps) <- colnames(ua_steps) %>%
#   str_replace_all("developed_dist0", "dist_developed0") %>%
#   str_replace_all("hydro_dist0", "dist_hydro0") %>%
#   str_replace_all("road_dist0", "dist_road0") %>%
#   str_replace_all("turbine_dist0", "dist_turbine0")

# # Limits the dist_turbine to 20km (Done withing 1b_Create_GIS now)
# ua_steps_dist <- ua_steps %>%
#   mutate(dist_turbine0 = if_else(dist_turbine0 < 20000, dist_turbine0, 20000)) %>%
#   mutate(dist_hydro0 = if_else(dist_hydro0 < 5000, dist_hydro0, 5000)) %>%
#   mutate(dist_developed0 = if_else(dist_developed0 < 2000, dist_developed0, 2000)) %>%
#   mutate(dist_road0 = if_else(dist_road0 < 2000, dist_road0, 2000))


# # Run procedure to calculate differences
# ua_steps_squared_diff <- ua_steps_squared %>%
#   group_by(behavior_behavior, step_id) %>%
#   arrange(behavior_behavior, step_id, case) %>%
#   mutate(across(matches("[0-9]"), diff)) %>%
#   ungroup() %>%
#   filter(case == 1) %>%
#   select(-c(step_id))


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


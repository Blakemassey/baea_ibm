###################### COLLISION RISK MODEL ####################################
### This script is used to create and run Collision Risk Models on the
### experiment outcomes

# Setup ------------------------------------------------------------------------

pacman::p_load(baear, gisr, ibmr)
pacman::p_load(gpindex, mapview, rgdal, sf, tictoc, tidyverse, lubridate,
  rstatix)

# Variables
site <-  "Grand_Lake" #"Wilson"

# Directories
exp_dir <- "C:/TEMP"
exp_turbines_dir <- file.path("C:/ArcGIS/Data/R_Input/EXP", site)
wind_output_dir <- "Output/Analysis/Wind"
exp_output_dir <- "Output/Experiment"

# Files
baea_behavior_file <- "Data/Baea/baea_behavior.rds"
wind_transits_sum_file <- file.path(exp_output_dir, paste0("wind_transits_sum_",
  str_to_lower(site), ".rds"))
site_n_turbines_file <- file.path(wind_output_dir, paste0(str_to_lower(site),
  "_n_turbines.rds"))
site_s_turbines_file <- file.path(wind_output_dir, paste0(str_to_lower(site),
  "_s_turbines.rds"))
flight_collision_risk_file <- file.path(wind_output_dir,
  paste0("flight_collision_risk_", str_to_lower(site), ".rds"))

# Behavior colors
behavior_colors <- CreateColorsByMetadata(file = file.path("Data/Assets",
  "behavior_colors.csv"), metadata_id = "behavior")
flight_color <- behavior_colors[which(names(behavior_colors) == "Flight")]

## Import Data -----------------------------------------------------------------

baea_behavior_org <- readRDS(file = baea_behavior_file)
wind_transits_sum_org <- readRDS(wind_transits_sum_file)
site_wt_n <- readRDS(site_n_turbines_file)
site_wt_s <- readRDS(site_s_turbines_file)
site_wt_n_buff <- site_wt_n %>% st_buffer(56)
site_wt_s_buff <- site_wt_s %>% st_buffer(56)

# Flight Statistics ------------------------------------------------------------

baea_flights <- baea_behavior_org %>%
  dplyr::select(id, behavior, speed) %>%
  filter(behavior == "Flight") %>%
  filter(speed != 0)  # Removed zeroes

# GPS transmitter speed is in knots. Multiple by 0.51444 to get m/sec
baea_speed <- mean(baea_flights$speed) * 0.514444

# Wind Farm Crossing Statistics ------------------------------------------------

wind_transits_sum <- readRDS(wind_transits_sum_file) %>%
  mutate(behavior_line = as.factor(behavior_line)) %>%
  mutate(scenario = as.factor(scenario))

# Summary statistics
wind_transits_sum_stats <- wind_transits_sum %>%
  group_by(behavior_line, scenario) %>%
  get_summary_stats(.)

## Collision Risk Analysis -----------------------------------------------------

# Turbines - Vestas V112-3.3 (data from general specifications document)
blade_n <- 3
rotor_radius <- 56 # meters
rotation_speed_rpm <- 12.8 # rpm static rotor speed
rotation_period <- 60/rotation_speed_rpm
chord_max <- 4 # meters
integration_interval <- 0.05
radius_proportion <- seq(0.05, 1, by = integration_interval)
chord_proportion <- c(0.73, 0.79, 0.88, 0.96, 1.00, 0.98, 0.92, 0.85,
  0.80, 0.75, 0.70, 0.64, 0.58, 0.52, 0.47,0.41, 0.37, 0.30, 0.24, 0.00)
  # These are the proportion of the total chord width
blade_pitch <- 30

# Flight metrics
upwind_proprtion <- .5
downwind_proprtion <- (1 - upwind_proprtion)

# Bald Eagle metrics
bird_length <- 0.9 # Mean in MA (mass.gov/doc/bald-eagle-factsheet/download)
bird_wingspan <- 2.1  # Mean in MA (mass.gov/doc/bald-eagle-factsheet/download)
bird_aspect_ratio <- bird_length/bird_wingspan
bird_speed <- baea_speed # metric is m/sec
glide <- 1 # flap = 0, glide = 1
glide_metric <- if_else(glide == 1, 2/pi, 1)

# Create table
rows_n <- 20
collision_risk_df <- tibble(
  radius_proportion = radius_proportion,
  chord_proportion = chord_proportion,
  alpha = rep(NA, rows_n),
  up_length = rep(NA, rows_n),
  up_p = rep(NA, rows_n),
  down_length = rep(NA, rows_n),
  down_p = rep(NA, rows_n))

# Single transit collision risk
collision_risk_df <- collision_risk_df %>%
  mutate(alpha = bird_speed * rotation_period /
    (radius_proportion * rotor_radius * 2 * pi)) %>%
  mutate(up_length_term1 =
    abs(chord_max * chord_proportion * sin((blade_pitch * pi)/180) +
    alpha * chord_max * chord_proportion * cos((blade_pitch * pi)/180))) %>%
  mutate(up_length_term2 = if_else(alpha < bird_aspect_ratio, bird_length,
    bird_wingspan * glide_metric * alpha)) %>%
  mutate(up_length = up_length_term1 + up_length_term2) %>%
  mutate(up_p_term = (blade_n/rotation_period)*up_length/bird_speed) %>%
  mutate(up_p = if_else(up_p_term < 1, up_p_term, 1)) %>%
  mutate(down_length_term1 =
    abs(-chord_max * chord_proportion * sin((blade_pitch * pi)/180) +
    alpha * chord_max * chord_proportion * cos((blade_pitch * pi)/180))) %>%
  mutate(down_length_term2 = if_else(alpha < bird_aspect_ratio, bird_length,
    bird_wingspan * glide_metric * alpha)) %>%
  mutate(down_length = down_length_term1 + down_length_term2) %>%
  mutate(down_p_term = (blade_n/rotation_period)*down_length/bird_speed) %>%
  mutate(down_p = if_else(down_p_term < 1, down_p_term, 1))

collision_risk_sum <- collision_risk_df %>%
  slice(1:(n()-1)) %>%
  mutate(radius_up_p = radius_proportion * up_p) %>%
  mutate(radius_down_p = radius_proportion * down_p) %>%
  dplyr::select(radius_up_p, radius_down_p) %>%
  add_row(
    radius_up_p = collision_risk_df %>% slice(n()) %>% pull(up_p)/2,
    radius_down_p = collision_risk_df %>% slice(n()) %>% pull(down_p)/2) %>%
  summarize(
    upwind_p_total = 2 * sum(radius_up_p) * integration_interval,
    downwind_p_total = 2 * sum(radius_down_p) * integration_interval)

collision_risk_avg <-
  (collision_risk_sum %>% pull(upwind_p_total) * upwind_proprtion) +
  (collision_risk_sum %>% pull(downwind_p_total) * downwind_proprtion)

print(paste("Collision risk in absence of avoidance is",
  round(collision_risk_avg, 3)))

# Collision risk for the given scenarios
# Band 2012: "No of collisions = number of transits x probability of collision"

exp_flight_collision_risk <- wind_transits_sum %>%
  filter(scenario != "Control") %>%
  filter(behavior_line == "Flight") %>%
  select(exp_id, scenario, n_turbines_steps_n, s_turbines_steps_n,
    n_turbine_tally, s_turbine_tally) %>%
  mutate(
    n_turbines_collision_risk = n_turbines_steps_n * collision_risk_avg,
    s_turbines_collision_risk = s_turbines_steps_n * collision_risk_avg) %>%
  mutate(
    n_turbines_collision_risk_95avoid = n_turbines_collision_risk * .05,
    s_turbines_collision_risk_95avoid = s_turbines_collision_risk * .05,
    ) %>%
  mutate(turbines_collision_risk_95avoid = NA_real_) %>%
  mutate(turbines_collision_risk_95avoid = if_else(scenario == "South",
    s_turbines_collision_risk_95avoid, turbines_collision_risk_95avoid)) %>%
  mutate(turbines_collision_risk_95avoid = if_else(scenario == "North",
    n_turbines_collision_risk_95avoid, turbines_collision_risk_95avoid)) %>%
  mutate(turbines_collision_risk_95avoid = if_else(scenario =="North and South",
    n_turbines_collision_risk_95avoid + s_turbines_collision_risk_95avoid,
    turbines_collision_risk_95avoid))

saveRDS(exp_flight_collision_risk, flight_collision_risk_file)

# Maps -------------------------------------------------------------------------

glimpse(exp_flight_collision_risk)

n_turbines_scenario_north <- exp_flight_collision_risk %>%
  filter(scenario == "North") %>%
  select(exp_id, scenario, n_turbine_tally) %>%
  unnest(n_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(intersects_prop = intersects_n/sum(intersects_n)) %>%
  mutate(id = paste0("N-", str_pad(1:n(), width = 2, side = "left",
    pad = "0"))) %>%
  select(scenario, id, intersects_n, intersects_prop) %>%
  left_join(site_wt_n_buff, .)
mapview(n_turbines_scenario_north, zcol = "intersects_prop")

s_turbines_scenario_south <- exp_flight_collision_risk %>%
  filter(scenario == "South") %>%
  select(exp_id, scenario, s_turbine_tally) %>%
  unnest(s_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(intersects_prop = intersects_n/sum(intersects_n)) %>%
  mutate(id = paste0("S-", str_pad(1:n(), width = 2, side = "left",
    pad = "0"))) %>%
  select(scenario, id, intersects_n, intersects_prop) %>%
  left_join(site_wt_s_buff, .)
mapview(s_turbines_scenario_south, zcol = "intersects_prop")

n_turbines_scenario_northsouth <- exp_flight_collision_risk %>%
  filter(scenario == "North and South") %>%
  select(exp_id, scenario, n_turbine_tally) %>%
  unnest(n_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(intersects_prop = intersects_n/sum(intersects_n)) %>%
  mutate(id = paste0("N-", str_pad(1:n(), width = 2, side = "left",
    pad = "0"))) %>%
  select(scenario, id, intersects_n, intersects_prop) %>%
  left_join(site_wt_n_buff, .)

s_turbines_scenario_northsouth <- exp_flight_collision_risk %>%
  filter(scenario == "North and South") %>%
  select(exp_id, scenario, s_turbine_tally) %>%
  unnest(s_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(intersects_prop = intersects_n/sum(intersects_n)) %>%
  mutate(id = paste0("S-", str_pad(1:n(), width = 2, side = "left",
    pad = "0"))) %>%
  select(scenario, id, intersects_n, intersects_prop) %>%
  left_join(site_wt_s_buff, .)

mapview(n_turbines_scenario_northsouth, zcol = "intersects_prop") +
mapview(s_turbines_scenario_northsouth, zcol = "intersects_prop")

# Map of turbine collisions



#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

# # Summary statistics
# wind_transits_sum_stats <- wind_transits_sum %>%
#   group_by(behavior_line, scenario) %>%
#   get_summary_stats(.) %>%
#   filter(behavior_line == "Flight") %>%
#   filter(variable == "n_turbines_steps_n" | variable == "s_turbines_steps_n")
# %>%
# #  select(scenario, behavior_line, variable, mean)
#
#
# wind_transits_flight_quantiles <-  wind_transits_sum %>%
#   filter(behavior_line == "Flight") %>%
#   group_by(behavior_line, scenario) %>%
#   summarise(
#     n_turbines_steps_mean = round(mean(n_turbines_steps_n), 1),
#     n_turbines_steps_q05 = quantile(n_turbines_steps_n, probs = .05),
#     n_turbines_steps_q95 = quantile(n_turbines_steps_n, probs = .95),
#     s_turbines_steps_mean = round(mean(s_turbines_steps_n), 1),
#     s_turbines_steps_q05 = quantile(s_turbines_steps_n, probs = .05),
#     s_turbines_steps_q95 = quantile(s_turbines_steps_n, probs = .95),
#     .groups = "drop") %>%
#   pivot_longer(., n_turbines_steps_mean:s_turbines_steps_q95)
#
# wind_transits_flight_stats <- wind_transits_sum_stats


# exp_id <- "sim_20210831_Wilson_C-01"
# exp_file <- paste0(exp_id, ".rds")
# exp_results <- readRDS(file.path(exp_dir, exp_id, exp_file))

# # Get step_data of experiment
# exp_step_data <- exp_results %>%
#       pluck(., "run_1") %>%
#       pluck(., "agents") %>%
#       pluck(., "all") %>%
#       pluck(., "agent") %>%
#       pluck(., "step_data") %>%
#       mutate(exp_id = exp_id) %>%
#       filter(id == 1) %>%
#       dplyr::select(exp_id, everything(.))
#
# # Determine the number of minutes surveyed (total
#
# exp_duration_daily <- exp_step_data %>%
#   mutate(julian = yday(datetime)) %>%
#   select(id, datetime, julian) %>%
#   group_by(julian) %>%
#   summarize(datetime_min = min(datetime),
#     datetime_max = max(datetime)) %>%
#   ungroup(.) %>%
#   mutate(datetime_duration = as.duration(datetime_max - datetime_min))
#
# exp_duration_total <- exp_duration_daily %>%
#   summarize(datetime_duration_total = sum(datetime_duration)) %>%
#   pull(datetime_duration_total)
#
# exp_time_mins <- exp_duration_total/60
# exp_time_hours <- exp_duration_total/60/60
#
# # The experiment ran for 153 days
# exp_time_hours/153 # Average daily hours
#
# # Get wind transits
# wind_transits_sum %>%
#   filter(exp_id == 1)
#
# ### Equation 1
#
# F = ε * λ * C
#
# # Fatalities (birds year^-1)=
# #   Project's footprint (hr km^3 year^-1)*
# #   Pre-construction avian exposure (bird-min hr^-1 km^-3) *
# #   Species Collision Probability (birds bird-min^-1)
#
# ### Equation 2
#
# λ = k / ω
#
# # Avian exposure =
# #   bird minutes /
# #   space & time surveyed
# #   (e.g., plot volume * count duration * number of plots for a pt count survey)
#
#
# ### Equation 3
#
# ε = τ * n * h * pi * r^2
#
# # Expansion factor =
# #   total relevant hours of operation *
# #   total number of turbines *
# #   height of hazardous space *
# #   pi *
# #   rotor radius^2
#
#
#
#
# # BMin:     observed number of bird minutes
# # Fatal:    annual avian fatalities on an operational wind facility
# # SmpHrKm:  total time and area surveyed for bird minutes
# # ExpFac:   expansion factor
# # aPriExp:  alpha parameter for the prior on lambda
# # bPriExp:  beta parameter for the prior on lambda
# # aPriCPr:  alpha parameter for the prior on C
# # bPriCPr:  beta parameter for the prior on C
#
# # The default of a negative value for BMin or Fatal indicates that no data were collected for those model inputs
#
# simFatal <- function(BMin=-1,
#                      Fatal=-1,
#                      SmpHrKm,
#                      ExpFac,
#                      aPriExp=1,
#                      bPriExp=1,
#                      aPriCPr=1,
#                      bPriCPr=1){
#   require(rv)
#
#   # Update the exposure prior
#   if(BMin >= 0){
#     aPostExp <- aPriExp + BMin
#     bPostExp <- bPriExp + SmpHrKm
#   } else {
#     aPostExp <- aPriExp
#     bPostExp <- bPriExp
#   }
#
#   Exp <- rvgamma(n = 1, aPostExp, bPostExp)
#
#   # Update the collisions prior
#   if(Fatal>=0){
#     aPostCPr <- aPriCPr + Fatal
#     bPostCPr <- ((rvmean(Exp) * ExpFac) - Fatal) + bPriCPr
#   } else {
#     aPostCPr <- aPriCPr
#     bPostCPr <- bPriCPr
#   }
#
#   CPr <- rvbeta(n=1, aPostCPr, bPostCPr)
#
#   Fatalities <- ExpFac * Exp * CPr
#   attr(Fatalities,"Exp") <- c(Mean = rvmean(Exp), SD = rvsd(Exp))
#   attr(Fatalities,"CPr") <- c(Mean = rvmean(CPr), SD = rvsd(CPr))
#
#   return(Fatalities)
# }
#




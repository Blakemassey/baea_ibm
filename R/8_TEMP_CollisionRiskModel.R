library(tidyverse)

# Turbines - Vestas V112-3.3 (data from general specifications document)
blade_n <- 3
rotor_radius <- 56 # meters
rotation_speed_rpm <- 12.8 # rpm static rotor speed
rotation_period <- 60/rotation_speed_rpm
chord_max <- 4 # meters
integration_interval <- 0.05
radius_proportion <- seq(0.05, 1, by = integration_interval)
chord_proportion <- c(NA, 0.73, 0.79, 0.88, 0.96, 1.00, 0.98, 0.92, 0.85,
  0.80, 0.75, 0.70, 0.64, 0.58, 0.52, 0.47,0.41, 0.37, 0.30, 0.24, 0.00)
  # These are the proportion of the total chord width
blade_pitch <- 30
upwind_proprtion <- .5
downwind_proprtion <- (1 - upwind_proprtion)

# Bald Eagle metrics
bird_length <- 0.9 # Mean in MA (mass.gov/doc/bald-eagle-factsheet/download)
bird_wingspan <- 2.1  # Mean in MA (mass.gov/doc/bald-eagle-factsheet/download)
bird_aspect_ratio <- bird_length/bird_wingspan
bird_speed <- 13.4112 # 30 mph converted to m/sec
glide <- 1 # flap = 0, glide = 1

# Band Model Turbine and Gannet metrics
blade_n <- 3
rotor_radius <- 45 # meters
rotation_speed_rpm <- 20.2 # rpm static rotor speed
rotation_period <- 60/rotation_speed_rpm
chord_max <- 2.43 # meters
integration_interval <- 0.05
radius_proportion <- seq(0.05, 1, by = integration_interval)
chord_proportion <- c(0.73, 0.79, 0.88, 0.96, 1.00, 0.98, 0.92, 0.85,
  0.80, 0.75, 0.70, 0.64, 0.58, 0.52, 0.47,0.41, 0.37, 0.30, 0.24, 0.00)
  # These are the proportion of the total chord width
blade_pitch <- 30
upwind_proprtion <- .5
downwind_proprtion <- (1 - upwind_proprtion)

bird_length <- 0.94
bird_wingspan <- 1.72
bird_aspect_ratio <- bird_length/bird_wingspan
bird_speed <- 10.5 # 30 mph converted to m/sec
glide <- 0 # 0 = flapping, 1 = gliding
glide_metric <- if_else(glide == 1, 1, 2/pi)

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
  select(radius_up_p, radius_down_p) %>%
  add_row(
    radius_up_p = collision_risk_df %>% slice(n()) %>% pull(up_p)/2,
    radius_down_p = collision_risk_df %>% slice(n()) %>% pull(down_p)/2) %>%
  summarize(
    upwind_p_total = 2 * sum(radius_up_p) * integration_interval,
    downwind_p_total = 2 * sum(radius_down_p) * integration_interval)

collision_risk_avg <-
  (collision_risk_sum %>% pull(upwind_p_total) * upwind_proprtion) +
  (collision_risk_sum %>% pull(downwind_p_total) * downwind_proprtion)
collision_risk_avg

## ENDED HERE ON 2022-03-22 - Everything is working so far w/the Excel data!



upwind_p_total <- 2 * (sum(collision_risk_df$radius_proportion[1:19] * collision_risk_df$up_p[1:19]) + (collision_risk_df$up_p[20]/2)) * integration_interval
downwind_p_total <- 2 * (sum(collision_risk_df$radius_proportion[1:19] * collision_risk_df$down_p[1:19]) + (collision_risk_df$down_p[20]/2)) * integration_interval
average_p <- upwind_p_total*upwind_proprtion + downwind_p_total*downwind_proprtion
average_p



library(tidyverse)

# Turbines - Vestas V112-3.3 (data from general specifications document)
blade_n <- 3
rotor_radius <- 56 # meters
rotation_speed_rpm <- 12.8 # rpm static rotor speed
rotation_period <- 60/rotation_speed_rpm
chord_max <- 4 # meters
integration_interval <- 0.05
radius_proportion <- seq(0,1, by = integration_interval)
chord_proportion <- c(NA, 0.73, 0.79, 0.88, 0.96, 1.00, 0.98, 0.92, 0.85,
  0.80, 0.75, 0.70, 0.64, 0.58, 0.52, 0.47,0.41, 0.37, 0.30, 0.24, 0.00)
# These are the proportion of the total chord width
blade_pitch <- 30
upwind_proprtion <- .5
downwind_proprtion <- (1 - upwind_proprtion)

# Bald Eagle metrics
bird_length <- 0.9 # Mean in MA (mass.gov/doc/bald-eagle-factsheet/download)
bird_wingspan <- 2.1  # Mean in MA (mass.gov/doc/bald-eagle-factsheet/download)
bird_aspect_ratio <- bird_length/bird_wingspan
bird_speed <- 13.4112 # 30 mph converted to m/sec
glide <- 1 # flap = 0, glide = 1

# Band Model Turbine and Gannet metrics
blade_n <- 3
rotor_radius <- 45 # meters
rotation_speed_rpm <- 20.2 # rpm static rotor speed
rotation_period <- 60/rotation_speed_rpm
chord_max <- 2.43 # meters
blade_pitch <- 30

bird_length <- 0.94
bird_wingspan <- 1.72
bird_aspect_ratio <- bird_length/bird_wingspan
bird_speed <- 10.5 # 30 mph converted to m/sec
glide <- 0 # 0 = flapping, 1 = gliding
glide_metric <- if_else(glide == 1, 1, 2/pi)

# Create table
rows_n <- 21
collision_risk_df <- tibble(
  radius_proportion = radius_proportion,
  chord_proportion = chord_proportion,
  alpha = rep(NA, rows_n),
  up_length = rep(NA, rows_n),
  up_p = c(1, rep(NA, rows_n - 1)),
  down_length = rep(NA, rows_n),
  down_p = c(1, rep(NA, rows_n - 1)))

collision_risk_df <- collision_risk_df %>%
  mutate(alpha = bird_speed * rotation_period /
    (radius_proportion * rotor_radius * 2 * pi)) %>%
  mutate(up_length_term1 = abs(chord_max * chord_proportion * sin((blade_pitch * pi)/180) +
    alpha * chord_max * chord_proportion * cos((blade_pitch * pi)/180))) %>%
  mutate(up_length_term2 = if_else(alpha < bird_aspect_ratio, bird_length,
    bird_wingspan * glide_metric * alpha)) %>%
  mutate(up_length = up_length_term1 + up_length_term2) %>%
  mutate(up_p_term = (blade_n/rotation_period)*up_length/bird_speed) %>%
  mutate(up_p = if_else(up_p_term < 1, up_p_term, 1)) %>%
  mutate(down_length_term1 = abs(-chord_max * chord_proportion * sin((blade_pitch * pi)/180) +
    alpha * chord_max * chord_proportion * cos((blade_pitch * pi)/180))) %>%
  mutate(down_length_term2 = if_else(alpha < bird_aspect_ratio, bird_length,
    bird_wingspan * glide_metric * alpha)) %>%
  mutate(down_length = down_length_term1 + down_length_term2) %>%
  mutate(down_p_term = (blade_n/rotation_period)*down_length/bird_speed) %>%
  mutate(down_p = if_else(down_p_term < 1, down_p_term, 1))

collision_risk_sum <- collision_risk_df %>%
  slice(-1)
  #remove last row

upwind_p_total <- 2 * (sum(collision_risk_sum$radius_proportion[1:19] * collision_risk_sum$up_p[1:19]) +
    (collision_risk_sum$up_p[20]/2)) * integration_interval
downwind_p_total <- 2 * (sum(collision_risk_sum$radius_proportion[1:19] * collision_risk_sum$down_p[1:19]) + (collision_risk_sum$down_p[20]/2)) * integration_interval
average_p <- upwind_p_total*upwind_proprtion + downwind_p_total*downwind_proprtion




# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #

alpha <- bird_speed * rotation_period / (radius_proportion * rotor_radius * 2 * pi)

CollisionRiskTab$alpha[u + 1] = sampledbirdSpeed[i] * (60/sampledRotorSpeed[i])/
  (CollisionRiskTab$radius[u + 1] * sampledRotorRadius[i] * 2 * pi)

CollisionRiskTab = data.frame(matrix(data = 0, nrow = 21, ncol = 7))
names(CollisionRiskTab) = c("radius", "chord", "alpha", "Up_length", "Up_P",
  "Down_length", "Down_P")
CollisionRiskTab$radius = seq(0,1, 0.05)

CollisionRiskTab$chord = c(NA, 0.73, 0.79, 0.88, 0.96, 1.00, 0.98, 0.92, 0.85,
  0.80, 0.75, 0.70, 0.64, 0.58, 0.52, 0.47,0.41, 0.37, 0.30, 0.24, 0.00)
  # Can be revised to match actual turbine blades

CollisionRiskTab$alpha[1] = NA
CollisionRiskTab$Up_length[1] = NA
CollisionRiskTab$Up_P[1] = 1
CollisionRiskTab$Down_length[1] = NA
CollisionRiskTab$Down_P[1] = 1

### populate collision risk table

for (u in 1:20) {

  #### First calculate alphas

  CollisionRiskTab$alpha[u + 1] = sampledbirdSpeed[i] *
    (60/sampledRotorSpeed[i])/
    (CollisionRiskTab$radius[u + 1] * sampledRotorRadius[i] * 2 * pi)

  #### Now calculate upwind length

  ifelse (CollisionRiskTab$alpha[u + 1] < (sampledbirdLength[i] /
      sampledWingSpan[i]),

    CollisionRiskTab$Up_length[u+1] <- sampledbirdLength[i] +
  	  abs(sampledBladeWidth[i] * CollisionRiskTab$chord[u + 1] * sin(Pitch[1]) +
  		(CollisionRiskTab$alpha[u + 1] * sampledBladeWidth[i] *
  		CollisionRiskTab$chord[u + 1]*cos(Pitch[1]))),


  	CollisionRiskTab$Up_length[u+1]) <- (sampledWingSpan[i] * Flap_Glide *
  	    CollisionRiskTab$alpha[u + 1]) +
    abs(sampledBladeWidth[i] * CollisionRiskTab$chord[u + 1] * sin(Pitch[1])+
  			(CollisionRiskTab$alpha[u + 1] * sampledBladeWidth[i] *
  			    CollisionRiskTab$chord[u + 1]*cos(Pitch[1])))

  #### Now calculate upwind probability of collision

  CollisionRiskTab$Up_P[u+1] <- min(1,
    (TurbineData$Blades[t]/(60/sampledRotorSpeed[i])) *
      CollisionRiskTab$Up_length[u+1]/sampledbirdSpeed[i])

  #### Now calculate downwind length

  ifelse (CollisionRiskTab$alpha[u + 1] < (sampledbirdLength[i] /
    sampledWingSpan[i]),

  sampledbirdLength[i] +
  	abs(-sampledBladeWidth[i]*CollisionRiskTab$chord[u + 1]*sin(Pitch[1])+
  		(CollisionRiskTab$alpha[u + 1] * sampledBladeWidth[i]*
  		CollisionRiskTab$chord[u + 1]*cos(Pitch[1])))
  			-> CollisionRiskTab$Down_length[i+1],

  (sampledWingSpan[i] * Flap_Glide * CollisionRiskTab$alpha[u + 1]) +
  	abs(-sampledBladeWidth[i]*CollisionRiskTab$chord[i + 1] * sin(Pitch[1])+
  		(CollisionRiskTab$alpha[u + 1] * sampledBladeWidth[i] *
  		CollisionRiskTab$chord[i + 1]*cos(Pitch[1])))
  			-> CollisionRiskTab$Down_length[u+1])

  #### Now calculate Down wind probability of collision

  CollisionRiskTab$Down_P[u+1] = min (1,
    (TurbineData$Blades[t]/(60/sampledRotorSpeed[i])) *
      CollisionRiskTab$Down_length[u+1]/sampledbirdSpeed[i])
}


Total_Up_Wind_P = 2 * (sum(CollisionRiskTab$radius[2:20] *
  CollisionRiskTab$Up_P[2:20]) + CollisionRiskTab$Up_P[21]/2) * 0.05

Total_Down_Wind_P = 2 * (sum(CollisionRiskTab$radius[2:20] *
  CollisionRiskTab$Down_P[2:20]) + CollisionRiskTab$Down_P[21]/2) * 0.05

P_Collision = (Prop_Upwind * Total_Up_Wind_P) + ((1-Prop_Upwind) *
  Total_Down_Wind_P)
P_Collision = 100 * P_Collision

print(paste ("##### The Probability of Collision in absence of avoidance is",
  round(P_Collision/100,3)))



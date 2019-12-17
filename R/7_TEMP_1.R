############################  IMPORT FILES  ####################################

## Import Baea behavior --------------------------------------------------------
baea_behavior_all <- readRDS(file="Data/Baea/baea_behavior.rds")

######################## ANALYZE MOVEMENTS  ####################################

# Create summary transition matrix
baea_movements_sum <- baea_behavior %>%
  arrange(id, datetime) %>%
  group_by(id) %>%  #  slice(1:150) %>%
  mutate(behavior_next = lead(behavior)) %>%
  mutate(step_time2 = lead(datetime) - datetime) %>%
  filter(step_time2 <= 20)  %>%
  filter(step_length > 42.43) %>%
  filter(!is.na(turn_angle)) %>%
  ungroup() %>%
  mutate(behavior_behavior = paste(behavior, "->", behavior_next)) %>%
  group_by(behavior_behavior) %>%
  summarize(count = n())

# Extract 'Perch -> Perch' movement for Bernoulli trail in MovementSubModel
baea_movements_perch_perch <- baea_behavior %>%
  arrange(id, datetime) %>%
  group_by(id) %>%  #  slice(1:150) %>%
  mutate(behavior_next = lead(behavior)) %>%
  mutate(step_time2 = lead(datetime) - datetime) %>%
  filter(step_time2 <= 20)  %>%
#  filter(step_length > 42.43) %>%
  filter(!is.na(turn_angle)) %>%
  ungroup() %>%
  mutate(behavior_behavior = paste(behavior, "->", behavior_next)) %>%
  filter(behavior_behavior == "Perch -> Perch")  %>%
  dplyr::select(id, datetime, step_time, step_time2, long_utm, lat_utm,
    speed, alt, agl, dx, dy, step_length, behavior, behavior_next,
    behavior_behavior, turn_angle)

perch_perch <- baea_movements_perch_perch$step_length
perch_perch_Bern <- 1-(sum(perch_perch <= 42.5)/length(perch_perch))

ggplot(baea_movements_perch_perch, aes(step_length)) +
  geom_histogram(aes(y = ..density..), binwidth = 42.5, boundary = 0,
    fill = "blue", color = "white") +
  xlab("Step Length (m)") + ylab("Density") +
  xlim(0, 5000) +
  theme(axis.text=element_text(colour="black")) +
  theme(axis.title.x = element_text(size = 16, angle = 0, vjust = 0, hjust=0.5)) +
  theme(axis.title.y = element_text(size = 16, angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(axis.text = element_text(size = 14, colour = "black")) +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = NA, size=2, color = "grey90")) +
  scale_y_continuous(labels = comma) +  ggtitle("Perch -> Perch") +
  theme(title = element_text(size = 20))
SaveGGPlot(filename = "Perch_Perch_StepLengths.png",
  path="Products/Graphs/Step_Length")

# Subset data for analysis of movement parameters (distance and direction)
baea_movements <- baea_behavior %>%
  arrange(id, datetime) %>%
  group_by(id) %>%  #  slice(1:150) %>%
  mutate(behavior_next = lead(behavior)) %>%
  mutate(step_time2 = lead(datetime) - datetime) %>%
  filter(step_time2 <= 20)  %>%
  filter(step_length > 42.43) %>%
  filter(!is.na(turn_angle)) %>%
  ungroup() %>%
  mutate(behavior_behavior = paste(behavior, "->", behavior_next)) %>%
  filter(behavior_next != "Nest",
      behavior_behavior != "Roost -> Roost",
      behavior_behavior != "Cruise -> Roost",
      behavior_behavior != "Roost -> Cruise") %>%
  dplyr::select(id, datetime, step_time, step_time2, long_utm, lat_utm, speed,
    alt, agl, dx, dy, step_length, behavior, behavior_next, behavior_behavior,
    turn_angle)

saveRDS(baea_movements, file = "Data/BAEA/baea_movements.rds")

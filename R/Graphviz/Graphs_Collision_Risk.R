
# Load packages, helpers, and functions
pacman::p_load(CircStats, extraDistr, gplots, ggplot2, ggpubr, gridExtra,
  magick, latex2exp, patchwork, reshape2, rstatix, tidyverse, VGAM, viridis)
suppressMessages(extrafont::loadfonts(device = "win"))
pacman::p_load(baear, gisr, ibmr)

# Directories
exp_dir <- "C:/Users/Blake/OneDrive/Work/R/Projects/baea_ibm/Output/Experiment"

# Scenarios Colors (for Ch4, GetColors(3, "vibrant", gray = TRUE))
scenarios_colors <- c("#BBBBBB", "#EE7733", "#EE3377", "#0077BB")
names(scenarios_colors) <- c("Control", "North", "North\nand\nSouth", "South")

# Theme (for LaTeX font)
theme_latex <- theme(
    text = element_text(family = "Latin Modern Roman", color = "black"),
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(size = 12),
    axis.title.y = element_text(margin = margin(0, 10, 0, 0, unit = "pt")),
    axis.title.x = element_text(margin = margin(10, 0, 0, 0, unit = "pt")),
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_text(size = 13, hjust = .5),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11, inherit.blank = FALSE))

# For Wind Area/Turbine Transits
theme_transits <- theme(
  axis.ticks = element_line(colour = NA),
  axis.title.y = element_text(color = "black",
    margin = margin(t = 0, r = 15, b = 0, l = 0)),
  axis.title.x = element_text(color = "black",
    margin = margin(t = 15, r = 0, b = 0, l = 0)),
  panel.background = element_rect(fill = "white", colour = NA),
  panel.border = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(colour = "grey90", linetype = "solid"),
  panel.grid.minor = element_line(size = rel(0.5)),
  plot.background = element_rect(color = "white"),
  strip.background = element_blank(),
  strip.text.x = element_text(color = "black", size = 12),
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 11))

# For Wind Area/Turbine Transits Stats
theme_transits_stats <- theme(
  axis.line = element_line(colour = NA),
  panel.grid.major.y = element_line(colour = "grey90", linetype = "solid"),
  panel.grid.minor.y = element_line(colour = NA))

# Import Data ------------------------------------------------------------------

collision_risk_sum_wilson <- readRDS(file.path(exp_dir,
    paste0("flight_collision_risk_wilson.rds"))) %>%
  mutate(area = "Wilson Pond") %>%
    mutate(scenario = str_replace_all(scenario, "^North and South$",
      "North\nand\nSouth")) %>%
    dplyr::select(area, exp_id, scenario,
      collision_risk = turbines_collision_risk_95avoid)

collision_risk_sum_grand_lake <- readRDS(file.path(exp_dir,
    paste0("flight_collision_risk_grand_lake.rds"))) %>%
  mutate(area = "Grand Lake") %>%
    mutate(scenario = str_replace_all(scenario, "^North and South$",
      "North\nand\nSouth")) %>%
    dplyr::select(area, exp_id, scenario,
      collision_risk = turbines_collision_risk_95avoid)

collision_risk_sum_all <- collision_risk_sum_wilson %>%
    bind_rows(., collision_risk_sum_grand_lake) %>%
    mutate(area = factor(area, levels = c("Wilson Pond", "Grand Lake")))  

gg_turbines_all_crm <- collision_risk_sum_all %>%
    ggplot(.) +
    geom_boxplot(aes(scenario, y = collision_risk,
      fill = scenario)) +
    facet_grid(cols = vars(area), scale = "free") +  
    scale_x_discrete(limits = c("North","South", "North\nand\nSouth")) +
    scale_fill_manual(values = scenarios_colors, name = "Scenarios") +
    labs(x = "Wind Farm Scenario") +
    labs(y = paste0("Total Predicted Collisions with Turbines\nPer Eagle ",
      "Per Breeding Season\n(95% Avoidance)")) +
    ggtitle(paste0("Collision Risk")) +
    theme_latex +
    theme_transits +
    theme_transits_stats +
    theme(legend.position = "none",
          plot.title = element_text(size = 15)) +
    theme(panel.spacing = unit(2, "lines"))

gg_turbines_all_crm

# Save SVG
ggsave(filename = paste0("Turbines_NS_CRM.svg"),
  plot = gg_turbines_all_crm,
  path = "Assets/Images/Ch4_Extra", scale = 1,
  width = 6, height = 4, units = "in", dpi = 300)  
#   
# # Import Data ------------------------------------------------------------------
# 
# collision_risk_sum_wilson_pond <- readRDS(file.path(exp_dir,
#     paste0("flight_collision_risk_wilson.rds"))) %>%
#   mutate(area = "Wilson Pond") %>%
#   mutate(scenario = str_replace_all(scenario, "^North and South$",
#     "North\nand\nSouth")) %>%
#   dplyr::select(area, exp_id, scenario, n_turbines_collision_risk_95avoid,
#     s_turbines_collision_risk_95avoid, turbines_collision_risk_95avoid)
# 
# collision_risk_sum_grand_lake <- readRDS(file.path(exp_dir,
#     paste0("flight_collision_risk_grand_lake.rds"))) %>%
#   mutate(area = "Grand Lake") %>%
#   mutate(scenario = str_replace_all(scenario, "^North and South$",
#     "North\nand\nSouth")) %>%
#   dplyr::select(area, exp_id, scenario, n_turbines_collision_risk_95avoid,
#     s_turbines_collision_risk_95avoid, turbines_collision_risk_95avoid)
# 
# collision_risk_sum_all <- bind_rows(collision_risk_sum_wilson_pond, 
#   collision_risk_sum_grand_lake)
# 
# collision_risk_sum_ns <- bind_rows(
#     collision_risk_sum %>%
#       filter(scenario %in% c("North", "North\nand\nSouth")) %>%
#       rename(collision_risk = n_turbines_collision_risk_95avoid) %>%
#       mutate(turbines = paste0(nest_title, "\nNorth Turbines")),
#     collision_risk_sum %>%
#       filter(scenario %in% c("South", "North\nand\nSouth")) %>%
#       rename(collision_risk = s_turbines_collision_risk_95avoid) %>%
#       mutate(turbines = paste0(nest_title, "\nSouth Turbines"))) %>%
#     dplyr::select(exp_id, scenario, turbines, collision_risk) %>%
#     mutate(scenario = ordered(scenario, levels = c("North","South",
#       "North\nand\nSouth")))
# 
#   # Collisions with North turbines (only relevant for North and North/South)
#   gg_turbines_ns_crm <- collision_risk_sum_ns %>%
#     ggplot(.) +
#     geom_boxplot(aes(x = scenario, y = collision_risk, fill = scenario)) +
#     facet_grid(cols = vars(turbines), scale = "free") +
#     scale_fill_manual(values = scenarios_colors, name = "Scenarios") +
#     labs(x = "Wind Farm Scenario",
#       y = "Collision Risk Per Eagle\nPer Breeding Season") +
#     theme_latex +
#     theme_transits +
#     theme_transits_stats +
#     theme(legend.position = "none")
#   gg_turbines_ns_crm
# 
# 
# # Start Nest For Loop ----------------------------------------------------------
# 
# for (i in c("Grand_Lake", "Wilson")){
#   nest_str <- i
#   nest_lower <- nest_str %>% str_to_lower(.)
#   nest_title <- nest_str %>% str_replace(., "_", " ") %>% str_to_title(.)
#   print(paste0("Starting: ", nest_title))  
#   
#   # Predicted Collision Risk North/South ---------------------------------------
# 
#   collision_risk_sum <- readRDS(file.path(exp_dir,
#       paste0("flight_collision_risk_", nest_str, ".rds"))) %>%
#     mutate(scenario = str_replace_all(scenario, "^North and South$",
#       "North\nand\nSouth")) %>%
#     dplyr::select(exp_id, scenario, n_turbines_collision_risk_95avoid,
#       s_turbines_collision_risk_95avoid, turbines_collision_risk_95avoid)
# 
#   collision_risk_sum_ns <- bind_rows(
#     collision_risk_sum %>%
#       filter(scenario %in% c("North", "North\nand\nSouth")) %>%
#       rename(collision_risk = n_turbines_collision_risk_95avoid) %>%
#       mutate(turbines = paste0(nest_title, "\nNorth Turbines")),
#     collision_risk_sum %>%
#       filter(scenario %in% c("South", "North\nand\nSouth")) %>%
#       rename(collision_risk = s_turbines_collision_risk_95avoid) %>%
#       mutate(turbines = paste0(nest_title, "\nSouth Turbines"))) %>%
#     dplyr::select(exp_id, scenario, turbines, collision_risk) %>%
#     mutate(scenario = ordered(scenario, levels = c("North","South",
#       "North\nand\nSouth")))
# 
#   # Collisions with North turbines (only relevant for North and North/South)
#   gg_turbines_ns_crm <- collision_risk_sum_ns %>%
#     ggplot(.) +
#     geom_boxplot(aes(x = scenario, y = collision_risk, fill = scenario)) +
#     facet_grid(cols = vars(turbines), scale = "free") +
#     scale_fill_manual(values = scenarios_colors, name = "Scenarios") +
#     labs(x = "Wind Farm Scenario",
#       y = "Collision Risk Per Eagle\nPer Breeding Season") +
#     theme_latex +
#     theme_transits +
#     theme_transits_stats +
#     theme(legend.position = "none")
#   gg_turbines_ns_crm
# 
#   # Save SVG
#   ggsave(filename = paste0("Turbines_NS_CRM_", nest_str, ".png"),
#     plot = gg_turbines_ns_crm,
#     path = file.path(tex_dir, "Figures/Ch4/Collision_Risk"), scale = 1,
#     width = 6, height = 4, units = "in", dpi = 300)
# 
#   # Clean up objects
#   rm(collision_risk_sum, collision_risk_sum_ns, gg_turbines_ns_crm)
# 
#   # Predicted Collision Risk All Turbines --------------------------------------
# 
#   collision_risk_sum_all <- readRDS(file.path(exp_dir,
#       paste0("flight_collision_risk_", nest_lower, ".rds"))) %>%
#     mutate(scenario = str_replace_all(scenario, "^North and South$",
#       "North\nand\nSouth")) %>%
#     dplyr::select(exp_id, scenario,
#       collision_risk = turbines_collision_risk_95avoid)
# 
#   gg_turbines_all_crm <- collision_risk_sum_all %>%
#     ggplot(.) +
#     geom_boxplot(aes(scenario, y = collision_risk,
#       fill = scenario)) +
#     scale_x_discrete(limits = c("North","South", "North\nand\nSouth")) +
#     scale_fill_manual(values = scenarios_colors, name = "Scenarios") +
#     labs(x = "Wind Farm Scenario") +
#     labs(y = paste0("Total Predicted Collisions with Turbines\nPer Eagle ",
#       "Per Breeding Season\n(95% Avoidance)")) +
#     ggtitle(paste0(nest_title, " Pond Collision Risk")) +
#     theme_latex +
#     theme_transits +
#     theme_transits_stats +
#     theme(legend.position = "none")
#   gg_turbines_all_crm
# 
#   ggsave(filename = paste0("Turbines_All_CRM_", nest_str, ".png"),
#     plot = gg_turbines_all_crm,
#     path = file.path(tex_dir, "Figures/Ch4/Collision_Risk"), scale = 1,
#     width = 6, height = 4, units = "in", dpi = 300)
# 
#   # Clean up objects
#   rm(collision_risk_sum_all, gg_turbines_all_crm)
# }
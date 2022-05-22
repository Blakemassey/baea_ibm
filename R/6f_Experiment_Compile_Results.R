#--------------------------- Experiment Results -------------------------------#
# This script compiles the experiment data results
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------

# Load packages
pacman::p_load(cartography, ctmm, dplyr, fasterize, forcats, gplots, ggplot2,
  ggthemes, ggpubr, grid, leaflet, lubridate, magick, mapview, move,
  OpenStreetMap, patchwork, plotly, prettymapr, purrr, raster, readr, rosm,
  rstatix, rsvg, sf, s2, stringr, tidyr, tmap, tmaptools, viridis, units,
  webshot, zoo)
pacman::p_load(baear, gisr, ibmr)
suppressMessages(extrafont::loadfonts(device = "win"))
set_thin_PROJ6_warnings(TRUE)

# Set options
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE, chunksize = 1e9,
  memfrac = .9)

# Experiment id
site <- "Wilson" #"Grand_Lake"
exp_ids <- 1:20 %>%
  str_pad(., width = 2, side = "left", pad = "0")

# Experiment simulation files in TEMP directory
exp_vec <- list.dirs("C:/TEMP", recursive = FALSE, full.names = TRUE) %>%
  str_subset(., paste0(site, "_[:alpha:]{1,}-")) %>%
  str_subset(., exp_ids %>% paste0("-", .) %>% paste0(., collapse = "|"))

# Variables
mapping <- FALSE

# Directories
input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
exp_dir <- "C:/TEMP"
exp_step_data_dir <- "Step_Data"
exp_output_dir <- "Output/Experiment"
ridge_file_dir <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines"
wind_output_dir <- "Output/Analysis/Wind"

# Files
ridge_poly_file <- file.path(ridge_file_dir, "ridge_poly.shp")
ridge_line_file <- file.path(ridge_file_dir, "ridge_line.shp")

# Plot themes
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))
theme_blank <- theme(legend.position = "none",
  text = element_text(family = "Latin Modern Roman"),
  plot.title = element_text(size=14),
  panel.grid = element_blank(), axis.title = element_blank(),
  axis.text = element_blank(), axis.ticks = element_blank(),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA))
theme_update(plot.title = element_text(hjust = 0.5))

# Behavior colors
behavior_colors <- CreateColorsByMetadata(file = file.path("Data/Assets",
  "behavior_colors.csv"), metadata_id = "behavior")
cruise_flight_colors <- behavior_colors %>% .[1:2]
sex_colors <- tibble(Female = col2hex("yellow"), Male = col2hex("tomato"))

## Import Data -----------------------------------------------------------------

# File directory and id
wind_transits_sum <-
  file.path(exp_vec, "Results", "wind_crossings_sum.rds") %>%
  map(readRDS) %>%
  bind_rows() %>%
  mutate(n_area_prop = n_area_steps_n/total_steps_n) %>%
  mutate(s_area_prop = s_area_steps_n/total_steps_n) %>%
  mutate(n_turbines_prop = n_turbines_steps_n/total_steps_n) %>%
  mutate(s_turbines_prop = s_turbines_steps_n/total_steps_n) %>%
  mutate(scenario = scenario %>%
    str_replace_all(., "^C$", "Control") %>%
    str_replace_all(., "^N$", "North") %>%
    str_replace_all(., "^S$", "South") %>%
    str_replace_all(., "^NS$", "North and South"))

# Save wind_transits_sum file to Project file
saveRDS(wind_transits_sum, file.path(exp_output_dir,
  paste0("wind_transits_sum_", str_to_lower(site), ".rds")))

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#
#
# # Graphs -----------------------------------------------------------------------
#
# # North Wind Area
# ggplot(wind_transits_sum) +
#   geom_boxplot(aes(scenario, y = n_area_prop, fill = behavior_line),
#     position = "dodge") +
#   theme_latex +
#   labs(x = "Wind Farm Scenario", y ="Proportion of Flights in North Wind Area")+
#   scale_fill_manual(values = cruise_flight_colors, name = "Behavior") +
#   theme(panel.background = element_rect(fill = "white", colour = NA),
#     axis.ticks = element_line(colour = NA),
#     panel.grid.major.y = element_line(colour = "grey90"),
#     panel.grid.minor = element_line(size = rel(0.5)))
#
# # South Wind Area
# ggplot(wind_transits_sum) +
#   geom_boxplot(aes(scenario, y = s_area_prop, fill = behavior_line),
#     position = "dodge") +
#   theme_latex +
#   labs(x = "Wind Farm Scenario", y ="Proportion of Flights in South Wind Area")+
#   scale_fill_manual(values = cruise_flight_colors, name = "Behavior") +
#   theme(panel.background = element_rect(fill = "white", colour = NA),
#     axis.ticks = element_line(colour = NA),
#     panel.grid.major.y = element_line(colour = "grey90"),
#     panel.grid.minor = element_line(size = rel(0.5)))
#
# # North Wind Turbines
# ggplot(wind_transits_sum) +
#   geom_boxplot(aes(scenario, y = n_area_steps_n, fill = behavior_line),
#     position = "dodge") +
#   theme_latex +
#   labs(x = "Wind Farm Scenario",
#     y ="Flight Steps Crossing North Wind Turbines") +
#   scale_fill_manual(values = cruise_flight_colors, name = "Behavior") +
#   theme(panel.background = element_rect(fill = "white", colour = NA),
#     axis.ticks = element_line(colour = NA),
#     panel.grid.major.y = element_line(colour = "grey90"),
#     panel.grid.minor = element_line(size = rel(0.5)))
#
# # South Wind Turbines
# ggplot(wind_transits_sum) +
#   geom_boxplot(aes(scenario, y = s_area_steps_n, fill = behavior_line),
#     position = "dodge") +
#   theme_latex +
#   labs(x = "Wind Farm Scenario",
#     y = "Flight Steps Crossing South Wind Turbines") +
#   scale_fill_manual(values = cruise_flight_colors, name = "Behavior") +
#   theme(panel.background = element_rect(fill = "white", colour = NA),
#     axis.ticks = element_line(colour = NA),
#     panel.grid.major.y = element_line(colour = "grey90"),
#     panel.grid.minor = element_line(size = rel(0.5)))
#
# # Statistics -------------------------------------------------------------------
#
# wind_transits_sum <- readRDS("Output/Experiment/wind_transits_sum.rds") %>%
#   mutate(behavior_line = as.factor(behavior_line)) %>%
#   mutate(scenario = as.factor(scenario))
#
# # Summary statistics
# wind_transits_sum_stats <- wind_transits_sum %>%
#   group_by(behavior_line, scenario) %>%
#   get_summary_stats(.)
#
# # Statistical tests
# wind_transits_sum_test_north <- wind_transits_sum %>%
#   group_by(behavior_line) %>%
#   t_test(n_area_prop ~ scenario) %>%
#   adjust_pvalue() %>%
#   add_significance("p.adj")
# wind_transits_sum_test_north
#
# # Visualization
# ggboxplot(
#   df, x = "supp", y = "len",
#   color = "supp", palette = "jco", facet.by = "dose",
#   ylim = c(0, 40)
#   ) +
#   stat_pvalue_manual(stat.test, label = "p.adj", y.position = 35)
#
# # T-test
# stat.test <- wind_transits_sum %>%
#   filter(behavior_line == "Flight") %>%
#   filter(scenario == "North" | scenario == "Control") %>%
#   t_test(n_area_prop ~ scenario, paired = TRUE)
# stat.test
#
# # Create a box plot
# p <- wind_transits_sum %>%
#   group_by(behavior_line) %>%
#   filter(scenario == "North" | scenario == "Control") %>%
#   ggboxplot(., x = "scenario", y = "n_area_prop", fill = "scenario",
#     palette = "jco") +
#   labs(x = "Wind Farm Scenario", y ="Proportion of Flights in North Wind Area")+
#   scale_fill_manual(values = viridis(2), name = "Wind Farm Scenario")
#
# # Add the p-value manually
# p <- wind_transits_sum %>%
#   group_by(behavior_line) %>%
#   filter(scenario == "North" | scenario == "Control") %>%
#   ggboxplot(., x = "scenario", y = "n_area_prop",
#   fill = "scenario", palette = "jco", facet.by = "behavior_line") +
#   labs(x = "Wind Farm Scenario", y ="Proportion of Flights in North Wind Area")+
#   stat_pvalue_manual(stat.test, label = "p.adj", y.position = 0.15)
#
# cruises_c_n <- wind_transits_sum %>%
#   filter(behavior_line == "Cruise") %>%
#   filter(scenario == "North" | scenario == "Control") %>%
#   anova(lm(.$n_area_prop ~ as.factor(.$scenario)))
#
# flights_c_n <- wind_transits_sum %>%
#   filter(behavior_line == "Cruise") %>%
#   filter(scenario == "Control" | scenario == "North")
#
# flights_c_s <- wind_transits_sum %>%
#   filter(behavior_line == "Cruise") %>%
#   filter(scenario == "Control" | scenario == "South")
#
# anova(lm(cruises_c_n$n_area_prop ~ as.factor(cruises_c_n$scenario)))
# anova(lm(flights_c_s$s_area_prop ~ as.factor(flights_c_s$scenario)))
#
# t.test(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0,
#         paired = FALSE, var.equal = FALSE, conf.level = 0.95)
#



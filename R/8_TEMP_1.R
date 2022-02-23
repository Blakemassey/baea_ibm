pacman::p_load(DT, dplyr, ggpubr, rstatix, ggplot2)
suppressMessages(extrafont::loadfonts(device = "win"))

# Directory
output_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Output"

# Themes
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 16)) +
  theme(axis.title = element_text(size = 18)) +
  theme(plot.title = element_text(size = 20))
theme_crossings <- theme(
  legend.position = "none",
  axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)),
  axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)),
  panel.background = element_rect(fill = "white", colour = NA),
  axis.ticks = element_line(colour = NA),
  panel.grid.major.y = element_line(colour = NA),
  panel.grid.minor = element_line(color = NA, size = rel(0.5)))

# Wind crossings
wc_sum <- readRDS(file.path(output_dir,
  "Experiment/wind_crossings_sum.rds")) %>%
  mutate(behavior_line = as.factor(behavior_line)) %>%
  mutate(scenario = as.factor(scenario))

# Behavior colors
cruise_flight_colors <- tibble(Cruise = gplots::col2hex("yellow"),
  Flight = gplots::col2hex("tomato"))


# Summary statistics
wc_sum_stats <- wc_sum %>%
  group_by(behavior_line, scenario) %>%
  get_summary_stats(.) %>%
  filter(variable %in% c("n_area_steps_n", "s_area_steps_n",
    "n_turbines_steps_n", "s_turbines_steps_n"))

ggplot(wc_sum) +
  geom_boxplot(aes(x = scenario, y = n_area_steps_n, fill = behavior_line),
    position = "dodge") +
  labs(x = "Wind Farm Scenario",
    y ="Steps Crossing North Wind Area")+
  scale_fill_manual(values = cruise_flight_colors, name = "Behavior")

ggplot(wc_sum) +
  geom_boxplot(aes(x = scenario, y = s_area_steps_n, fill = behavior_line),
    position = "dodge") +
  labs(x = "Wind Farm Scenario",
    y ="Steps Crossing South Wind Area")+
  scale_fill_manual(values = cruise_flight_colors, name = "Behavior")

ggplot(wc_sum) +
  geom_boxplot(aes(x = scenario, y = n_turbines_steps_n, fill = behavior_line),
    position = "dodge") +
  labs(x = "Wind Farm Scenario",
    y ="Steps Crossing South Wind Area")+
  scale_fill_manual(values = cruise_flight_colors, name = "Behavior")


# Filter data
wc_cruise <- wc_sum %>%
  filter(behavior_line == "Cruise")
wc_flight <- wc_sum %>%
  filter(behavior_line == "Flight")

# Multiple pairwise comparisons against reference group ("Control") ------------
wc_cruise_north_area <- wc_cruise %>%
  t_test(n_area_prop ~ scenario, ref.group = "Control") %>%
  adjust_pvalue() %>%
  add_significance("p.adj")

wc_cruise_south_area <- wc_cruise %>%
  t_test(s_area_prop ~ scenario, ref.group = "Control") %>%
  adjust_pvalue() %>%
  add_significance("p.adj")

wc_flight_north_area <- wc_flight %>%
  t_test(n_area_prop ~ scenario, ref.group = "Control") %>%
  adjust_pvalue() %>%
  add_significance("p.adj")

wc_flight_south_area <- wc_flight %>%
  t_test(s_area_prop ~ scenario, ref.group = "Control") %>%
  adjust_pvalue() %>%
  add_significance("p.adj")

wc_cruise_north_area_plot <-
  ggboxplot(wc_cruise,
    x = "scenario", y = "n_area_prop",
    xlab = "Wind Farm Scenario",
    ylab = "North Area Crossings - Cruise\n(Proportion of Total Steps)",
    color = "scenario", palette = "jco") +
  stat_pvalue_manual(wc_cruise_north_area, label.size = 5,
    label = "p = {p.adj}{p.adj.signif}",
    y.position = c(.15, .165, .18)) +
  theme_latex +
  theme_crossings
wc_cruise_north_area_plot

wc_cruise_south_area_plot <-
  ggboxplot(wc_cruise,
    x = "scenario", y = "s_area_prop",
    xlab = "Wind Farm Scenario",
    ylab = "South Area Crossings - Cruise\n(Proportion of Total Steps)",
    color = "scenario", palette = "jco") +
  stat_pvalue_manual(wc_cruise_south_area, label.size = 5,
    label = "p = {p.adj}{p.adj.signif}",
    y.position = c(.14, .155, .17)) +
  theme_latex +
  theme_crossings
wc_cruise_south_area_plot

wc_flight_north_area_plot <-
  ggboxplot(wc_flight,
    x = "scenario", y = "n_area_prop",
    xlab = "Wind Farm Scenario",
    ylab = "North Area Crossings - Flight\n(Proportion of Total Steps)",
    color = "scenario", palette = "jco") +
  stat_pvalue_manual(wc_flight_north_area, label.size = 5,
    label = "p = {p.adj}{p.adj.signif}",
    y.position = c(.023, .025, .027)) +
  theme_latex +
  theme_crossings
wc_flight_north_area_plot

wc_flight_south_area_plot <-
  ggboxplot(wc_flight,
    x = "scenario", y = "s_area_prop",
    xlab = "Wind Farm Scenario",
    ylab = "South Area Crossings - Flight\n(Proportion of Total Steps)",
    color = "scenario", palette = "jco") +
  stat_pvalue_manual(wc_flight_south_area, label.size = 5,
    label = "p = {p.adj}{p.adj.signif}",
    y.position = c(.036, .039, .042)) +
  theme_latex +
  theme_crossings
wc_flight_south_area_plot

wc_cruise_north_turbines <- wc_cruise %>%
  t_test(n_turbines_prop ~ scenario, ref.group = "Control") %>%
  adjust_pvalue() %>%
  add_significance("p.adj")

wc_cruise_south_turbines <- wc_cruise %>%
  t_test(s_turbines_prop ~ scenario, ref.group = "Control") %>%
  adjust_pvalue() %>%
  add_significance("p.adj")

wc_flight_north_turbines <- wc_flight %>%
  t_test(n_turbines_prop ~ scenario, ref.group = "Control") %>%
  adjust_pvalue() %>%
  add_significance("p.adj")

wc_flight_south_turbines <- wc_flight %>%
  t_test(s_turbines_prop ~ scenario, ref.group = "Control") %>%
  adjust_pvalue() %>%
  add_significance("p.adj")


wc_cruise_north_turbines_plot <-
  ggboxplot(wc_cruise,
    x = "scenario", y = "n_turbines_prop",
    xlab = "Wind Farm Scenario",
    ylab = "North Turbines Crossings - Cruise\n(Proportion of Total Steps)",
    color = "scenario", palette = "jco") +
  stat_pvalue_manual(wc_cruise_north_turbines, label.size = 5,
    label = "p = {p.adj}{p.adj.signif}",
    y.position = c(.05, .0575, .065)) +
  theme_latex +
  theme_crossings
wc_cruise_north_turbines_plot

wc_cruise_south_turbines_plot <-
  ggboxplot(wc_cruise,
    x = "scenario", y = "s_turbines_prop",
    xlab = "Wind Farm Scenario",
    ylab = "South Turbines Crossings - Cruise\n(Proportion of Total Steps)",
    color = "scenario", palette = "jco") +
  stat_pvalue_manual(wc_cruise_south_turbines, label.size = 5,
    label = "p = {p.adj}{p.adj.signif}",
    y.position = c(.055, .06, .065)) +
  theme_latex +
  theme_crossings
wc_cruise_south_turbines_plot

wc_flight_north_turbines_plot <-
  ggboxplot(wc_flight,
    x = "scenario", y = "n_turbines_prop",
    xlab = "Wind Farm Scenario",
    ylab = "North Turbines Crossings - Flight\n(Proportion of Total Steps)",
    color = "scenario", palette = "jco") +
  stat_pvalue_manual(wc_flight_north_turbines, label.size = 5,
    label = "p = {p.adj}{p.adj.signif}",
    y.position = c(.023, .025, .027)) +
  theme_latex +
  theme_crossings
wc_flight_north_turbines_plot

wc_flight_south_turbines_plot <-
  ggboxplot(wc_flight,
    x = "scenario", y = "s_turbines_prop",
    xlab = "Wind Farm Scenario",
    ylab = "South Turbines Crossings - Flight\n(Proportion of Total Steps)",
    color = "scenario", palette = "jco") +
  stat_pvalue_manual(wc_flight_south_turbines, label.size = 5,
    label = "p = {p.adj}{p.adj.signif}",
    y.position = c(.0115, .013, .0145)) +
  theme_latex +
  theme_crossings
wc_flight_south_turbines_plot


# Multiple pairwise comparisons ------------------------------------------------
wc_north_area_cruise_test <- wc_north_area_cruise %>%
  t_test(n_area_prop ~ scenario) %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
wc_north_area_cruise_test

wc_north_area_cruise_plot <-
  ggboxplot(wc_north_area_cruise,
    x = "scenario", y = "n_area_prop",
    xlab = "Wind Farm Scenario",
    ylab = "North Area Crossings\n(Proportion of Total Steps)",
    color = "scenario", palette = "jco") +
  stat_pvalue_manual(wc_north_area_cruise_test, label = "p = {p.adj}",
    y.position = c(.15, .175, .225, .125, .2, .15))
ggpar(wc_north_area_cruise_plot, legend = "none")

# Multiple pairwise comparisons against reference group ("Control")
wc_north_area_cruise_test <- wc_north_area_cruise %>%
  t_test(n_area_prop ~ scenario, ref.group = "Control") %>%
  adjust_pvalue() %>%
  add_significance("p.adj")
wc_north_area_cruise_test

wc_north_area_cruise_plot <-
  ggboxplot(wc_north_area_cruise,
    x = "scenario", y = "n_area_prop",
    xlab = "Wind Farm Scenario",
    ylab = "North Area Crossings\n(Proportion of Total Steps)",
    color = "scenario", palette = "jco") +
  stat_pvalue_manual(wc_north_area_cruise_test, label = "p = {p.adj}{p.adj.signif}",
    y.position = c(.15, .165, .18)) +
  theme_latex +
  theme_crossings
ggpar(wc_north_area_cruise_plot, legend = "none")

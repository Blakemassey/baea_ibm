#------------------------ Dissertation Figures --------------------------------#
# Maps and Flowcharts: Save as .svg
# Figures: Save as .pdf or .png
# Note: When Overleaf compiles identically named files in different directories
#   the second file is not recognized and the first file is repeated in PDF.
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------

# Load packages, helpers, and functions
pacman::p_load(CircStats, extraDistr, gplots, ggplot2, ggpubr, gridExtra,
  magick, latex2exp, patchwork, reshape2, rstatix, tidyverse, VGAM, viridis)
suppressMessages(extrafont::loadfonts(device = "win"))
pacman::p_load(baear, gisr, ibmr)

# Variables
sim_id <- "sim_20210725"
sims <- c(77, 85, 86, 87, 88)
match_baea <- TRUE

# Directories
pkg_dir <- "C:/Users/Blake/OneDrive/Work/R/Projects/multiscale_optim"
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"
exp_dir <- "Output/Experiment"
baea_calibration_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects",
  "baea_ibm/Output/Sim/Calibration")
sim_dir <- file.path("C:/TEMP", paste0(sim_id, "-"))
sim_calibration_dir <- "Calibration"

# Files
baea_ridge_sum_file <- file.path(baea_calibration_dir, "baea_ridge_sum.rds")

# Functions
RecodeNestIdToName <- function(value){
  recoded_value <- fct_recode(value,
    "Ellis" = "282A",
    "Sandy" = "423R01",
    "Hebron" = "659A",
    "Musquash" = "446R01")
  return(recoded_value)
}

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

# Theme (for blank background)
theme_blank <- theme(legend.position = "none",
  text = element_text(family = "Latin Modern Roman"),
  plot.title = element_text(size=14),
  panel.grid = element_blank(), axis.title = element_blank(),
  axis.text = element_blank(), axis.ticks = element_blank(),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA))

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

# Behavior colors (for Ch2/3, GetColors(5, "muted", gray = TRUE))
behavior_colors <- c("#44AA99", "#332288", "#DDCC77", "#117733", "#AA4499")
names(behavior_colors) <-  c("Cruise", "Flight", "Nest", "Perch", "Roost")

# Sex Color (for Ch2/3)
sex_colors <- tibble(Female = col2hex("yellow"), Male = col2hex("tomato"))

# Cruise/Flight Colors (for Ch4, from GetColors(3, "bright", gray = TRUE))
cruise_flight_colors <- c("#CCBB44", "#228833")
names(cruise_flight_colors) <- c("Cruise", "Flight")

# Scenarios Colors (for Ch4, GetColors(3, "vibrant", gray = TRUE))
scenarios_colors <- c("#BBBBBB", "#EE7733", "#EE3377", "#0077BB")
names(scenarios_colors) <- c("Control", "North", "North\nand\nSouth", "South")


# ---------------------------- CHAPTER 2 ---------------------------------------

# Homerange Size/Terrain Metrics -----------------------------------------------
hr_metrics_org <- readRDS("Output/Analysis/Homerange/hr_all_metrics.rds")
hr_metrics <- hr_metrics_org
unique(hr_metrics$id)
sort(colnames(hr_metrics))

hr_metrics_terrain_95 <- hr_metrics_org %>%
  mutate(Area_km = ud_95_total) %>%
  mutate(TPI = ud_95_tpi_mean) %>%
  mutate(TRI = ud_95_tri_mean) %>%
  mutate(Roughness = ud_95_roughness_mean) %>%
  dplyr::select(id, Area_km, TPI, TRI, Roughness) %>%
    melt(., id.var = "id") %>%
  mutate(ud = "95%") %>% map_if(is.factor, as.character)

hr_metrics_terrain_50 <- hr_metrics_org %>%
  mutate(Area_km = ud_50_total) %>%
  mutate(TPI = ud_50_tpi_mean) %>%
  mutate(TRI = ud_50_tri_mean) %>%
  mutate(Roughness = ud_50_roughness_mean) %>%
  dplyr::select(id, Area_km, TPI, TRI, Roughness) %>%
    melt(., id.var = "id") %>%
  mutate(ud = "50%") %>% map_if(is.factor, as.character)

hr_metrics_terrain <- bind_rows(hr_metrics_terrain_95, hr_metrics_terrain_50)
rm(hr_metrics_org, hr_metrics_terrain_50, hr_metrics_terrain_95)

gg_terrain_1 <- ggplot(data = hr_metrics_terrain %>%
    filter(variable == "Area_km"), aes(group = ud, x = factor(0), y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab("Home Range Size\n    ") +
  ylab(latex2exp::TeX("Total Area ($\\km^2$)")) +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  theme_minimal() +
  theme_latex +
  theme(plot.margin = margin(0, 20, 20, 0, "pt"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank())

gg_terrain_2 <- ggplot(data = hr_metrics_terrain %>% filter(variable == "TRI"),
    aes(group = ud, x = factor(0), y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab("Terrain Ruggedness\nIndex") +
  ylab(latex2exp::TeX("Index Value")) +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  theme_minimal() +
  theme_latex +
  theme(plot.margin = margin(0, 20, 20, 0, "pt"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank())

gg_terrain_3 <- ggplot(data = hr_metrics_terrain %>% filter(variable == "TPI"),
    aes(group = ud, x = factor(0), y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab(latex2exp::TeX("Terrain Position\n       Index")) +
  ylab("Index Value") +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  theme_minimal() +
  theme_latex +
  theme(plot.margin = margin(0, 20, 10, 0, "pt"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank())

gg_terrain_4 <- ggplot(data = hr_metrics_terrain %>%
    filter(variable == "Roughness"), aes(group = ud, x = factor(0), y = value))+
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab(latex2exp::TeX("Roughness\n    ")) +
  ylab("Index Value") +
  theme_minimal() +
  theme_latex +
  theme(plot.margin = margin(0, 20, 10, 0, "pt"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank())

gg_terrain_all <- gg_terrain_1 + gg_terrain_2 + gg_terrain_3 + gg_terrain_4

gg_terrain_combined <- ggarrange(gg_terrain_1, gg_terrain_2, gg_terrain_3,
  gg_terrain_4, ncol = 2, nrow = 2, common.legend = TRUE, legend = "right") +
  theme_minimal() +
  theme_latex +
  guides(shape = guide_legend(override.aes = list(size = 2)),
    color = guide_legend(override.aes = list(size = 2)))
gg_terrain_combined

ggsave(filename = "Homerange_Size_Terrain.png", plot = gg_terrain_combined,
  path = file.path(tex_dir, "Figures/Ch2"),
  scale = 1, width = 6, height = 6, units = "in",
  dpi = 300)

# Clean up objects
rm(gg_terrain_1, gg_terrain_2, gg_terrain_3, gg_terrain_4, gg_terrain_all,
  gg_terrain_combined)

# Homerange Cover Type Metrics -------------------------------------------------
hr_metrics_org <- readRDS("Output/Analysis/Homerange/hr_all_metrics.rds")

hr_metrics_cover_95 <- hr_metrics_org %>%
  mutate(Developed = ud_95_developed_prop) %>%
  mutate(Forest = ud_95_forest_prop) %>%
  mutate('Open\nWater' = ud_95_open_water_prop) %>%
  mutate(Pasture = ud_95_pasture_prop) %>%
  mutate('Shrub\nHerb' = ud_95_shrub_herb_prop) %>%
  mutate('Wetland' = ud_95_wetland_prop) %>%
  mutate(Area_km = ud_95_total) %>%
  dplyr::select(id, Developed, Forest, 'Open\nWater', Pasture, 'Shrub\nHerb',
      Wetland) %>%
    melt(., id.var = "id") %>%
  mutate(ud = "95%") %>% map_if(is.factor, as.character)

hr_metrics_cover_50 <- hr_metrics_org %>%
  mutate(Developed = ud_50_developed_prop) %>%
  mutate(Forest = ud_50_forest_prop) %>%
  mutate('Open\nWater' = ud_50_open_water_prop) %>%
  mutate(Pasture = ud_50_pasture_prop) %>%
  mutate('Shrub\nHerb' = ud_50_shrub_herb_prop) %>%
  mutate('Wetland' = ud_50_wetland_prop) %>%
  dplyr::select(id, Developed, Forest, 'Open\nWater', Pasture, 'Shrub\nHerb',
      Wetland) %>%
    melt(., id.var = "id") %>%
  mutate(ud = "50%") %>% map_if(is.factor, as.character)

hr_metrics_cover <- bind_rows(hr_metrics_cover_95, hr_metrics_cover_50)

gg_hr_cover <- ggplot(data = hr_metrics_cover %>% filter(variable != "Area_km"),
    aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab("Cover Type") + ylab("Proportion of Home Range") + ggtitle("") +
  guides(fill = guide_legend(title = " Utilization\nDistribution")) +
  theme_minimal() +
  theme_latex +
  theme(axis.text.x = element_text(color = "black")) +
  theme(panel.grid.major.x = element_blank()) +
  theme(legend.position = "right") +
  theme(legend.title = element_text(size = 10),
    legend.text  = element_text(size = 10),
    legend.key.size = unit(2, "lines")) +
  theme(panel.grid.major.x = element_blank())+
  theme(plot.background = element_rect(fill = "white", color = "white",
    inherit.blank = FALSE))
gg_hr_cover

ggsave(filename = "Homerange_Cover_Type.png", plot = gg_hr_cover,
  path = file.path(tex_dir, "Figures/Ch2"),
  scale = 1, width = 6, height = 5, units = "in",
  dpi = 300)

# Clean up objects
rm(hr_metrics_org, hr_metrics_cover_50, hr_metrics_cover_95, hr_metrics_cover,
  gg_hr_cover)

# Conspecific and Nest Distance Fits -------------------------------------------

baea_dist <- readRDS("Data/BAEA/baea_dist.rds")
fits_baea_dist <- readRDS("Output/Analysis/Territorial/fits_baea_dist.rds")

baea_dist_lines <- data.frame(x=baea_dist$con_nest_km,
  HalfNorm = dhnorm(baea_dist$con_nest_km,
    fits_baea_dist$halfnorm$estimate["sigma"]),
  Exponential = dexp(baea_dist$con_nest_km,
    fits_baea_dist$exponential$estimate["rate"]),
  Pareto = VGAM::dgpd(baea_dist$con_nest_km, location = 0,
    scale = fits_baea_dist$pareto$estimate["scale"],
    shape = fits_baea_dist$pareto$estimate["shape"]),
  Gamma = dgamma(baea_dist$con_nest_km, fits_baea_dist$gamma$estimate["shape"],
    fits_baea_dist$gamma$estimate["rate"]),
  Weibull = stats::dweibull(baea_dist$con_nest_km,
    fits_baea_dist$weibull$estimate["shape"],
    fits_baea_dist$weibull$estimate["scale"])) %>%
  filter(x > 0)

line_size = .8

gg_baea_dist <- ggplot(baea_dist) +
  geom_histogram(aes(x = con_nest_km, y = ..density..), boundary = 0, bins = 11,
    color = "black", fill = "grey80") +
  ggtitle(NULL) +
  xlab("Nest and Conspecific Distance Metric (km)") +
  ylab("Probability Density") +
  geom_line(data = baea_dist_lines, aes(x, HalfNorm, color = "Half Normal"),
    size = line_size) +
  geom_line(data = baea_dist_lines, aes(x, Exponential, color = "Exponential"),
    size = line_size) +
  geom_line(data = baea_dist_lines, aes(x, Gamma, color = "Gamma"),
    size = line_size) +
  geom_line(data = baea_dist_lines, aes(x, Weibull, color = "Weibull"),
    size = line_size) +
  geom_line(data = baea_dist_lines, aes(x, Pareto, color = "Pareto"),
    size = line_size) +
  scale_color_viridis_d(name = "    Fitted \nDistributions", option = "D") +
  scale_x_continuous(expand = expansion(add = c(.25, 0)),
    limits = c(0,10), breaks = c(0:10)) +
  scale_y_continuous(expand = expansion(add = c(.01, 0)), limits = c(0, 1)) +
  theme_minimal() +
  theme_latex +
  guides(shape = guide_legend(override.aes = list(size = 2)),
    color = guide_legend(override.aes = list(size = 2))) +
  theme(legend.key.size = unit(1, "lines")) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(plot.background = element_rect(fill = "white",
    color = "white",
    inherit.blank = FALSE))

ggsave(filename = "Fits_Baea_Dist.png", plot = gg_baea_dist,
  path = file.path(tex_dir, "Figures/Ch2"), scale = 1, width = 6, height = 4,
  units = "in", dpi = 300)

# Behavioral Proportion Daily --------------------------------------------------

# Load data
baea_behavior <- readRDS("Data/Baea/baea_behavior.rds")
baea_behavior_simple <- baea_behavior %>%
  dplyr::select(datetime, id, time_proportion, bh_nest:behavior)

# Summarize daily behavior by time_proportion
breaks = 20
baea_behavior_sum <- baea_behavior %>%
  mutate(behavior = factor(behavior)) %>%
  mutate(bins = CutProportion(time_proportion, breaks)) %>%
  mutate(bins_mid = factor(CutProportionMid(time_proportion, breaks))) %>%
  group_by(sex, bins_mid) %>%
  count(behavior) %>%
  mutate(value = n/sum(n)) %>%
  mutate(bins_mid = as.numeric(as.character(bins_mid))) %>%
  ungroup(.) %>%
  arrange(sex, bins_mid)

# Make Plot
gg_behave_prop <- ggplot(baea_behavior_sum, aes(x = bins_mid, y = value,
    ymax = 1, fill = behavior)) +
  facet_grid(~ sex, labeller = labeller(sex = Capitalize)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = behavior_colors, name = "Behavior") +
  labs(x = "Daily Period", y = "Behavior Proportion", title = "") +
  theme_minimal() +
  theme_latex +
  theme(panel.spacing = unit(1, "lines")) +
  theme(strip.text = element_text(size = 12, vjust = 0)) +
  theme(axis.text.x.bottom = element_text(angle = 45, hjust = 0.65)) +
  theme(axis.text.y.left = element_text(hjust = 0.5)) +
  guides(shape = guide_legend(override.aes = list(size = 2)),
    color = guide_legend(override.aes = list(size = 2))) +
  theme(legend.title = element_text(size = 12),
    legend.text = element_text(size = 11, hjust = 0),
    legend.key.size = unit(1, "lines")) +
  scale_x_continuous(breaks = seq(0, 1, .1),
    expand = expansion(mult = c(.01, .01))) +
  scale_y_continuous(expand = expansion(mult = c(.00, .01))) +
  theme(axis.ticks = element_line(color = "grey50", size = .65)) +
  theme(axis.ticks.length = unit(5, "pt")) +
  theme(plot.background = element_rect(fill = "white", color = "white",
    inherit.blank = FALSE))

# Save Plot
ggsave(filename = "Behavior_Proportion_Bar.png", plot = gg_behave_prop,
  path = file.path(tex_dir, "Figures/Ch2"), scale = 1, width = 6, height = 4,
  units = "in", dpi = 300)

rm(baea_behavior, baea_behavior_simple, breaks, baea_behavior_sum,
  gg_behave_prop)

# Transition Probabilities -----------------------------------------------------

df_trans_org <- readRDS(file = "Output/Analysis/Transitions/df_trans.rds")
nest_dist_mode <- CalculateMode(df_trans_org$julian_date)
time_prop_mode <- CalculateMode(df_trans_org$time_proportion)

df_trans <- df_trans_org %>%
  mutate(lci = if_else(start_st == 5 & end_st == 5 &
    time_proportion == time_prop_mode, 1, lci)) %>%
  mutate(uci = if_else(start_st == 3 & end_st == 5 &
    time_proportion == time_prop_mode, 0, uci))

state_names <- c('1' = "Cruise", '2' = "Flight", '3' = "Nest", '4' = "Perch",
  '5' = "Roost")

# Tex Strings
tex_head <- tibble(
  tex_str = c("Start State", "End State"),
  tex_name = c("start", "end"))

# Tex Dataframe
tex_df <- bind_rows(
  bind_rows(tex_head) %>% mutate(title_size = 11))

# Create Tex Text Plots
for (i in seq_len(nrow(tex_df))){
  tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
  tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
  title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
  gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
    theme(plot.title = element_text(size = title_size_i))
  ggsave(file = "Output/Analysis/Territorial/TEMP.png", plot = gg_tex,
    width = 5, height = .75)
  tex_i <- image_trim(image_read("Output/Analysis/Territorial/TEMP.png"))
  file.remove("Output/Analysis/Territorial/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}

rm(i, tex_df, tex_name_i, tex_str_i, tex_i)

# Metrics for filtering data
time_prop_mode <- CalculateMode(df_trans %>% pull(time_proportion)) # 0.5030271
julian_date_mode <- CalculateMode(df_trans %>% pull(julian_date)) # 152.9254
range(df_trans$julian_date)

# For getting all of the ggplot theme options
if(FALSE) theme_get()

# Plot Transitions by Date
gg_trans_date <- ggplot(
  data = df_trans %>%
    filter(time_proportion == time_prop_mode),
    aes(julian_date, prob)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = 'grey90', color = 'grey50') +
  geom_line() +
  labs(x = "Date (Julian)", y = "Transition Probability") +
  facet_grid(rows = vars(end_st), cols = vars(start_st),
    labeller = as_labeller(state_names)) +
  theme_minimal() + theme_latex +
#  scale_x_continuous(limits = c(75, 235)) +
  scale_y_continuous(limits = c(-0.001, 1.001), expand = expansion(add = 0))+
  theme_minimal() + theme_latex +
  theme(panel.spacing = unit(1, "lines")) +
  theme(strip.text = element_text(size = 11, vjust = 0)) +
  theme(axis.title = element_text(size = 12, vjust = 0)) +
  theme(axis.text = element_text(size = 10, color = 'black')) +
  theme(axis.text.y.left = element_text(angle = 0, hjust = .6)) +
  theme(axis.text.x.bottom = element_text(angle = 45, vjust = .7)) +
  theme(plot.margin = unit(c(.6, .75, 0.2, 0.2), "cm")) +
  theme(axis.ticks = element_line(color = "grey50", size = .5)) +
  theme(axis.ticks.length = unit(5, "pt"))  +
  theme(panel.spacing.y = unit(1.5, "lines"))
gg_trans_date

# Save Plot
ggsave(filename = "Trans_Probs_Date_NO_LABEL.png", plot = gg_trans_date,
  path = "C:/TEMP/TEMP_Images", scale = 1, width = 6, height = 6,
  units = "in", dpi = 300)

# Plot Transitions by Time
gg_trans_time <- ggplot(
  data = df_trans %>%
    filter(time_proportion > 0 && time_proportion < 1) %>%
    filter(julian_date == julian_date_mode),
    aes(time_proportion, prob))+
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = 'grey90', color = 'grey50') +
  geom_line() +
  labs(x = "Time (Daily Proportion)", y = "Transition Probability") +
  facet_grid(rows = vars(end_st), cols = vars(start_st),
    labeller = as_labeller(state_names)) +
  scale_x_continuous(limits = c(-0.001, 1.001), expand = expansion(add = 0))+
  scale_y_continuous(limits = c(-0.001, 1.001), expand = expansion(add = 0))+
  theme_minimal() + theme_latex +
  theme(panel.spacing = unit(1, "lines")) +
  theme(strip.text = element_text(size = 11, vjust = 0)) +
  theme(axis.title = element_text(size = 12, vjust = 0)) +
  theme(axis.text = element_text(size = 10, color = 'black')) +
  theme(axis.text.y.left = element_text(angle = 0, hjust = .6)) +
  theme(axis.text.x.bottom = element_text(angle = 45, vjust = .7)) +
  theme(plot.margin = unit(c(.6, .75, 0.2, 0.2), "cm")) +
  theme(axis.ticks = element_line(color = "grey50", size = .5)) +
  theme(axis.ticks.length = unit(5, "pt"))  +
  theme(panel.spacing.y = unit(1.5, "lines"))
gg_trans_time

# Save Plot
ggsave(filename = "Trans_Probs_Time_NO_LABEL.png",
  path = "C:/TEMP/TEMP_Images", scale = 1, width = 6, height = 6,
  units = "in", dpi = 300)

# Composite Graph
plot_trans_date <- image_read(file.path("C:/TEMP/TEMP_Images",
  "Trans_Probs_Date_NO_LABEL.png"))

trans_prob_date_fig <-  image_blank(1755, 1800, color = "white") %>%
  image_composite(., plot_trans_date %>% image_chop(., "20x0"),
    offset = "+0+0") %>%
  image_composite(., tex_start, offset = "+795+35") %>%
  image_composite(., image_rotate(tex_end, 90), offset = "+1720+775")
trans_prob_date_fig

plot_trans_time <- image_read(file.path("C:/TEMP/TEMP_Images",
  "Trans_Probs_Time_NO_LABEL.png"))

trans_prob_time_fig <- image_blank(1755, 1800, color = "white") %>%
  image_composite(., plot_trans_time %>% image_chop(., "20x0"),
    offset = "+0+0") %>%
  image_composite(., tex_start, offset = "+795+35") %>%
  image_composite(., image_rotate(tex_end, 90), offset = "+1720+775")
trans_prob_time_fig

# Export to Dissertation
trans_prob_time_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Trans_Prob_Time.png")
image_write(trans_prob_time_fig, path = trans_prob_time_fig_file,
  format = ".png")

# Export to Dissertation
trans_prob_date_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Trans_Prob_Date.png")
image_write(trans_prob_date_fig, path = trans_prob_date_fig_file,
  format = ".png")

# Delete temp files
file.remove(file.path("C:/TEMP/TEMP_Images", "Trans_Probs_Time_NO_LABEL.png"))
file.remove(file.path("C:/TEMP/TEMP_Images", "Trans_Probs_Date_NO_LABEL.png"))

# Clean up objects
rm(df_trans_org, nest_dist_mode, df_trans, state_names,
  tex_head, tex_end, tex_start, time_prop_mode, julian_date_mode,
  gg_trans_date, plot_trans_date, trans_prob_date_fig, plot_trans_time,
  trans_prob_time_fig, trans_prob_time_fig_file, trans_prob_date_fig_file)

# Movement Step Lengths --------------------------------------------------------
baea_movements_wb <- readRDS("Data/BAEA/baea_movements_wb.rds")
move_pars <- readRDS("Output/Analysis/Movements/move_pars.rds")

vec_length <- 100
weibull_dens <- data.frame(grp=factor(), pred=numeric(), dens=numeric())
for (i in 1:nrow(move_pars)){
  pars_i <- move_pars[i,]
  grp = rep(pars_i$behavior_behavior, vec_length)
  pred = seq(pars_i$min_step, pars_i$max_step, length = vec_length)
  dens = dweibull(pred, shape=pars_i$weibull_shape, scale=pars_i$weibull_scale)
  weibull_dens <- rbind(weibull_dens, data.frame(grp, pred, dens))
}

MetersToKilometers <- function(x){
  x_out <- x/1000
  return(x_out)
}
Multiplier100 <- function(x){
  x_100 <- x*100
  x_100 <- str_pad(x_100, 5, side = "right", pad = 0)
  x_100 <- if_else(x_100 == "00000", "0", x_100)
  return(x_100)
}

fill_color <- viridis(5)[2]
line_color <- inferno(5)[4]
if(FALSE) tmaptools::palette_explorer()

ind_list <- lapply(sort(unique(baea_movements_wb$behavior_behavior)),
    function(i){
  grp_i = str_replace_all(i, "->", "$\\\\rightarrow$ phantom(x)}{")

  grp_i_title <- paste0("$\\overset{", grp_i, "}$") %>%
    latex2exp::TeX(.)

  ggplot(baea_movements_wb[baea_movements_wb$behavior_behavior == i,],
    aes(x = step_length)) +
  geom_histogram(aes(y = ..density.., weight = weights), binwidth = 500,
    size = .1, boundary = 0, color = "black", fill = fill_color) +
  geom_line(data = weibull_dens[weibull_dens$grp == i, ], aes(x = pred,
    y = dens), size = .8, color = line_color) +
  xlab(NULL) + ylab(NULL) + ggtitle(NULL) +
  theme_minimal() +
  theme_latex +
  scale_y_continuous(labels = Multiplier100)  +
  scale_x_continuous(labels = MetersToKilometers)  +
  theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
  theme(plot.title = element_text(size = 11, vjust = -2, hjust = 0.5))
  # Old code for annotations of Weibull parameters:
  # annotate("text", Inf, Inf, hjust = 1.1, vjust = 1.1, size = 5,
  # label = paste0("Weibull Distribution\n", "shape = ",
  #   signif(move_pars[move_pars$behavior_behavior == i, "weibull_shape"],
  #     3), "\n", "scale = ",
  #   signif(move_pars[move_pars$behavior_behavior == i, "weibull_scale"],
  #     3)))
})

layout <- '
ABC#
DEXG
HIJK
LMNO
#PQ#'

movements_step_length_plots <- wrap_plots(A = ind_list[[1]],
  B = ind_list[[2]],
  C = ind_list[[3]],
  D = ind_list[[4]],
  E = ind_list[[5]],
  X = ind_list[[6]],
  G = ind_list[[7]],
  H = ind_list[[8]],
  I = ind_list[[9]],
  J = ind_list[[10]],
  K = ind_list[[11]],
  L = ind_list[[12]],
  M = ind_list[[13]],
  N = ind_list[[14]],
  O = ind_list[[15]],
  P = ind_list[[16]],
  Q = ind_list[[17]],
  design = layout)
movements_step_length_plots

# Save Temp (No Label) File
movements_step_length_no_label_fig_file = file.path("C:/TEMP/TEMP_Images",
  "Movements_Step_Length_NO_LABEL.png")
ggsave(filename = basename(movements_step_length_no_label_fig_file),
  plot =  movements_step_length_plots,
  path = dirname(movements_step_length_no_label_fig_file), scale = 1,
  width = 5.5, height = 5.5, units = "in", dpi = 300)

# Create Tex Strings
tex_head <- tibble(
  tex_str = c("Probability", "Step Length (km)"),
  tex_name = c("lab_probability", "lab_step_length"))
tex_head_df <- bind_rows(
  bind_rows(tex_head) %>% mutate(title_size = 12))

# Create Tex Strings
tex_state <- tibble(
  tex_str = c("Start State", "End State"),
  tex_name = c("start", "end"))
tex_state_df <- bind_rows(
  bind_rows(tex_state) %>% mutate(title_size = 12))

# Create Tex Strings
tex_behavior <- tibble(
  tex_str = c("Cruise", "Flight", "Nest", "Perch", "Roost"),
  tex_name = c("cruise", "flight", "nest", "perch", "roost"))
tex_behavior_df <- bind_rows(
  bind_rows(tex_behavior) %>% mutate(title_size = 11))

tex_df <- bind_rows(tex_head_df, tex_state_df, tex_behavior_df)

# Create Tex Text Plots
for (i in seq_len(nrow(tex_df))){
  tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
  tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
  title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
  gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
    theme(plot.title = element_text(size = title_size_i))
  ggsave(file = "C:/TEMP/TEMP_Images/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("C:/TEMP/TEMP_Images/TEMP.png"))
  file.remove("C:/TEMP/TEMP_Images/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}
rm(i, tex_head, tex_head_df, tex_state, tex_state_df, tex_behavior,
  tex_behavior_df, tex_df, tex_name_i, tex_str_i, tex_i)

# Image background
backgrd <- image_blank(1800, 1800, color = "white")

# Create Final Plot and Export to Dissertation
movements_step_length_fig <- image_read(movements_step_length_no_label_fig_file)
movements_step_length_labels_fig <- backgrd %>%
  image_composite(., movements_step_length_fig, offset = "+45+100") %>%
  image_composite(., image_rotate(tex_lab_probability, 270),
    offset = "+10+750") %>%
  image_composite(., tex_lab_step_length, offset = "+750+1750") %>%
  image_composite(., tex_end, offset = "+850+0") %>%
  image_composite(., image_rotate(tex_start, 90), offset = "+1750+790") %>%
  image_composite(., tex_cruise, offset = "+275+65") %>%
  image_composite(., tex_flight, offset = "+660+65") %>%
  image_composite(., tex_nest, offset = "+1065+65") %>%
  image_composite(., tex_perch, offset = "+1440+65") %>%
  image_composite(., image_rotate(tex_cruise, 90), offset = "+1685+150") %>%
  image_composite(., image_rotate(tex_flight, 90), offset = "+1685+510") %>%
  image_composite(., image_rotate(tex_nest, 90), offset = "+1685+850") %>%
  image_composite(., image_rotate(tex_perch, 90), offset = "+1685+1150") %>%
  image_composite(., image_rotate(tex_roost, 90), offset = "+1685+1490")
  #image_composite(., tex_roost, offset = "+1200+50")
movements_step_length_labels_fig

movements_step_length_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Movements_Step_Length.png")
image_write(movements_step_length_labels_fig,
  path = movements_step_length_fig_file, format=".png")
file.remove(movements_step_length_no_label_fig_file)

# Movement Directions ----------------------------------------------------------

baea_movements_vm <- readRDS("Data/BAEA/baea_movements_vm.rds")
move_pars <- readRDS("Output/Analysis/Movements/move_pars.rds")

bin_width = (2*pi)/24
breaks <- seq(0, (2*pi), by=((2*pi)/12))
labels <- c(0, "", "", expression(pi / 2), "", "",
   expression(pi), "", "", expression(1.5*pi), "", "",
   expression(2*pi))
minor_breaks <- seq(0, 2*pi, by=bin_width)
minor_labels <- c(0, "", "", expression(pi / 4), "", "", expression(pi / 2),
  "", "", expression(3/4*pi), "", "", expression(pi),
  "", "", expression(5/4*pi), "", "", expression(1.5*pi),
  "", "", expression(7/4*pi), "", "")
limits <- c(0, 2*pi)
vec_length <- 100
von_mises_dens <- data.frame(grp=factor(), pred=numeric(), dens=numeric())

for (i in 1:nrow(move_pars)){
  pars_i <- move_pars[i,]
  grp = rep(pars_i$behavior_behavior, vec_length)
  pred = seq(limits[1], limits[2], length = vec_length)
  dens = dmixedvm(pred, mu1 = pars_i$mvm_mu1, mu2 = pars_i$mvm_mu2,
    kappa1 = pars_i$mvm_kappa1, kappa2 = pars_i$mvm_kappa2,
    p = pars_i$mvm_prop)
  von_mises_dens <- rbind(von_mises_dens, data.frame(grp, pred, dens))
}

baea_movements_vm_sub <- baea_movements_vm %>%
  filter(!behavior %in% c("Perch", "Nest", "Roost"))

fill_color <- viridis(5)[2] #"grey80"
line_color <- inferno(5)[4]
#tmaptools::palette_explorer()

# Individual Step-Type Plots Using Polar Coordinates
ind_list = lapply(sort(unique(baea_movements_vm_sub$behavior_behavior)),
  function(i){
    grp_i = str_replace_all(i, "->", "$\\\\rightarrow$")
    ggplot(baea_movements_vm[baea_movements_vm$behavior_behavior == i, ],
      aes(x = turn_angle)) +
    geom_histogram(aes(y = ..density.., weight=weights), fill = fill_color,
      color = "black", boundary = 0, size = .1, binwidth = (2*pi)/24) +
    geom_line(data = von_mises_dens[von_mises_dens$grp == i, ],
      aes(x = pred, y = dens), size = .8, colour = line_color) +
    coord_polar(start = (1.5*pi), direction = -1) +
    scale_y_continuous(labels = NULL) +
    scale_x_continuous(limits = limits, labels = minor_labels,
      breaks = minor_breaks[-25], minor_breaks = minor_breaks, expand = c(0,0))+
    theme(axis.ticks = element_blank()) +
    #facet_grid(. ~ behavior_behavior) +
    theme_minimal() +
    theme_latex +
    theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
    theme(axis.text = element_text(size = 9)) +
#    theme(axis.text = element_text(size = 7)) +
    theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
    theme(plot.title = element_text(size = 10, vjust = -2, hjust = 0.5)) +
    #theme(plot.title = element_text(size = 8, vjust = -2, hjust = 0.5)) +
    theme(legend.position = "none") +
    theme(panel.grid.major = element_line(colour = "grey90"))  +
    theme(panel.grid.minor = element_line(colour = "grey90"))  +
    ggtitle(TeX(grp_i)) +
    labs(x = NULL, y = NULL)
})

layout <- c(
  patchwork::area(t = 1, l = 1),
  patchwork::area(t = 1, l = 2),
  patchwork::area(t = 1, l = 3),
  patchwork::area(t = 2, l = 1),
  patchwork::area(t = 2, l = 2),
  patchwork::area(t = 2, l = 3),
  patchwork::area(t = 2, l = 4)
)
#plot(layout)

movements_direction_plots <- ind_list[[1]] + ind_list[[2]] + ind_list[[3]] +
  ind_list[[4]] + ind_list[[5]] + ind_list[[6]] + ind_list[[7]] +
  plot_layout(design = layout)

# Save Temp (No Label) File
movements_direction_no_lab_fig_file <- file.path("C:/TEMP/TEMP_Images",
  "Movements_Direction_NO_LABEL.png")
ggsave(filename = basename(movements_direction_no_lab_fig_file),
  plot =  movements_direction_plots,
  path = dirname(movements_direction_no_lab_fig_file), scale = 1, width = 6,
  height = 3.5, units = "in", dpi = 300)

# Create Tex Strings
tex_head <- tibble(
  tex_str = c("Probability (Polar Coordinates)", "Direction (Radians)"),
  tex_name = c("lab_density", "lab_direction"))
tex_df <- bind_rows(
  bind_rows(tex_head) %>% mutate(title_size = 11))

# Create Tex Text Plots
for (i in seq_len(nrow(tex_df))){
  tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
  tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
  title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
  gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
    theme(plot.title = element_text(size = title_size_i))
  ggsave(file = "C:/TEMP/TEMP_Images/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("C:/TEMP/TEMP_Images/TEMP.png"))
  file.remove("C:/TEMP/TEMP_Images/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}
rm(i, tex_df, tex_name_i, tex_str_i, tex_i)

# Image background
backgrd <- image_blank(1800, 1100, color = "white")

# Create Final Plot and Export to Dissertation
movements_direction_fig <- image_read(movements_direction_no_lab_fig_file)
movements_direction_labels_fig <- backgrd %>%
  image_composite(., movements_direction_fig, offset = "+50+00") %>%
  image_composite(., image_rotate(tex_lab_density, 270), offset = "+20+230") %>%
  image_composite(., tex_lab_direction, offset = "+770+1030")
movements_direction_labels_fig
movements_direction_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Movements_Direction.png")
image_write(movements_direction_labels_fig,
  path = movements_direction_fig_file, format = ".png")

file.remove(file.path(movements_direction_no_lab_fig_file))

# Movement Kernels -------------------------------------------------------------

move_dens <- data.frame(grp = character(), x = numeric(), y = numeric(),
  dens = numeric())
for (i in 1:nrow(move_pars)){
  move_pars_i <- move_pars[i, ]
  ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise", "Flight"),
    FALSE, TRUE)
  kernel_i <- CreateMoveKernelWeibullVonMises(
      max_r = NULL,
      cellsize = 30,
      mu1 = move_pars_i$mvm_mu1[1],
      mu2 = move_pars_i$mvm_mu2[1],
      kappa1 = move_pars_i$mvm_kappa1[1],
      kappa2 = move_pars_i$mvm_kappa2[1],
      mix = move_pars_i$mvm_prop[1],
      shape = move_pars_i$weibull_shape[1],
      scale = move_pars_i$weibull_scale[1],
      ignore_von_mises = ignore_von_mises)
  r <- (30*((nrow(kernel_i)-1)/2))+(30/2)
  kernel_raster <- raster::raster(kernel_i, xmn=-r, xmx=r, ymn=-r, ymx=r)
  #print(raster::freq(kernel_raster))
  df <- data.frame(raster::rasterToPoints(kernel_raster)) # NA cells removed.
  names(df)[3] <- "dens"
  df$behavior_behavior <- move_pars_i$behavior_behavior
  move_dens <- rbind(move_dens, df)
}

# All plots on one figure
ind_list = lapply(sort(unique(move_dens$behavior_behavior)), function(i){
  grp_i = str_replace_all(i, "->", "$\\\\rightarrow$")
  gg_move_kernel <- move_dens %>%
      filter(behavior_behavior == i) %>%
    ggplot(., aes(x = x, y = y)) +
    geom_raster(aes(fill = dens)) +
    coord_fixed(ratio = 1) +
    scale_fill_viridis_c(name = "Probability", option = "C", direction = -1,
      labels = scales::scientific) +
    scale_x_continuous(expand = c(0.005, 0.005), labels = MetersToKilometers) +
    scale_y_continuous(expand = c(0.005, 0.005), labels = MetersToKilometers) +
    theme_minimal() +
    theme_latex +
    theme(plot.margin = margin(3, 6, 3, 6, "pt")) +
    #theme(axis.text = element_text(size = 7)) +
    theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
    #theme(legend.text = element_text(size = 7)) +
    #theme(legend.title = element_text(size = 8)) +
    #theme(plot.title = element_text(size = 9, vjust = -1, hjust = 0.5)) +
    #guides(fill = guide_colourbar(barwidth = .5, barheight = 3)) +
    theme(legend.position = "none") +
    ggtitle(NULL) + labs(x = NULL, y = NULL)
  return(gg_move_kernel)
})

layout <- '
ABC#
DEXG
HIJK
LMNO
#PQ#'

movements_kernel_plots <- wrap_plots(A = ind_list[[1]],
  B = ind_list[[2]], C = ind_list[[3]], D = ind_list[[4]], E = ind_list[[5]],
  X = ind_list[[6]], G = ind_list[[7]], H = ind_list[[8]], I = ind_list[[9]],
  J = ind_list[[10]], K = ind_list[[11]], L = ind_list[[12]], M =ind_list[[13]],
  N = ind_list[[14]], O = ind_list[[15]], P = ind_list[[16]], Q =ind_list[[17]],
  design = layout)
movements_kernel_plots

# Save Temp (No Label) File
movements_kernel_no_label_fig_file = file.path("C:/TEMP/TEMP_Images",
  "Movements_Kernel_NO_LABEL.png")
ggsave(filename = basename(movements_kernel_no_label_fig_file),
  plot =  movements_kernel_plots,
  path = dirname(movements_kernel_no_label_fig_file), scale = 1, width = 5,
  height = 5.75, units = "in", dpi = 300)

# Create Tex Strings
tex_head <- tibble(
  tex_str = c("Distance (km)"),
  tex_name = c("lab_distance"))
tex_head_df <- bind_rows(
  bind_rows(tex_head) %>% mutate(title_size = 11))

# Create Tex Strings
tex_state <- tibble(
  tex_str = c("Start State", "End State"),
  tex_name = c("start", "end"))
tex_state_df <- bind_rows(
  bind_rows(tex_state) %>% mutate(title_size = 12))

# Create Tex Strings
tex_behavior <- tibble(
  tex_str = c("Cruise", "Flight", "Nest", "Perch", "Roost"),
  tex_name = c("cruise", "flight", "nest", "perch", "roost"))
tex_behavior_df <- bind_rows(
  bind_rows(tex_behavior) %>% mutate(title_size = 11))

tex_df <- bind_rows(tex_head_df, tex_state_df, tex_behavior_df)

# Create Tex Text Plots
for (i in seq_len(nrow(tex_df))){
  tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
  tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
  title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
  gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
    theme(plot.title = element_text(size = title_size_i))
  ggsave(file = "C:/TEMP/TEMP_Images/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("C:/TEMP/TEMP_Images/TEMP.png"))
  file.remove("C:/TEMP/TEMP_Images/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}

rm(i, tex_head, tex_head_df, tex_state, tex_state_df, tex_behavior,
  tex_behavior_df, tex_df, tex_name_i, tex_str_i, tex_i)

# Create Manual Legend
legend_tbl <- tibble(x = c(0, 1), y = c(.5, .5))
gg_legend <- legend_tbl %>%
  ggplot(., aes(x = x, y = y)) +
  geom_point(aes(fill = x)) +
  coord_fixed(ratio = 1) +
  guides(fill = guide_colourbar(ticks = FALSE, barwidth = .5,
    barheight = 2.75)) +
  scale_fill_viridis_c(name = "Probability", option = "C", direction = -1,
    breaks = c(min(legend_tbl$x), max(legend_tbl$x)),
    labels = c("Min (kernel)", "Max (kernel)")) +
  theme_latex

gg_legend_only <- as_ggplot(get_legend(gg_legend))
gg_legend_only

# Save Temp (No Label) File
movements_kernel_legend_fig_file = file.path("C:/TEMP/TEMP_Images",
  "Movements_Kernel_Legend.png")
ggsave(filename = basename(movements_kernel_legend_fig_file),
  plot =  gg_legend_only,
  path = dirname(movements_kernel_no_label_fig_file), scale = 1, width = 1.2,
  height = 1, units = "in", dpi = 300)

# Image background
backgrd <- image_blank(1660, 1800, color = "white")

# Create Final Plot and Export to Dissertation
movements_kernel_fig <- image_read(movements_kernel_no_label_fig_file)
movements_legend_fig <- image_read(movements_kernel_legend_fig_file)

movements_kernel_labels_fig_ver_1 <- backgrd %>%
  image_composite(., movements_kernel_fig, offset = "+45+65") %>%
  image_composite(., image_rotate(tex_lab_distance, 270),
    offset = "+10+750") %>%
  image_composite(., tex_lab_distance, offset = "+750+1755") %>%
  image_composite(., tex_end, offset = "+750+0") %>%
  image_composite(., image_rotate(tex_start, 90), offset = "+1620+790") %>%
  image_composite(., tex_cruise, offset = "+250+65") %>%
  image_composite(., tex_flight, offset = "+625+65") %>%
  image_composite(., tex_nest, offset = "+985+65") %>%
  image_composite(., tex_perch, offset = "+1305+65") %>%
  image_composite(., image_rotate(tex_cruise, 90), offset = "+1550+150") %>%
  image_composite(., image_rotate(tex_flight, 90), offset = "+1550+510") %>%
  image_composite(., image_rotate(tex_nest, 90), offset = "+1550+850") %>%
  image_composite(., image_rotate(tex_perch, 90), offset = "+1550+1150") %>%
  image_composite(., image_rotate(tex_roost, 90), offset = "+1550+1490") %>%
  image_composite(., movements_legend_fig, offset = "+1200+1400")

movements_kernel_labels_fig_ver_1a <- backgrd %>%
  image_composite(., movements_kernel_fig, offset = "+45+65") %>%
  image_composite(., image_rotate(tex_lab_distance, 270),
    offset = "+10+750") %>%
  image_composite(., tex_lab_distance, offset = "+750+1755") %>%
  image_composite(., tex_end, offset = "+750+0") %>%
  image_composite(., image_rotate(tex_start, 90), offset = "+1620+790") %>%
  image_composite(., tex_cruise, offset = "+250+65") %>%
  image_composite(., tex_flight, offset = "+625+65") %>%
  image_composite(., tex_nest, offset = "+985+65") %>%
  image_composite(., tex_perch, offset = "+1305+65") %>%
  image_composite(., image_rotate(tex_cruise, 90), offset = "+1550+150") %>%
  image_composite(., image_rotate(tex_flight, 90), offset = "+1550+510") %>%
  image_composite(., image_rotate(tex_nest, 90), offset = "+1550+850") %>%
  image_composite(., image_rotate(tex_perch, 90), offset = "+1550+1150") %>%
  image_composite(., image_rotate(tex_roost, 90), offset = "+1550+1490") %>%
  image_composite(., movements_legend_fig, offset = "+90+1435")

movements_kernel_labels_fig_ver_2 <- backgrd %>%
  image_composite(., movements_kernel_fig, offset = "+45+65") %>%
  image_composite(., image_rotate(tex_lab_distance, 270),
    offset = "+10+750") %>%
  image_composite(., tex_lab_distance, offset = "+750+1755") %>%
  image_composite(., tex_end, offset = "+750+0") %>%
  image_composite(., image_rotate(tex_start, 90), offset = "+1620+790") %>%
  image_composite(., tex_cruise, offset = "+250+65") %>%
  image_composite(., tex_flight, offset = "+625+65") %>%
  image_composite(., tex_nest, offset = "+985+65") %>%
  image_composite(., tex_perch, offset = "+1315+390") %>%
  image_composite(., image_rotate(tex_cruise, 90), offset = "+1210+160") %>%
  image_composite(., image_rotate(tex_flight, 90), offset = "+1550+510") %>%
  image_composite(., image_rotate(tex_nest, 90), offset = "+1550+850") %>%
  image_composite(., image_rotate(tex_perch, 90), offset = "+1550+1150") %>%
  image_composite(., image_rotate(tex_roost, 90), offset = "+1210+1490") %>%
  image_composite(., movements_legend_fig, offset = "+1315+1430")

movements_kernel_labels_fig_ver_1
movements_kernel_labels_fig_ver_1a
movements_kernel_labels_fig_ver_2

movements_kernel_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Movements_Kernel.png")
image_write(movements_kernel_labels_fig_ver_2, path =movements_kernel_fig_file,
  format = ".png")
file.remove(movements_kernel_no_label_fig_file)
file.remove(movements_kernel_legend_fig_file)

# ---------------------------- CHAPTER 3 ---------------------------------------

# Plot ConNestDist Scale Logistic Function -------------------------------------
con_nest_pars <- readRDS("Output/Analysis/Territorial/con_nest_pars.rds")

shape <- con_nest_pars$gamma$shape
rate <- con_nest_pars$gamma$rate

x_min <- con_nest_pars$rescale$x_min
y_min <- con_nest_pars$rescale$y_min
y_max <- con_nest_pars$rescale$y_max

x_min_new <- con_nest_pars$rescale$x_min_new
y_min_new <- con_nest_pars$rescale$y_min_new
y_max_new <- con_nest_pars$rescale$y_max_new

y_diff_new <- y_max_new - y_min_new

# Plot the Cumulative Distribution Function of the Gamma distribution
x = seq(0, 30, .5)
y_pgamma <- pgamma(x, shape = shape, rate = rate)
y_rescale <- y_min_new + (((y_pgamma-y_min)/(y_max-y_min)) * (y_diff_new))
df_rescale <- as.data.frame(cbind(x, y_pgamma, y_rescale))
text_size = 10; line_size = .8; point_size = 2; space_legend = 1

gg_connest_pgamma <- ggplot(df_rescale, aes(x, y_pgamma)) +
  geom_line(colour = "slateblue4", size = 1.5) +
  theme(legend.position = "none") +
  theme_minimal() +
  theme_latex +
  theme(text = element_text(family = "Latin Modern Roman")) +
  theme(line = element_line(size = line_size)) +
  guides(shape = guide_legend(override.aes = list(size = point_size)),
    color = guide_legend(override.aes = list(size = point_size))) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(x = 'Conspecific and Nest Distance Value (km)', y =
      'Cumulative Distribution\nFunction (Probability)', title = NULL)
gg_connest_pgamma
ggsave(filename = "ConNest_PGamma.png", plot = gg_connest_pgamma,
  path = file.path(tex_dir, "Figures/Ch3"), scale = 1, width = 6, height = 2.5,
  units = "in", dpi = 300)

gg_connest_rescale <- ggplot(df_rescale, aes(x, y_rescale)) +
  geom_line(colour = "slateblue4", size = 1.5) +
  theme(legend.position = "none") +
  theme_minimal() +
  theme_latex +
  theme(line = element_line(size = line_size)) +
  guides(shape = guide_legend(override.aes = list(size = point_size)),
    color = guide_legend(override.aes = list(size = point_size))) +
  theme(panel.grid.minor.x = element_blank()) +
  labs(x = 'Conspecific and Nest Distance Value (km)', y =
      'IBM Logistic Scale\nParameter', title = NULL)
gg_connest_rescale
ggsave(filename = "ConNest_Rescale.png", plot = gg_connest_rescale,
  path = file.path(tex_dir, "Figures/Ch3"), scale = 1, width = 6, height = 2.4,
  units = "in", dpi = 300)

# Plot Logistic Function -------------------------------------------------------
x <- seq(-5, 5, .1)
pred_logistic_lines <- data.frame(x=x,
  y_01 = LogisticByInflection(x, inflection = 0, scale = df_rescale %>%
      filter(x == 1) %>% pull(y_rescale)),
  y_02 = LogisticByInflection(x, inflection = 0, scale = df_rescale %>%
      filter(x == 2) %>% pull(y_rescale)),
  y_03 = LogisticByInflection(x, inflection = 0, scale = df_rescale %>%
      filter(x == 3) %>% pull(y_rescale)),
  y_04 = LogisticByInflection(x, inflection = 0, scale = df_rescale %>%
      filter(x == 4) %>% pull(y_rescale)),
  y_05 = LogisticByInflection(x, inflection = 0, scale = df_rescale %>%
      filter(x == 5) %>% pull(y_rescale)),
  y_06 = LogisticByInflection(x, inflection = 0, scale = df_rescale %>%
      filter(x == 6) %>% pull(y_rescale)),
  y_07 = LogisticByInflection(x, inflection = 0, scale = df_rescale %>%
      filter(x == 7) %>% pull(y_rescale)),
  y_08 = LogisticByInflection(x, inflection = 0, scale = df_rescale %>%
      filter(x == 8) %>% pull(y_rescale)),
  y_09 = LogisticByInflection(x, inflection = 0, scale = df_rescale %>%
      filter(x == 9) %>% pull(y_rescale)),
  y_10 = LogisticByInflection(x, inflection = 0, scale = df_rescale %>%
      filter(x == 10) %>% pull(y_rescale)),
  y_20 = LogisticByInflection(x, inflection = 0, scale = df_rescale %>%
      filter(x == 20) %>% pull(y_rescale))) %>%
  mutate(y_01 = ifelse(x < -1, NA, y_01)) %>%
  mutate(y_02 = ifelse(x < -2, NA, y_02)) %>%
  mutate(y_03 = ifelse(x < -3, NA, y_03)) %>%
  mutate(y_04 = ifelse(x < -4, NA, y_04)) %>%
  mutate(y_05 = ifelse(x < -5, NA, y_05))

# Make plot with multiple lines
df_logistic <- as.data.frame(cbind(x, pred_logistic_lines))
text_size = 10; line_size = .8; point_size = 2; space_legend = 1

gg_connest_logistic <- ggplot(df_logistic) +
  ggtitle(NULL) +
  labs(x = 'Conspecific and Nest Distance Relative Position (km)',
    y = 'Kernel Surface (Probability)') +
  geom_line(data = pred_logistic_lines, aes(x, y_20, color = "20"),
    size = line_size) +
  geom_line(data = pred_logistic_lines, aes(x, y_10, color = "10"),
    size = line_size) +
  geom_line(data = pred_logistic_lines, aes(x, y_08, color = "08"),
    size = line_size) +
  geom_line(data = pred_logistic_lines, aes(x, y_05, color = "05"),
    size = line_size) +
  geom_line(data = pred_logistic_lines, aes(x, y_03, color = "03"),
    size = line_size) +
  geom_line(data = pred_logistic_lines, aes(x, y_02, color = "02"),
    size = line_size) +
  geom_line(data = pred_logistic_lines, aes(x, y_01, color = "01"),
    size = line_size) +
  scale_color_viridis_d(name = "Conspecific and\nNest Distance\nValue (km)",
    option = "D", labels=c("1", "2", "3", "5", "8", "10", "20")) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(0)) +
  scale_x_continuous(limits = c(-5,5), breaks = -5:5, expand = expansion(0)) +
  theme_minimal() +
  theme_latex +
  theme(line = element_line(size = line_size)) +
  guides(shape = guide_legend(override.aes = list(size = point_size)),
    color = guide_legend(override.aes = list(size = point_size))) +
  theme(legend.key.size = unit(space_legend, "lines")) +
  theme(panel.grid.minor.x = element_blank())
gg_connest_logistic
ggsave(filename = "ConNest_Logistic.png", plot = gg_connest_logistic,
  path = file.path(tex_dir, "Figures/Ch3"), scale = 1, width = 6, height = 3.5,
  units = "in", dpi = 300)

# Calibration Hydro Distance ---------------------------------------------------

for (i in seq_len(length(sims))){
  sim_i <- sims[i]
  sim_dir_i <- paste0(sim_dir, str_pad(sim_i, 2, side = "left", pad = "0"))
  sim_perch_dist_i <- list.files(path = file.path(sim_dir_i,
      sim_calibration_dir), pattern = "sim_perch_dist_*")  %>%
    map(~ readRDS(file.path(sim_dir_i, sim_calibration_dir, .))) %>%
    reduce(bind_rows)
  if(i == 1){
    sim_perch_dist <- sim_perch_dist_i
  } else {
    sim_perch_dist <- bind_rows(sim_perch_dist, sim_perch_dist_i)
  }
}

gg_sim_dist <- ggplot(sim_perch_dist) +
  geom_histogram(aes(x = hydro_dist, y = after_stat(count/sum(count))),
    boundary = 0, binwidth = 30, color = "black",
    fill = behavior_colors["Perch"]) +
  ggtitle("Simulation") +
  xlab("Hydro Distance (m)") +
  ylab(NULL) +
  theme_minimal() +
  theme_latex

# Compare simulation and empirical data
baea_perch_dist <- readRDS(file.path(baea_calibration_dir,
  "baea_perch_dist.rds"))

gg_baea_dist <- ggplot(baea_perch_dist) +
  geom_histogram(aes(x = hydro_dist, y = after_stat(count/sum(count))),
    boundary = 0, binwidth = 30, color = "black",
    fill = behavior_colors["Perch"]) +
  ggtitle("Empirical") +
  xlab("Hydro Distance (m)") +
  ylab("Proportion of Perch Locations") +
  theme_minimal() +
  theme_latex

gg_combine_hydro_dist <- gg_baea_dist + gg_sim_dist
gg_combine_hydro_dist

ggsave(filename = "Calibration_Hydro_Dist.svg", plot = gg_combine_hydro_dist,
  path = file.path(tex_dir, "Figures/Ch3"), scale = 1, width = 6, height = 4,
  units = "in", dpi = 300)

# Calibration Ridgeline Flights ------------------------------------------------

for (i in seq_len(length(sims))){
  sim_i <- sims[i]
  sim_dir_i <- paste0(sim_dir, str_pad(sim_i, 2, side = "left", pad = "0"))
  sim_ridge_sum_i <- list.files(path = file.path(sim_dir_i,
      sim_calibration_dir), pattern = "sim_ridge_sum_*")  %>%
    map(~ readRDS(file.path(sim_dir_i, sim_calibration_dir, .))) %>%
    reduce(bind_rows) %>%
    mutate(nest_name = RecodeNestIdToName(nest_id))
  if(i == 1){
    sim_ridge_sum <- sim_ridge_sum_i
  } else {
    sim_ridge_sum <- bind_rows(sim_ridge_sum, sim_ridge_sum_i)
  }
}

# Compare simulation and empirical data
baea_ridge_sum <- readRDS(baea_ridge_sum_file) %>%
  mutate(nest_name = RecodeNestIdToName(nest_id))

# Graph ridge-crossing summary data
gg_combine_ridge <- ggplot() +
  geom_errorbar(data = baea_ridge_sum,
    aes(x = nest_name, #y = ridge_steps_prop,
      ymin = quant_05, ymax = quant_95,
      width = .125, color = "95% CI"), size = 1) +
  geom_jitter(data = sim_ridge_sum, aes(x = nest_name, y = ridge_steps_prop,
    fill = "Run Result"), color = "black",  shape = 24, size = 2, width = .05,
    show.legend = TRUE) +
  geom_point(data = baea_ridge_sum, aes(x = nest_name, y = ridge_steps_prop,
    color = "Mean"),
    size = 3) +
  scale_fill_manual(name = "Simulation", values = viridis(1, 1, direction= -1))+
  scale_color_manual(name = "Empirical Data", values = c("black", "blue")) +
  guides(colour = guide_legend(override.aes =
      list(linetype = c("solid", "blank"),
        shape = c(NA, 16))))  +
  scale_y_continuous(limits = c(0, 0.15), expand = expansion(0)) +
  scale_x_discrete(expand = expansion(.1)) +
  theme_minimal() +
  theme_latex +
  theme(axis.text.x = element_text(color = "black")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Nest Area") + ylab("Ridge-Crossing Step Proportion") +
  theme(legend.position = "right")
gg_combine_ridge

ggsave(filename = "Calibration_Ridge_Crossings.svg", plot = gg_combine_ridge,
  path = file.path(tex_dir, "Figures/Ch3"), scale = 1, width = 6, height = 4,
  units = "in", dpi = 300, bg = "white")

# ---------------------------- CHAPTER 4 ---------------------------------------

# Start Nest For Loop ----------------------------------------------------------

for (i in c("Grand_Lake", "Wilson")){
  nest_str <- i
  nest_lower <- nest_str %>% str_to_lower(.)
  nest_title <- nest_str %>% str_replace(., "_", " ") %>% str_to_title(.)
  print(paste0("Starting: ", nest_title))

  # Get Windrose Data ----------------------------------------------------------
  if(nest_str == "Wilson"){
    site_wind <- readRDS("Output/Analysis/Wind/greenville_wind.rds")
    windrose_filename <- "Greenville_Windrose.svg"
  }

  if(nest_str == "Grand_Lake"){
    site_wind <- readRDS("Output/Analysis/Wind/millinocket_wind.rds")
    windrose_filename <- "Millinocket_Windrose.svg"
  }

  wind_df <- site_wind %>%
    filter(ws != 0) %>%
    mutate(ws_bin = ws) %>%
    mutate(ws_bin = str_replace_all(ws,
      c("0" = "Calm",
        "3.5" = "2.0 - 4.9",
        "6" = "5.0 - 6.9",
        "8.5" = "7.0 - 9.9",
        "12.5" = "10.0 - 14.9",
        "17.5" = "15.0 - 19.9",
        "21" = "20.0+"))) %>%
    mutate(ws_bin = as_factor(ws_bin))

  # Colors for speed (fill in ggplot)
  spd_colors <- rev(viridis(n_distinct(wind_df$ws_bin, na.rm = FALSE)))

  # Figure out the wind direction bins
  dirres = 22.5
  dir_breaks <- c(-dirres/2, seq(dirres/2, 360 - dirres/2, by = dirres),
    360 + dirres/2)
  dir_labels <- c(paste(360-dirres/2,"-",dirres/2),
    paste(seq(dirres/2, 360-3*dirres/2, by = dirres), "-",
      seq(3*dirres/2, 360-dirres/2, by = dirres)),
    paste(360-dirres/2,"-",dirres/2))

  # Assign each wind direction to a bin
  wind_df_binned <- wind_df %>%
    mutate(dir_binned = cut(wind_df$wd, breaks = dir_breaks,
      ordered_result = TRUE))
  levels(wind_df_binned$dir_binned) <- dir_labels

  # Labels on y-axis
  y_labels <- data.frame(x = pi, y = c(5, 10, 15), labels = c("5%", "10%",
    "15%"))

  # Create windrose plot
  gg_windrose <- ggplot(data = wind_df_binned,
    aes(x = dir_binned, fill = fct_rev(ws_bin), y = probab)) +
    geom_col() +
    geom_text(data = y_labels, inherit.aes = FALSE, aes(x = x, y = y,
      label = labels), family = "Latin Modern Roman", size = 3.5) +
    scale_x_discrete(drop = FALSE,
      labels = c("N","NNE","NE","ENE", "E", "ESE", "SE","SSE",
        "S","SSW", "SW","WSW", "W", "WNW","NW","NNW")) +
    scale_y_continuous(breaks = c(0, 5, 10, 15)) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed\n    (mph)", values = spd_colors,
      drop = FALSE) +
    theme_minimal() +
    theme_latex +
    theme(plot.background = element_rect(color = "white")) +
    theme(plot.title = element_text(vjust = -2, hjust = 0.5)) +
    theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
    theme(axis.title = element_text(size = 14)) +
    theme(axis.text.x = element_text(size = 10, face = "bold", angle = 0,
      vjust = .5, hjust = 0.5)) +
    theme(axis.ticks.y = element_blank(), # Disables default y-axis
      axis.text.y = element_blank()) +
    theme(panel.grid.major = element_line(colour = "grey90"))  +
    theme(panel.grid.minor = element_line(colour = "grey90"))  +
    theme(legend.title = element_text(size = 12)) +
    theme(legend.text = element_text(size = 10)) +
    labs(x = "Direction (compass)", y = "Wind Percent (polar coordinates)")
  gg_windrose

  ggsave(filename = windrose_filename, plot = gg_windrose,
    path = file.path(tex_dir, "Figures/Ch4/Windroses"), scale = 1, width = 6,
    height = 4, units = "in", dpi = 300)

  # Wind Area Transits ---------------------------------------------------------

  # Read in RDS
  wind_transits_sum <- readRDS(file.path(exp_dir,
      paste0("wind_transits_sum_", nest_lower, ".rds"))) %>%
    mutate(scenario = str_replace_all(scenario, "^North and South$",
      "North\nand\nSouth"))

  wind_transits_sum_ns <- bind_rows(
    wind_transits_sum %>%
      dplyr::select(scenario, area_steps_n = n_area_steps_n, behavior_line) %>%
      mutate(area = paste0(nest_title, "\nNorth Wind Area")),
    wind_transits_sum %>%
      dplyr::select(scenario, area_steps_n = s_area_steps_n, behavior_line) %>%
      mutate(area = paste0(nest_title, "\nSouth Wind Area")))

  # Transit Areas (Cruise and Flight)
  gg_transits_areas <- ggplot(wind_transits_sum_ns) +
    geom_boxplot(aes(scenario, y = area_steps_n, fill = behavior_line),
      position = "dodge") +
    facet_grid(cols = vars(area)) +
    ylim(0, NA) +
    labs(x = "Wind Farm Scenario",
      y ="Path Transits") +
    scale_fill_manual(values = cruise_flight_colors, name = "Behavior") +
    theme_latex +
    theme_transits  +
    theme(axis.text = element_text(color = "black")) +
    theme(legend.key = element_blank())
  gg_transits_areas

  ggsave(filename = paste0("Wind_Areas_", nest_str, ".svg"),
    plot = gg_transits_areas,
    path = file.path(tex_dir, "Figures/Ch4/Transits"), scale = 1,
    width = 6, height = 4, units = "in", dpi = 300)

  # Clean up objects
  rm(wind_transits_sum, wind_transits_sum_ns, gg_transits_areas)

  # Wind Turbine Transits ------------------------------------------------------

  # Read in RDS
  wind_transits_sum <- readRDS(file.path(exp_dir,
      paste0("wind_transits_sum_", nest_lower ,".rds"))) %>%
    mutate(scenario = str_replace_all(scenario, "^North and South$",
      "North\nand\nSouth"))

  wind_transits_sum_ns <- bind_rows(
    wind_transits_sum %>%
      dplyr::select(scenario, turbines_steps_n = n_turbines_steps_n,
        behavior_line) %>%
      mutate(area = paste0(nest_title, "\nNorth Wind Turbines")),
    wind_transits_sum %>%
      dplyr::select(scenario, turbines_steps_n = s_turbines_steps_n,
        behavior_line) %>%
      mutate(area = paste0(nest_title, "\nSouth Wind Turbines")))

  # Turbines (Flight)
  gg_transits_turbines <- ggplot(wind_transits_sum_ns) +
    geom_boxplot(aes(scenario, y = turbines_steps_n, fill = behavior_line),
      position = "dodge") +
    facet_grid(cols = vars(area)) +
    ylim(0, NA) +
    labs(x = "Wind Farm Scenario",
      y = "Path Transits") +
    scale_fill_manual(values = cruise_flight_colors, name = "Behavior") +
    theme_latex +
    theme_transits  +
    theme(legend.key = element_blank())
  gg_transits_turbines

  # Save SVG file
  ggsave(filename = paste0("Wind_Turbines_", nest_str, ".svg"),
    plot = gg_transits_turbines,
    path = file.path(tex_dir, "Figures/Ch4/Transits"), scale = 1,
    width = 6, height = 4, units = "in", dpi = 300)

  # Clean up objects
  rm(wind_transits_sum, wind_transits_sum_ns, gg_transits_turbines)

  # Turbine Transits Stats -----------------------------------------------------

  # Filter data
  transit_flights <- readRDS(file.path(exp_dir,
      paste0("wind_transits_sum_", nest_lower, ".rds"))) %>%
    filter(behavior_line == "Flight") %>%
    mutate(scenario = str_replace_all(scenario, "^North and South$",
      "North\nand\nSouth"))

  transit_flights_ns <- bind_rows(
    transit_flights %>%
      dplyr::select(scenario, turbines_steps_n = n_turbines_steps_n,
        behavior_line) %>%
      mutate(area = paste0(nest_title, "\nNorth Wind Turbines")),
    transit_flights %>%
      dplyr::select(scenario, turbines_steps_n = s_turbines_steps_n,
        behavior_line) %>%
      mutate(area = paste0(nest_title, "\nSouth Wind Turbines")))

  transit_flights_stats <- transit_flights_ns %>%
    group_by(area) %>%
    t_test(turbines_steps_n ~ scenario, ref.group = "Control") %>%
    adjust_pvalue(.) %>%
    add_significance("p.adj") %>%
    add_y_position(.)

  if(nest_str == "Wilson"){
    y_position <- c(16, 18.5, 21, 23, 25.5, 28)
    y_max <- 29.9
  }

  if(nest_str == "Grand_Lake"){
    y_position = c(11, 13.5, 16, 15.5, 18, 20.5)
    y_max = 22.5
  }

  gg_flights_stats <- transit_flights_ns %>%
    ggboxplot(.,
      x = "scenario", y = "turbines_steps_n",
      xlab = "Wind Farm Scenario",
      ylab = "Turbine Transits",
      label.rectangle = TRUE,
      fill = "scenario", facet.by = "area") +
    stat_pvalue_manual(transit_flights_stats, label.size = 4,
      label = "p = {p.adj}{p.adj.signif}",
      y.position = y_position,
      family = "Latin Modern Roman") +
    scale_fill_manual(values = scenarios_colors, name = "Scenarios") +
    scale_y_continuous(expand = c(0, NA), limits = c(0, y_max)) +
    theme_latex +
    theme_transits +
    theme_transits_stats +
    theme(legend.position = "none") +
    theme(legend.key = element_blank())
  gg_flights_stats

  # Save SVG
  ggsave(filename = paste0("Flights_Stats_", nest_str, ".svg"),
    plot = gg_flights_stats,
    path = file.path(tex_dir, "Figures/Ch4/Transits"), scale = 1,
    width = 6, height = 4, units = "in", dpi = 300)

  # Clean up objects
  rm(transit_flights, transit_flights_ns, transit_flights_stats,
    gg_flights_stats)

  # Predicted Collision Risk North/South ---------------------------------------

  collision_risk_sum <- readRDS(file.path(exp_dir,
      paste0("flight_collision_risk_", nest_str, ".rds"))) %>%
    mutate(scenario = str_replace_all(scenario, "^North and South$",
      "North\nand\nSouth")) %>%
    dplyr::select(exp_id, scenario, n_turbines_collision_risk_95avoid,
      s_turbines_collision_risk_95avoid, turbines_collision_risk_95avoid)

  collision_risk_sum_ns <- bind_rows(
    collision_risk_sum %>%
      filter(scenario %in% c("North", "North\nand\nSouth")) %>%
      rename(collision_risk = n_turbines_collision_risk_95avoid) %>%
      mutate(turbines = paste0(nest_title, "\nNorth Turbines")),
    collision_risk_sum %>%
      filter(scenario %in% c("South", "North\nand\nSouth")) %>%
      rename(collision_risk = s_turbines_collision_risk_95avoid) %>%
      mutate(turbines = paste0(nest_title, "\nSouth Turbines"))) %>%
    dplyr::select(exp_id, scenario, turbines, collision_risk) %>%
    mutate(scenario = ordered(scenario, levels = c("North","South",
      "North\nand\nSouth")))

  # Collisions with North turbines (only relevant for North and North/South)
  gg_turbines_ns_crm <- collision_risk_sum_ns %>%
    ggplot(.) +
    geom_boxplot(aes(x = scenario, y = collision_risk, fill = scenario)) +
    facet_grid(cols = vars(turbines), scale = "free") +
    scale_fill_manual(values = scenarios_colors, name = "Scenarios") +
    labs(x = "Wind Farm Scenario",
      y = "Collision Risk Per Eagle\nPer Breeding Season") +
    theme_latex +
    theme_transits +
    theme_transits_stats +
    theme(legend.position = "none")
  gg_turbines_ns_crm

  # Save SVG
  ggsave(filename = paste0("Turbines_NS_CRM_", nest_str, ".svg"),
    plot = gg_turbines_ns_crm,
    path = file.path(tex_dir, "Figures/Ch4/Collision_Risk"), scale = 1,
    width = 6, height = 4, units = "in", dpi = 300)

  # Clean up objects
  rm(collision_risk_sum, collision_risk_sum_ns, gg_turbines_ns_crm)

  # Predicted Collision Risk All Turbines --------------------------------------

  collision_risk_sum_all <- readRDS(file.path(exp_dir,
      paste0("flight_collision_risk_", nest_lower, ".rds"))) %>%
    mutate(scenario = str_replace_all(scenario, "^North and South$",
      "North\nand\nSouth")) %>%
    dplyr::select(exp_id, scenario,
      collision_risk = turbines_collision_risk_95avoid)

  gg_turbines_all_crm <- collision_risk_sum_all %>%
    ggplot(.) +
    geom_boxplot(aes(scenario, y = collision_risk,
      fill = scenario)) +
    scale_x_discrete(limits = c("North","South", "North\nand\nSouth")) +
    scale_fill_manual(values = scenarios_colors, name = "Scenarios") +
    labs(x = "Wind Farm Scenario") +
    labs(y = paste0("Total Predicted Collisions with Turbines\nPer Eagle ",
      "Per Breeding Season\n(95% Avoidance)")) +
    ggtitle(paste0(nest_title, " Pond Collision Risk")) +
    theme_latex +
    theme_transits +
    theme_transits_stats +
    theme(legend.position = "none")
  gg_turbines_all_crm

  ggsave(filename = paste0("Turbines_All_CRM_", nest_str, ".svg"),
    plot = gg_turbines_all_crm,
    path = file.path(tex_dir, "Figures/Ch4/Collision_Risk"), scale = 1,
    width = 6, height = 4, units = "in", dpi = 300)

  # Clean up objects
  rm(collision_risk_sum_all, gg_turbines_all_crm)

}

# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #

# Movement Step Lengths (DEPRECATED) -------------------------------------------
# Uses individual titles for each graph
baea_movements_wb <- readRDS("Data/BAEA/baea_movements_wb.rds")
move_pars <- readRDS("Output/Analysis/Movements/move_pars.rds")

vec_length <- 100
weibull_dens <- data.frame(grp=factor(), pred=numeric(), dens=numeric())
for (i in 1:nrow(move_pars)){
  pars_i <- move_pars[i,]
  grp = rep(pars_i$behavior_behavior, vec_length)
  pred = seq(pars_i$min_step, pars_i$max_step, length = vec_length)
  dens = dweibull(pred, shape=pars_i$weibull_shape, scale=pars_i$weibull_scale)
  weibull_dens <- rbind(weibull_dens, data.frame(grp, pred, dens))
}

MetersToKilometers <- function(x){
  x_out <- x/1000
  return(x_out)
}
Multiplier100 <- function(x){
  x_100 <- x*100
  x_100 <- str_pad(x_100, 5, side = "right", pad = 0)
  x_100 <- if_else(x_100 == "00000", "0", x_100)
  return(x_100)
}

#fill_color <- RColorBrewer::brewer.pal(3, "Set1")[2] #"grey80"
#line_color <- RColorBrewer::brewer.pal(3, "Set1")[1]
fill_color <- viridis(5)[2] #"grey80"
line_color <- inferno(5)[4]
#tmaptools::palette_explorer()

ind_list <- lapply(sort(unique(baea_movements_wb$behavior_behavior)),
    function(i){
  grp_i = str_replace_all(i, "->", "$\\\\rightarrow$")
  ggplot(baea_movements_wb[baea_movements_wb$behavior_behavior == i,],
    aes(x = step_length)) +
  geom_histogram(aes(y = ..density.., weight=weights), binwidth = 500,
    size = .1, boundary = 0, color = "black", fill = fill_color) +
  geom_line(data = weibull_dens[weibull_dens$grp == i, ], aes(x = pred,
    y = dens), size = .8, color = line_color) +
  xlab(NULL) + ylab(NULL) + ggtitle(TeX(grp_i)) +
  theme_minimal() + theme_latex +
  scale_y_continuous(labels = Multiplier100)  +
  scale_x_continuous(labels = MetersToKilometers)  +
  theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
  theme(plot.title = element_text(size = 8, vjust = -2, hjust = 0.5))
  # Old code for annotations of Weibull parameters:
  # annotate("text", Inf, Inf, hjust = 1.1, vjust = 1.1, size = 5,
  # label = paste0("Weibull Distribution\n", "shape = ",
  #   signif(move_pars[move_pars$behavior_behavior == i, "weibull_shape"],
  #     3), "\n", "scale = ",
  #   signif(move_pars[move_pars$behavior_behavior == i, "weibull_scale"],
  #     3)))
})
#theme_get()

layout <- '
ABC#
DEXG
HIJK
LMNO
#PQ#
'

movements_step_length_plots <- wrap_plots(A = ind_list[[1]],
  B = ind_list[[2]],
  C = ind_list[[3]],
  D = ind_list[[4]],
  E = ind_list[[5]],
  X = ind_list[[6]],
  G = ind_list[[7]],
  H = ind_list[[8]],
  I = ind_list[[9]],
  J = ind_list[[10]],
  K = ind_list[[11]],
  L = ind_list[[12]],
  M = ind_list[[13]],
  N = ind_list[[14]],
  O = ind_list[[15]],
  P = ind_list[[16]],
  Q = ind_list[[17]],
  design = layout)
movements_step_length_plots

# Save Temp (No Label) File
movements_step_length_no_label_fig_file = file.path("C:/TEMP/TEMP_Images",
  "Movements_Step_Length_NO_LABEL.png")
ggsave(filename = basename(movements_step_length_no_label_fig_file),
  plot =  movements_step_length_plots,
  path = dirname(movements_step_length_no_label_fig_file), scale = 1, width = 6,
  height = 6, units = "in", dpi = 300)

# Create Tex Strings
tex_head <- tibble(
  tex_str = c("Probability", "Step Length (km)"),
  tex_name = c("lab_density", "lab_step_length"))
tex_df <- bind_rows(
  bind_rows(tex_head) %>% mutate(title_size = 11))

# Create Tex Text Plots
for (i in seq_len(nrow(tex_df))){
  tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
  tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
  title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
  gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
    theme(plot.title = element_text(size = title_size_i))
  ggsave(file = "C:/TEMP/TEMP_Images/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("C:/TEMP/TEMP_Images/TEMP.png"))
  file.remove("C:/TEMP/TEMP_Images/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}
rm(i, tex_df, tex_name_i, tex_str_i, tex_i, tex_head)

# Image background
backgrd <- image_blank(1800, 1850, color = "white")

# Create Final Plot and Export to Dissertation
movements_step_length_fig <- image_read(movements_step_length_no_label_fig_file)
movements_step_length_labels_fig <- backgrd %>%
  image_composite(., movements_step_length_fig, offset = "+20+00") %>%
  image_composite(., image_rotate(tex_lab_density, 270), offset = "+20+700") %>%
  image_composite(., tex_lab_step_length, offset = "+800+1775")
movements_step_length_labels_fig
movements_step_length_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Movements_Step_Length.png")
if(FALSE){
  image_write(movements_step_length_labels_fig,
    path = movements_step_length_fig_file, format=".png")
}
file.remove(movements_step_length_no_label_fig_file)


# Movement Directions (DEPRECATED) ---------------------------------------------
baea_movements_vm <- readRDS("Data/BAEA/baea_movements_vm.rds")
move_pars <- readRDS("Output/Analysis/Movements/move_pars.rds")

bin_width = (2*pi)/24
breaks <- seq(0, (2*pi), by=((2*pi)/12))
labels <- c(0, "", "", expression(pi / 2), "", "",
   expression(pi), "", "", expression(1.5*pi), "", "",
   expression(2*pi))
minor_breaks <- seq(0, 2*pi, by=bin_width)
minor_labels <- c(0, "", "", expression(pi / 4), "", "", expression(pi / 2),
  "", "", expression(3/4*pi), "", "", expression(pi),
  "", "", expression(5/4*pi), "", "", expression(1.5*pi),
  "", "", expression(7/4*pi), "", "")
limits <- c(0, 2*pi)
vec_length <- 100
von_mises_dens <- data.frame(grp=factor(), pred=numeric(), dens=numeric())

for (i in 1:nrow(move_pars)){
  pars_i <- move_pars[i,]
  grp = rep(pars_i$behavior_behavior, vec_length)
  pred = seq(limits[1], limits[2], length = vec_length)
  dens = dmixedvm(pred, mu1 = pars_i$mvm_mu1, mu2 = pars_i$mvm_mu2,
    kappa1 = pars_i$mvm_kappa1, kappa2 = pars_i$mvm_kappa2,
    p = pars_i$mvm_prop)
  von_mises_dens <- rbind(von_mises_dens, data.frame(grp, pred, dens))
}

baea_movements_vm_sub <- baea_movements_vm %>%
  filter(!behavior %in% c("Perch", "Nest", "Roost"))

fill_color <- viridis(5)[2] #"grey80"
line_color <- inferno(5)[4]
#tmaptools::palette_explorer()

# Individual Step-Type Plots Using Polar Coordinates
ind_list = lapply(sort(unique(baea_movements_vm_sub$behavior_behavior)),
  function(i){
    grp_i = str_replace_all(i, "->", "$\\\\rightarrow$")
    ggplot(baea_movements_vm[baea_movements_vm$behavior_behavior == i, ],
      aes(x = turn_angle)) +
    geom_histogram(aes(y = ..density.., weight=weights), fill = fill_color,
      color = "black", boundary = 0, size = .1, binwidth = (2*pi)/24) +
    geom_line(data = von_mises_dens[von_mises_dens$grp == i, ],
      aes(x = pred, y = dens), size = .8, colour = line_color) +
    coord_polar(start = (1.5*pi), direction = -1) +
    scale_y_continuous(labels = NULL) +
    scale_x_continuous(limits = limits, labels = minor_labels,
      breaks = minor_breaks[-25], minor_breaks = minor_breaks, expand = c(0,0))+
    theme(axis.ticks = element_blank()) +
    #facet_grid(. ~ behavior_behavior) +
    theme_minimal() + theme_latex +
    theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
    theme(axis.text = element_text(size = 7)) +
    theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
    theme(plot.title = element_text(size = 8, vjust = -2, hjust = 0.5)) +
    theme(legend.position = "none") +
    theme(panel.grid.major = element_line(colour = "grey90"))  +
    theme(panel.grid.minor = element_line(colour = "grey90"))  +
    ggtitle(TeX(grp_i)) +
    labs(x = NULL, y = NULL)
})

layout <- c(
  patchwork::area(t = 1, l = 1),
  patchwork::area(t = 1, l = 2),
  patchwork::area(t = 1, l = 3),
  patchwork::area(t = 2, l = 1),
  patchwork::area(t = 2, l = 2),
  patchwork::area(t = 2, l = 3),
  patchwork::area(t = 2, l = 4)
)
#plot(layout)

movements_direction_plots <- ind_list[[1]] + ind_list[[2]] + ind_list[[3]] +
  ind_list[[4]] + ind_list[[5]] + ind_list[[6]] + ind_list[[7]] +
  plot_layout(design = layout)

# Save Temp (No Label) File
movements_direction_no_lab_fig_file = file.path("Output/Analysis/Movements",
  "Movements_Direction_NO_LABEL.png")
ggsave(filename = basename(movements_direction_no_lab_fig_file),
  plot =  movements_direction_plots,
  path = dirname(movements_direction_no_lab_fig_file), scale = 1, width = 6,
  height = 3.5, units = "in", dpi = 300)

# Create Tex Strings
tex_head <- tibble(
  tex_str = c("Probability (polar coordinates)", "Direction (radians)"),
  tex_name = c("lab_density", "lab_direction"))
tex_df <- bind_rows(
  bind_rows(tex_head) %>% mutate(title_size = 11))

# Create Tex Text Plots
for (i in seq_len(nrow(tex_df))){
  tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
  tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
  title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
  gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
    theme(plot.title = element_text(size = title_size_i))
  ggsave(file = "C:/TEMP/TEMP_Images/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("C:/TEMP/TEMP_Images/TEMP.png"))
  file.remove("C:/TEMP/TEMP_Images/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}
rm(i, tex_df, tex_name_i, tex_str_i, tex_i)

# Image background
backgrd <- image_blank(1800, 1100, color = "white")

# Create Final Plot and Export to Dissertation
movements_direction_fig <- image_read(movements_direction_no_lab_fig_file)
movements_direction_labels_fig <- backgrd %>%
  image_composite(., movements_direction_fig, offset = "+20+00") %>%
  image_composite(., image_rotate(tex_lab_density, 270), offset = "+20+250") %>%
  image_composite(., tex_lab_direction, offset = "+720+1030")
movements_direction_labels_fig
movements_direction_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Movements_Direction.png")
if(FALSE){
  image_write(movements_direction_labels_fig,
    path = movements_direction_fig_file, format=".png")
}
file.remove(file.path(movements_direction_no_lab_fig_file))


# Movement Kernels (DEPRECATED) ------------------------------------------------

move_dens <- data.frame(grp = character(), x = numeric(), y = numeric(),
  dens = numeric())
for (i in 1:nrow(move_pars)){
  move_pars_i <- move_pars[i, ]
  ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise", "Flight"),
    FALSE, TRUE)
  kernel_i <- CreateMoveKernelWeibullVonMises(
      max_r = NULL,
      cellsize = 30,
      mu1 = move_pars_i$mvm_mu1[1],
      mu2 = move_pars_i$mvm_mu2[1],
      kappa1 = move_pars_i$mvm_kappa1[1],
      kappa2 = move_pars_i$mvm_kappa2[1],
      mix = move_pars_i$mvm_prop[1],
      shape = move_pars_i$weibull_shape[1],
      scale = move_pars_i$weibull_scale[1],
      ignore_von_mises = ignore_von_mises)
  r <- (30*((nrow(kernel_i)-1)/2))+(30/2)
  kernel_raster <- raster::raster(kernel_i, xmn=-r, xmx=r, ymn=-r, ymx=r)
  print(freq(kernel_raster))
  df <- data.frame(raster::rasterToPoints(kernel_raster)) # NA cells removed.
  names(df)[3] <- "dens"
  df$behavior_behavior <- move_pars_i$behavior_behavior
  move_dens <- rbind(move_dens, df)
}

# All plots on one figure
ind_list = lapply(sort(unique(move_dens$behavior_behavior)), function(i){
  grp_i = str_replace_all(i, "->", "$\\\\rightarrow$")
  ggplot(move_dens[move_dens$behavior_behavior == i, ], aes(x = x, y = y)) +
    geom_raster(aes(fill = dens)) +
    coord_fixed(ratio = 1) +
    scale_fill_viridis_c(name = "Probability", option = "C", direction = -1,
      labels = scales::scientific) +
    scale_x_continuous(expand = c(0.005, 0.005), labels = MetersToKilometers) +
    scale_y_continuous(expand = c(0.005, 0.005), labels = MetersToKilometers) +
    theme_minimal() +
    theme_latex +
    theme(plot.margin = margin(3, 6, 3, 6, "pt")) +
    theme(axis.text = element_text(size = 7)) +
    theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
    theme(legend.text = element_text(size = 7)) +
    theme(legend.title = element_text(size = 8)) +
    theme(plot.title = element_text(size = 9, vjust = -1, hjust = 0.5)) +
    guides(fill = guide_colourbar(barwidth = .5, barheight = 3)) +
    ggtitle(TeX(grp_i)) + labs(x = NULL, y = NULL)
})

layout <- '
ABC#
DEXG
HIJK
LMNO
#PQ#
'
movements_kernel_plots <- wrap_plots(A = ind_list[[1]],
  B = ind_list[[2]], C = ind_list[[3]], D = ind_list[[4]], E = ind_list[[5]],
  X = ind_list[[6]], G = ind_list[[7]], H = ind_list[[8]], I = ind_list[[9]],
  J = ind_list[[10]], K = ind_list[[11]], L = ind_list[[12]], M =ind_list[[13]],
  N = ind_list[[14]], O = ind_list[[15]], P = ind_list[[16]], Q =ind_list[[17]],
  design = layout)

# Save Temp (No Label) File
movements_kernel_no_label_fig_file = file.path("Output/Analysis/Movements",
  "Movements_Kernel_NO_LABEL.png")
ggsave(filename = basename(movements_kernel_no_label_fig_file),
  plot =  movements_kernel_plots,
  path = dirname(movements_kernel_no_label_fig_file), scale = 1, width = 10,
  height = 7.5, units = "in", dpi = 300)

# Create Tex Strings
tex_head <- tibble(
  tex_str = c("Distance (km)", "Distance (km)"),
  tex_name = c("lab_density", "lab_kernel"))
tex_df <- bind_rows(
  bind_rows(tex_head) %>% mutate(title_size = 11))

# Create Tex Text Plots
for (i in seq_len(nrow(tex_df))){
  tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
  tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
  title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
  gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
    theme(plot.title = element_text(size = title_size_i))
  ggsave(file = "Output/Analysis/Movements/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("Output/Analysis/Movements/TEMP.png"))
  file.remove("Output/Analysis/Movements/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}
rm(i, tex_df, tex_name_i, tex_str_i, tex_i, tex_head)

# Image background
backgrd <- image_blank(2700, 2300, color = "white")

# Create Final Plot and Export to Dissertation
movements_kernel_fig <- image_read(movements_kernel_no_label_fig_file) %>%
  image_chop(., "90x0")
movements_kernel_labels_fig <- backgrd %>%
  image_composite(., movements_kernel_fig, offset = "+00+00") %>%
  image_composite(., image_rotate(tex_lab_density, 270), offset = "+25+900") %>%
  image_composite(., tex_lab_kernel, offset = "+1300+2240")
movements_kernel_labels_fig
movements_kernel_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Movements_Kernel.png")
if(FALSE){
  image_write(movements_kernel_labels_fig,
    path = movements_kernel_fig_file, format=".png")
}
file.remove(movements_kernel_no_label_fig_file)

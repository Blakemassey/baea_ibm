############################################################################# ##
#### -------------------------- CHAPTER 2 --------------------------------- ####
############################################################################# ##
# Maps and Flowcharts: Save as .svg
# Figures: Save as .pdf

# Load packages, helpers, and functions
pacman::p_load(CircStats, ggplot2, gridExtra, magick, latex2exp, patchwork,
  reshape2, tidyverse, viridis)
suppressMessages(extrafont::loadfonts(device="win"))
pacman::p_load(baear, gisr, ibmr)

pkg_dir <- "C:/Users/Blake/OneDrive/Work/R/Projects/multiscale_optim"
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"

# Theme (for LaTeX font)
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))

# Theme (for blank background)
theme_blank <- theme(legend.position = "none",
  text = element_text(family="Latin Modern Roman"),
  plot.title = element_text(size=14),
  panel.grid = element_blank(), axis.title = element_blank(),
  axis.text = element_blank(), axis.ticks = element_blank(),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA))

# Homerange Size/Terrain Metrics -----------------------------------------------
hr_metrics_org <- readRDS("Output/Analysis/Homerange/hr_all_metrics.rds")
hr_metrics <- hr_metrics_org
unique(hr_metrics$id)
sort(colnames(hr_metrics))

hr_metrics_terrain_95 <- hr_metrics_org %>%
  mutate(Developed = ud_95_developed_prop) %>%
  mutate(Forest = ud_95_forest_prop) %>%
  mutate('Open Water' = ud_95_open_water_prop) %>%
  mutate(Pasture = ud_95_pasture_prop) %>%
  mutate('Shrub Herb' = ud_95_shrub_herb_prop) %>%
  mutate('Wetland' = ud_95_wetland_prop) %>%
  mutate(Area_km = ud_95_total) %>%
  mutate(TPI = ud_95_tpi_mean) %>%
  mutate(TRI = ud_95_tri_mean) %>%
  mutate(Roughness = ud_95_roughness_mean) %>%
  dplyr::select(id, Area_km, Developed, Forest, 'Open Water', Pasture,
    'Shrub Herb', Wetland, TPI, TRI, Roughness) %>%
    melt(., id.var = "id") %>%
  mutate(ud = "95%") %>% map_if(is.factor, as.character)

hr_metrics_terrain_50 <- hr_metrics_org %>%
  mutate(Developed = ud_50_developed_prop) %>%
  mutate(Forest = ud_50_forest_prop) %>%
  mutate('Open Water' = ud_50_open_water_prop) %>%
  mutate(Pasture = ud_50_pasture_prop) %>%
  mutate('Shrub Herb' = ud_50_shrub_herb_prop) %>%
  mutate('Wetland' = ud_50_wetland_prop) %>%
  mutate(Area_km = ud_50_total) %>%
  mutate(TPI = ud_50_tpi_mean) %>%
  mutate(TRI = ud_50_tri_mean) %>%
  mutate(Roughness = ud_50_roughness_mean) %>%
  dplyr::select(id, Area_km, Developed, Forest, 'Open Water', Pasture,
    'Shrub Herb', Wetland, TPI, TRI, Roughness) %>%
    melt(., id.var = "id") %>%
  mutate(ud = "50%") %>% map_if(is.factor, as.character)

hr_metrics_terrain <- bind_rows(hr_metrics_terrain_95, hr_metrics_terrain_50)
rm(hr_metrics_org, hr_metrics_terrain_50, hr_metrics_terrain_95)

pointSize = 2; textSize = 5; spaceLegend = 1
gg_hr_size_1 <- ggplot(data = hr_metrics_terrain %>%
    filter(variable == "Area_km"),aes(group = ud, x = factor(0), y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab("Home Range Size") +
  ylab(latex2exp::TeX("Total Area ($\\km^2$)")) +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  ggtitle("") + guides(NA) + theme_latex + theme(legend.position = "none") +
  theme_minimal() +
  theme_latex +
    theme(axis.text = element_text(size = 7)) +
    theme(axis.title = element_text(size = 9)) +
    theme(plot.title = element_text(size = 11)) +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
      color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize),
      legend.text  = element_text(size = textSize),
      legend.key.size = unit(spaceLegend, "lines")) +
      theme(panel.grid.major.x = element_blank())

gg_hr_size_2 <- ggplot(data = hr_metrics_terrain %>% filter(variable == "TRI"),
    aes(group = ud, x = factor(0), y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab("Terrain Ruggedness Index (TRI)") +
  ylab(latex2exp::TeX("Index Value")) +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  ggtitle("") + guides(NA) + theme_latex + theme(legend.position = "none") +
  theme_minimal() +
  theme_latex +
    theme(axis.text = element_text(size = 7)) +
    theme(axis.title = element_text(size = 9)) +
    theme(plot.title = element_text(size = 11)) +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
      color = guide_legend(override.aes = list(size = pointSize))) +
      theme(legend.title = element_text(size = textSize),
        legend.text  = element_text(size = textSize),
        legend.key.size = unit(spaceLegend, "lines")) +
      theme(panel.grid.major.x = element_blank())

gg_hr_size_3 <- ggplot(data = hr_metrics_terrain %>% filter(variable == "TPI"),
    aes(group = ud, x = factor(0), y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab(latex2exp::TeX("Terrain Position Index (TRI)")) +
  ylab("Index Value") +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  ggtitle("") + guides(NA) + theme_latex + theme(legend.position = "none") +
  theme_minimal() +
  theme_latex +
    theme(axis.text = element_text(size = 7)) +
    theme(axis.title = element_text(size = 9)) +
    theme(plot.title = element_text(size = 11)) +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
      color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_text(size = textSize),
      legend.text  = element_text(size = textSize),
      legend.key.size = unit(spaceLegend, "lines")) +
      theme(panel.grid.major.x = element_blank())

gg_hr_size_4 <- ggplot(data = hr_metrics_terrain %>%
    filter(variable == "Roughness"), aes(group = ud, x = factor(0), y = value))+
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab(latex2exp::TeX("Roughness Index (TRI)")) +
  ylab("Index Value") +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  ggtitle("") + guides(NA) + theme_latex + theme(legend.position = "none") +
  theme_minimal() + theme_latex +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.title = element_text(size = 9)) +
  theme(plot.title = element_text(size = 11)) +
  guides(shape = guide_legend(override.aes = list(size = pointSize)),
    color = guide_legend(override.aes = list(size = pointSize))) +
  theme(legend.title = element_text(size = textSize),
    legend.text  = element_text(size = textSize),
    legend.key.size = unit(spaceLegend, "lines")) +
  theme(panel.grid.major.x = element_blank())

gg_hr_size_all <- gg_hr_size_1 + gg_hr_size_2 + gg_hr_size_3 + gg_hr_size_4

ggsave(filename = "Homerange_Size_Terrain.png", plot = gg_hr_size_all,
  path = file.path(tex_dir, "Figures/Ch2"),
  scale = 1, width = 6, height = 6, units = "in",
  dpi = 300)

# Clean up objects
rm(gg_hr_size_all, gg_hr_size_1, gg_hr_size_2, gg_hr_size_3, gg_hr_size_4)

# Homerange Cover Type Metrics -------------------------------------------------
hr_metrics_org <- readRDS("Output/Analysis/Homerange/hr_all_metrics.rds")

hr_metrics_cover_95 <- hr_metrics_org %>%
  mutate(Developed = ud_95_developed_prop) %>%
  mutate(Forest = ud_95_forest_prop) %>%
  mutate('Open Water' = ud_95_open_water_prop) %>%
  mutate(Pasture = ud_95_pasture_prop) %>%
  mutate('Shrub Herb' = ud_95_shrub_herb_prop) %>%
  mutate('Wetland' = ud_95_wetland_prop) %>%
  mutate(Area_km = ud_95_total) %>%
  dplyr::select(id, Area_km, Developed, Forest, 'Open Water', Pasture,
    'Shrub Herb', Wetland) %>%
    melt(., id.var = "id") %>%
  mutate(ud = "95%") %>% map_if(is.factor, as.character)

hr_metrics_cover_50 <- hr_metrics_org %>%
  mutate(Developed = ud_50_developed_prop) %>%
  mutate(Forest = ud_50_forest_prop) %>%
  mutate('Open Water' = ud_50_open_water_prop) %>%
  mutate(Pasture = ud_50_pasture_prop) %>%
  mutate('Shrub Herb' = ud_50_shrub_herb_prop) %>%
  mutate('Wetland' = ud_50_wetland_prop) %>%
  mutate(Area_km = ud_50_total) %>%
  mutate(TPI = ud_50_tpi_mean) %>%
  mutate(TRI = ud_50_tri_mean) %>%
  mutate(Roughness = ud_50_roughness_mean) %>%
  dplyr::select(id, Area_km, Developed, Forest, 'Open Water', Pasture,
    'Shrub Herb', Wetland) %>%
    melt(., id.var = "id") %>%
  mutate(ud = "50%") %>% map_if(is.factor, as.character)

hr_metrics_cover <- bind_rows(hr_metrics_cover_95, hr_metrics_cover_50)

pointSize = 2; textSize = 5; spaceLegend = 1
gg_hr_cover <- ggplot(data = hr_metrics_cover %>% filter(variable != "Area_km"),
    aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab("Cover Type") + ylab("Proportion of Home Range") + ggtitle("") +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  theme_minimal() +
  theme_latex +
    theme(axis.text = element_text(size = 7)) +
    theme(axis.title = element_text(size = 9)) +
    theme(plot.title = element_text(size = 11)) +
        guides(shape = guide_legend(override.aes = list(size = pointSize)),
               color = guide_legend(override.aes = list(size = pointSize))) +
        theme(legend.title = element_text(size = textSize),
              legend.text  = element_text(size = textSize),
              legend.key.size = unit(spaceLegend, "lines")) +
      theme(panel.grid.major.x = element_blank())
gg_hr_cover

ggsave(filename = "Homerange_Cover_Type.png", plot = gg_hr_cover,
  path = file.path(tex_dir, "Figures/Ch2"),
  scale = 1, width = 6, height = 4, units = "in",
  dpi = 300)

# Conspecific and Nest Distance Fits -------------------------------------------

pacman::p_load(extraDistr, VGAM)

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

text_size = 10; line_size = .8; point_size = 2; space_legend = 1

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
  scale_color_viridis_d(name = "Fitted \nDistributions", option = "D") +
  ylim(c(0, 1)) +
  scale_x_continuous(limits = c(0,10), breaks = c(0:10)) + #breaks = c(0:10),
  theme_minimal() +
  theme_latex +
  theme(axis.text = element_text(size = 9)) +
  theme(axis.title = element_text(size = 11)) +
  theme(plot.title = element_text(size = 13)) +
    guides(shape = guide_legend(override.aes = list(size = point_size)),
      color = guide_legend(override.aes = list(size = point_size))) +
    theme(legend.title = element_text(size = text_size),
      legend.text  = element_text(size = text_size),
      legend.key.size = unit(space_legend, "lines")) +
    theme(panel.grid.minor.x = element_blank())
gg_baea_dist

ggsave(filename = "Fits_Baea_Dist.png", plot = gg_baea_dist,
  path = file.path(tex_dir, "Figures/Ch2"), scale = 1, width = 6, height = 4,
  units = "in", dpi = 300)

# Behavioral Proportion Daily --------------------------------------------------

# Load data
baea_behavior <- readRDS("Data/Baea/baea_behavior.rds")
baea_behavior_simple <- baea_behavior %>%
  dplyr::select(datetime, id, time_proportion, bh_nest:behavior)

# Utility Functions
CutProportion <- function(data, breaks = breaks) {
  b <- seq(0, 1, length=2*breaks+1)
  brk <- b[0:breaks*2+1]
  k <- cut(data, breaks=brk)
}
CutProportionMid <- function(data, breaks = breaks) {
  b <- seq(0, 1, length = 2*breaks + 1)
  brk <- b[0:breaks*2 + 1]
  mid <- b[1:breaks*2]
  k <- cut(data, breaks=brk)
  mid[k]
}
Capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}
sex_names <- list('female' = "Female", 'male' = "Male")
sex_labeller <- function(variable, value){
  return(sex_names[value])
}

# Wrangle data
breaks = 20
behavior_colors <- CreateColorsByMetadata(file=
    "Data/Assets/behavior_colors.csv", metadata_id="behavior")
df <- baea_behavior
df$behavior <- factor(df$behavior)
df$bins <- CutProportion(df$time_proportion, breaks)
df$bins_mid <- factor(CutProportionMid(df$time_proportion, breaks))
melted <- reshape::melt(plyr::ddply(df, plyr::.(sex, bins_mid),
  function(x){prop.table(table(x$behavior))}))
names(melted)[names(melted) == 'variable'] <- 'behavior'
melted$bins_mid <- as.numeric(as.character(melted$bins_mid))

# Make Plot
point_size = 2; space_legend = .7
gg_behave_prop <-   ggplot(melted, aes(x = bins_mid, y = value, ymax = 1,
  fill = behavior)) +
  facet_grid(~ sex, labeller = labeller(sex = Capitalize)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = behavior_colors, name = "Behavior") +
  labs(x = "Daily Period", y = "Behavior Proportion", title = "") +
  theme_minimal() +
  theme_latex +
  theme(panel.spacing = unit(1, "lines")) +
  theme(strip.text = element_text(size = 11, vjust = 0)) +
  theme(axis.text = element_text(size = 9, color = 'black')) +
  theme(axis.text.x.bottom = element_text(angle = 45, hjust = 0.65)) +
  theme(axis.text.y.left = element_text(hjust = 0.5)) +
  theme(axis.title = element_text(size = 11)) +
  theme(plot.title = element_text(size = 13)) +
  guides(shape = guide_legend(override.aes = list(size = point_size)),
    color = guide_legend(override.aes = list(size = point_size))) +
  theme(legend.title = element_text(size = 10),
    legend.text  = element_text(size = 9, hjust = 0),
    legend.key.size = unit(space_legend, "lines")) +
  theme(panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks=seq(0, 1, .1),
    expand = expand_scale(mult = c(.01, .01))) +
  scale_y_continuous(expand = expand_scale(mult = c(.00, .01))) +
  theme(axis.ticks = element_line(color = "grey50", size = .65)) +
  theme(axis.ticks.length = unit(5, "pt"))

gg_behave_prop
theme_get()

# Save Plot
ggsave(filename = "Behavior_Proportion_Bar.png", plot = gg_behave_prop,
  path = file.path(tex_dir, "Figures/Ch2"), scale = 1, width = 6, height = 4,
  units = "in", dpi = 300)

# Transition Probabilities -----------------------------------------------------

df_trans <- readRDS(file = "Output/Analysis/Transitions/df_trans.rds") %>%
  mutate(date = as.Date(julian_date, origin=as.Date("2015-01-01")))
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

# Image background
backgrd <- image_blank(1800, 1800, color = "white")

# Metrics for figure labels

CalculateMode(df_trans %>% pull(time_proportion)) # 0.5030271
CalculateMode(df_trans %>% pull(date)) #"2015-06-02"
range(df_trans$date) #"2015-03-21" "2015-08-17"

# Plot Transitions by Date
gg_trans_date <- ggplot(
  data = df_trans %>%
    filter(
      time_proportion == CalculateMode(df_trans %>% pull(time_proportion))) %>%
    filter(time_proportion > 0 && time_proportion < 1),
    aes(date, prob)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = 'grey90', color = 'grey50') +
  geom_line() + labs(x = "Date", y = "Probability") +
  facet_grid(rows = vars(end_st), cols = vars(start_st),
    labeller = as_labeller(state_names)) +
  theme_minimal() + theme_latex +
#  scale_x_continuous(limits = c(75, 235)) +
  scale_y_continuous(limits = c(-0.001, 1.001), expand = expand_scale(add = 0))+
  theme(panel.spacing = unit(1, "lines")) +
  theme(strip.text = element_text(size = 9, vjust = 0)) +
  theme(axis.text = element_text(size = 8, color = 'black')) +
  theme(axis.text.y.left = element_text(angle = 0, hjust = .6)) +
  theme(axis.text.x.bottom = element_text(angle = 45, vjust = .7)) +
  theme(axis.title.x = element_text(size = 9, vjust = 0)) +
  theme(axis.title.y = element_text(size = 9, vjust = 0)) +
  theme(plot.margin = unit(c(.6,.75,0.2,0.2), "cm"))+
  theme(axis.ticks = element_line(color = "grey50", size = .75)) +
  theme(axis.ticks.length = unit(5, "pt"))
gg_trans_date

# Save Plot
ggsave(filename = "Trans_Probs_Date_NO_LABEL.png", plot = gg_trans_date,
  path = file.path(tex_dir, "Figures/Ch2"), scale = 1, width = 6, height = 6,
  units = "in", dpi = 300)

# Plot Transitions by Time
gg_trans_time <- ggplot(
    data = df_trans %>%
      filter(time_proportion > 0 && time_proportion < 1) %>%
      filter(julian_date == CalculateMode(df_trans %>% pull(julian_date))),
    aes(time_proportion, prob))+
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = 'grey90', color = 'grey50') +
  geom_line() +
  labs(x = "Time (Daily Proportion)", y = "Probability") +
  facet_grid(rows = vars(end_st), cols = vars(start_st),
    labeller = as_labeller(state_names)) +
  scale_x_continuous(limits = c(-0.001, 1.001), expand = expand_scale(add = 0))+
  scale_y_continuous(limits = c(-0.001, 1.001), expand = expand_scale(add = 0))+
  theme_minimal() + theme_latex +
  theme(panel.spacing = unit(1, "lines")) +
  theme(strip.text = element_text(size = 9, vjust = 0)) +
  theme(axis.title = element_text(size = 9, vjust = 0)) +
  theme(axis.text = element_text(size = 8, color = 'black')) +
  theme(axis.text.y.left = element_text(angle = 0, hjust = .6)) +
  theme(axis.text.x.bottom = element_text(angle = 45, vjust = .7)) +
  theme(plot.margin = unit(c(.6,.75,0.2,0.2), "cm")) +
  theme(axis.ticks = element_line(color = "grey50", size = .5)) +
  theme(axis.ticks.length = unit(5, "pt"))
gg_trans_time

# Save Plot
ggsave(filename = "Trans_Probs_Time_NO_LABEL.png",
  path = file.path(tex_dir, "Figures/Ch2"), scale = 1, width = 6, height = 6,
  units = "in", dpi = 300)
#theme_get()

# Composite Graph
plot_trans_date <- image_read(file.path(tex_dir, "Figures/Ch2",
  "Trans_Probs_Date_NO_LABEL.png"))
trans_prob_date_fig <- backgrd %>%
  image_composite(., plot_trans_date, offset = "+0+0") %>%
  image_composite(., tex_start, offset = "+775+35") %>%
  image_composite(., image_rotate(tex_end, 90), offset = "+1720+815")
trans_prob_date_fig

plot_trans_time <- image_read(file.path(tex_dir, "Figures/Ch2",
  "Trans_Probs_Time_NO_LABEL.png"))
trans_prob_time_fig <- backgrd %>%
  image_composite(., plot_trans_time, offset = "+0+0") %>%
  image_composite(., tex_start, offset = "+775+35") %>%
  image_composite(., image_rotate(tex_end, 90), offset = "+1720+815")
trans_prob_time_fig

# Export to Dissertation
trans_prob_time_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Trans_Prob_Time.png")
image_write(trans_prob_time_fig, path = trans_prob_time_fig_file, format=".png")
file.remove(file.path(tex_dir, "Figures/Ch2",
   "Trans_Probs_Time_NO_LABEL.png"))

trans_prob_date_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Trans_Prob_Date.png")
image_write(trans_prob_date_fig, path = trans_prob_date_fig_file, format=".png")
file.remove(file.path(tex_dir, "Figures/Ch2",
  "Trans_Probs_Date_NO_LABEL.png"))

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
movements_step_length_no_label_fig_file = file.path("Output/Analysis/Movements",
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
  ggsave(file = "Output/Analysis/Movements/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("Output/Analysis/Movements/TEMP.png"))
  file.remove("Output/Analysis/Movements/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}
rm(i, tex_df, tex_name_i, tex_str_i, tex_i, tex_head)

# Image background
backgrd <- image_blank(1800, 1800, color = "white")

# Create Final Plot and Export to Dissertation
movements_step_length_fig <- image_read(movements_step_length_no_label_fig_file)
movements_step_length_labels_fig <- backgrd %>%
  image_composite(., movements_step_length_fig, offset = "+20+00") %>%
  image_composite(., image_rotate(tex_lab_density, 270), offset = "+20+700") %>%
  image_composite(., tex_lab_step_length, offset = "+800+1745")
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
    theme_minimal() + theme_latex +
    theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
    theme(axis.text = element_text(size = 7)) +
    theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
    theme(plot.title = element_text(size = 8, vjust = -2, hjust = 0.5)) +
    theme(legend.position="none") +
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
  ggsave(file = "Output/Analysis/Movements/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("Output/Analysis/Movements/TEMP.png"))
  file.remove("Output/Analysis/Movements/TEMP.png")
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
image_write(movements_direction_labels_fig,
  path = movements_direction_fig_file, format=".png")
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
image_write(movements_kernel_labels_fig,
  path = movements_kernel_fig_file, format=".png")
file.remove(movements_kernel_no_label_fig_file)


############################################################################# ##
#### -------------------------- CHAPTER 3 --------------------------------- ####
############################################################################# ##

# Plot ConNestDist Scale Logistic Function -------------------------------------
con_nest_pars <- readRDS("Output/Analysis/Territorial/con_nest_pars.rds")

shape <- con_nest_pars$gamma$shape
rate <- con_nest_pars$gamma$rate

x_min <- con_nest_pars$rescale$x_min
y_min <- con_nest_pars$rescale$y_min
x_min_new <- con_nest_pars$rescale$x_min_new
y_min_new <- con_nest_pars$rescale$y_min_new
y_diff_new <- y_max_new - y_min_new

# Plot the Cumulative Distribution Function of the Gamma distribution
x = seq(0, 30, .5)
y_pgamma <- pgamma(x, shape=shape, rate=rate)
y_rescale <- y_min_new + (((y_pgamma-y_min)/(y_max-y_min)) * (y_diff_new))
df_rescale <- as.data.frame(cbind(x, y_pgamma, y_rescale))
text_size = 10; line_size = .8; point_size = 2; space_legend = 1

gg_connest_pgamma <- ggplot(df_rescale, aes(x, y_pgamma)) +
  geom_line(colour = "slateblue4", size = 1.5) +
  theme(legend.position = "none") +
  theme_minimal() +
  theme(text = element_text(family = "Latin Modern Roman")) +
  theme(line = element_line(size = line_size)) +
  theme(axis.text = element_text(size = 9)) +
  theme(axis.title = element_text(size = 11)) +
    guides(shape = guide_legend(override.aes = list(size = point_size)),
      color = guide_legend(override.aes = list(size = point_size))) +
    theme(legend.title = element_text(size = text_size),
      legend.text  = element_text(size = text_size),
      legend.key.size = unit(space_legend, "lines")) +
    theme(panel.grid.minor.x = element_blank()) +
  labs(x = 'Conspecific and Nest Distance Value (km)', y =
      'Cumulative Distribution Function (Probability)', title = NULL)
gg_connest_pgamma
ggsave(filename = "ConNest_PGamma.png", plot = gg_connest_pgamma,
  path = file.path(tex_dir, "Figures/Ch3"), scale = 1, width = 6, height = 4,
  units = "in", dpi = 300)

gg_connest_rescale <- ggplot(df_rescale, aes(x, y_rescale)) +
  geom_line(colour = "slateblue4", size = 1.5) +
  theme(legend.position = "none") +
  theme_minimal() +
  theme(text = element_text(family = "Latin Modern Roman")) +
  theme(line = element_line(size = line_size)) +
  theme(axis.text = element_text(size = 9)) +
  theme(axis.title = element_text(size = 11)) +
    guides(shape = guide_legend(override.aes = list(size = point_size)),
      color = guide_legend(override.aes = list(size = point_size))) +
    theme(legend.title = element_text(size = text_size),
      legend.text  = element_text(size = text_size),
      legend.key.size = unit(space_legend, "lines")) +
    theme(panel.grid.minor.x = element_blank()) +
  labs(x = 'Conspecific and Nest Distance Value (km)', y =
      'IBM Logistic Scale Parameter', title = NULL)
gg_connest_rescale
ggsave(filename = "ConNest_Rescale.png", plot = gg_connest_rescale,
  path = file.path(tex_dir, "Figures/Ch3"), scale = 1, width = 6, height = 4,
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
      filter(x == 20) %>% pull(y_rescale)) %>%
  mutate(y_01 = ifelse(x < -1, NA, y_01)) %>%
  mutate(y_02 = ifelse(x < -2, NA, y_02)) %>%
  mutate(y_03 = ifelse(x < -3, NA, y_03)) %>%
  mutate(y_04 = ifelse(x < -4, NA, y_04)) %>%
  mutate(y_05 = ifelse(x < -5, NA, y_05))
)

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
  ylim(c(0, 1)) +
  scale_x_continuous(limits = c(-5,5), breaks = -5:5) + #breaks = c(0:10),
  theme_minimal() +
  theme(text = element_text(family = "Latin Modern Roman")) +
  theme(line = element_line(size = line_size)) +
  theme(axis.text = element_text(size = 9)) +
  theme(axis.title = element_text(size = 11)) +
  theme(plot.title = element_text(size = 13, hjust = 0.5)) +
    guides(shape = guide_legend(override.aes = list(size = point_size)),
      color = guide_legend(override.aes = list(size = point_size))) +
    theme(legend.title = element_text(size = text_size),
      legend.text  = element_text(size = text_size),
      legend.key.size = unit(space_legend, "lines")) +
    theme(panel.grid.minor.x = element_blank())
gg_connest_logistic
ggsave(filename = "ConNest_Logistic.png", plot = gg_connest_logistic,
  path = file.path(tex_dir, "Figures/Ch3"), scale = 1, width = 6, height = 4,
  units = "in", dpi = 300)

# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #


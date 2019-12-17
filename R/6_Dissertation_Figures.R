############################################################################# ##
#### -------------------------- CHAPTER 2 --------------------------------- ####
############################################################################# ##

# Load packages, helpers, and functions
pacman::p_load(tidyverse, reshape2, ggplot2, gridExtra, patchwork)
suppressMessages(extrafont::loadfonts(device="win"))
pacman::p_load(baear, gisr, ibmr)

pkg_dir <- "C:/Users/Blake/OneDrive/Work/R/Projects/multiscale_optim"
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"

# Theme (for LaTeX font)
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))

# Homerange Size/Terrain Metrics -----------------------------------------------
hr_metrics_org <- readRDS("Output/Analysis/Homerange/hr_all_metrics.rds")

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


############################################################################# ##
#### -------------------------- CHAPTER 3 --------------------------------- ####
############################################################################# ##




# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #

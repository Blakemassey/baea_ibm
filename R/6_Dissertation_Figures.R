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

# BAEA Conspecific and Nest Distance Fits --------------------------------------

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

gg_behave_prop <- PlotBehaviorProportionBar(baea_behavior, title = "") +
  theme(text = element_text(family = "Latin Modern Roman"))

ggsave(filename = "Proportion_Bar.svg", plot = gg_behave_prop,
  path = file.path(tex_dir, "Figures/Ch2"), scale = 1, width = 6, height = 5,
  units = "in", dpi = 300)

############################################################################# ##
#### -------------------------- CHAPTER 3 --------------------------------- ####
############################################################################# ##




# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #

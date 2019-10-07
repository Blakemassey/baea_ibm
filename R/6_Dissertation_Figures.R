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

# HOMERANGE METRICS ------------------------------------------------------------
homerange_metrics <- readRDS("Output/Analysis/Homerange/homerange_metrics.rds")

unique(homerange_metrics$id)
sort(colnames(homerange_metrics))

homerange_metrics_95 <- homerange_metrics %>%
  mutate(Developed = ud_95_developed_prop) %>%
  mutate(Forest = ud_95_forest_prop) %>%
  mutate('Open Water' = ud_95_open_water_prop) %>%
  mutate(Pasture = ud_95_pasture_prob) %>%
  mutate('Shrub Herb' = ud_95_shrub_herb_prop) %>%
  mutate('Wetland' = ud_95_wetland_prop) %>%
  mutate(Area_km = ud_95_total) %>%
  dplyr::select(id, Area_km, Developed, Forest, 'Open Water', Pasture,
    'Shrub Herb', Wetland) %>%
    melt(., id.var = "id") %>%
  mutate(ud = "95%") %>% map_if(is.factor, as.character)

homerange_metrics_50 <- homerange_metrics %>%
  mutate(Developed = ud_50_developed_prop) %>%
  mutate(Forest = ud_50_forest_prop) %>%
  mutate('Open Water' = ud_50_open_water_prop) %>%
  mutate(Pasture = ud_50_pasture_prop) %>%
  mutate('Shrub Herb' = ud_50_shrub_herb_prop) %>%
  mutate('Wetland' = ud_50_wetland_prop) %>%
  mutate(Area_km = ud_50_total) %>%
  dplyr::select(id, Area_km, Developed, Forest, 'Open Water', Pasture,
    'Shrub Herb', Wetland) %>%
    melt(., id.var = "id") %>%
  mutate(ud = "50%") %>% map_if(is.factor, as.character)

homerange_metrics_all <- bind_rows(homerange_metrics_95, homerange_metrics_50)

gg1 <- ggplot(data = homerange_metrics_all %>% filter(variable == "Area_km"),
    aes(group = ud, x = factor(0), y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab("Total Size") +
  ylab(latex2exp::TeX("Home Range Area ($\\km^2$)")) +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  ggtitle("") + guides(NA) + theme_latex + theme(legend.position = "none") +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    theme(axis.text = element_text(size = 9)) +
    theme(axis.title = element_text(size = 9)) +
    theme(plot.title = element_text(size = 11))


pointSize = 2; textSize = 5; spaceLegend = 1
gg2 <- ggplot(data = homerange_metrics_all %>% filter(variable != "Area_km"),
    aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab("Cover Type") + ylab("Proportion of Home Range") + ggtitle("") +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  theme_latex +
    theme(axis.text = element_text(size = 7)) +
    theme(axis.title = element_text(size = 9)) +
    theme(plot.title = element_text(size = 11)) +
        guides(shape = guide_legend(override.aes = list(size = pointSize)),
               color = guide_legend(override.aes = list(size = pointSize))) +
        theme(legend.title = element_text(size = textSize),
              legend.text  = element_text(size = textSize),
              legend.key.size = unit(spaceLegend, "lines"))

gg_hr_size_type <- gg1 + gg2 + plot_layout(ncol=2,widths=c(1,3)) #pkg patchwork
gg_hr_size_type

ggsave(filename = "Size_And_Cover_Type.png", plot = gg_hr_size_type,
  path = file.path(tex_dir, "Figures/Ch2"),
  scale = 1, width = 6, height = 3, units = "in",
  dpi = 300)

# BEHAVIORAL PROPORTION OVER DAILY PERIOD --------------------------------------

# Load data
baea_behavior <- readRDS("Data/Baea/baea_behavior.rds")

gg_behave_prop <- PlotBehaviorProportionBar(baea_behavior, title = "") +
  theme(text = element_text(family = "Latin Modern Roman"))

ggsave(filename = "Proportion_Bar.svg", plot = gg_behave_prop,
  path = file.path(tex_dir, "Figures/Ch2"),
  scale = 1, width = 6, height = 5, units = "in",
  dpi = 300)
#SaveGGPlot("Products/Graphs/Behavior/Proportion_Bar.svg", bg = "transparent")


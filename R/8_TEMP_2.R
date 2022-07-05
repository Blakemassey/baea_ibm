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
    filter(variable == "Area_km"), aes(group = ud, x = factor(0), y = value)) +
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

gg_hr_size_all

ggsave(filename = "Homerange_Size_Terrain.png", plot = gg_hr_size_all,
  path = file.path(tex_dir, "Figures/Ch2"),
  scale = 1, width = 6, height = 6, units = "in",
  dpi = 300)

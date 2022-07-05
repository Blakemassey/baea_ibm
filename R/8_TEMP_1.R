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
  xlab("Home Range Size") +
  ylab(latex2exp::TeX("Total Area ($\\km^2$)")) +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank())

gg_terrain_2 <- ggplot(data = hr_metrics_terrain %>% filter(variable == "TRI"),
    aes(group = ud, x = factor(0), y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab("Terrain Ruggedness Index (TRI)") +
  ylab(latex2exp::TeX("Index Value")) +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank())

gg_terrain_3 <- ggplot(data = hr_metrics_terrain %>% filter(variable == "TPI"),
    aes(group = ud, x = factor(0), y = value)) +
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab(latex2exp::TeX("Terrain Position Index")) +
  ylab("Index Value") +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank())

gg_terrain_4 <- ggplot(data = hr_metrics_terrain %>%
    filter(variable == "Roughness"), aes(group = ud, x = factor(0), y = value))+
  geom_boxplot(aes(fill = as.factor(ud))) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  xlab(latex2exp::TeX("Roughness")) +
  ylab("Index Value") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
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

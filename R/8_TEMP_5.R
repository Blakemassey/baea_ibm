
# Nest Collision Risk Maps ---------------------------------------------------

n_turbines_scenario_north <- exp_flight_collision_risk %>%
  filter(scenario == "North") %>%
  dplyr::select(exp_id, scenario, n_turbine_tally) %>%
  unnest(n_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(intersects_prop = intersects_n/sum(intersects_n)) %>%
  mutate(id = paste0("N-", str_pad(turbine, width = 2, side = "left",
    pad = "0"))) %>%
  dplyr::select(scenario, id, intersects_n, intersects_prop) %>%
  left_join(nest_wt_n_buff, .) %>%
  mutate(scenario = "North",
    intersects_n = replace_na(intersects_n, 0),
    intersects_prop = replace_na(intersects_prop, 0))

s_turbines_scenario_south <- exp_flight_collision_risk %>%
  filter(scenario == "South") %>%
  dplyr::select(exp_id, scenario, s_turbine_tally) %>%
  unnest(s_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(intersects_prop = intersects_n/sum(intersects_n)) %>%
  mutate(id = paste0("S-", str_pad(turbine, width = 2, side = "left",
    pad = "0"))) %>%
  dplyr::select(scenario, id, intersects_n, intersects_prop) %>%
  left_join(nest_wt_s_buff, .) %>%
  mutate(scenario = "South",
    intersects_n = replace_na(intersects_n, 0),
    intersects_prop = replace_na(intersects_prop, 0))

n_turbines_scenario_northsouth <- exp_flight_collision_risk %>%
  filter(scenario == "North and South") %>%
  dplyr::select(exp_id, scenario, n_turbine_tally) %>%
  unnest(n_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(id = paste0("N-", str_pad(turbine, width = 2, side = "left",
    pad = "0"))) %>%
  dplyr::select(scenario, id, intersects_n) %>%
  left_join(nest_wt_n_buff, .) %>%
  mutate(scenario = "North and South",
    intersects_n = replace_na(intersects_n, 0))

s_turbines_scenario_northsouth <- exp_flight_collision_risk %>%
  filter(scenario == "North and South") %>%
  dplyr::select(exp_id, scenario, s_turbine_tally) %>%
  unnest(s_turbine_tally) %>%
  group_by(turbine) %>%
  summarize(scenario = unique(scenario),
    intersects_n = sum(n)) %>%
  ungroup(.) %>%
  mutate(id = paste0("S-", str_pad(turbine, width = 2, side = "left",
    pad = "0"))) %>%
  dplyr::select(scenario, id, intersects_n) %>%
  left_join(nest_wt_s_buff, .) %>%
  mutate(scenario = "North and South",
    intersects_n = replace_na(intersects_n, 0))

ns_turbines_scenario_northsouth <- bind_rows(n_turbines_scenario_northsouth,
  s_turbines_scenario_northsouth) %>%
  mutate(intersects_prop = intersects_n/sum(intersects_n))


# Nest Overview Map --

# Nest overview map
nest <- readRDS(file.path(wind_input_dir, paste0(nest_lower, "_nest.rds")))
nest_map_center <- readRDS(file.path(wind_input_dir,
  paste0(nest_lower, "_map_center.rds")))

if(nest_str == "Wilson"){
  sfc <- st_sfc(st_point(c(st_coordinates(nest_map_center)[1],
    st_coordinates(nest_map_center)[2] - 500))) # 83000
  st_geometry(nest_map_center) <- sfc
  st_crs(nest_map_center) <- 32619
}

nest_bb_sfc <- st_buffer(nest_map_center, 8000) %>% bb(.) %>% st_as_sfc(.)

nest_overview_center <- nest

if(nest_str == "Wilson"){
  sfc <- st_sfc(st_point(c(st_coordinates(nest)[1] + 10000,
    st_coordinates(nest)[2] - 78000))) # 83000
}
if(nest_str == "Grand_Lake"){
  sfc <- st_sfc(st_point(c(st_coordinates(nest)[1] - 55000,
    st_coordinates(nest)[2] - 45000))) #30000
}

st_geometry(nest_overview_center) <- sfc
st_crs(nest_overview_center) <- 32619

nest_overview_buff <- st_buffer(nest_overview_center, 110000) %>% bb(.)
nest_overview_bb <- bb_poly(bb(nest_overview_buff, ext = 1))

# Nest basemap
nest_natgeo_osm <- maptiles::get_tiles(x = nest_bb_sfc,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 12, forceDownload = TRUE)

nest_overview_bb_osm <- maptiles::get_tiles(x = nest_overview_bb,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 6, forceDownload = TRUE)

nest_overview <-
  tm_layout(asp = 1,
    inner.margins = -.02,
    fontfamily = "Latin Modern Roman",
    frame.lwd = 3
    ) +
  tm_shape(nest_overview_bb_osm, is.master = TRUE) +
    tm_rgb() +
  tm_shape(nest_bb_sfc) +
    tm_borders(col = "red", lwd = 3) +
  tm_scale_bar(text.size = 1.35, breaks = c(0, 50, 100),
    position = c(.275, -0))
nest_overview

tmap_save(tm = nest_overview, filename = file.path(tex_dir,
  "Figures/Ch4", "Maps_Turbine_Transits", nest_title, "Transits_Overview.svg"),
  width = 2.5, height = 2.5, units = "in")

# Nest Map Template --

tmap_nest <-
  tm_layout(fontfamily = "Latin Modern Roman",
    asp = 1,
    frame.lwd = 3,
    legend.position = c(.693, .0015),
    legend.bg.color = "grey",
    legend.frame = "black",
    legend.text.size = 1.4,
    legend.title.size = 1.75) +
  tm_shape(nest_bb_sfc, is.master = TRUE, ext = .8) + #.935
    tm_borders(col = "red") +
  tm_shape(nest_natgeo_osm, raster.downsample = FALSE) +
    tm_rgb() +
  tm_shape(nest) +
    tm_bubbles(col = nest_color, border.lwd = 1,  size = .6, #.4
      border.col = "black") +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    text.size = 1.5, #.8
    position = c(.8, .8)) +
  tm_scale_bar(text.size = 1.75,
    breaks = c(0, 2, 4), position = c(.05, .01)) +
  tm_layout()
tmap_nest

# Nest North Turbines Transits Maps ------------------------------------------

tmap_nest_n_turbines_transits <-
  tmap_nest +
  tm_shape(n_turbines_scenario_north) +
    tm_bubbles(col = "intersects_prop", border.lwd = 1,  size = .5, #.25
      palette = "Purples", legend.col.reverse = TRUE,
      border.col = "black", title.col = "     Transits\n   Proportion") +
  tm_credits("North", size = 2.25, position = c(.015, .885))
tmap_nest_n_turbines_transits

tmap_save(tm = tmap_nest_n_turbines_transits, filename = file.path(tex_dir,
  "Figures/Ch4", "Maps_Turbine_Transits", nest_title, "Transits_North.svg"),
  height = 6, width = 6)

# Nest South Turbines Transits Maps --------------------------------------------

tmap_nest_s_turbines_transits <-
  tmap_nest +
  tm_shape(s_turbines_scenario_south) +
    tm_bubbles(col = "intersects_prop", border.lwd = 1,  size = .5,
      palette = "Purples", legend.col.reverse = TRUE,
      border.col = "black", title.col = "     Transits\n   Proportion") +
  tm_credits("South", size = 2.25, position = c(.015, .885))
tmap_nest_s_turbines_transits

tmap_save(tm = tmap_nest_s_turbines_transits, filename = file.path(tex_dir,
  "Figures/Ch4", "Maps_Turbine_Transits", nest_title, "Transits_South.svg"),
  height = 6, width = 6)

# Nest All Turbines Transits Maps ----------------------------------------------

tmap_nest_ns_turbines_transits <-
  tmap_nest +
  tm_shape(ns_turbines_scenario_northsouth) +
    tm_bubbles(col = "intersects_prop", border.lwd = 1,  size = .5,
      palette = "Purples", legend.col.reverse = TRUE,
      border.col = "black", title.col = "     Transits\n   Proportion") +
  tm_credits("North and South", size = 2.25, position = c(.015, .885))
tmap_nest_ns_turbines_transits

tmap_save(tm = tmap_nest_ns_turbines_transits, filename = file.path(tex_dir,
  "Figures/Ch4", "Maps_Turbine_Transits", nest_title, "Transits_NorthSouth.svg"),
  height = 6, width = 6)

n_map_img <- file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_North.svg") %>%
  image_read(.) %>%
  image_trim(.)

s_map_img <- file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_South.svg") %>%
  image_read(.) %>%
  image_trim(.)

ns_map_img <- file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_NorthSouth.svg") %>%
  image_read(.) %>%
  image_trim(.)

overview_map_img <- file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_Overview.svg") %>%
  image_read(.) %>%
  image_trim(.)

backgrd <- image_blank(height = 1128, width = 1128, color = "white")

transits_fig <- backgrd %>%
  image_composite(., n_map_img, offset = "+0+0") %>%
  image_composite(., s_map_img, offset = "+574+0") %>%
  image_composite(., ns_map_img, offset = "+287+574") %>%
  image_composite(., overview_map_img, offset = "+880+875")
transits_fig

# Export
maps_fig_file = file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
  nest_title, "Transits_Combined.png")
image_write(transits_fig, path = maps_fig_file, format = "png")

maps_fig_file = file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
  nest_title, "Transits_Combined.svg")
image_write(transits_fig, path = maps_fig_file, format = "svg",
  flatten = TRUE)

# Clean up files
file.remove(file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_North.svg"))
file.remove(file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_South.svg"))
file.remove(file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_NorthSouth.svg"))
file.remove(file.path(tex_dir, "Figures/Ch4", "Maps_Turbine_Transits",
    nest_title, "Transits_Overview.svg"))

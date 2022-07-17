# BAEA/Sim Step Density Maps ---------------------------------------------------

# Import files
me_turbines_buff <- readRDS(me_turbines_buff_file)
baea_hr <- readRDS(baea_hr_file)
ridge_poly <- read_sf(ridge_poly_file)
nests_sim <- nests_study %>% slice(c(2, 4, 7, 13)) %>% st_transform(wgs84n19)

maine <- read_sf(maine_file) %>%
  st_transform(., crs = wgs84) %>%
  mutate(state = "Maine")  %>%
  dplyr::select(state)

# Use "Tmap_baselayers.R" script to get other baselayers
maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1, width = 2))
maine_bb <- bb_poly(bb(maine_bb_sf, ext = 1.15))
maine_om <- read_osm(maine_bb, zoom = 5, minNumTiles = 9, type = om_nat_geo)

for (i in seq_len(length(sims))){
  sim_i <- sims[i]
  sim_step_data_i <- file.path(paste0(sim_id_dir, sim_i),
    sim_step_data_dir,
    paste0(sim_id, "-", sim_i, "_01_step_data.rds")) %>%
    readRDS(.)
  if(i == 1){
    sim_step_data <- sim_step_data_i
  } else {
    sim_step_data <- bind_rows(sim_step_data, sim_step_data_i)
  }
}

for (j in unique(sim_step_data$baea_id)){
  sim_step_data_j <- sim_step_data %>%
    filter(baea_id == j) %>%
    dplyr::select(id, baea_id, baea_year, step_data_matched) %>%
    unnest(., cols = step_data_matched) %>%
    as_tibble(.) %>%
    dplyr::select(sim = id, x, y) %>%
    st_as_sf(., coords = c("x", "y"), crs = 32619,
      agr = "constant")

  baea_year <- sim_step_data %>%
    filter(baea_id == j) %>%
    pull(baea_year) %>% unique(.)
  baea_step_id <- baea_hr %>% filter(id == j) %>%
    arrange(datetime) %>%
    filter(year(date) == baea_year) %>%
    as_tibble(.) %>%
    dplyr::select(baea = id, long_utm, lat_utm) %>%
    st_as_sf(., coords = c("long_utm", "lat_utm"), crs = 32619,
      agr = "constant")

  baea_id_bb1_sfc <- st_as_sfc(bb(baea_step_id, relative = TRUE, height = 1,
      width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))

  # Get sim map bb (for final map extent)
  sim_j_bb1_sfc <- sim_step_data_j %>%
    st_as_sf(., coords = c("x", "y"), crs = wgs84n19, agr = "constant") %>%
    bb(., relative = TRUE, height = 1, width = 1) %>%
    st_as_sfc(.) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))

  # Get combined bb
  combined_bb1_sfc <- st_union(sim_j_bb1_sfc, baea_id_bb1_sfc) %>%
    st_transform(., crs = crs(base))

  # Crop base
  base_clip_aggregated <- base %>%
    crop(., bb(combined_bb1_sfc, ext = 1)) %>%
    aggregate(., fact = 20)

  # Rasterize points
  sim_data_j_raster <- sim_step_data_j %>%
    as_Spatial(.) %>%
    rasterize(., base_clip_aggregated, fun = "count") %>%
    subset(., subset = "sim")
  baea_data_id_raster <- baea_step_id %>%
    as_Spatial(.) %>%
    rasterize(., base_clip_aggregated, fun = "count") %>%
    subset(., subset = "baea")

  sim_data_j_raster_standardized <- sim_data_j_raster /
    cellStats(sim_data_j_raster, stat = "sum")
  baea_data_id_raster_standardized <- baea_data_id_raster /
    cellStats(baea_data_id_raster, stat = "sum")

  # Clip wind turbines
  me_turbines_buff_clip <- me_turbines_buff %>%
    st_transform(., crs = st_crs(combined_bb1_sfc)) %>%
    st_crop(., bb(combined_bb1_sfc, ext = 2))
  # Clip ridgeline polygon
  ridge_poly_clip <- ridge_poly %>%
    st_transform(., st_crs(combined_bb1_sfc)) %>%
    st_crop(., bb(combined_bb1_sfc, ext = 2))
  # Get combined map scalebar distances
  combined_dist_sf <- st_as_sfc(bb(combined_bb1_sfc, relative = TRUE,
    height = 1, width = 1))
  combined_x_dist <- as.numeric(approx_distances(bb(combined_dist_sf,
    ext = 1.15))[1])/1000/5
  combined_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0,
    combined_x_dist), scales::pretty_breaks(2))[1]))

  # Get osm baselayer for sim_step_sf_k and baea_step_sf_id
  sim_k_bb2_sfc <- st_as_sfc(bb(sim_j_bb1_sfc, relative = TRUE,
    height = 2, width = 2)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  baea_id_bb2_sfc <- st_as_sfc(bb(baea_step_id, relative = TRUE,
    height = 2, width = 2)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  combined_om <- st_union(sim_k_bb2_sfc, baea_id_bb2_sfc) %>%
    read_osm(., minNumTiles = 21, zoom = 10, type = om_nat_geo)
    # may need to add and adjust 'zoom' arg

  # Inset map
  combined_bb = CreateMapExtentBB(combined_bb1_sfc, asp = 1, ext = 1.15)
  maine_overview <-
    tm_layout(frame.lwd = .4) +
    tm_shape(maine_om, raster.downsample = FALSE) +
    tm_rgb() +
    tm_shape(maine) + # setting this as master sets lat/long
    tm_borders(col = "black") +
    tm_shape(combined_bb) +
    tm_borders(col = "red")

  density_base_map <-
    tm_shape(combined_om, raster.downsample = FALSE) +
    tm_rgb() +
    tm_layout(asp = 1,
      fontfamily = "Latin Modern Roman",
      main.title = NULL) +
    tm_legend(show = FALSE) +
    tm_scale_bar(text.size = 1.5,
      breaks = combined_x_breaks,
      position = c(.05, .01)) +
    tm_compass(type = "4star",
      show.labels = 1,
      size = 2.5,
      text.size = 1.25,
      position = c(.84, .82)) +
    tm_xlab("") + tm_ylab("")

  density_ridges_map <-
    tm_shape(nests_sim) +
    tm_symbols(shape = 21, border.col = "black", border.lwd = 1,
      col = nest_color, size = .25) +
    tm_shape(ridge_poly_clip) +
    tm_borders(col = "wheat4", alpha = .6) +
    tm_shape(ridge_poly_clip) +
    tm_fill("forestgreen", alpha = .4) +
    tm_shape(me_turbines_buff_clip, title = "Wind Turbines") +
    tm_polygons(col = turbine_color,
      border.col = "black",  lwd = 1)

  raster_breaks <- pretty(c(0,
    max(cellStats(baea_data_id_raster_standardized, "max"),
    cellStats(sim_data_j_raster_standardized, "max"))))

  baea_id_map <- density_base_map +
    tm_shape(baea_data_id_raster_standardized, raster.downsample = FALSE,
      bbox = bb(combined_bb1_sfc, ext = 1.25), is.master = TRUE) +
    tm_raster(alpha = .9, palette = "viridis", legend.reverse = TRUE,
      title = "DEFAULT", style = "cont", breaks = raster_breaks) +
    density_ridges_map +
    tm_layout(main.title = paste0(j, "\nEmpirical Data"),
      main.title.size = 1.85,
      main.title.position = "center")

  sim_j_map <- density_base_map +
    tm_shape(sim_data_j_raster_standardized,
      bbox = bb(combined_bb1_sfc, ext = 1.25), is.master = TRUE) +
    tm_raster(alpha = .9, palette = "viridis", legend.reverse = TRUE,
      title = "DEFAULT", style = "cont", breaks = raster_breaks) +
    density_ridges_map +
    tm_layout(main.title = paste0(j, "\nSimulation Data (n = 10)"),
      main.title.size = 1.85,
      main.title.position = "center")

  legend_only <- tm_shape(sim_data_j_raster_standardized,
      bbox = bb(combined_bb1_sfc, ext = 1.25), is.master = TRUE) +
    tm_raster(alpha = .9, palette = "viridis", legend.reverse = TRUE,
      title = "Habitat Use\nProportion", style = "cont",
      breaks = raster_breaks) +
    tm_layout(legend.only = TRUE,
      legend.text.size = .75,
      legend.title.size = 1,
      fontfamily = "Latin Modern Roman")

  # Export to LaTeX Folder
  baea_id_map_file <- file.path("C:/TEMP/TEMP_Images",
    paste0("BAEA_", j, ".png"))
  tmap_save(tm = baea_id_map, filename = baea_id_map_file,
    unit = "in", dpi = 300, height = 6, width = 6)

  sim_j_map_file <- file.path("C:/TEMP/TEMP_Images",
    paste0("SIM_", j, ".png"))
  tmap_save(tm = sim_j_map, filename = sim_j_map_file,
    unit = "in", dpi = 300, height = 6, width = 6)

  legend_file <- file.path("C:/TEMP/TEMP_Images",
    paste0("Legend_", j, ".png"))
  tmap_save(tm = legend_only, filename = legend_file, unit = "in", dpi = 300,
    height = 3, width = 1.5)

  overview_file <- file.path("C:/TEMP/TEMP_Images",
    paste0("Overview_", j, ".png"))
  tmap_save(tm = maine_overview, filename = overview_file, unit = "in",
    dpi = 300, height = 1, width = .85)

  baea_map_img <- baea_id_map_file %>%
    image_read(.) %>%
    image_trim(.) %>%
    image_resize(., "864x864")

  sim_map_img <- sim_j_map_file %>%
    image_read(.) %>%
    image_trim(.) %>%
    image_resize(., "864x864")

  legend_title_img <- legend_file %>%
    image_read(.) %>%
    image_trim(.) %>%
    image_crop("223x100+0+0")

  legend_scale_img <- legend_file %>%
    image_read(.) %>%
    image_trim(.) %>%
    image_crop("223x500+0+100")

  legend_scale_height <- image_info(legend_scale_img) %>%
    pull(height)
  legend_scale_offset <- 875 - legend_scale_height - 250

  legend_title_height <- image_info(legend_title_img) %>%
    pull(height)
  legend_title_offset <- 875 - legend_title_height - legend_scale_height - 250

  overview_img <- overview_file %>%
    image_read(.) %>%
    image_trim(.)

  backgrd <- image_blank(height = 875, width = 1752, color = "white")

  density_fig <- backgrd %>%
    image_composite(., baea_map_img, offset = "+0+10") %>%
    image_composite(., sim_map_img, offset = "+755+10") %>%
    image_composite(., legend_title_img, offset = paste0("+1520+",
      legend_title_offset)) %>%
    image_composite(., legend_scale_img, offset = paste0("+1560+",
      legend_scale_offset)) %>%
    image_composite(., overview_img, offset = "+1505+680")
  density_fig

  # Export PNG
  density_fig_file = file.path(tex_dir, "Figures/Ch3", "Density_Maps",
    paste0(j, "_BAEA_Sim_Density_Map.png"))
  image_write(density_fig, path = density_fig_file, format = "png")

  # Export SVG
  if(FALSE){
    density_fig_file = file.path(tex_dir, "Figures/Ch3", "Density_Maps",
      paste0(j, "_BAEA_Sim_Density_Map.svg"))
    image_write(density_fig, path = density_fig_file, format = "svg",
      flatten = TRUE)
  }

  # Clean up files
  if(TRUE){
    file.remove(baea_id_map_file)
    file.remove(sim_j_map_file)
    file.remove(legend_file)
    file.remove(overview_file)
  }
}

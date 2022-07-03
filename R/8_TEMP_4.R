j <- "Cruise"
nest_title


# Nest Path Density Maps -------------------------------------------------------

for (j in c("Cruise", "Flight")){
  exp_paths_raster_c <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_", nest_title, "_", j, "_C.rds")))
  exp_paths_raster_n <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_", nest_title, "_", j, "_N.rds")))
  exp_paths_raster_ns <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_", nest_title, "_", j, "_NS.rds")))
  exp_paths_raster_s <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_", nest_title, "_", j, "_S.rds")))

  # Get bb (for final map extent)
  paths_c_bb1_sfc <- st_as_sfc(bb(exp_paths_raster_c, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  paths_n_bb1_sfc <- st_as_sfc(bb(exp_paths_raster_n, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  paths_ns_bb1_sfc <- st_as_sfc(bb(exp_paths_raster_ns, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  paths_s_bb1_sfc <- st_as_sfc(bb(exp_paths_raster_s, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))

  # Get combined bb
  combined_bb <- st_union(paths_c_bb1_sfc, paths_n_bb1_sfc,
      paths_ns_bb1_sfc, paths_s_bb1_sfc) %>%
    st_transform(., crs = crs(base)) %>%
    bb(., relative = TRUE, height = 1, width = 1, asp.limit = 1)
  combined_bb_sfc <- combined_bb %>%
    st_as_sfc(.)

  combined_bb_om = maptiles::get_tiles(x = combined_bb,
    cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
    verbose = TRUE, zoom = 10, forceDownload = TRUE)
  if(FALSE) mapview(combined_bb)

  # Base map
  paths_density_map_base <-
    tm_layout(asp = 1, fontfamily = "Latin Modern Roman") +
    tm_shape(combined_bb_sfc, is.master = TRUE, ext = .935) +
      tm_borders(col = "red") +
    tm_shape(combined_bb_om, raster.downsample = FALSE) +
      tm_rgb() +
    tm_compass(type = "4star",  show.labels = 1, size = 2,
      position = c(.83, .825)) +
    tm_scale_bar(text.size = 1, breaks = c(0, 5, 10),
      position = c(.05, .01)) +
    tm_layout(fontfamily = "Latin Modern Roman",
      asp = 1,
      outer.margins = 0,
      inner.margins = 0,
      frame = "black",
      title.color = "black",
      title.bg.color = NA, #"ivory3",
      title.bg.alpha = .85,
      title.position = c(.275,.95),
      title.fontfamily = "Latin Modern Roman",
      title.fontface = "bold",
      title.size = 1, #.75
      title.snap.to.legend = FALSE,
      legend.show = FALSE)
  paths_density_map_base

  turbines_n_present <-  tm_shape(nest_wt_n_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color_present,
      border.col = turbine_color_present,
      lwd = 1)

  turbines_n_absent <-  tm_shape(nest_wt_n_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color_absent,
      border.col = turbine_color_absent, lwd = 1)

  turbines_s_present <-  tm_shape(nest_wt_s_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color_present,
      border.col = turbine_color_present, lwd = 1)

  turbines_s_absent <-  tm_shape(nest_wt_s_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color_absent,
      border.col = turbine_color_absent, lwd = 1)

  tm_nest <- tm_shape(nest) +
    tm_symbols(shape = 21, border.col = "black", border.lwd = 1,
      col = nest_color, size = .35)

  paths_density_map_c <- paths_density_map_base +
    tm_shape(exp_paths_raster_c, raster.downsample = FALSE) +
    tm_raster(palette = plasma(20, direction = 1), alpha = .7,
      legend.reverse = TRUE, style = "log10_pretty",
      title = "Path Density") +
    turbines_n_absent +
    turbines_s_absent +
    tm_nest +
    tm_credits("Control",
      size = 1, position = c(.0175, .91))

  paths_density_map_n <- paths_density_map_base +
    tm_shape(exp_paths_raster_n, raster.downsample = FALSE) +
    tm_raster(palette = plasma(20, direction = 1), alpha = .7,
      legend.reverse = TRUE, style = "log10_pretty",
      title = "Path Density") +
    turbines_n_present +
    turbines_s_absent +
    tm_nest +
    tm_credits("North",
      size = 1, position = c(.0175, .91))

  paths_density_map_s <- paths_density_map_base +
    tm_shape(exp_paths_raster_s, raster.downsample = FALSE) +
    tm_raster(palette = plasma(20, direction = 1), alpha = .7,
      legend.reverse = TRUE, style = "log10_pretty",
      title = "Path Density") +
    turbines_n_absent +
    turbines_s_present +
    tm_nest +
    tm_credits("South",
      size = 1, position = c(.0175, .91))

  paths_density_map_ns <- paths_density_map_base +
    tm_shape(exp_paths_raster_ns, raster.downsample = FALSE) +
    tm_raster(palette = plasma(20, direction = 1), alpha = .7,
      legend.reverse = TRUE, style = "log10_pretty",
      title = "Path Density") +
    turbines_n_present +
    turbines_s_present +
    tm_nest +
    tm_credits("North and South", #fontfamily = "Latin Modern Roman",
      size = 1, position = c(.0175, .91))

  # Arrange map of probability surfaces for testing
  tmap_paths_density_arrange <- tmap_arrange(paths_density_map_c,
    paths_density_map_n, paths_density_map_s, paths_density_map_ns, ncol = 2)

  tmap_save(tm = tmap_paths_density_arrange, filename = file.path(
    "C:/TEMP/TEMP_Images", paste0("Path_Density_", j, ".png")),
    unit = "in", dpi = 300, height = 6, width = 8*.8)

  paths_density_map_img <- file.path(
    "C:/TEMP/TEMP_Images", paste0("Path_Density_", j, ".png")) %>%
  image_read(.) %>%
  image_trim(.)

  legend_only <- tm_shape(exp_paths_raster_c, raster.downsample = FALSE) +
    tm_raster(palette = plasma(20, direction = 1), alpha = .7,
      legend.reverse = TRUE, style = "log10_pretty",
      title = "Path Density") +
    tm_layout(legend.only = TRUE,
      fontfamily = "Latin Modern Roman",
      legend.title.size = 1.2, #1
      legend.text.size = .8)
  tmap_save(tm = legend_only, filename = file.path(
    "C:/TEMP/TEMP_Images",paste0("Path_Density_", j, "_Legend.png")),
    unit = "in", dpi = 300, height = 3, width = 3)

  legend_img <- file.path(
      "C:/TEMP/TEMP_Images", paste0("Path_Density_", j, "_Legend.png")) %>%
    image_read(.) %>%
    image_trim(.)

  backgrd <- image_blank(2280, 1764, color = "white")

  covar_sigma_fig <- backgrd %>%
    image_composite(., paths_density_map_img, offset = "+0+0") %>%
    image_composite(., legend_img, offset = "+1910+750")

  # Export
  maps_fig_file = file.path(tex_dir, "Figures/Ch4/Maps_Path_Density",
    nest_title, paste0("Path_Density_", j, ".png"))
  image_write(covar_sigma_fig, path = maps_fig_file, format = ".png")

}


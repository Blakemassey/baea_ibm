# Grand_Lake Nest Area ---------------------------------------------------------

# Filter nest data
nest_grand_lake <- readRDS(file.path(wind_input_dir, "grand_lake_nest.rds"))

grand_lake_map_center <- readRDS(file.path(wind_input_dir,
  "grand_lake_map_center.rds"))

grand_lake_bb_sfc <- st_buffer(grand_lake_map_center, 8000) %>% bb(.) %>%
  st_as_sfc(.)
mapview(grand_lake_bb_sfc)

# Basemaps
grand_lake_natgeo_osm <- maptiles::get_tiles(x = grand_lake_bb_sfc,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 12, forceDownload = TRUE)

# Colors
nest_color <- "red"
wind_area_color <- "darkorange"
turbine_color <- "white"

# Grand_Lake Turbines ----------------------------------------------------------

grand_lake_wt_n = st_read(file.path(exp_turbines_dir, "Grand_Lake",
  "grand_lake_n_turbines.shp"))
grand_lake_wt_s = st_read(file.path(exp_turbines_dir, "Grand_Lake",
  "grand_lake_s_turbines.shp"))

grand_lake_wt_n_buff <- grand_lake_wt_n %>% st_buffer(56)
grand_lake_wt_s_buff <- grand_lake_wt_s %>% st_buffer(56)

mapview(grand_lake_wt_n_buff) + mapview(grand_lake_wt_s_buff)

# Grand_Lake Overview Map ------------------------------------------------------

grand_lake_overview_center <- nest_grand_lake
sfc <- st_sfc(st_point(c(st_coordinates(nest_grand_lake)[1] - 55000,
  st_coordinates(nest_grand_lake)[2] - 5000)))
st_geometry(grand_lake_overview_center) <- sfc
st_crs(grand_lake_overview_center) <- 32619

grand_lake_overview_buff <- st_buffer(grand_lake_overview_center, 110000) %>%
  bb(.)
mapview(grand_lake_overview_buff)
grand_lake_overview_bb <- bb_poly(bb(grand_lake_overview_buff, ext = 1))

grand_lake_overview_bb_osm <- maptiles::get_tiles(x = grand_lake_overview_bb,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 6, forceDownload = TRUE)

grand_lake_n_area <- readRDS(file.path(wind_input_dir, "grand_lake_n_area.rds"))
grand_lake_s_area <- readRDS(file.path(wind_input_dir, "grand_lake_s_area.rds"))

grand_lake_overview <-
  tm_layout(asp = 1, inner.margins = -.02) +
  tm_shape(grand_lake_overview_bb_osm, is.master = TRUE) +
    tm_rgb() +
  tm_shape(grand_lake_bb_sfc) +
    tm_borders(col = "red", lwd = 3) +
  tm_scale_bar(text.size = .75, breaks = c(0, 50, 100),
    position = c(.3, -.03))
grand_lake_overview

# Grand_Lake Wind Area Scenario Maps -------------------------------------------

tmap_grand_lake_wind_areas <-
  tm_layout(asp = 1) +
  tm_shape(grand_lake_bb_sfc, is.master = TRUE, ext = .935) +
    tm_borders(col = "red") +
  tm_shape(grand_lake_natgeo_osm, raster.downsample = FALSE) +
    tm_rgb() +
  tm_shape(nest_grand_lake, title = "Grand Lake Nest") +
    tm_bubbles(col = nest_color, border.lwd = 1,  size = .4,
    border.col = "black") +
  tm_shape(grand_lake_n_area, title = "Grand Lake Nest") +
    tm_polygons(col = wind_area_color, border.col = "black",  lwd = 1) +
  tm_shape(grand_lake_s_area, title = "Grand Lake Nest") +
    tm_polygons(col = wind_area_color, border.col = "black",  lwd = 1) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    position = c(.85, .87)) +
  tm_scale_bar(text.size = .75, breaks = c(0, 1, 2), position = c(.05, .01))
tmap_grand_lake_wind_areas

tmap_save(tm = tmap_grand_lake_wind_areas, filename = file.path(tex_dir,
  "Figures/Ch4", "Maps_Scenarios", "Grand_Lake", "Wind_Areas.svg"),
  insets_tm = grand_lake_overview, insets_vp =  viewport(x = 0.853, y = .141,
  width = 0.25, height = 0.25), unit = "in", dpi = 300, height = 6, width = 6.1)

# Grand_Lake Turbine Scenario Maps ----------------------------------------------

maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1, width = 2))
maine_bb <- bb_poly(bb(maine_bb_sf, ext = 1.15))
maine_om = read_osm(maine_bb, zoom = 5, minNumTiles = 9, type = om_nat_geo)

# Basemaps
grand_lake_natgeo_osm <- maptiles::get_tiles(x = grand_lake_bb_sfc,
  cachedir = "C:/Temp/Maptiles", provider = esri_natgeo_info, crop = TRUE,
  verbose = TRUE, zoom = 12, forceDownload = TRUE)

tmap_grand_lake_c <-
  tm_layout(asp = 1) +
  tm_shape(grand_lake_bb_sfc, is.master = TRUE, ext = .935) +
    tm_borders(col = "red") +
  tm_shape(grand_lake_natgeo_osm, raster.downsample = FALSE) +
    tm_rgb() +
  tm_shape(nest_grand_lake, title = "Grand_Lake Nest") +
    tm_bubbles(col = nest_color, border.lwd = 1,  size = .4,
    border.col = "black") +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    position = c(.85, .87)) +
  tm_scale_bar(text.size = .75, breaks = c(0, 1, 2), position = c(.05, .01))

tmap_grand_lake_n <- tmap_grand_lake_c +
  tm_shape(grand_lake_wt_n_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color,
      border.col = "black",  lwd = 1)

tmap_grand_lake_s <- tmap_grand_lake_c +
  tm_shape(grand_lake_wt_s_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color, border.col = "black",  lwd = 1)

tmap_grand_lake_ns <- tmap_grand_lake_n +
  tm_shape(grand_lake_wt_n_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color, border.col = "black",  lwd = 1) +
  tm_shape(grand_lake_wt_s_buff, title = "Wind Turbines") +
    tm_polygons(col = turbine_color, border.col = "black",  lwd = 1)

tmap_save(tm = tmap_grand_lake_c, filename = file.path(tex_dir, "Figures/Ch4",
  "Maps_Scenarios", "Grand_Lake", "Control.svg"),
  insets_tm = grand_lake_overview,
  insets_vp =  viewport(x = 0.853, y = .141, width = 0.25, height = 0.25),
  unit = "in", dpi = 300, height = 6, width = 6)
tmap_save(tm = tmap_grand_lake_n, filename = file.path(tex_dir, "Figures/Ch4",
  "Maps_Scenarios", "Grand_Lake", "North.svg"), insets_tm = grand_lake_overview,
  insets_vp =  viewport(x = 0.853, y = .141, width = 0.25, height = 0.25),
  unit = "in", dpi = 300, height = 6, width = 6)
tmap_save(tm = tmap_grand_lake_s, filename = file.path(tex_dir, "Figures/Ch4",
  "Maps_Scenarios", "Grand_Lake", "South.svg"), insets_tm = grand_lake_overview,
  insets_vp =  viewport(x = 0.853, y = .141, width = 0.25, height = 0.25),
  unit = "in", dpi = 300, height = 6, width = 6)
tmap_save(tm = tmap_grand_lake_ns, filename = file.path(tex_dir, "Figures/Ch4",
  "Maps_Scenarios", "Grand_Lake", "NorthSouth.svg"),
  insets_tm = grand_lake_overview,
  insets_vp =  viewport(x = 0.853, y = .141, width = 0.25, height = 0.25),
  unit = "in", dpi = 300, height = 6, width = 6)

# Grand_Lake SSF Maps ----------------------------------------------------------

# SSF Fits
ssf_fits_best_org <- readRDS(fits_best_file) #%>% slice(c(step_type_index))
ssf_fits_best <- ssf_fits_best_org

exp_dir <- "C:/ArcGIS/Data/R_Input/EXP/Grand_Lake"
exp_scenarios <- list.dirs(exp_dir, recursive = FALSE)

# Get nest
nest_grand_lake <- readRDS(file.path(wind_input_dir, "grand_lake_nest.rds"))

# For Individual Scenario Maps
for (j in seq_len(length(exp_scenarios))){
  exp_scenario_j <- exp_scenarios[j]
  exp_scenario_j_name <- basename(exp_scenarios[j])
  ssf_tmap_list <- vector(mode = "list", length = 20)
  for (i in seq_len(nrow(ssf_fits_best))){
    step_type_i_numeric <- ssf_fits_best %>% slice(i) %>% pull(step_type) %>%
      str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
        "perch" = "4", "roost" = "5"))
    ssf_prob_dir <- file.path(exp_scenario_j, "Step_Types_Prob")
    ssf_prob_file <- list.files(ssf_prob_dir, pattern =
      paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
    if (i ==  1){
      # Use "Tmap_baselayers.R" script to get other baselayers
      nest_bbox <- st_as_sfc(st_bbox(st_buffer(nest_grand_lake, dist = 10000)))
      nest_buffer <- st_buffer(nest_grand_lake, dist = 10000)
      nest_bb_sf <- st_as_sfc(bb(nest_buffer, relative = TRUE, height = 1.35,
        width = 1.35))
      Sys.sleep(1)
      nest_om = read_osm(nest_bb_sf, type = om_nat_geo, zoom = 11)
        #type = "osm", minNumTiles=9,
      nest_om_bb <- bb_poly(nest_om)
    }
    ssf_prob_i <- raster(ssf_prob_file) #%>% slice(1)
    ssf_prob_i_crop <- crop(ssf_prob_i, nest_buffer)
    ssf_prob_i_mask <- mask(ssf_prob_i_crop, nest_buffer)
    step_type_i_text <- step_type_i_numeric %>%
      str_replace_all("1", "Cruise") %>%
      str_replace_all("2", "Flight") %>%
      str_replace_all("3", "Nest") %>%
      str_replace_all("4", "Perch") %>%
      str_replace_all("5", "Roost")
    writeLines(paste0("Mapping: ", step_type_i_text))
    step_type_i_arrow <- step_type_i_text %>%
      str_replace_all("_", "$\\\\rightarrow$ ") %>%
      latex2exp::TeX(.)
    ssf_prob_i_nest_map <-
      tm_shape(nest_om) +
        tm_rgb() +
     tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
     tm_raster(palette = viridis(20, direction = 1), alpha = .6,
       legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_scale_bar(breaks = c(0, 5, 10), text.size = .4, lwd = .25,
        position = c(.03, .0)) +
      tm_compass(type = "4star", text.size = 0.55, show.labels = 1, size = 1.75,
        position = c(.8, .775), lwd = .25) +
      tm_shape(nest_grand_lake) +
      tm_symbols(shape = 20, #border.col = "black", border.lwd = .5,
        col = "black", size = .075) +
      tm_layout(asp = .8,
        frame = NA, #"black",
        title.color = "black",
        title.bg.color = NA, #"ivory3",
        title.bg.alpha = .85,
        title.position = c(.275,.95),
        title.fontfamily = "Latin Modern Roman",
        title = step_type_i_arrow,
        title.size = .6,
        title.snap.to.legend = FALSE,
        legend.bg.color = "ivory1",
        legend.frame = "grey",
        legend.frame.lwd = 1,
        legend.height = .4,
        legend.title.size = .4,
        legend.text.size = .35,
        legend.position = c(.785,.007),
        legend.outside = FALSE,
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman")
        #+ tm_credits(step_type_arrow, position=c("right","top"))
    #ssf_prob_i_nest_map
    tmap_position <- switch(step_type_i_numeric,
      "1_1" = 1,  "1_2" = 2,  "1_4" = 3,
      "2_1" = 5,  "2_2" = 6,  "2_4" = 7,  "2_5" = 8,
      "3_1" = 9,  "3_2" = 10, "3_4" = 11, "3_5" = 12,
      "4_1" = 13, "4_2" = 14, "4_4" = 15, "4_5" = 16,
                  "5_2" = 18, "5_4" = 19)
    writeLines(as.character(tmap_position))
    ssf_tmap_list[[tmap_position]] <- ssf_prob_i_nest_map
  }

  tmap_blank <-
    tm_shape(nest_om_bb, is.master = TRUE) +
      tm_fill(col = "white") +
    tm_shape(nest_buffer, is.master = TRUE) +
      tm_polygons(col = "white", border.col = "white") +
    tm_layout(asp = .8, legend.show = FALSE, frame = FALSE)

  for (i in seq_len(length(ssf_tmap_list))){
    if(is.null(ssf_tmap_list[[i]])) ssf_tmap_list[[i]] <- tmap_blank
  }

  # Arrange map of probability surfaces for testing
  ssf_tmap_nest_arrange <- tmap_arrange(
    ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]],
    ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
    ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
    ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
    ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
    ssf_tmap_list[[16]], ssf_tmap_list[[17]], ssf_tmap_list[[18]],
    ssf_tmap_list[[19]], ssf_tmap_list[[20]], ncol = 4)

  tmap_save(tm = ssf_tmap_nest_arrange, filename = file.path(tex_dir,
    "Figures/Ch4/Maps_SSF_Probability", "Grand_Lake", paste0("SSF_",
    exp_scenario_j_name, ".png")), unit = "in", dpi = 300,
    height = 8, width = 8*.8)
}

# Grand_Lake Path Density Maps -------------------------------------------------

# Variables
mapping <- FALSE
nest_color <- "red"
wind_area_color <- "darkorange"
turbine_color <- "white"
turbine_color_present <- "white"
turbine_color_absent <- "black"

grand_lake_wt_n = st_read(file.path(exp_turbines_dir, "Grand_Lake",
  "grand_lake_n_turbines.shp"))
grand_lake_wt_s = st_read(file.path(exp_turbines_dir, "Grand_Lake",
  "grand_lake_s_turbines.shp"))

grand_lake_wt_n_buff <- grand_lake_wt_n %>% st_buffer(56)
grand_lake_wt_s_buff <- grand_lake_wt_s %>% st_buffer(56)

if(mapping) mapview(grand_lake_wt_n_buff) + mapview(grand_lake_wt_s_buff)

# Get nest
nest_grand_lake <- nests_study %>% slice(c(5)) %>% st_transform(wgs84n19)

for (j in c("Cruise", "Flight")){
  exp_lines_raster_c <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_Grand_Lake_", j, "_C.rds")))
  exp_lines_raster_n <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_Grand_Lake_", j, "_N.rds")))
  exp_lines_raster_ns <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_Grand_Lake_", j, "_NS.rds")))
  exp_lines_raster_s <- readRDS(file.path(exp_output_dir, line_density_dir,
    line_density_agg_dir, paste0("Exp_Lines_Grand_Lake_", j, "_S.rds")))

  # Get bb (for final map extent)
  lines_c_bb1_sfc <- st_as_sfc(bb(exp_lines_raster_c, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  lines_n_bb1_sfc <- st_as_sfc(bb(exp_lines_raster_n, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  lines_ns_bb1_sfc <- st_as_sfc(bb(exp_lines_raster_ns, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))
  lines_s_bb1_sfc <- st_as_sfc(bb(exp_lines_raster_s, relative = TRUE,
      height = 1, width = 1)) %>%
    st_transform(., crs = as.character(OpenStreetMap::osm()))

  # Get combined bb
  combined_bb_sfc <- st_union(lines_c_bb1_sfc, lines_n_bb1_sfc,
      lines_ns_bb1_sfc, lines_s_bb1_sfc) %>%
    bb(., relative = TRUE, height = 1, width = 1, asp.limit = 1) %>%
    st_as_sfc(.) %>%
    st_transform(., crs = crs(base))
  combined_bb_om = read_osm(combined_bb_sfc, type = om_nat_geo, zoom = 11)
  if(mapping) mapview(combined_bb_sfc)

  # Get line density rasters
  for (i in c("C", "N", "NS", "S")){
    if(i == "C"){
      exp_lines_raster_i <- exp_lines_raster_c
      credits_text <- "Control"
    }
    if(i == "N"){
      exp_lines_raster_i <- exp_lines_raster_n
      credits_text <- "North"
    }
    if(i == "NS"){
      exp_lines_raster_i <- exp_lines_raster_ns
      credits_text <- "North and South"
    }
    if(i == "S"){
      exp_lines_raster_i <- exp_lines_raster_s
      credits_text <- "South"
    }

    lines_density_i_map <-
      tm_shape(combined_bb_om) +
        tm_rgb() +
      tm_shape(exp_lines_raster_i, raster.downsample = FALSE) +
      tm_raster(palette = plasma(20, direction = 1), alpha = .7,
        legend.reverse = TRUE, style = "log10_pretty",
        title = "Path Density") +
      tm_shape(nest_grand_lake) +
      tm_symbols(shape = 21, border.col = "black", border.lwd = 1,
        col = nest_color, size = .5) +
      tm_compass(type = "4star",  show.labels = 1, size = 2.5,
        position = c(.85, .87)) +
      tm_scale_bar(text.size = .75, breaks = c(0, 10, 20),
        position = c(.05, .01)) +
      tm_credits(credits_text, #fontfamily = "Latin Modern Roman",
        size = 1.5, position = c(.0175, .91)) +
      tm_layout(asp = 1,
        outer.margins = 0,
        inner.margins = 0,
        frame = NA, #"black",
        title.color = "black",
        title.bg.color = NA, #"ivory3",
        title.bg.alpha = .85,
        title.position = c(.275,.95),
        title.fontfamily = "Latin Modern Roman",
        title.size = .75,
        title.snap.to.legend = FALSE,
        legend.bg.color = "white",
        legend.frame = "grey",
        legend.frame.lwd = 1,
        legend.height = .2, # negative number = exact legend height
        legend.width = .23, # negative number = exact legend width
        legend.title.size = .95,
        legend.text.size = .75,
        legend.position = c(.775,.015),
        legend.outside = FALSE,
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman")

    turbines_n_present <-  tm_shape(grand_lake_wt_n_buff, title = "Wind Turbines") +
      tm_polygons(col = turbine_color_present,
        border.col = turbine_color_present,
        lwd = 1)

    turbines_n_absent <-  tm_shape(grand_lake_wt_n_buff, title = "Wind Turbines") +
      tm_polygons(col = turbine_color_absent,
        border.col = turbine_color_absent, lwd = 1)

    turbines_s_present <-  tm_shape(grand_lake_wt_s_buff, title = "Wind Turbines") +
      tm_polygons(col = turbine_color_present,
        border.col = turbine_color_present, lwd = 1)

    turbines_s_absent <-  tm_shape(grand_lake_wt_s_buff, title = "Wind Turbines") +
      tm_polygons(col = turbine_color_absent,
        border.col = turbine_color_absent, lwd = 1)

    if(i == "C"){
      lines_density_i_map <- lines_density_i_map +
        turbines_n_absent +
        turbines_s_absent
    }
    if(i == "N"){
      lines_density_i_map <- lines_density_i_map +
        turbines_n_present +
        turbines_s_absent
    }
    if(i == "NS"){
      lines_density_i_map <- lines_density_i_map +
        turbines_n_present +
        turbines_s_present
    }
    if(i == "S"){
      lines_density_i_map <- lines_density_i_map +
        turbines_n_absent +
        turbines_s_present
    }

    tmap_save(tm = lines_density_i_map, filename = file.path(tex_dir,
      "Figures/Ch4/Maps_Path_Density", "Grand_Lake",
      paste0("Path_Density_", j, "_", i,".svg")),
      unit = "in", dpi = 300, height = 6, width = 6)
    tmap_save(tm = lines_density_i_map, filename = file.path(tex_dir,
      "Figures/Ch4/Maps_Path_Density", "Grand_Lake",
      paste0("Path_Density_", j, "_", i,".png")),
      unit = "in", dpi = 300, height = 6, width = 6)
  }
}


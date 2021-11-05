#-------------------------- CALIBRATION CHECK ---------------------------------#
# This script checks the sim run output and calculates the calibration metrics
# ---------------------------------------------------------------------------- #

# Setup ------------------------------------------------------------------------

# Load packages
pacman::p_load(cartography, ctmm, dplyr, fasterize, forcats, gplots, ggplot2,
  ggthemes, ggpubr, grid, leaflet, lubridate, magick, mapview, move,
  OpenStreetMap, patchwork, plotly, prettymapr, purrr, raster, readr, rosm,
  rsvg, sf, s2, stringr, tmap, tmaptools, viridis, units, webshot, zoo)
pacman::p_load(baear, gisr, ibmr)
suppressMessages(extrafont::loadfonts(device="win"))
set_thin_PROJ6_warnings(TRUE)

# Set options
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE, chunksize = 1e9,
  memfrac = .9)

# Individual sim file
sim_rds_vec <- "sim_20210725-63.rds"

# All sim files in TEMP directory
if(FALSE){
  sim_only_dir <- list.dirs("C:/TEMP", recursive = FALSE) %>%
    str_subset(., "[:digit:]-[:digit:]{2}$")
  sim_rds_vec <- vector(mode = 'character', length = 0)
  for (m in seq_len(length(sim_only_dir))){
    sim_only_dir_m <- sim_only_dir[m]
    sim_rds_m <- list.files(path = sim_only_dir_m, pattern = ".rds$")
    sim_rds_vec <- append(sim_rds_vec, sim_rds_m)
  }
  sim_rds_vec <- (sim_rds_vec[!is.na(sim_rds_vec) & sim_rds_vec != ""])
  if(FALSE) sim_rds_vec <- sim_rds_vec[c(39:52)]
}

# Directories
input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
sim_dir <- "C:/TEMP"
sim_calibration_dir <- "Calibration"
baea_calibration_dir <- "Output/Sim/Calibration"
ridge_file_dir <- "C:/ArcGIS/Data/R_Input/BAEA/Ridgelines"

# Files
baea_terr_file <- "Data/BAEA/baea_terr.rds"
baea_ridge_sum_file <- "Output/Sim/Calibration/baea_ridge_sum.rds"

# Plot themes
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))
theme_blank <- theme(legend.position = "none",
  text = element_text(family = "Latin Modern Roman"),
  plot.title = element_text(size=14),
  panel.grid = element_blank(), axis.title = element_blank(),
  axis.text = element_blank(), axis.ticks = element_blank(),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA))
theme_update(plot.title = element_text(hjust = 0.5))

# Coordinate systems
wgs84 <- 4326 # WGS84 Lat/Long
wgs84n19 <- 32619 # WGS84 UTM 19N

# Functions
MakeLines <- function(x, y, x_end, y_end) {
  st_linestring(matrix(c(x, x_end, y, y_end), 2, 2))
}

RecodeNestIdToName <- function(value){
  recoded_value <- fct_recode(value,
    "Ellis" = "282A",
    "Sandy" = "423R01",
    "Hebron" = "659A",
    "Musquash" = "446R01")
  return(recoded_value)
}

# Behavior colors
behavior_colors <- CreateColorsByMetadata(file = file.path("Data/Assets",
  "behavior_colors.csv"), metadata_id = "behavior")
sex_colors <- tibble(#female = col2hex("yellow"), male = col2hex("tomato"),
  Female = col2hex("yellow"), Male = col2hex("tomato"))

nests_df <- read_csv("Data/Nests/Nests_Study_Use_Dates.csv",
  show_col_types = FALSE)

# Run for sim_rds_vec
for (m in seq_len(length(sim_rds_vec))){
  sim_rds <- sim_rds_vec[m]
  print(sim_rds)

  # Read in sim_out file
  # File Directory and ID
  sim_id <- tools::file_path_sans_ext(sim_rds)
  sim_runs <- readRDS(file.path(sim_dir, sim_id, sim_rds))

  if(!dir.exists(file.path(sim_dir, sim_id))){
    dir.create(file.path(sim_dir, sim_id))
  }

  if(!dir.exists(file.path(sim_dir, sim_id, sim_calibration_dir))){
    dir.create(file.path(sim_dir, sim_id, sim_calibration_dir))
  }

  # Hydro Distance -------------------------------------------------------------

  # Hydro Dist file
  input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
  hydro_dist_ras <- raster(file.path(input_dir, "dist_hydro_30mc.tif"))

  # File Directory and ID
  sim_runs <- readRDS(file.path(sim_dir, sim_id, sim_rds))

  for (i in seq_len(length(sim_runs))){
    sim_out <- sim_runs %>% pluck(i)
    sim_step_data <- CompileAllAgentsStepData(sim = sim_out) %>%
      mutate(behavior = as.factor(behavior)) %>%
      group_by(id) %>%
        mutate(step_type = paste0(behavior, "_", lead(behavior))) %>%
        mutate(previous_step_type = lag(step_type)) %>%
      ungroup() %>%
      filter(!is.na(datetime))
    sim_step_data <- ConvertStepDataCoordinates(sim_step_data)

    sim_step_data$behavior <- fct_recode(sim_step_data$behavior, "Cruise" = "1",
      "Flight" = "2", "Nest" = "3", "Perch" = "4", "Roost" = "5")
    sim_step_data$behavior <- as.character(sim_step_data$behavior)

    # Create Spatialdataframe of baea w/'Perch' behavior
    sim_perch <- sim_step_data %>%
      filter(behavior == "Perch")
    sim_perch_xy <- sim_perch %>% dplyr::select(x, y)
    sim_perch_sp <- SpatialPointsDataFrame(sim_perch_xy, sim_perch,
      proj4string = CRS(SRS_string = paste0("EPSG:", wgs84n19)), match.ID =TRUE)

    # Crop hydro_dist raster to the perch locations area
    hydro_dist_crop <- crop(hydro_dist_ras, as(st_as_sfc(st_bbox(sim_perch_sp)),
      "Spatial"), snap = "out")
    extent(hydro_dist_crop)

    # Extract hydro_dist from baea_perch
    hydro_dist <- raster::extract(hydro_dist_crop, sim_perch_sp, df = FALSE)
    sim_perch_dist <- cbind(sim_perch, hydro_dist)

    # Save perch_dist file
    saveRDS(sim_perch_dist, file.path(sim_dir, sim_id, sim_calibration_dir,
      paste0("sim_perch_dist_", i, ".rds")))

    rm(sim_out, sim_step_data, sim_perch, sim_perch_xy, sim_perch_sp,
      hydro_dist_crop, hydro_dist)
  }

  sim_perch_dist <- list.files(path = file.path(sim_dir, sim_id,
      sim_calibration_dir), pattern =  paste0("sim_perch_dist_*"))  %>%
    map(~ readRDS(file.path(sim_dir, sim_id, sim_calibration_dir, .))) %>%
    reduce(bind_rows)

  gg_sim_dist <- ggplot(sim_perch_dist) +
    geom_histogram(aes(x = hydro_dist, y = after_stat(count/sum(count))),
      boundary = 0, binwidth = 30, color = "black",
      fill = behavior_colors["Perch"]) +
    ggtitle("Simulation") +
    xlab("Hydro Distance Metric (m)") +
    ylab("Proportion of Perch Locations") +
    theme_minimal() + theme_latex +
    theme(axis.text = element_text(size = 8)) +
    theme(axis.title = element_text(size = 9)) +
    theme(plot.title = element_text(size = 11, hjust = 0.5)) +
    theme(legend.title = element_text(size = 11, hjust = 0.5)) +
    theme(panel.grid.minor.x = element_blank())
  if(FALSE) gg_sim_dist

  # Compare simulation and empirical data
  baea_perch_dist <- readRDS(file.path(baea_calibration_dir,
    "baea_perch_dist.rds"))

  gg_baea_dist <- ggplot(baea_perch_dist) +
    geom_histogram(aes(x = hydro_dist, y = after_stat(count/sum(count))),
      boundary = 0, binwidth = 30, color = "black",
      fill = behavior_colors["Perch"]) +
    ggtitle("Empirical") +
    xlab("Hydro Distance Metric (m)") +
    ylab("Proportion of Perch Locations") +
    theme_minimal() + theme_latex +
    theme(axis.text = element_text(size = 8)) +
    theme(axis.title = element_text(size = 9)) +
    theme(plot.title = element_text(size = 11, hjust = 0.5)) +
    theme(legend.title = element_text(size = 11, hjust = 0.5)) +
    theme(panel.grid.minor.x = element_blank())
  gg_baea_dist

  gg_combine_hydro_dist <- gg_baea_dist + gg_sim_dist

  gg_combine_hydro_dist
  ggsave(filename = "gg_combine_hydro_dist.png", plot = gg_combine_hydro_dist,
    path = file.path(sim_dir, sim_id, sim_calibration_dir), scale = 1,
    width = 6, height = 4, units = "in", dpi = 300)

  # Ridgeline Flights ----------------------------------------------------------

  # Ridgeline files
  ridge_poly_file <- file.path(ridge_file_dir, "ridge_poly.shp")
  ridge_line_file <- file.path(ridge_file_dir, "ridge_line.shp")

  ridge_poly <- read_sf(ridge_poly_file) %>%
    st_transform(., crs = CRS(SRS_string = paste0("EPSG:", wgs84n19))) %>%
    st_set_crs(wgs84n19)

  if(FALSE) mapview(ridge_poly)

  # BAEA data (for nest ids)
  baea_terr <- readRDS(baea_terr_file)

  # File directory and id
  sim_runs <- readRDS(file.path(sim_dir, sim_id, sim_rds))

  # Sim data
  for (i in seq_len(length(sim_runs))){
    sim_out <- sim_runs %>% pluck(i)
    print(paste0("Starting run: ", i, " of ", length(sim_runs)))

    sim_agents_input <- sim_out %>% pluck("agents", "input")
    sim_step_data <- CompileAllAgentsStepData(sim = sim_out) %>%
      mutate(behavior = as.factor(behavior)) %>%
      group_by(id) %>%
        mutate(step_type = paste0(behavior, "_", lead(behavior))) %>%
        mutate(previous_step_type = lag(step_type)) %>%
      ungroup() %>%
      filter(!is.na(datetime)) %>%
      ConvertStepDataCoordinates(.)

    sim_step_data$behavior <- fct_recode(sim_step_data$behavior, "Cruise" = "1",
      "Flight" = "2", "Nest" = "3", "Perch" = "4", "Roost" = "5")
    sim_step_data$behavior <- as.character(sim_step_data$behavior)

    sim_steps <- sim_step_data %>%
      mutate(behavior_next = lead(behavior)) %>%
      mutate(behavior_behavior = paste(behavior, "->", behavior_next)) %>%
      filter(step_length > 42.43) %>%
     filter(behavior_behavior != "Nest -> Nest",
       behavior_behavior != "Roost -> Roost",
       behavior_behavior != "Cruise -> Roost",
       behavior_behavior != "Roost -> Cruise") %>%
      group_by(id) %>%
      mutate(x_end = lead(x),
             y_end = lead(y)) %>%
      ungroup(.) %>%
      filter(!is.na(x_end))

    table(sim_steps$behavior_behavior)

    baea_id_nest <- baea_terr %>%
      group_by(id) %>%
      summarize(nest_id = unique(nest_site), .groups = "drop")

    for (j in unique(sim_steps$id)){
      if (j == unique(sim_steps$id)[1]){
        sim_ridge_sum <- sim_steps %>%
          left_join(., sim_agents_input, by = c("id", "sex")) %>%
          group_by(id) %>%
          summarize(nest_id = unique(nest_id), sex = Capitalize(first(sex)),
            .groups = "drop") %>%
          mutate(total_steps_n = NA_integer_, ridge_steps_n = NA_integer_,
            ridge_steps_prop = NA_real_)
      }
      row_j <- which(sim_ridge_sum$id == j)
      print(paste0("Starting id: ", j, " (", row_j, " of ",
        length(unique(sim_step_data$id)), ")"))

      # Create Lines and Points
      sim_steps_j <- sim_steps %>%
        filter(id == j)
      sim_lines_j_sfc <- sim_steps_j %>%
        filter(id == j) %>%
        dplyr::select(x, y, x_end, y_end) %>%
        pmap(.f = MakeLines) %>%
        st_as_sfc(crs = wgs84n19)
      sim_lines_j_sf <- sim_steps_j %>%
        st_as_sf(., geom = sim_lines_j_sfc) %>%
        tibble::rowid_to_column("row_id")

      sim_lines_j_bb_sf <- st_as_sfc(bb(sim_lines_j_sf, relative = TRUE,
        height = 1, width = 1))

      ridge_poly_crop <- suppressWarnings(st_crop(st_buffer(ridge_poly,
        dist = 0), st_buffer(sim_lines_j_bb_sf, dist = 0))) # buffer fixes topo

      if(TRUE) mapview(sim_lines_j_sf) + mapview::mapview(ridge_poly_crop)

      sim_lines_j_intersects <- st_intersects(sim_lines_j_sf,
          ridge_poly, sparse = TRUE) %>%
        as.data.frame(.) %>%
        as_tibble(.) %>%
        rename(row_id = row.id,
          intersect_poly_id = col.id)

      sim_lines_j_intersects_sum <- sim_lines_j_sf %>%
        left_join(., sim_lines_j_intersects, by = "row_id") %>%
        dplyr::select(row_id, intersect_poly_id) %>%
        st_drop_geometry(.) %>%
        group_by(row_id) %>%
        summarize(ridge_cross = any(!is.na(intersect_poly_id))) %>%
        ungroup(.) %>%
        summarize(total_steps_n = n(),
          ridge_steps_n = sum(ridge_cross)) %>%
        mutate(id = j) %>%
        mutate(ridge_steps_prop = ridge_steps_n/total_steps_n)

      sim_ridge_sum[row_j, "total_steps_n"] <- sim_lines_j_intersects_sum %>%
        pull(total_steps_n)
      sim_ridge_sum[row_j, "ridge_steps_n"] <- sim_lines_j_intersects_sum %>%
        pull(ridge_steps_n)
      sim_ridge_sum[row_j, "ridge_steps_prop"] <- sim_lines_j_intersects_sum %>%
        pull(ridge_steps_prop)
    }
    rm(row_j, sim_steps_j, sim_lines_j_sf, sim_lines_j_sfc, sim_lines_j_bb_sf,
      ridge_poly_crop, sim_lines_j_intersects, sim_lines_j_intersects_sum)
    saveRDS(sim_ridge_sum, file.path(sim_dir, sim_id, sim_calibration_dir,
      paste0("sim_ridge_sum_", i, ".rds")))
    rm(i, j, sim_ridge_sum)
  }

  sim_ridge_sum <- list.files(path = file.path(sim_dir, sim_id,
      sim_calibration_dir), pattern =  paste0(("sim_ridge_sum_*")))  %>%
    map(~ readRDS(file.path(sim_dir, sim_id, sim_calibration_dir, .))) %>%
    reduce(bind_rows) %>%
    mutate(nest_name = RecodeNestIdToName(nest_id))

  # Clean up objects
  rm(ridge_line_file, ridge_poly_file, ridge_poly, baea_terr, sim_out,
    sim_agents_input, sim_step_data, sim_steps, baea_id_nest)

  # Compare simulation and empirical data
  baea_ridge_sum <- readRDS(baea_ridge_sum_file) %>%
    mutate(nest_name = RecodeNestIdToName(nest_id))

  # Graph ridge-crossing summary data
  gg_combine_ridge <- ggplot() +
    geom_point(data = baea_ridge_sum, aes(x = nest_name, y = ridge_steps_prop))+
    geom_errorbar(data = baea_ridge_sum,
      aes(x = nest_name, y = ridge_steps_prop, ymin = quant_05, ymax = quant_95,
        width = .1, color = "Mean + 95% CI")) +
    geom_point(data = sim_ridge_sum, aes(x = nest_name, y = ridge_steps_prop,
      fill = sex), color = "black",  shape = 24, size = 2, show.legend = TRUE) +
    scale_fill_manual(name = "Simulation", values = sex_colors) +
    scale_color_manual(name = "Empirical", values = c("black", "black")) +
    guides(colour = guide_legend(override.aes = list(
      linetype = c("solid"),
      shape = c(16)))) +
    theme_minimal() +
    theme_latex +
    theme(axis.text = element_text(size = 8)) +
    theme(axis.title = element_text(size = 9)) +
    theme(plot.title = element_text(size = 11, hjust = 0.5)) +
    theme(legend.title = element_text(size = 11)) +
    xlab("Nest Area") + ylab("Ridge-Crossing Step Proportion") +
    labs(fill = "Agent Sex") +
    theme(legend.position = "right")

  gg_combine_ridge

  ggsave(filename = "gg_combine_ridge.png", plot = gg_combine_ridge,
    path = file.path(sim_dir, sim_id, sim_calibration_dir), scale = 1,
    width = 6, height = 4, units = "in", dpi = 300, bg = "white")

  # Consecutive Behavior Steps -------------------------------------------------

  # File directory and id
  sim_runs <- readRDS(file.path(sim_dir, sim_id, sim_rds))

  # Sim data
  for (i in seq_len(length(sim_runs))){
    sim_out <- sim_runs %>% pluck(i)
    print(paste0("Starting run: ", i, " of ", length(sim_runs)))
    sim_agents_input <- sim_out %>% pluck(1, "agents", "input")
    sim_behavior_i <- CompileAllAgentsStepData(sim = sim_out) %>%
      mutate(behavior = as.factor(behavior)) %>%
      group_by(id) %>%
      ungroup() %>%
      filter(!is.na(datetime)) %>%
      ConvertStepDataCoordinates(.)
    sim_behavior_i$behavior <- fct_recode(sim_behavior_i$behavior,
      "Cruise" = "1", "Flight" = "2", "Nest" = "3", "Perch" = "4", "Roost" ="5")
    sim_behavior_i$behavior <- as.character(sim_behavior_i$behavior)
    if(i == 1){
      sim_behavior <- sim_behavior_i
    } else {
      sim_behavior <- bind_rows(sim_behavior, sim_behavior_i)
    }
  }

  # Summarize behaviors' consecutive lengths
  cruise <- table(data.frame(unclass(rle(sim_behavior$behavior))) %>%
      filter(values == "Cruise")) %>% as_tibble(.) %>% rename(behavior = values)
  flight <- table(data.frame(unclass(rle(sim_behavior$behavior))) %>%
      filter(values == "Flight")) %>% as_tibble(.) %>% rename(behavior = values)
  nest <- table(data.frame(unclass(rle(sim_behavior$behavior))) %>%
      filter(values == "Nest")) %>% as_tibble(.) %>% rename(behavior = values)
  perch <- table(data.frame(unclass(rle(sim_behavior$behavior))) %>%
      filter(values == "Perch")) %>% as_tibble(.) %>% rename(behavior = values)
  roost <- table(data.frame(unclass(rle(sim_behavior$behavior))) %>%
      filter(values == "Roost")) %>% as_tibble(.) %>% rename(behavior = values)
  sim_behavior_consecutive <- bind_rows(cruise, flight, nest, perch, roost) %>%
    group_by(behavior) %>%
    mutate(prop = n/sum(n)) %>%
    mutate(total_n = sum(n))

  sim_behavior_consecutive

  saveRDS(sim_behavior_consecutive, file.path(sim_dir, sim_id,
    sim_calibration_dir, "sim_behavior_consecutive.rds"))

  rm(sim_out, sim_agents_input, cruise, flight, nest, perch, roost)

  # Set up list for graph
  behaviors <- sim_behavior_consecutive %>% pull(behavior) %>% unique(.)
  gg_sim_consecutive_list <- vector(mode = "list", length = length(behaviors))
  names(gg_sim_consecutive_list) <- behaviors

  gg_sim_consecutive_list <- lapply(sort(behaviors), function(i){
    gg_step_consecutive_i <- ggplot(sim_behavior_consecutive %>%
        filter(behavior == i)) +
      geom_col(aes(x = as.integer(lengths), y = prop),
        width = 1, color = "black", fill = behavior_colors[i]) +
      xlab(paste0(i)) +
      ylab("Proportion") +
      ggtitle(NULL) +
      theme_minimal() + theme_latex +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5),
        expand = expansion(mult = c(0, .05)))  +
      theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
      theme(axis.text = element_text(size = 7)) +
      theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
      theme(plot.title = element_text(size = 8, vjust = -2, hjust = 0.5))
  })

  # Read baea_behavior_consecutive
  baea_behavior_consecutive <- readRDS(file.path(baea_calibration_dir,
    "baea_behavior_consecutive.rds"))

  combined_behavior_consecutive <- bind_rows(
    baea_behavior_consecutive %>% mutate(source = "Empirical"),
    sim_behavior_consecutive %>% mutate(source = "Simulation"))

  # All plots on one figure
  gg_behavior_consecutive <- ggplot(combined_behavior_consecutive) +
      geom_col(aes(x = as.integer(lengths), y = prop, fill = behavior),
        color = "black", width = 1) +
      facet_grid(rows = vars(source), cols = vars(behavior)) +
      scale_fill_manual(values = behavior_colors, name = "Behavior") +
      xlab("Behavior State Consecutive Steps") +
      ylab("Proportion") +
      ggtitle(NULL) +
      theme_minimal() + theme_latex +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5),
        expand = expansion(mult = c(0, .05)))  +
      theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
      theme(axis.text = element_text(size = 7)) +
      theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
      theme(plot.title = element_text(size = 8, vjust = -2, hjust = 0.5))
  gg_behavior_consecutive

  # All plots on one figure with LaTeX
  gg_baea_list = lapply(sort(unique(baea_behavior_consecutive$behavior)),
    function(i){
    baea_behavior_consecutive_i <- baea_behavior_consecutive %>%
      filter(behavior == i)
    x_max <- ceiling(max(as.integer(baea_behavior_consecutive_i$lengths))/20)*20
    ggplot(baea_behavior_consecutive_i) +
      geom_col(aes(x = as.integer(lengths), y = prop),
        fill = behavior_colors[i], color = "black", width = 1) +
      xlab(NULL) + ylab(NULL) + ggtitle(NULL) +
      theme_minimal() + theme_latex +
      theme(legend.position="none") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5),
        limits = c(0, x_max)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
        expand = expansion(mult = c(0, .05)))  +
      theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
      theme(axis.text = element_text(size = 7)) +
      theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
      theme(plot.title = element_text(size = 8, vjust = -2, hjust = 0.5))
  })

  gg_sim_list = lapply(sort(unique(sim_behavior_consecutive$behavior)),
    function(i){
    sim_behavior_consecutive_i <- sim_behavior_consecutive %>%
      filter(behavior == i)
    x_max <- ceiling(max(as.integer(sim_behavior_consecutive_i$lengths))/20)*20
    ggplot(sim_behavior_consecutive_i) +
      geom_col(aes(x = as.integer(lengths), y = prop),
        fill = behavior_colors[i], color = "black", width = 1) +
      xlab(NULL) + ylab(NULL) + ggtitle(NULL) +
      theme_minimal() + theme_latex +
      theme(legend.position = "none") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5),
        limits = c(0, x_max)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5),
        expand = expansion(mult = c(0, .05)))  +
      theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
      theme(axis.text = element_text(size = 7)) +
      theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
      theme(plot.title = element_text(size = 8, vjust = -2, hjust = 0.5))
  })

  # Graph layout
  layout <- '
  ABCDE
  GHIJK
  '

  # Plot graph
  gg_behavior_plots <- wrap_plots(A = gg_baea_list[[1]],
    B = gg_baea_list[[2]], C = gg_baea_list[[3]], D = gg_baea_list[[4]],
    E = gg_baea_list[[5]], G = gg_sim_list[[1]],
    H = gg_sim_list[[2]], I = gg_sim_list[[3]], J = gg_sim_list[[4]],
    K = gg_sim_list[[5]],  design = layout)
  gg_behavior_plots

  # File name for temp (no label)
  gg_behavior_no_label_fig_file = file.path(sim_dir, sim_id,
    sim_calibration_dir, "gg_behavior_no_label_fig_file.png")

  # Save temp (no label) file
  ggsave(filename = basename(gg_behavior_no_label_fig_file),
    plot =  gg_behavior_plots,
    path = dirname(gg_behavior_no_label_fig_file), scale = 1, width = 8.25,
    height = 5, units = "in", dpi = 300)

  # Create tex strings
  tex_head <- tibble(
    tex_str = c("Behavior State Consecutive Steps", "Proportion", "Cruise",
      "Flight", "Nest", "Perch", "Roost", "Empirical", "Simulation"),
    tex_name = c("lab_consecutive_steps", "lab_prop",
      "lab_cruise", "lab_flight", "lab_nest", "lab_perch", "lab_roost",
      "lab_emp", "lab_sim"))
  tex_df <- bind_rows(bind_rows(tex_head) %>% mutate(title_size =
      c(rep(11, times = 2), rep(9, times = 7))))

  # Create tex text plots
  for (i in seq_len(nrow(tex_df))){
    tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
    tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
    title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
    gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
      theme(plot.title = element_text(size = title_size_i))
    ggsave(file = "C:/TEMP/TEMP.png", plot = gg_tex, width = 5, height = .75)
    tex_i <- image_trim(image_read("C:/TEMP/TEMP.png"))
    file.remove("C:/TEMP/TEMP.png")
    assign(paste0("tex_", tex_name_i), tex_i)
  }
  rm(i, tex_df, tex_name_i, tex_str_i, tex_i, tex_head)

  # Image background
  backgrd <- image_blank(2700, 1800, color = "white")

  # Create final plot and export to dissertation
  behavior_consecutive_no_label_fig <- image_read(gg_behavior_no_label_fig_file)
  behavior_consecutive_label_fig <- backgrd %>%
    image_composite(., behavior_consecutive_no_label_fig, offset = "+85+110")%>%
    image_composite(., image_rotate(tex_lab_prop, 270), offset = "+30+700") %>%
    image_composite(., image_rotate(tex_lab_emp, 90), offset = "+2550+330") %>%
    image_composite(., image_rotate(tex_lab_sim, 90), offset = "+2550+1100") %>%
    image_composite(., tex_lab_consecutive_steps, offset = "+1000+1650") %>%
    image_composite(., tex_lab_cruise, offset = "+325+50") %>%
    image_composite(., tex_lab_flight, offset = "+800+50") %>%
    image_composite(., tex_lab_nest, offset = "+1295+50") %>%
    image_composite(., tex_lab_perch, offset = "+1770+50") %>%
    image_composite(., tex_lab_roost, offset = "+2245+50")

  behavior_consecutive_label_fig

  behavior_consecutive_file = file.path(sim_dir, sim_id, sim_calibration_dir,
    "gg_combine_behavior_consecutive.png")
  image_write(behavior_consecutive_label_fig,
    path = behavior_consecutive_file, format = ".png")
  file.remove(gg_behavior_no_label_fig_file)

  # Behavior Time Proportions --------------------------------------------------

  # Wrangle data
  breaks = 20
  sim_behavior_sum <- sim_behavior %>%
    dplyr::select(datetime, id, time_proportion, behavior, sex) %>%
    mutate(behavior = factor(behavior)) %>%
    mutate(bins = CutProportion(time_proportion, breaks)) %>%
    mutate(bins_mid = factor(CutProportionMid(time_proportion, breaks))) %>%
    group_by(bins_mid) %>%   #group_by(sex, bins_mid) %>%
    dplyr::count(behavior) %>%
    mutate(value = n/sum(n)) %>%
    mutate(bins_mid = as.numeric(as.character(bins_mid))) %>%
    ungroup(.) %>%
    arrange(bins_mid) # arrange(sex, bins_mid)

  # Save data
  sim_behavior_sum
  saveRDS(sim_behavior_sum, file.path(sim_dir, sim_id, sim_calibration_dir,
    "sim_behavior_sum.rds"))

  # Make Plot
  point_size = 2; space_legend = .7
  gg_sim_behavior_prop <- ggplot(sim_behavior_sum, aes(x = bins_mid, y = value,
      ymax = 1, fill = behavior)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = behavior_colors, name = "Behavior") +
    labs(x = "", y = "", title = "") +
    #labs(x = "Daily Period", y = "Behavior Proportion", title = "") +
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
    theme(legend.position = 'none') +
    # theme(legend.title = element_text(size = 10),
    #   legend.text  = element_text(size = 9, hjust = 0),
    #   legend.key.size = unit(space_legend, "lines")) +
    theme(panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()) +
    scale_x_continuous(breaks = seq(0, 1, .1),
      expand = expansion(mult = c(.01, .01))) +
    scale_y_continuous(expand = expansion(mult = c(.00, .01))) +
    theme(axis.ticks = element_line(color = "grey50", size = .65)) +
    theme(axis.ticks.length = unit(5, "pt"))
  gg_sim_behavior_prop

  # Get empirical data
  baea_behavior_sum <- readRDS(file.path(baea_calibration_dir,
    "baea_behavior_sum.rds"))

  # Make plot with empirical data
  point_size = 2; space_legend = .7
  gg_baea_behavior_prop <- ggplot(baea_behavior_sum, aes(x = bins_mid,
      y = value, ymax = 1, fill = behavior)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = behavior_colors, name = "Behavior") +
    labs(x = "", y = "", title = "") +
  #  labs(x = "Daily Period", y = "Behavior Proportion", title = "") +
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
    theme(legend.position = 'none') +
    theme(legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9, hjust = 0),
      legend.key.size = unit(space_legend, "lines")) +
    theme(panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()) +
    scale_x_continuous(breaks = seq(0, 1, .1),
      expand = expansion(mult = c(.01, .01))) +
    scale_y_continuous(expand = expansion(mult = c(.00, .01))) +
    theme(axis.ticks = element_line(color = "grey50", size = .65)) +
    theme(axis.ticks.length = unit(5, "pt"))
  gg_baea_behavior_prop

  # Extract the legend
  point_size = 2; space_legend = .7
  behavior_legend <- get_legend(ggplot(baea_behavior_sum,
      aes(x = bins_mid, y = value, ymax = 1, fill = behavior)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = behavior_colors, name = "Behavior") +
    theme_minimal() +
    theme_latex +
    guides(shape = guide_legend(override.aes = list(size = point_size)),
      color = guide_legend(override.aes = list(size = point_size))) +
    theme(legend.title = element_text(size = 10),
      legend.text  = element_text(size = 9, hjust = 0),
      legend.key.size = unit(space_legend, "lines")))

  # Create legend for ggplot
  gg_behavior_legend <- as_ggplot(behavior_legend)
  ggsave(file = "C:/TEMP/TEMP.png", plot = gg_behavior_legend, width = 2,
    height = 2)
  tex_behavior_legend <- image_trim(image_read("C:/TEMP/TEMP.png"))
  file.remove("C:/TEMP/TEMP.png")

  # Save Temp (No Label) Files
  gg_baea_no_label_fig_file = file.path(sim_dir, sim_id, sim_calibration_dir,
    "gg_baea_no_label_fig.png")
  gg_sim_no_label_fig_file = file.path(sim_dir, sim_id, sim_calibration_dir,
    "gg_sim_no_label_fig.png")

  ggsave(filename = basename(gg_baea_no_label_fig_file),
    plot =  gg_baea_behavior_prop,
    path = dirname(gg_baea_no_label_fig_file), scale = 1, width = 7.5,
    height = 3, units = "in", dpi = 300)
  ggsave(filename = basename(gg_sim_no_label_fig_file),
    plot =  gg_sim_behavior_prop,
    path = dirname(gg_sim_no_label_fig_file), scale = 1, width = 7.5,
    height = 3, units = "in", dpi = 300)

  # Create tex strings
  tex_head <- tibble(
    tex_str = c("Daily Period", "Behavior Proportion", "Empirical",
      "Simulation"),
    tex_name = c("lab_period", "lab_prop", "lab_emp", "lab_sim"))
  tex_df <- bind_rows(bind_rows(tex_head) %>% mutate(title_size =
      c(rep(11, times = 2), rep(9, times = 2))))

  # Create tex text plots
  for (i in seq_len(nrow(tex_df))){
    tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
    tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
    title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
    gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
      theme(plot.title = element_text(size = title_size_i))
    ggsave(file = "C:/TEMP/TEMP.png", plot = gg_tex, width = 5, height = .75)
    tex_i <- image_trim(image_read("C:/TEMP/TEMP.png"))
    file.remove("C:/TEMP/TEMP.png")
    assign(paste0("tex_", tex_name_i), tex_i)
  }
  rm(i, tex_df, tex_name_i, tex_str_i, tex_i, tex_head)

  # Image background
  backgrd <- image_blank(2700, 1800, color = "white")

  # Create final plot and export to dissertation
  behavior_baea_no_label_fig <- image_read(gg_baea_no_label_fig_file)
  behavior_sim_no_label_fig <- image_read(gg_sim_no_label_fig_file) %>%
    image_chop(., "0x60")
  behavior_daily_label_fig <- backgrd %>%
    image_composite(., behavior_baea_no_label_fig, offset = "+100+50") %>%
    image_composite(., behavior_sim_no_label_fig, offset = "+100+860") %>%
    image_composite(., image_rotate(tex_lab_prop, 270), offset = "+70+700") %>%
    image_composite(., image_rotate(tex_lab_emp, 90), offset = "+2375+390") %>%
    image_composite(., image_rotate(tex_lab_sim, 90), offset = "+2375+1100") %>%
    image_composite(., tex_lab_period, offset = "+1200+1670") %>%
    image_composite(., tex_behavior_legend, offset = "+2450+735")
  behavior_daily_label_fig

  behavior_daily_file = file.path(sim_dir, sim_id, sim_calibration_dir,
    "gg_combine_behavior_daily.png")
  image_write(behavior_daily_label_fig, path = behavior_daily_file,
    format = ".png")
  file.remove(gg_baea_no_label_fig_file)
  file.remove(gg_sim_no_label_fig_file)
}

### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###

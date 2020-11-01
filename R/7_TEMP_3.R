## Ridgeline Flights -----------------------------------------------------------

# Source data directories
file_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
ridge_poly_file <- file.path(file_dir, "Ridgelines", "ridge_poly.shp")
ridge_line_file <- file.path(file_dir, "Ridgelines", "ridge_line.shp")

ridge_poly <- read_sf(ridge_poly_file) %>%
  st_transform(., crs = CRS(SRS_string = paste0("EPSG:", wgs84n19))) %>%
  st_set_crs(wgs84n19)
#mapview(ridge_poly)

# BAEA data
baea_terr <- readRDS("Data/BAEA/baea_terr.rds")

# File Directory and ID
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
    filter(step_length > 42.43) %>%
    group_by(id) %>%
    mutate(x_end = lead(x),
           y_end = lead(y)) %>%
    ungroup(.) %>%
    filter(!is.na(x_end))

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
    row_n <- which(sim_ridge_sum$id == j)
    print(paste0("Starting id: ", j, " (", row_n, " of ",
      length(unique(sim_step_data$id)), ")"))

    # Create Lines and Points
    sim_steps_j <- sim_steps %>%
      filter(id == j)
    sim_lines_j <- sim_steps_j %>%
      filter(id == j) %>%
      dplyr::select(x, y, x_end, y_end) %>%
      pmap(.f = MakeLines) %>%
      st_as_sfc(crs = wgs84n19) %>%
      {cbind(sim_steps_j, geom = .)} %>%
      st_sf(.) %>%
      tibble::rowid_to_column("row_id")

    sim_lines_j_bb_sf <- st_as_sfc(bb(sim_lines_j, relative = TRUE, height = 4,
        width = 4))

    ridge_poly_crop <- suppressWarnings(st_crop(st_buffer(ridge_poly, dist = 0),
      st_buffer(sim_lines_j_bb_sf, dist = 0))) # buffers fix known topo problems

    # mapview::mapview(sim_lines_j) +
    # mapview::mapview(ridge_poly_crop)

    sim_lines_intersect_df <- st_intersects(sim_lines_j,
        ridge_poly, sparse = TRUE) %>%
      as.data.frame(.) %>%
      as_tibble(.) %>%
      rename(row_id = row.id,
        intersect_poly_id = col.id)

    sim_lines_intersect_sf <- sim_lines_j %>%
      left_join(., sim_lines_intersect_df, by = "row_id")

    intersect_sum <- sim_lines_intersect_sf %>%
      as_tibble(.) %>%
      summarise(total_steps_n = n(),
        ridge_steps_n = sum(!is.na(intersect_poly_id))) %>%
      mutate(id = j) %>%
      mutate(ridge_steps_prop = ridge_steps_n/total_steps_n)

    sim_ridge_sum[row_n, "total_steps_n"] <- intersect_sum %>%
      pull(total_steps_n)
    sim_ridge_sum[row_n, "ridge_steps_n"] <- intersect_sum %>%
      pull(ridge_steps_n)
    sim_ridge_sum[row_n, "ridge_steps_prop"] <- intersect_sum %>%
      pull(ridge_steps_prop)
  }
  rm(row_n, sim_steps_j, sim_lines_j, sim_lines_j_bb_sf, ridge_poly_crop,
    sim_lines_intersect_df, sim_lines_intersect_sf, intersect_sum)
  saveRDS(sim_ridge_sum, file.path(sim_dir, sim_id, sim_calibration_dir,
    paste0("sim_ridge_sum_", i, ".rds")))
  rm(i, j, sim_ridge_sum)
}

sim_ridge_sum <- list.files(path = file.path(sim_dir, sim_id,
    sim_calibration_dir), pattern =  paste0(("sim_ridge_sum_*")))  %>%
  map(~ readRDS(file.path(sim_dir, sim_id, sim_calibration_dir, .))) %>%
  reduce(bind_rows)

# Clean up objects
rm(ridge_line_file, ridge_poly_file, ridge_poly, baea_terr, sim_out,
  sim_agents_input, sim_step_data, sim_steps, baea_id_nest)

# Compare simulation and empirical data
baea_ridge_sum <- readRDS("Output/Sim/Calibration/baea_ridge_sum.rds")

# Graph ridge-crossing summary data
gg_combine_ridge <- ggplot() +
  geom_point(data = baea_ridge_sum, aes(x = nest_id, y = ridge_steps_prop)) +
  geom_errorbar(data = baea_ridge_sum,
    aes(x = nest_id, y = ridge_steps_prop, ymin = quant_05, ymax = quant_95,
      width = .1, color = "Mean + 95% CI")) +
  geom_point(data = sim_ridge_sum, aes(x = nest_id, y = ridge_steps_prop,
    fill = sex), color = "black",  shape = 24, size = 2, show.legend = TRUE) +
  scale_fill_manual(name = "Simulation", values = sex_colors) +
  scale_color_manual(name = "Empirical", values = c("black", "black")) +
  guides(colour = guide_legend(override.aes = list(
    linetype = c("solid"),
    shape = c(16)))) +
  theme_minimal() +
  xlab("Nest ID") + ylab("Ridge-Crossing Step Proportion") +
  labs(fill = "Agent Sex") +
  theme(legend.position = "right")

gg_combine_ridge

ggsave(filename = "gg_combine_ridge.png", plot = gg_combine_ridge,
  path = file.path(sim_dir, sim_id, sim_calibration_dir), scale = 1, width = 6,
  height = 4, units = "in", dpi = 300)

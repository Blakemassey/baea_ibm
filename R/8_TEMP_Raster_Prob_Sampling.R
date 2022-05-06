# Attempt at getting the probability bins and proportional area calculations
# Extract layers from 'sim'
# Test procedure for sampling

# Load packages
pacman::p_load(baear, gisr, ibmr, raster)
pacman::p_load(gpindex, rgdal, tictoc, tidyverse, lubridate)

# Variables
behavior_i <- "Flight"
behavior_next_i <- "Perch"
nest_id_i <- "282A"
sex <- "male"
kernel_weights <- c(3, 1, 1)
plotting <- TRUE

# Directories and files
sim_file <- "C:/Work/Sim_Data/sim_20210725.rds"
move_pars_file <- "Output/Analysis/Movements/move_pars.rds"

# Import Data
sim <- readRDS(sim_file)
behavior_levels <- c("Cruise", "Flight", "Nest", "Perch", "Roost")
move_pars <- readRDS(move_pars_file) %>%
  mutate(behavior_num = as.numeric(factor(behavior, levels = behavior_levels)),
    behavior_next_num = as.numeric(factor(behavior_next,
    levels = behavior_levels))) %>%
    mutate(ids = paste0(behavior_num, "_", behavior_next_num))
base <- sim$spatial$base

# Get nest coordinates
nest_id_coords <- sim$agents$input %>%
  filter(nest_id == nest_id_i) %>%
  slice(1)
start_x <- nest_id_coords %>% pull(start_x) - 1500
start_y <- nest_id_coords %>% pull(start_y) - 1500

start_df_sf <- tibble(x = start_x, y = start_y) %>%
  sf::st_as_sf(., coords = c("x", "y"), crs = 32619,
  agr = "constant")

# Get nest_id species data
move_pars_i <- move_pars %>%
  filter(behavior == behavior_i & behavior_next == behavior_next_i)
line_type <- move_pars_i %>% pull(ids)

ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise",
  "Flight"), FALSE, TRUE)

pars_kernel <- CreateMoveKernelWeibullVonMises(
  max_r = NULL,
  cellsize = 30,
  mu1 = move_pars_i$mvm_mu1[1],
  mu2 = move_pars_i$mvm_mu2[1],
  kappa1 = move_pars_i$mvm_kappa1[1],
  kappa2 = move_pars_i$mvm_kappa2[1],
  mix = move_pars_i$mvm_prop[1],
  shape = move_pars_i$weibull_shape[1],
  scale = move_pars_i$weibull_scale[1],
  ignore_von_mises = ignore_von_mises)
r <- (30*((nrow(pars_kernel)-1)/2))+(30/2)

move_raster <- raster::raster(pars_kernel, xmn=-r, xmx=r, ymn=-r, ymx=r)
move_kernel <- raster::shift(move_raster, dx = start_x, dy = start_y)
plot(move_kernel)

ssf_layer_id <- which(names(sim$spatial$ssf_layers[[line_type]]) == nest_id_i)
ssf_layer <- sim$spatial$ssf_layers[[line_type]] %>% pluck(ssf_layer_id)
plot(ssf_layer)
ssf_layer_crop <- raster::crop(ssf_layer, move_kernel)
plot(ssf_layer_crop)

con_nest_dist_layer_id <- which(names(sim$spatial$con_nest_dist) == nest_id_i)
con_nest_dist_layer <- sim$spatial$con_nest_dist %>% pluck(con_nest_dist_layer_id)
plot(con_nest_dist_layer)
con_nest_dist_layer_crop <- raster::crop(con_nest_dist_layer, move_kernel)
plot(con_nest_dist_layer_crop)

# Con_Nest Kernel
con_nest_dist_layer_id <- which(names(sim$spatial$con_nest_dist) == nest_id_i)
con_nest_raster <- sim$spatial$con_nest_dist[[con_nest_dist_layer_id]]
if(plotting) plot(con_nest_raster, colNA = "black")
pars_gamma <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$gamma
pars_rescale <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$rescale
con_nest_prob <- CreateRasterConNestDistProb(con_nest_raster,
  raster_extent = raster::extent(move_kernel), pars_gamma = pars_gamma,
  pars_rescale = pars_rescale, x = start_x, y = start_y,
  base = base)
if(plotting) plot(con_nest_prob, colNA = "black")
raster::crs(con_nest_prob) <- raster::crs(base)
con_nest_ext <- raster::extend(con_nest_prob, move_kernel, value = NA)
con_nest_crop <- raster::crop(con_nest_ext, move_kernel, snap = "in")
con_nest_mask <- raster::mask(con_nest_crop, move_kernel, value = NA)
con_nest_kernel <- con_nest_mask/raster::cellStats(con_nest_mask,
  stat = "sum")
con_nest_kernel[con_nest_kernel == 0] <- NA
if(plotting) plot(con_nest_kernel, colNA = "black")

ssf_kernel <- raster::mask(ssf_layer_crop, move_kernel, snap = "in")
con_nest_kernel <- raster::mask(con_nest_kernel, move_kernel, snap = "in")

plot(move_kernel)
plot(ssf_kernel)
plot(con_nest_kernel)

if (behavior_next_i %in% c("Perch", "Roost")){
  land <- sim$spatial$landscape$land[[nest_id_i]]
  land_ext <- raster::extend(land, move_kernel, value = 0)
  land_crop <- raster::crop(land_ext, move_kernel, snap = "in")
  land_kernel <- raster::mask(land_crop, move_kernel, snap = "in")
  land_kernel[is.na(land_kernel)] <- 0
  if(plotting) plot(land_kernel, colNA = "black")
}

# Maine outline kernel
maine_outline <- sim$spatial$landscape$maine_outline[[nest_id_i]]
maine_outline_ext <- raster::extend(maine_outline, move_kernel, value = NA)
maine_outline_crop <- raster::crop(maine_outline_ext, move_kernel,
  snap = "in")
maine_outline_kernel <- raster::mask(maine_outline_crop, move_kernel,
  value = NA)
if(plotting) plot(maine_outline_kernel, colNA = "black")

kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
  con_nest_kernel))
WeightedGeoMean <- function(x){
  geomeans <- geometric_mean(x, w = kernel_weights, na.rm = TRUE)
  return(geomeans)
}
kernel_geomeans <- raster::calc(kernel_stack, fun = WeightedGeoMean)
plot(kernel_geomeans)

if (behavior_next_i %in% c("Perch", "Roost")){
  prob_raster <- kernel_geomeans * maine_outline_kernel * land_kernel
} else {
  prob_raster <- kernel_geomeans * maine_outline_kernel
}
plot(prob_raster)

prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
raster::cellStats(prob_raster, stat = "sum")
if(plotting) plot(prob_raster, colNA = "black")
prob_raster[is.na(prob_raster)] <- 0
raster::crs(prob_raster) <- raster::crs(sim$spatial$base)
if(plotting) plot(prob_raster, colNA = "black")
sum(prob_raster[] == 0)

prob_min <- raster::cellStats(prob_raster, "min")
prob_max <- raster::cellStats(prob_raster, "max")
prob_breaks <- seq(prob_min, prob_max, length.out = 11)

# Histogram of original probability values
if(plotting){
  raster::as.data.frame(prob_raster) %>%
    filter(!is.na(layer)) %>%
  ggplot(.) +
    geom_bar(aes(layer), fill="#69b3a2", color="#e9ecef") +
    scale_x_binned(name = "Probability", labels = scales::comma,
      breaks = prob_breaks)
}

prob_raster_reclass <- cut(prob_raster, breaks=prob_breaks) # Doesn't include 0
cellStats(prob_raster_reclass, "countNA")
if(plotting){
  rasterVis::gplot(prob_raster_reclass) +
    geom_tile(aes(fill = as.factor(value))) +
    scale_fill_viridis_d(name = "Bin") +
    coord_equal() + theme_classic()
}
ncell(prob_raster)

if(plotting){
  reclass_freq_table <- raster::as.data.frame(prob_raster_reclass) %>%
    mutate(layer = as.factor(layer)) %>%
    filter(!is.na(layer)) %>%
    count(layer, name = "reclass_cell_n")
  reclass_freq_table
  ggplot(reclass_freq_table) +
    geom_col(aes(x = layer, y = reclass_cell_n), fill="#69b3a2",
        color="#e9ecef") +
      scale_y_continuous(name = "Count", breaks = scales::breaks_pretty(n = 10),
        labels = scales::comma) +
      scale_x_discrete(name = "Reclassed Bin Cells", breaks = 1:10,
        labels = 1:10) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "grey90"))
}

prob_bin <- sample(1:10, size = 1, replace = T, prob = 1:10/10)
#prob_bin <- 5
plot(prob_raster_reclass)
prob_raster_sample_bin <- mask(prob_raster, prob_raster_reclass, inverse = TRUE,
  maskvalue = prob_bin, updatevalue = 0)
plot(prob_raster)
sum(prob_raster[] == 0)
sum(prob_raster[] != 0)
plot(prob_raster_sample_bin)
if(mapping){
  # This manually creates prob_raster_reclass with only the choosen prob_bin
  prob_raster_reclass_bin <- prob_raster_reclass
  prob_raster_reclass_bin[prob_raster_reclass_bin != prob_bin] <- 0
  plot(prob_raster_reclass)
  plot(prob_raster_reclass_bin)
}
sum(is.na(prob_raster_sample_bin[]))
sum(prob_raster_sample_bin[] == 0)
sum(prob_raster_sample_bin[] != 0)

# Testing
sampling_n <- 2000

for (i in seq_len(sampling_n)){
  if(i == 1){
    sample_df <- tibble(sample_id = 1:sampling_n, prob_bin = NA_integer_,
      cell_value = NA_real_, cell_id = NA_integer_, x = NA_integer_,
      y = NA_integer_)
  }
  if(i %% 100 == 0) print(i)
  prob_bin <- sample(1:10, size = 1, replace = T, prob = 1:10/10)
  prob_raster_sample_bin <- mask(prob_raster, prob_raster_reclass,
    inverse = TRUE, maskvalue = prob_bin, updatevalue = 0)
  # destination_cell <- suppressWarnings(sampling::strata(data = data.frame(
  #   cell = 1:raster::ncell(prob_raster_sample_bin)), stratanames = NULL,
  #   size = 1, method = "systematic",
  #   pik = prob_raster_sample_bin@data@values)) %>%
  #     pull(ID_unit)
  destination_cell <- sample(1:raster::ncell(prob_raster_sample_bin), size = 1,
    replace = T, prob = prob_raster_sample_bin@data@values)
  prob_raster_value <- prob_raster_sample_bin[destination_cell]
  destination_xy <- raster::xyFromCell(prob_raster, destination_cell)
  sample_df[i, "prob_bin"] <- prob_bin
  sample_df[i, "cell_value"] <- prob_raster_value
  sample_df[i, "cell_id"] <- destination_cell
  sample_df[i, "x"] <- destination_xy[1]
  sample_df[i, "y"] <- destination_xy[2]
}

if(plotting){
  sample_freq_table <- sample_df %>%
    mutate(prob_bin = as.factor(prob_bin)) %>%
    count(prob_bin, name = "prob_bin_n")
  sample_freq_table
  ggplot(sample_freq_table) +
    geom_col(aes(x = prob_bin, y = prob_bin_n), fill="#69b3a2",
        color="#e9ecef") +
    scale_y_continuous(name = "Count", breaks = scales::breaks_pretty(n = 10),
      labels = scales::comma) +
    scale_x_discrete(name = "Sampling Bin", breaks = 1:10,
      labels = 1:10) +
    theme_classic() +
    theme(panel.grid.major.y = element_line(color = "grey90"))
  sample_df_sf <- sf::st_as_sf(sample_df, coords = c("x", "y"), crs = 32619,
    agr = "constant")
  #mapview::mapview(hydro_dist_crop) +
  mapview::mapview(prob_raster_reclass) +
  mapview::mapview(sample_df_sf) +
  mapview::mapview(start_df_sf, col.regions = "green")
}

  # Hydro Dist file
  input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
  hydro_dist_ras <- raster(file.path(input_dir, "dist_hydro_30mc.tif"))

    # Crop hydro_dist raster to the perch locations area
    hydro_dist_crop <- crop(hydro_dist_ras, prob_raster, snap = "out")
    extent(hydro_dist_crop)

    # Extract hydro_dist from baea_perch
    hydro_dist <- raster::extract(hydro_dist_crop, sim_perch_sp, df = FALSE)
    sim_perch_dist <- cbind(sim_perch, hydro_dist)


#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#



# Setup ------------------------------------------------------------------------
pacman::p_load(cartography, ctmm, dplyr, fasterize, forcats, ggplot2, ggthemes,
  grid, leaflet, lubridate, magick, mapview, move, OpenStreetMap, plotly,
  prettymapr, purrr, raster, rosm, rsvg, sf, stringr, s2, tmap, tmaptools,
  viridis, units, webshot, zoo)
pacman::p_load(baear, gisr, ibmr)
set_thin_PROJ6_warnings(TRUE)
theme_update(plot.title = element_text(hjust = 0.5))
suppressMessages(extrafont::loadfonts(device = "win"))
#devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/ibmr")

# Individual sim file
sim_file <- "C:/Work/Sim_Data/sim_20210725.rds"

# Directories

# ESRI baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

# Read sim
sim <- readRDS(sim_file)

sim <- UpdateAgentStates(init = TRUE, sim = sim)
sim <- UpdateAgentStepDataBAEA2(init = TRUE, sim = sim,
  rep_intervals = rep_intervals)
step_data <- sim$agents$all[[2]][["step_data"]]
sim <- UpdateAgentParsData(init = TRUE, sim = sim)
sim <- UpdateSpatialBAEA(init = TRUE, sim = sim)

sim <- UpdateSpatialBAEA(init = TRUE, sim = sim)

# For testing
if(FALSE) {
  x <- step_data$x[i] + 1500; y <- step_data$y[i] + 1500
  step_data$x[i] <- x; step_data$y[i] <- y
}
if(FALSE){
  if(i == 1) step_data$x[i] <- 478445; step_data$y[i] <- 4972105
}
plotting <- TRUE

behavior_trans <- "2_2"
sex <- "male"

# Check all move kernels
for (i in seq_len(length(sim$spatial$classes[[sex]][["move_kernels"]]))){
  name_i <- names(sim$spatial$classes[[sex]][["move_kernels"]][[i]]) %>%
    str_replace(., "\\.\\.\\.\\.", " -> ")
  move_kernel_i <- sim$spatial$classes[[sex]][["move_kernels"]][[i]]
  plot(move_kernel_i, col = viridis(100), main = name_i)
}

# Move Kernel
move_org <- sim$spatial$classes[[sex]][["move_kernels"]][[behavior_trans]]
if(plotting) plot(move_org, col = viridis(100))
move_org_shift <- raster::shift(move_org, dx = step_data$x[i],
  dy = step_data$y[i])
move_rotated <- suppressWarnings(RotateRaster(move_org,
  Rad2Deg(step_data$exp_angle[i]), resolution = raster::res(base)))
move_crop <- raster::crop(move_rotated, move_org, snap = "near")
move_resample <- raster::resample(move_rotated, move_org, method = "ngb",
  progress = FALSE)
move_shift <- raster::shift(move_resample, dx = step_data$x[i],
  dy = step_data$y[i])
raster::crs(move_shift) <- raster::crs(base)
move_shift_ext <- raster::extend(move_shift, move_org_shift, value = NA)
move_shift_crop <- raster::crop(move_shift_ext, move_org_shift, snap = "in")
move_kernel_mask <- raster::mask(move_shift_crop, move_org_shift, value =NA)
move_kernel <- move_kernel_mask/raster::cellStats(move_kernel_mask,
  stat = "sum")
move_kernel[move_kernel == 0] <- NA
if(plotting) plot(move_kernel, colNA = "black")

# Con_Nest Kernel
con_nest_raster <- sim$spatial$con_nest_dist[[agent_states$nest_id]]
if(plotting) plot(con_nest_raster, colNA = "black")
pars_gamma <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$gamma
pars_rescale <- sim$pars$classes[[sex]]$constant$fixed$con_nest_pars$rescale
con_nest_prob <- CreateRasterConNestDistProb(con_nest_raster,
  raster_extent = raster::extent(move_kernel), pars_gamma = pars_gamma,
  pars_rescale = pars_rescale, x = step_data$x[i], y = step_data$y[i],
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

# SSF_Kernel
ssf_org <- sim$spatial$ssf_layers[[`behavior_trans`]][[agent_states$nest_id]]
ssf_ext <- raster::extend(ssf_org, move_kernel, value = NA)
ssf_crop <- raster::crop(ssf_ext, move_kernel, snap = "in")
ssf_mask <- raster::mask(ssf_crop, move_kernel, snap = "in")
ssf_kernel_mask <- raster::extend(ssf_mask, move_kernel, value = NA)
ssf_kernel <- ssf_kernel_mask/raster::cellStats(ssf_kernel_mask,
  stat = "sum")
ssf_kernel[ssf_kernel == 0] <- NA
if(plotting) plot(ssf_kernel, colNA = "black")

if(plotting){
  mapview::mapview(ssf_kernel)
  mapview::mapview(move_kernel)
  mapview::mapview(con_nest_kernel)
}

# Get osm baselayer for sim_step_sf_k and baea_step_sf_id
step_bb_sfc <- st_as_sfc(bb(move_kernel, relative = TRUE,
  height = 2, width = 2)) %>%
  st_transform(., crs = as.character(OpenStreetMap::osm()))
step_om <- step_bb_sfc %>%
  read_osm(., zoom = 11, #minNumTiles = 21,
    type = om_nat_geo)  # may need to add and adjust 'zoom' arg

step_map <-
  tm_layout(asp = 1) +
  tm_shape(step_om, raster.downsample = FALSE) +
    tm_rgb() +
  tm_scale_bar(text.size = .65,
    breaks = c(0, 5, 10),
    position = c(.05, .01)) +
  tm_legend(bg.color = "white", title.size = .75, text.size = .5)
    #format=list(scientific=TRUE, digits=2))
step_map

step_map_ssf <- step_map +
  tm_shape(ssf_kernel, raster.downsample = FALSE) +
  tm_raster(palette = "cividis", alpha = .75, legend.reverse = TRUE,
    title = "SSF Kernel")
step_map_ssf
tmap_save(tm = step_map_ssf, filename = file.path("C:/TEMP/Kernel_Maps",
  "SSF_Kernel_2.png"), unit = "in", dpi = 300, height = 4, width = 4)

step_map_move <- step_map +
  tm_shape(move_kernel, raster.downsample = FALSE) +
  tm_raster(palette = "plasma", alpha = .75, legend.reverse = TRUE,
    title = "Move Kernel")
step_map_move
tmap_save(tm = step_map_move, filename = file.path("C:/TEMP/Kernel_Maps",
  "Move_Kernel_2.png"), unit = "in", dpi = 300, height = 4, width = 4)

step_map_con <- step_map +
  tm_shape(con_nest_kernel, raster.downsample = FALSE) +
  tm_raster(palette = "inferno", alpha = .75, legend.reverse = TRUE,
    title = "Conspecific Kernel")
step_map_con
tmap_save(tm = step_map_con, filename = file.path("C:/TEMP/Kernel_Maps",
  "Conspecific_Kernel_2.png"), unit = "in", dpi = 300, height = 4, width = 4)

kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
  con_nest_kernel))
WeightedGeoMean <- function(x){
  geomeans <- gpindex::geometric_mean(x, w = c(1, 1, 1), na.rm = TRUE)
  return(geomeans)
}

prob_raster <- raster::calc(kernel_stack, fun = WeightedGeoMean)
prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")

step_map_prob <- step_map +
  tm_shape(prob_raster, raster.downsample = FALSE) +
  tm_raster(palette = "viridis", alpha = .75, legend.reverse = TRUE,
    title = "Redistribution Kernel 1:1:1")
step_map_prob

tmap_save(tm = step_map_prob, filename = file.path("C:/TEMP/Kernel_Maps",
  "Probability_Kernel_111_2.png"), unit = "in", dpi = 300, height = 4, width = 4)

WeightedGeoMean <- function(x){
  geomeans <- gpindex::geometric_mean(x, w = c(3, 1, 1), na.rm = TRUE)
  return(geomeans)
}
prob_raster <- raster::calc(kernel_stack, fun = WeightedGeoMean)
prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
step_map_prob <- step_map +
  tm_shape(prob_raster, raster.downsample = FALSE) +
  tm_raster(palette = "viridis", alpha = .75, legend.reverse = TRUE,
    title = "Redistribution Kernel 3:1:1")
step_map_prob
tmap_save(tm = step_map_prob, filename = file.path("C:/TEMP/Kernel_Maps",
  "Probability_Kernel_311_2.png"), unit = "in", dpi = 300, height = 4, width = 4)

WeightedGeoMean <- function(x){
  geomeans <- gpindex::geometric_mean(x, w = c(3, 1, 3), na.rm = TRUE)
  return(geomeans)
}
prob_raster <- raster::calc(kernel_stack, fun = WeightedGeoMean)
prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
step_map_prob <- step_map +
  tm_shape(prob_raster, raster.downsample = FALSE) +
  tm_raster(palette = "viridis", alpha = .75, legend.reverse = TRUE,
    title = "Redistribution Kernel 3:1:3")
step_map_prob
tmap_save(tm = step_map_prob, filename = file.path("C:/TEMP/Kernel_Maps",
  "Probability_Kernel_313_2.png"), unit = "in", dpi = 300, height = 4, width = 4)






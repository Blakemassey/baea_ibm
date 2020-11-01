########################## CREATE SIM ##########################################
### This script is used to create a 'sim' list object, consisting of 'agents',
### 'pars', and 'spatial' lists.

pacman::p_load(fasterize, dplyr, lubridate, momentuHMM, raster, rgdal,
  tidyverse)
pacman::p_load(baear, gisr, ibmr)

######################## REAL BAEA AGENTS ######################################

baea_terr <- readRDS("Data/BAEA/baea_terr.rds")
base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")

nests_agents <- baea_terr %>%
  group_by(id) %>%
  slice(1) %>%
  dplyr::select(id, nest_site, nest_long_utm, nest_lat_utm) %>%
  mutate(nest_id = nest_site) %>%
  dplyr::rename(name = id, long_utm = nest_long_utm, lat_utm = nest_lat_utm) %>%
  ungroup() %>%
  ConvertNestIdToNum(.) %>%
  mutate(x = CenterXYInCell(long_utm, lat_utm, xmin(base), ymin(base),
    res(base)[1], "x")) %>%
  mutate(y = CenterXYInCell(long_utm, lat_utm, xmin(base), ymin(base),
    res(base)[1], "y"))

## Create agents input ---------------------------------------------------------
n <- nrow(nests_agents)*2
id <- 1:n
sex <- rep(c("male", "female"), length.out = n)
age <- sample(5:10, n, TRUE)

nest_id <- rep(unlist(nests_agents[, "nest_id"]), each=2)
start_x <- rep(unlist(nests_agents[, "x"]), each=2)
start_y <- rep(unlist(nests_agents[, "y"]), each=2)
input <- tibble(id, sex, age, nest_id, start_x, start_y)

## Compile "agents" list -------------------------------------------------------
agents <- NamedList(input)

## Clean up environment --------------------------------------------------------
rm(age, baea_terr, id, input, n, nest_id, sex, start_x, start_y)

################################### PARS #######################################

## Create "global" parameters --------------------------------------------------
sim_start <- as.POSIXct("2015-03-15", tz = "UTC")
sim_period <- NULL #period(4, "days")
sim_end <- as.POSIXct("2015-08-15", tz = "UTC")
rep_period <- period(1, "month")
rep_interval <- c("01Jan", "01Jul")
rep_interval_custom <- NULL

step_period <- period(1, "day")
time_step_period <- period(15, "minutes")

sim_seasons <- data.frame(start = c("01Mar", "01Sep"), season = c("breeding",
  "winter"), stringsAsFactors = FALSE)

birth_day = "01Sep"
input_age_period = "years"
report_age_period = "months"

global <- NamedList(sim_start, sim_period, sim_end, rep_period, rep_interval,
  rep_interval_custom, step_period, time_step_period, sim_seasons, birth_day,
  input_age_period, report_age_period)

## Clean up environment --------------------------------------------------------
rm(birth_day, input_age_period, rep_interval, rep_interval_custom, rep_period,
  report_age_period, sim_end, sim_period, sim_seasons, sim_start, step_period,
  time_step_period)

## Create "Class" movement parameters ------------------------------------------

## Male ------------------------------------------------------------------------
## constant
baea_hmm_full <- readRDS(file = "Output/Analysis/Transitions/baea_hmm_full.rds")
baea_hmm_full$mle$beta
beta_est <- baea_hmm_full$mle$beta %>% as.data.frame(.)
rownames(beta_est) <- rownames(beta_est) %>%
  str_replace_all(c("\\(Intercept\\)" = "Intercept", "cosinor" = "")) %>%
  str_replace_all(c(", period = 365" = "", ", period = 1" = ""))
behavior_betas <- beta_est

move_pars <- readRDS(file = "Output/Analysis/Movements/move_pars.rds") %>%
  as.data.frame(.)
con_nest_pars <- readRDS(file = "Output/Analysis/Territorial/con_nest_pars.rds")

fixed <- NamedList(con_nest_pars, move_pars, behavior_betas)
constant <- NamedList(fixed)

## season
breeding <- list(NA)
winter <- list(NA)
season <- NamedList(breeding, winter)

## julian
julian <- list(NA)

male <- NamedList(constant, season, julian)

## Female ----------------------------------------------------------------------
## constant
fixed <- NamedList(con_nest_pars, move_pars, behavior_betas) # All same as male
constant <- NamedList(fixed)
## season
breeding <- list(NA)
winter <- list(NA)
season <- NamedList(breeding, winter)
## julian
julian <- list(NA)

female <- NamedList(constant, season, julian)

## Compile "pars" list ---------------------------------------------------------

classes <- NamedList(male, female)
pars <- NamedList(global, classes)

## Clean up environment --------------------------------------------------------
RemoveExcept(c("agents", "pars", "nests_agents"))
gc()

################################# SPATIAL ######################################

base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")

## Create Nest and Conspecific Distance Rasters --------------------------------

nests_active <- readRDS("Data/Nests/Nests_rds/nests_active.rds")
fits_baea_dist = readRDS("Output/Analysis/Territorial/fits_baea_dist.rds")

## Calculate Con_Nest Distance -------------------------------------------------

con_nest_dist <- CreateSimConNestDistRasters(agents = agents,
  nest_set = nests_active, base = base, output_dir = "Output/Sim/Territorial",
  max_r = 45000, write_con_nest_all = FALSE)

nests <- data.frame(nests_agents[c("x","y")], data.frame(nest_id =
  nests_agents["nest_id"]))

## Landscape layers ------------------------------------------------------------
maine_outline_file <- "C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp"

# Source data directories
file_dir <- "C:/ArcGIS/Data/R_Input/BAEA"

# Raster files
elev_file <- file.path(file_dir, "elev_30mc.tif")
land_file <- file.path(file_dir, "land_30mc.tif")

# Import Rasters
elev <- raster(elev_file) # other rasters' extents are set to this layer
land <- crop(raster(land_file), elev)

# Create empty raster of Maine outline
maine_outline <- sf::st_read(maine_outline_file)
maine_outline_raster <- fasterize(maine_outline, base, field = "DATA_SECUR",
  fun = "any")
maine_outline_raster[is.na(maine_outline_raster)] <- 0

# Create empty maine_outline list for each nest site (using the con_nest_dist)
maine_outline_list <- purrr::map(unique(names(con_nest_dist)), ~ NULL)
names(maine_outline_list) <- unique(names(con_nest_dist))

# Create maine_outline raster for each nest site
for (i in unique(names(con_nest_dist))){
  con_nest_dist_i <- con_nest_dist[[i]]
  maine_outline_i <- crop(maine_outline_raster, con_nest_dist_i)
  maine_outline_i <- extend(maine_outline_i, con_nest_dist_i, value = 0)
  maine_outline_mask_i <- mask(maine_outline_i, con_nest_dist_i)
  plot(maine_outline_mask_i)
  maine_outline_list[[which(names(maine_outline_list) == i)]] <-
    maine_outline_mask_i
}
maine_outline <- maine_outline_list

# Create empty land list for each nest site (using the con_nest_dist)
land_list <- purrr::map(unique(names(con_nest_dist)), ~ NULL)
names(land_list) <- unique(names(con_nest_dist))

# Create land raster for each nest site
for (i in unique(names(con_nest_dist))){
  con_nest_dist_i <- con_nest_dist[[i]]
  land_i <- crop(land, con_nest_dist_i)
  land_ext_i <- extend(land_i, con_nest_dist_i, value = NA)
  land_mask_i <- mask(land_ext_i, con_nest_dist_i)
  plot(land_mask_i)
  land_list[[which(names(land_list) == i)]] <- land_mask_i
}
land <- land_list

landscape <- NamedList(forest, land, maine_outline)

## SSF Layers ------------------------------------------------------------------

# Directories
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"

ssf_source <- "Step_Types_Prob" # THIS CONTROLS SSF LAYERS INPUT

if(!exists("con_nest_dist")){
  sim <- readRDS(file = "C:/Work/R/Data/Simulation/sim_01.rds")
  con_nest_dist <- sim %>% pluck("spatial", "con_nest_dist")
}

ssf_layers <- list.files(file.path(ssf_raster_dir, ssf_source),
  pattern = ".tif$")

ssf_step_types <- str_replace_all(ssf_layers, ".tif", "")
ssf_list <- purrr::map(unique(ssf_step_types), ~ NULL)
names(ssf_list) <- unique(ssf_step_types)

for (i in seq_along(ssf_layers)){
  ssf_layer_i <- ssf_layers[i]
  ssf_step_type_i <- str_replace_all(ssf_layer_i, ".tif", "")
  print(ssf_layer_i)

  # Create Raster
  ssf_i <- raster(file.path(ssf_raster_dir, ssf_source, ssf_layer_i))

  # Create ssf_layer raster for each nest site
  ssf_i_list <- purrr::map(unique(names(con_nest_dist)), ~ NULL)
  names(ssf_i_list) <- unique(names(con_nest_dist))

  for (j in unique(names(con_nest_dist))){
    #j <- unique(names(con_nest_dist))[1] # FOR TESTING
    con_nest_dist_j <- con_nest_dist[[which(names(con_nest_dist) == j)]]
    #plot(con_nest_dist_j)
    ssf_i_j_ext <- extend(ssf_i, con_nest_dist_j, value = NA)
    plot(ssf_i_j_ext)
    ssf_i_j_crop <- crop(ssf_i_j_ext, con_nest_dist_j)
    plot(ssf_i_j_crop)
    ssf_i_j_mask <- mask(ssf_i_j_crop, con_nest_dist_j)
    plot(ssf_i_j_mask, main = paste0(i, ": ", j))

    # NEW SECTION - NO TRANSFORMATION (RESCALING DONE AT STEP-LEVEL)
    ssf_i_j_final <- ssf_i_j_mask
    writeRaster(ssf_i_j_final, file.path(ssf_raster_dir,
      "Step_Types_Prob_Crop", paste0(j, "_", ssf_layer_i)),
      format = "GTiff", overwrite = TRUE)
    ssf_i_list[[which(names(ssf_i_list) == j)]] <- ssf_i_j_final
  }
  ssf_list[[which(names(ssf_list) == ssf_step_type_i)]] <- ssf_i_list
}
gc()

ssf_layers <- c(NamedList(ssf_source), ssf_list)

## Combine all Spatial Layers --------------------------------------------------

spatial <- list()
spatial[["base"]] <- base
spatial[["nests"]] <- nests
spatial[["con_nest_dist"]] <- con_nest_dist
spatial[["landscape"]] <- landscape
spatial[["ssf_layers"]] <- ssf_layers

#spatial <- NamedList(base, nests, con_nest_dist, landscape, ssf_layers)
#agents <- sim$agents
#pars <- sim$pars

## Clean up environment --------------------------------------------------------
RemoveExcept(c("agents", "pars", "spatial"))

################################## SIM #########################################

sim <- list()
sim[["agents"]] <- agents
sim[["pars"]] <- pars
sim[["spatial"]] <- spatial

sim <- NamedList(agents, pars, spatial)
#RemoveExcept("sim")
saveRDS(sim, file="C:/Work/R/Data/Simulation/sim_20200823.rds")

# Recompile sim
sim <- readRDS(file="C:/Work/R/Data/Simulation/sim_20200823.rds")
agents <- sim$agents
pars <- sim$pars
spatial <- sim$spatial
sim$spatial$ssf_layers <- ssf_layers
sim <- NamedList(agents, pars, spatial)
saveRDS(sim, file = "C:/Work/R/Data/Simulation/sim_20201016.rds")

RemoveExcept(c("baea", "nest_set", "base", "max_r", "write_home_dist",
  "write_con_dist", "write_con_dist_nest", "write_con_dist"))


#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

# ORIGINAL METHOD - RESCALING GLOBALLY
# ssf_i_j_rescale <- ssf_i_j_ext
# ssf_i_j_rescale[] <- scales::rescale(ssf_i_j_ext[], to = c(-9, 9))
# plot(ssf_i_j_rescale)
# ssf_i_j_inv_logit <- calc(ssf_i_j_rescale, fun = boot::inv.logit)
# ssf_i_j_final <- mask(ssf_i_j_inv_logit, con_nest_dist_j)
# plot(ssf_i_j_final)
# writeRaster(ssf_i_j_final, file.path(ssf_raster_dir,
#   "Step_Types_Rescale_Prob", paste0(j, "_", ssf_layer_i)),
#   format = "GTiff", overwrite = TRUE)

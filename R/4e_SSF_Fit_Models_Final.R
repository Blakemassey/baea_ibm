#--------------------------- SSF Model Fit Final ------------------------------#
# This script is to run model fitting procedures for the SSF using different
# covariates to determine the best fitting model given the covariate candidates
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------

# Load packages
pacman::p_load(AICcmodavg, arrangements, plyr, dplyr, future, furrr, optimx,
  ggplot2, lubridate, optimx, purrr, raster, rgenoud, reproducible, sf, stars,
  stringr, survival, tibble, tictoc, tidyr, tmap, tmaptools, viridis,
  whitebox, xtable)
pacman::p_load(baear, gisr, ibmr)
whitebox::wbt_init() # required for WhiteboxTools to work
suppressMessages(extrafont::loadfonts(device="win"))
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE,
  memfrac = .85)
wbt_version() # check WhiteboxTools version

# Set run parameters -----------------------------------------------------------

model_id <- GetDateTime()
save_individual_maps <- FALSE

# Groups to update SSF_Raster/Covars_Crop layers (e.g. 1_1.tif)
#step_type_group_updates <- c("ac", "af", "sc", "sf", "ap", "ar", "sp", "sr")
step_type_group_updates <- c()
step_type_updates <- tribble(
   ~step_type,  ~step_type_group,
  "cruise_cruise", "ac",
  "cruise_flight", "af",
  "cruise_perch",  "ap",
  "flight_cruise", "ac",
  "flight_flight", "af",
  "flight_perch",  "ap",
  "flight_roost",  "ar",
  "nest_cruise",   "sc",
  "nest_flight",   "sf",
  "nest_perch",    "sp",
  "nest_roost",    "sr",
  "perch_cruise",  "sc",
  "perch_flight",  "sf",
  "perch_perch",   "sp",
  "perch_roost",   "sr",
  "roost_flight",  "sf",
  "roost_perch",   "sp") %>%
  filter(step_type_group %in% step_type_group_updates) %>%
  pull(step_type)

# Theme (for LaTeX font)
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))

# Model directories
ua_data_dir <- "Output/Analysis/SSF/UA_Data"
mod_dir <- "Output/Analysis/SSF/Models"
mod_fit_dir <- file.path(mod_dir, "model_fits")
mod_best_dir <- file.path(mod_dir, "model_fits_best")

# Model files
fits_best_file <- file.path(mod_best_dir, "model_fits_best.rds")
preds_tbl_file <- file.path(mod_best_dir, "preds_tbl.rds")

# SSF directories
input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
elev_file <- file.path(input_dir, "elev_30mc.tif")
kernel_dir <- file.path(input_dir, "SSF_Rasters/Processed_Sections/Kernel")
terrain_dir <- file.path(input_dir, "SSF_Rasters/Processed_Sections/Terrain")

# SSF directories
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"
covars_full_dir <- file.path(ssf_raster_dir, "Covars_Full")
covars_crop_dir <- file.path(ssf_raster_dir, "Covars_Crop")
ssf_value_dir <- file.path(ssf_raster_dir, "Step_Types")
ssf_prob_dir <- file.path(ssf_raster_dir, "Step_Types_Prob")
map_temp_dir <- "C:/TEMP/SSF_Maps"

# Maine files
maine_raster_trim_file <- "C:/ArcGIS/Data/BlankRaster/maine_trim.tif"
maine_polygon_file <- "C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp"

# Nests
nests_study_file <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/",
  "baea_ibm/Data/Nests/Nests_rds/nests_study.rds")

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

# Coordinate systems
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Compile Best Models ----------------------------------------------------------

# Find all of the step_type folders in mod_fit_dir
step_types <- list.dirs(file.path(mod_fit_dir), full.names = FALSE,
  recursive = FALSE)

# Compile best models for each step_type into a model_best_fit file
for (i in seq_len(length(step_types))){
  step_type_i <- step_types[i]
  writeLines(paste0(step_type_i, " (", i, " of ", length(step_types), ")"))
  ssf_fits_best_step_type_i <- list.files(path = file.path(mod_fit_dir,
    step_type_i), pattern = paste0("ssf_fit_",
        step_type_i, "*")) %>%
    map(~ readRDS(file.path(mod_fit_dir, step_type_i, .))) %>%
    reduce(bind_rows) %>%
    as_tibble(.) %>%
    dplyr::select(step_type, covars_all, models_best) %>%
    unnest(models_best) %>%
    arrange(fit_aicc) %>%
    slice(which.min(fit_aicc))
  if(i == 1){
    ssf_fits_best <- ssf_fits_best_step_type_i
  } else {
    ssf_fits_best <- bind_rows(ssf_fits_best, ssf_fits_best_step_type_i)
  }
}
rm(step_type_i, ssf_fits_best_step_type_i)

saveRDS(ssf_fits_best, fits_best_file)

# Create preds table -----------------------------------------------------------

# Get ssf fits
ssf_fits_best_org <- readRDS(fits_best_file) #%>% slice(c(step_type_index))
ssf_fits_best_org %>% dplyr::select(step_type, fit_covars_clean, model_full)
ssf_fits_best <- ssf_fits_best_org

ssf_fits_best %>% pluck("model_full") %>% unlist()

# Determine all the raster_sigma layers
preds_all <- unlist(ssf_fits_best$fit_covars_clean) %>% str_remove_all("\\^2")
preds_unique <- unique(preds_all)
preds_rasters <- unique(str_replace_all(preds_unique, "[0-9]", ""))

# Raster classes (used in next step)
raster_classes <- c(developed = "kernel_class",
  forest = "kernel_class",
  open_water = "kernel_class",
  pasture = "kernel_class",
  shrub_herb = "kernel_class",
  wetland = "kernel_class",
  road = "kernel_class",
  eastness = "kernel_class",
  northness = "kernel_class",
  wind_class = "kernel_class",
  developed_dist = "extract_class",
  hydro_dist = "extract_class",
  turbine_dist = "extract_class",
  road_dist = "extract_class",
  tpi = "terrain_class",
  tri = "terrain_class",
  roughness = "terrain_class")

preds_tbl <- tibble(preds_unique) %>%
  mutate(sigma = as.integer(str_extract(preds_unique, "[0-9]{1,3}"))) %>%
  mutate(covar = str_replace_all(preds_unique, "[0-9]", "")) %>%
  dplyr::select(covar, sigma) %>%
  mutate(covar_sigma = paste0(covar, sigma)) %>%
  mutate(raster_class = recode(covar, !!!raster_classes)) %>%
  mutate(raster_layer = vector(mode = "list", length = nrow(.))) %>%
  arrange(covar, sigma)

saveRDS(preds_tbl, preds_tbl_file)

# Generate ssf layers for Maine ------------------------------------------------

ssf_fits_best <- readRDS(fits_best_file)
ssf_fits_best %>% pluck("model_full") %>% unlist()

maine_raster_trim <- raster(maine_raster_trim_file)

# Update probability layers for each ssf in update group based on original fits

ssf_fits_best_updates <- ssf_fits_best %>%
  filter(step_type %in% step_type_updates)

if(nrow(ssf_fits_best_updates) > 0){
  for (i in 1:nrow(ssf_fits_best_updates)){
    step_type_i <- ssf_fits_best_updates %>% slice(i) %>% pull(step_type)
    writeLines(paste0("Creating SSF Layer for: ", step_type_i))

    covars_i <- ssf_fits_best_updates %>% slice(i) %>% pluck("covar_fitted",
      1) %>% pull("covar_clean") %>% str_remove_all("\\^2") %>% unique()

    # Create Raster_Brick
    covars_list <- vector(mode = "list", length = length(covars_i))
    for (j in seq_along(covars_i)){
      covars_i_j <- covars_i[j]
      writeLines(paste0("covariates: ", covars_i_j))
      raster_file <- file.path(covars_crop_dir, paste0(covars_i_j, ".tif"))
      covars_list[[j]] <- raster(raster_file)
      writeLines(paste0(extent(covars_list[[j]])))
    }
    covars_brick <- raster::brick(covars_list)
    rm(covars_list)

    # Generate formula
    covars_clean_i <- ssf_fits_best_updates %>% slice(i) %>%
      pluck("covar_fitted", 1) %>%
      pull("covar_clean")

    covars_i <- ifelse(str_detect(covars_clean_i, "\\^2"),
      paste0("covars_brick[['", str_remove_all(covars_clean_i, "\\^2"),"']]^2"),
      paste0("covars_brick[['", covars_clean_i, "']]"))

    coefs_i <- ssf_fits_best_updates %>% slice(i) %>% pluck("covar_fitted",
      1) %>% pull("coef_signif")

    # Generate formulas
    ssf_formula <- paste0("(", paste0(paste0(coefs_i, "*",covars_i),
      collapse = ") + ("), ")")
    writeLines(ssf_formula)

    # Create value raster, then crop and mask
    ssf_value_raster <- eval(parse(text = ssf_formula))
    #plot(ssf_value_raster, main = step_type_i)

    # Calculate probability
    ssf_prob_raster <- raster::calc(ssf_value_raster, fun = boot::inv.logit)
    #plot(ssf_prob_raster, main = step_type_i)
    #hist(ssf_prob_raster)

    # Write Rasters to output dir
    step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
      "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
    writeRaster(ssf_value_raster, file.path(ssf_raster_dir, "Step_Types",
      step_type_i_numeric), format = "GTiff", overwrite = TRUE)
    writeRaster(ssf_prob_raster, file.path(ssf_prob_dir, step_type_i_numeric),
      format = "GTiff", overwrite = TRUE)
    rm(coefs_i, covars_brick, raster_file, ssf_formula, ssf_value_raster,
      ssf_prob_raster, step_type_i_numeric)
    gc()
  }
}

# Generate Maps For Maine ------------------------------------------------------

# Maine by step-type maps

ssf_fits_best <- readRDS(fits_best_file)

# Maine Outline
maine <- read_sf(maine_polygon_file) %>% st_transform(., crs = 4326) %>%
  mutate(state = "Maine") %>% dplyr::select(state)

# Nests
nests_study <- st_as_sf(x = readRDS(nests_study_file), coords = c("long","lat"),
  crs = "+proj=longlat +datum=WGS84") %>% filter(!name %in% c("Davis", "Upper"))

# For Individual Maps
if(save_individual_maps){
  for (i in seq_len(nrow(ssf_fits_best))){
    writeLines(i)
    step_type_i_numeric <- ssf_fits_best %>% slice(i) %>% pull(step_type) %>%
      str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
        "perch" = "4", "roost" = "5"))
    ssf_prob_file <- list.files(ssf_prob_dir, pattern =
      paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
    ssf_prob_i <- read_stars(ssf_prob_file)
    if (i ==  1){
      # Use "Tmap_baselayers.R" script to get other baselayers
      maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1.15,
        width = 1.35))
      maine_om = read_osm(maine_bb_sf, zoom = 7, minNumTiles = 9, type =
        om_nat_geo)
    }
    step_type_i_text <- step_type_i_numeric %>%
      str_replace_all("1", "Cruise") %>%
      str_replace_all("2", "Flight") %>%
      str_replace_all("3", "Nest") %>%
      str_replace_all("4", "Perch") %>%
      str_replace_all("5", "Roost")
    step_type_i_arrow <- step_type_i_text %>%
      str_replace_all("_", "$\\\\rightarrow$ ") %>%
      latex2exp::TeX(.)
    ssf_prob_i_map <-
      tm_shape(maine_om) +
        tm_rgb() +
      tm_shape(ssf_prob_i, raster.downsample = FALSE) +
        tm_raster(palette = viridis(20, direction = 1),
          alpha = .8,
          legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_scale_bar(breaks = c(0, 50, 100), text.size = .75,
        position = c(.75, .01)) +
      tm_compass(type = "4star",  show.labels = 1, size = 3,
        position = c(.85, .75)) +
      tm_shape(maine) + tm_borders(col = "black") +  # ME outline overlays grid
      tm_shape(nests_study %>% filter(nest_site != "446R01")) +
      tm_symbols(shape = 8, col = "black", size = .4) +
      tm_layout(#asp = .75,
        title.bg.color = "white",
        title.position = c("left", "top"),
        title.fontfamily = "Latin Modern Roman",
        title = step_type_i_arrow,
        #title.size = ,
        title.snap.to.legend = FALSE,
        legend.position = c(.80,.10),
        legend.outside = FALSE,
        legend.bg.color = "white",
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman",
        frame = FALSE) #+ tm_credits(step_type_arrow, position=c("right","top"))
    tmap_save(tm = ssf_prob_i_map, filename = file.path(map_temp_dir,
      paste0("SSF_Probability_Map_", step_type_i_text, ".svg")), unit = "in",
      dpi = 300, height = 8, width = 6)
  }
}

# All Maine Step-Type Maps Combined --------------------------------------------

# List for output
ssf_tmap_list <- vector(mode = "list", length = 20)

# For Individual Maps
for (i in seq_len(nrow(ssf_fits_best))){
  #writeLines(i)
  step_type_i_numeric <- ssf_fits_best %>% slice(i) %>% pull(step_type) %>%
    str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
      "perch" = "4", "roost" = "5"))
  ssf_prob_file <- list.files(ssf_prob_dir, pattern =paste0(step_type_i_numeric,
    "\\.tif$"), full.names = TRUE)
  ssf_prob_i <- read_stars(ssf_prob_file)
  step_type_i_text <- step_type_i_numeric %>%
    str_replace_all("1", "Cruise") %>%
    str_replace_all("2", "Flight") %>%
    str_replace_all("3", "Nest") %>%
    str_replace_all("4", "Perch") %>%
    str_replace_all("5", "Roost")
  step_type_i_arrow <- step_type_i_text %>%
    str_replace_all("_", "$\\\\rightarrow$ ") %>%
    latex2exp::TeX(.)
  ssf_prob_i_map <-
    tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1),
        legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_layout(asp = .75,
        title.position = c("LEFT", "TOP"),
        title.fontfamily = "Latin Modern Roman",
        title = step_type_i_arrow,
        title.size = .5,
        title.snap.to.legend =  FALSE,
        legend.position = c("RIGHT", "BOTTOM"),
        legend.height = .3,
        legend.title.size = .65,
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman",
        frame = FALSE)
  ssf_prob_i_map
  tmap_position <- switch(step_type_i_numeric,
    "1_1" = 1,  "1_2" = 2,  "1_4" = 3,
    "2_1" = 5,  "2_2" = 6,  "2_4" = 7,  "2_5" = 8,
    "3_1" = 9,  "3_2" = 10, "3_4" = 11, "3_5" = 12,
    "4_1" = 13, "4_2" = 14, "4_4" = 15, "4_5" = 16,
                "5_2" = 18, "5_4" = 19)
  writeLines(as.character(tmap_position))
  ssf_tmap_list[[tmap_position]] <- ssf_prob_i_map
}

tmap_blank <-
  tm_shape(ssf_prob_i, raster.downsample = TRUE) +
  tm_raster(palette = "white", style = "cont") +
  tm_layout(asp = .75, legend.show = FALSE, frame = FALSE)

for (i in seq_len(length(ssf_tmap_list))){
  if(is.null(ssf_tmap_list[[i]])) ssf_tmap_list[[i]] <- tmap_blank
}

# Test arrangement and position of main.title, legend, etc.
test_map_arrangement <- FALSE
if (test_map_arrangement){
  ssf_tmap_arrange_test <- tmap_arrange(
    tmap_blank, tmap_blank, tmap_blank, tmap_blank,
    tmap_blank, tmap_blank, tmap_blank, tmap_blank,
    tmap_blank, tmap_blank, ssf_tmap_list[[1]], tmap_blank,
    tmap_blank, tmap_blank, tmap_blank, tmap_blank,
    tmap_blank, tmap_blank, tmap_blank, tmap_blank,
    ncol = 4)
  ssf_tmap_arrange_test
  tmap_save(tm = ssf_tmap_arrange_test, filename =  file.path(
    "C:/TEMP/SSF_Maps/", paste0("SSF_Probability_Maps_Overview_",
    model_id, ".svg")), unit = "in", dpi = 300, height = 8, width = 6)
}

# Arrange map of probability surfaces
ssf_tmap_arrange <- tmap_arrange(
  ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]],
  ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
  ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
  ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
  ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
  ssf_tmap_list[[16]], ssf_tmap_list[[17]], ssf_tmap_list[[18]],
  ssf_tmap_list[[19]],ssf_tmap_list[[20]], ncol = 4)

tmap_save(tm = ssf_tmap_arrange, filename =  file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", model_id, ".svg")), unit = "in",
  dpi = 300, height = 8, width = 6)

# THIS MAY NOT WORK!!!!!!!!
# If it fails, convert svg to pdf by opening file in Chrome and printing to PDF
rsvg::rsvg_pdf(file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", model_id, ".svg")),
  file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", model_id, ".png")))

file.remove(file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", model_id, ".svg")))

# All Nest Step_Type Maps Combined ---------------------------------------------

tmap_mode("plot")

# Nests
nests_study <- st_as_sf(x = readRDS(nests_study_file), coords = c("long","lat"),
  crs = "+proj=longlat +datum=WGS84") %>% filter(!name %in%
    c("Davis", "Upper")) %>% st_transform(wgs84n19)
nests_sim <- nests_study %>% slice(c(2, 4, 7, 13))

# For Individual Maps
for (j in seq_len(nrow(nests_sim))){
  # Get nest
  nest_j <- nests_sim %>% slice(j)
  nest_j_name <- nest_j %>% pull(name)
  # List for output
  ssf_tmap_list <- vector(mode = "list", length = 20)
  for (i in seq_len(nrow(ssf_fits_best))){
    step_type_i_numeric <- ssf_fits_best %>% slice(i) %>% pull(step_type) %>%
      str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
        "perch" = "4", "roost" = "5"))
    ssf_prob_file <- list.files(ssf_prob_dir, pattern =
      paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
    if (i ==  1){
      # Use "Tmap_baselayers.R" script to get other baselayers
      nest_bbox <- st_as_sfc(st_bbox(st_buffer(nest_j, dist = 6000)))
      nest_buffer <- st_buffer(nest_j, dist = 6000)
      nest_bb_sf <- st_as_sfc(bb(nest_buffer, relative = TRUE, height = 1.35,
        width = 1.35))
      Sys.sleep(60)
      nest_om = read_osm(nest_bb_sf, type = om_nat_geo, zoom = 12)
        #type = "osm") #zoom = 12, minNumTiles=9,
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
      tm_scale_bar(breaks = c(0, 1, 2), text.size = .25, lwd = .25,
        position = c(.825, .02)) +
      tm_compass(type = "4star", text.size = 0.45, show.labels = 1, size = 1.2,
        position = c(.85, .825), lwd = .25) +
      tm_shape(nests_sim) +
      tm_symbols(shape = 8, col = "red", size = .075) +
      tm_layout(#asp = .75,
        title.bg.color = "white",
        title.position = c("left", "top"),
        title.fontfamily = "Latin Modern Roman",
        title = step_type_i_arrow,
        title.size = .4,
        title.snap.to.legend = FALSE,
        legend.title.size = .275,
        legend.text.size = .25,
        legend.position = c(.025,.05),
        legend.outside = FALSE,
        legend.bg.color = "white",
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman",
        frame = FALSE) #+ tm_credits(step_type_arrow, position=c("right","top"))
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
      tm_polygons(col = "white", border.col = "grey") +
    tm_layout(asp = .75, legend.show = FALSE, frame = FALSE)

  for (i in seq_len(length(ssf_tmap_list))){
    if(is.null(ssf_tmap_list[[i]])) ssf_tmap_list[[i]] <- tmap_blank
  }
  # Arrange map of probability surfaces
  ssf_tmap_nest_arrange <- tmap_arrange(
    ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]],
    ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
    ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
    ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
    ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
    ssf_tmap_list[[16]], ssf_tmap_list[[17]], ssf_tmap_list[[18]],
    ssf_tmap_list[[19]],ssf_tmap_list[[20]], ncol = 4)

  tmap_save(tm = ssf_tmap_nest_arrange, filename =file.path("C:/TEMP/SSF_Maps/",
    paste0("SSF_Probability_Maps_Nest_", model_id, "_", nest_j_name, ".svg")),
    unit = "in", dpi = 300, height = 8, width = 6)

  rsvg::rsvg_pdf(file.path("C:/TEMP/SSF_Maps/",
    paste0("SSF_Probability_Maps_Nest_", model_id, "_", nest_j_name, ".svg")),
    file.path("C:/TEMP/SSF_Maps/",
    paste0("SSF_Probability_Maps_Nest_", model_id, "_", nest_j_name, ".pdf")))

  file.remove(file.path("C:/TEMP/SSF_Maps/",
    paste0("SSF_Probability_Maps_Nest_", model_id, "_", nest_j_name, ".svg")))

}

# Generate Covariate Table -----------------------------------------------------

for (i in seq_len(nrow(ssf_fits_best))){
  step_type_i_latex <- ssf_fits_best %>%
    slice(i) %>%
    pull(step_type) %>%
    str_replace_all("cruise", "Cruise") %>%
    str_replace_all("flight", "Flight") %>%
    str_replace_all("nest", "Nest") %>%
    str_replace_all("perch", "Perch") %>%
    str_replace_all("roost", "Roost") #%>%
    #  str_replace_all("_", "$\\\\rightarrow$ ") #%>%
    #  latex2exp::TeX(.)

  covars_all_i <- ssf_fits_best %>%
    slice(i) %>%
    pluck("covars_all", 1)
  covars_all_i_poly2 <-  bind_rows(covars_all_i,
      covars_all_i %>%
      dplyr::filter(poly2) %>%
      mutate(covar = paste0(covar, "^2"))) %>%
    dplyr::select(covar) %>%
    arrange(covar)
  covars_all_i_poly2

  covar_matrix_i <- ssf_fits_best %>%
    slice(i) %>%
    pluck("covar_matrix", 1) %>%
    dplyr::select(covar, poly2, scale_fitted) %>%
    mutate(covar_sigma = paste0(covar, scale_fitted))
  covar_matrix_i_poly2 <-  bind_rows(covar_matrix_i,
      covar_matrix_i %>%
      dplyr::filter(poly2) %>%
      mutate(covar = paste0(covar, "^2")) %>%
      mutate(covar_sigma = paste0(covar_sigma, "^2"))) %>%
    dplyr::select(covar, covar_sigma) %>%
    arrange(covar)
  covar_matrix_i_poly2

  covars_all_matrix_i <- left_join(covars_all_i_poly2, covar_matrix_i_poly2)

  covar_fitted_i <- ssf_fits_best %>%
    slice(i) %>%
    pluck("covar_fitted", 1) %>%
    dplyr::rename(covar_sigma = covar_clean) %>%
    dplyr::select(-covar)

  covar_table_i <- left_join(covars_all_matrix_i, covar_fitted_i) %>%
    filter(!is.na(covar_sigma))

  step_type_col <- c(step_type_i_latex, rep(NA, nrow(covar_table_i)-1))

  covar_table_i <- covar_table_i %>%
    add_column(step_type = step_type_col, .before = 1)

  model_full_i <- covar_table_i %>%
    filter(!is.na(covar_sigma)) %>%
    mutate(coef_i = signif(coef_signif, digits = 3)) %>%
    mutate(model_terms = paste0(coef_i, "*", covar_sigma)) %>%
    pull(model_terms) %>% paste0(., collapse = " + ")

  model_table_i <- tibble(step_type = step_type_i_latex,
    model_full = model_full_i)

  if(i == 1){
    covar_table <- covar_table_i
    model_table <- model_table_i
  } else {
    covar_table <- bind_rows(covar_table, covar_table_i)
    model_table <- bind_rows(model_table, model_table_i)
  }
}
covar_table_final <- covar_table %>%
  dplyr::select(step_type, covar, covar_sigma, coef_signif) %>%
  dplyr::rename("Step Type" = step_type, "Candidates" = covar,
    "Covariates (sigma)" = covar_sigma, "Coefficient" = coef_signif)

model_table_final <- model_table %>%
  dplyr::rename("Step Type" = step_type, Model = model_full)

# Run RMarkdown Report ---------------------------------------------------------

nest_map_files <- list.files(path = "C:/TEMP/SSF_Maps/",  full.names = TRUE,
  pattern = paste0("SSF_Probability_Maps_Nest_", model_id, "*"))

rmarkdown::render(input = "R/RMarkdown/SSF_Map_Table.Rmd",
  params = list(
    covar_table = covar_table_final,
    map_file = file.path("C:/TEMP/SSF_Maps/",
      paste0("SSF_Probability_Maps_Overview_", model_id, ".pdf")),
    model_table = model_table_final,
    nest_map_files = nest_map_files),
  output_dir = "C:/TEMP/SSF_Maps", output_file = paste0("SSF_Fits_", model_id,
    ".pdf"), quiet = TRUE, clean = TRUE)

# Delete unneeded map files
file.remove(file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", model_id, ".pdf")))

for (i in seq_len(length(nest_map_files))){
  file.remove(nest_map_files[i])
}

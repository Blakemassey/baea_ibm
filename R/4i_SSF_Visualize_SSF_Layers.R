###################### SSF_Fit_Clogit_Models ###################################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(tidyverse, ggplot2, lubridate, mapview, raster, sf, stars,
  stringr, tmap, tmaptools, viridis, whitebox)
pacman::p_load(baear, gisr, ibmr)
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE, memfrac = .85)
suppressMessages(extrafont::loadfonts(device="win"))
tmap_mode("view")

# Set to "" for "model_best_fits.rds"
model_id <- NA
#model_id <- "2021-02-11_1814"
nest_index <- c(2, 4, 7, 13)

# Model directories
mod_dir <- "Output/Analysis/SSF/Models"
mod_best_dir <- file.path(mod_dir, "model_fits_best")

# Model files
model_file <- ifelse(is.na(model_id), "model_fits_best.rds", paste0(
  "model_fits_best_", model_id, ".rds"))
fits_best_file <- file.path(mod_best_dir, model_file)

# SSF directories
ssf_raster_dir <- "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"
ssf_prob_model_dir <- ifelse(is.na(model_id), file.path(ssf_raster_dir,
  "Step_Types_Prob"), file.path(ssf_raster_dir, "Step_Types_Prob_Model_ID"))
ssf_prob_model_id_dir <- ifelse(is.na(model_id), file.path(ssf_raster_dir,
  "Step_Types_Prob"), file.path(ssf_prob_model_dir, model_id))
covars_crop_dir <- file.path(ssf_raster_dir, "Covars_Crop")

# Coordinate systems
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Nests
nests_study_file <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/",
  "baea_ibm/Data/Nests/Nests_rds/nests_study.rds")
nests_study <- st_as_sf(x = readRDS(nests_study_file), coords = c("long","lat"),
  crs = wgs84) %>% filter(!name %in% c("Davis", "Upper")) %>%
  st_transform(crs = wgs84n19)
nests_sim <- nests_study %>% slice(c(2, 4, 7, 13))

## Get best fit models table
ssf_fits_best <- readRDS(fits_best_file)

################################# FUNCTIONS ####################################

# Function for ViewProbsNestMap  -----------------------------------------------
ViewProbsNestMap <- function(nest_name,
                             step_types,
                             model_id,
                             opacity = .6){
  nest_i_point <- nests_sim %>% slice(which(nests_sim$name == nest_name))
  nest_i_buffer <- st_buffer(nest_i_point, dist = 6000)
  nest_i_bbox <- st_as_sfc(st_bbox(nest_i_buffer))
  writeLines(paste0("Nest: ", nest_name))
  step_types_list <- vector(mode = "list", length = length(step_types))
  step_types_arrow <- vector(mode = "character", length = length(step_types))
  for (i in seq_len(length(step_types))){
    step_type_i <- ssf_fits_best %>%
      slice(which(ssf_fits_best == step_types[i])) %>%
      pull(step_type)
    step_type_i_numeric <- step_type_i %>%
      str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
      "perch" = "4", "roost" = "5"))
    step_type_i_text <- step_type_i_numeric %>%
      str_replace_all(c("1" = "Cruise", "2" = "Flight", "3" = "Nest",
        "4" = "Perch", "5" = "Roost"))
    step_type_i_arrow_html <- step_type_i_text %>%
      str_replace_all("_", " &#8594; ")
    writeLines(paste0("Step Type: ", as.character(step_type_i_text)))
    ssf_prob_file <- list.files(ssf_prob_model_id_dir,
      pattern = paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
    ssf_prob_i <- raster(ssf_prob_file) #%>% slice(1)
    ssf_prob_i_crop <- crop(ssf_prob_i, nest_i_buffer)
    ssf_prob_i_mask <- mask(ssf_prob_i_crop, nest_i_buffer)
    names(ssf_prob_i_mask) <- step_type_i_text
    step_types_list[[i]] <- ssf_prob_i_mask
    step_types_arrow[[i]] <- step_type_i_arrow_html
  }
  step_types_brick <- raster::brick(step_types_list)
  rm(step_types_list)
  tmap_i <-
    tm_basemap("Esri.NatGeoWorldMap") +  #"OpenStreetMap","Stamen.Watercolor"
    tm_shape(step_types_brick, name = "SSF Probability") +
    tm_raster(alpha = opacity, palette = "viridis", legend.reverse = TRUE,
      n = 10,
      #style = "cont",
      title = paste0(step_types_arrow, "<br>Probability")) +
    tm_facets(free.scales.raster = FALSE, as.layers = FALSE, ncol = 3) +
    #tm_tiles("Stamen.TonerLabels") +
    tm_shape(nests_sim, name = "Nests") +
    tm_dots(col = "red", size = .25) +
    tm_mouse_coordinates()  +
    tm_scale_bar(position=c("left", "bottom")) +
    tm_layout(
      asp = 2,
     # title = names(ssf_prob_i_mask),
      title.position = c("left", "top"),
      legend.title.size = .75,
      legend.text.size = .5,
      title = step_types_arrow, #names(ssf_prob_i_mask),
      title.size = .4,
      title.snap.to.legend = FALSE) +
    tm_view(view.legend.position = c("right","bottom"))

  tmap_i_leaflet <- tmap_leaflet(tmap_i)
  # Update attribution options
  for (k in seq_len(length(tmap_i_leaflet[[1]]))){
    tmap_i_leaflet[[1]][[k]][["children"]][[1]][["x"]][["options"]] <-
      list(attributionControl = FALSE)
  }
  return(tmap_i_leaflet)
}

# Function for ViewProbNestMap  ------------------------------------------------
ViewProbNestMap <- function(nest_name,
                            step_type,
                            model_id,
                            opacity = .6){
  nest_i_point <- nests_sim %>% slice(which(nests_sim$name == nest_name))
  nest_i_buffer <- st_buffer(nest_i_point, dist = 6000)
  nest_i_bbox <- st_as_sfc(st_bbox(nest_i_buffer))
  writeLines(paste0("Nest: ", as.character(nest_name)))

  step_type_index <- which(ssf_fits_best %>% pull(step_type) == step_type)
  step_type_i <- ssf_fits_best %>% slice(step_type_index) %>% pull(step_type)
  step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
    "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
  step_type_i_text <- step_type_i_numeric %>%
    str_replace_all("1", "Cruise") %>%
    str_replace_all("2", "Flight") %>%
    str_replace_all("3", "Nest") %>%
    str_replace_all("4", "Perch") %>%
    str_replace_all("5", "Roost")
  step_type_i_arrow_html <- step_type_i_text %>%
      str_replace_all("_", " &#8594; ")
  writeLines(paste0("Step Type: ", as.character(step_type_i_text)))
  ssf_prob_file <- list.files(ssf_prob_model_id_dir,
    pattern = paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
  ssf_prob_i <- raster(ssf_prob_file) #%>% slice(1)
  ssf_prob_i_crop <- crop(ssf_prob_i, nest_i_buffer)
  ssf_prob_i_mask <- mask(ssf_prob_i_crop, nest_i_buffer)

  tmap_i <-
    tm_basemap("Esri.NatGeoWorldMap") +  #"OpenStreetMap","Stamen.Watercolor"
    tm_shape(ssf_prob_i_mask, name = "SSF Probability",
      is.master = TRUE,
      raster.downsample = FALSE) +
    tm_raster(alpha = opacity, n = 20,
      title = paste0(step_type_i_arrow_html, "<br>Probability"),
      palette = "viridis", legend.reverse = TRUE) +
    tm_shape(nests_sim, name = "Nests") +
    tm_dots(col = "red", size = .35) +
    tm_layout(asp = .5,
      title = nest_name, # Bug in tmap version keeps title from showing
      title.size = 1,
      title.snap.to.legend = FALSE,
      title.position = c("left", "top")) +
    tm_mouse_coordinates()
  tmap_i_leaflet <- tmap_i %>%
    tmap_leaflet
  k <- str_which(map(tmap_i_leaflet$x$calls, "method"), "addRasterImage")
  m <- suppressWarnings(str_which(pluck(tmap_i_leaflet$x$calls, k, "args"),
    "SSF Probability"))
  tmap_i_leaflet$x$calls[[k]]$args[[m]] <- 'Probability'
  # Update attribution options
  tmap_i_leaflet[["x"]][["options"]] <- list(attributionControl = FALSE)
  tmap_i_leaflet_map <- tmap_i_leaflet %>%
    leafem::addImageQuery(ssf_prob_i_mask, project=TRUE, layerId='Probability',
  			digits = 5, prefix='')
  return(tmap_i_leaflet_map)
}


# Function for ViewProbsNestMap  -----------------------------------------------
ViewProbNestsMap <- function(nest_names,
                             step_type,
                             model_id,
                             opacity = .6){
  nests_point <- nests_sim %>% slice(which(nests_sim$name %in% nest_names))
  writeLines(paste0("Nests: ", nest_names))
  nests_buffer <- st_buffer(nests_point, dist = 6000)
  nests_bbox <- st_as_sfc(st_bbox(nests_buffer))
  step_type_i <- step_type
  step_type_i_numeric <- step_type_i %>%
    str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
    "perch" = "4", "roost" = "5"))
  step_type_i_text <- step_type_i_numeric %>%
    str_replace_all(c("1" = "Cruise", "2" = "Flight", "3" = "Nest",
      "4" = "Perch", "5" = "Roost"))
  step_type_i_arrow_html <- step_type_i_text %>%
    str_replace_all("_", " &#8594; ")
  writeLines(paste0("Step Type: ", as.character(step_type_i_text)))
  ssf_prob_file <- list.files(ssf_prob_model_id_dir,
     pattern = paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
  ssf_prob <- raster(ssf_prob_file) #%>% slice(1)
  ssf_prob_crop <- crop(ssf_prob, nests_buffer)
  ssf_prob_mask <- mask(ssf_prob_crop, nests_buffer)
  step_type_list <- vector(mode = "list", length = nrow(nests_point))
  for (i in seq_len(nrow(nests_point))){
    nest_i <- nests_point %>% slice(i)
    nest_i_buffer <- st_buffer(nest_i, dist = 6000)
    ssf_prob_i_crop <- crop(ssf_prob, nest_i_buffer)
    ssf_prob_i_mask <- mask(ssf_prob_i_crop, nest_i_buffer)
    names(ssf_prob_i_mask) <- step_type_i_text
    step_type_list[[i]] <- ssf_prob_i_mask
  }
  tmap_i <-
    tm_basemap("Esri.NatGeoWorldMap") +  #"OpenStreetMap","Stamen.Watercolor"
    tm_shape(nests_buffer, name = "Nests Buffer", is.master = TRUE) +
    tm_borders(col = "grey", alpha = .25) +
    tm_facets(by = "name", as.layers = FALSE, ncol = 2) +
    tm_shape(nests_point, name = "Nests") +
    tm_dots(col = "red", size = .25)
  i <- 1
  while (i <= length(step_type_list)) {
    print(i)
    tmap_i <- tmap_i +
      tm_shape(step_type_list[[i]], name = "SSF Probability") +
      tm_raster(alpha = opacity, palette = "viridis", legend.reverse = TRUE,
        title = paste0(step_type_i_arrow_html, "<br>Probability"),
        breaks = seq(0, 1, .05), legend.show = ifelse(i == 1, TRUE, FALSE))
    i <- i+1
  }
  tmap_i <- tmap_i +
    tm_mouse_coordinates()  +
    tm_scale_bar(position=c("left", "bottom")) +
    tm_layout(
      legend.title.size = .5,
      legend.text.size = .3)
  tmap_i_leaflet <- tmap_i %>%
    tmap_leaflet(height = 1000)

  # Update attribution options
  for (k in seq_len(length(tmap_i_leaflet[[1]]))){
    tmap_i_leaflet[[1]][[k]][["children"]][[1]][["x"]][["options"]] <-
      list(attributionControl = FALSE)
  }
  return(tmap_i_leaflet)
}


########################### GENERATE LAYERS ####################################

# Generate dir for each model_id if they don't already exist -------------------
if(!is.na(model_id) && !dir.exists(ssf_prob_model_id_dir)){
  dir.create(ssf_prob_model_id_dir)
  for (i in seq_len(nrow(ssf_fits_best))){
    step_type_i <- ssf_fits_best %>% slice(i) %>% pull(step_type)
    writeLines(paste0("Creating SSF Layer for: ", step_type_i))
    covars_i <- ssf_fits_best %>% slice(i) %>% pluck("covar_fitted", 1) %>%
      pull("covar_clean") %>% str_remove_all("\\^2") %>% unique()
    # Create Raster_Brick
    covars_list <- vector(mode = "list", length = length(covars_i))
    for (j in seq_along(covars_i)){
      covars_i_j <- covars_i[j]
      writeLines(paste0("covariates: ", covars_i_j))
      raster_file <- file.path(covars_crop_dir, paste0(covars_i_j, ".tif"))
      covars_list[[j]] <- raster(raster_file)
    }
    covars_brick <- raster::brick(covars_list)
    rm(covars_list)
    # Generate formula
    covars_clean_i <- ssf_fits_best %>% slice(i) %>% pluck("covar_fitted", 1)%>%
      pull("covar_clean")
    covars_i <- ifelse(str_detect(covars_clean_i, "\\^2"),
      paste0("covars_brick[['", str_remove_all(covars_clean_i, "\\^2"),"']]^2"),
      paste0("covars_brick[['", covars_clean_i, "']]"))
    coefs_i <- ssf_fits_best %>% slice(i) %>% pluck("covar_fitted", 1) %>%
      pull("coef_signif")
    # Generate formulas
    ssf_formula <- paste0("(", paste0(paste0(coefs_i, "*",covars_i),
      collapse = ") + ("), ")")
    writeLines(ssf_formula)
    # Create value raster and probability layers
    ssf_value_raster <- eval(parse(text = ssf_formula))
    ssf_prob_raster <- raster::calc(ssf_value_raster, fun = boot::inv.logit)
    # Write Rasters to output dir
    step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
      "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
    writeRaster(ssf_prob_raster, file.path(ssf_prob_model_id_dir,
      step_type_i_numeric), format = "GTiff", overwrite = TRUE)
    rm(coefs_i, covars_brick, raster_file, ssf_formula, ssf_value_raster,
      ssf_prob_raster, step_type_i_numeric)
    gc()
  }
}

########################### GENERATE PLOTS #####################################

# Plot Multiple Probability Layers at Nest in Viewer----------------------------
nest_name <- nests_sim %>% slice(3) %>% pull(name)
step_types <- ssf_fits_best %>% slice(1:nrow(ssf_fits_best)) %>% pull(step_type)

probs_nest_map <- ViewProbsNestMap(nest_name, step_types, model_id, opacity =.6)

# Plot Single Probability Layer at Nest Map in Viewer --------------------------
nest_name <- nests_sim %>% slice(1) %>% pull(name)
step_type <- ssf_fits_best %>% slice(3) %>% pull(step_type)

prob_nest_map <- ViewProbNestMap(nest_name, step_type, model_id, opacity = .6)

# Plot Multiple Probability Layers at Nest in Viewer----------------------------
nest_names <- nests_sim %>% slice(1:4) %>% pull(name)
step_type <- ssf_fits_best %>% slice(6) %>% pull(step_type)

prob_nests_map <- ViewProbNestsMap(nest_names, step_type, model_id, opacity =.6)

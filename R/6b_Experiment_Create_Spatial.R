###################### EXPERIMENT CREATE SPATIAL ###############################

## Load packages, scripts, and input parameters --------------------------------
pacman::p_load(arcgisbinding, tidyverse, ggplot2, lubridate, mapview,
  matrixStats, nngeo, purrr, raster, reproducible, sf, stars, tmap, tmaptools,
  units, viridis, whitebox, xtable)
pacman::p_load(baear, gisr, ibmr)
whitebox::wbt_init() # required for WhiteboxTools to work
suppressMessages(extrafont::loadfonts(device = "win"))
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE,
  memfrac = .85)
wbt_version() # check WhiteboxTools version
arc.check_product()

# Set run parameters (IMPORTANT) dist_turbine file and ssf_raster_dir
dist_turbine_raster_file <- "dist_turbines_wilson_c.tif"
ssf_raster_dir <- "C:/ArcGIS/Data/R_Input/EXP/SSF_Rasters_C"
model_id <- "Wilson_C"

# Model directories
mod_dir <- "Output/Analysis/SSF/Models"
mod_best_dir <- file.path(mod_dir, "model_fits_best")

# Model files
fits_best_file <- file.path(mod_best_dir, "model_fits_best.rds")
preds_tbl_file <- file.path(mod_best_dir, "preds_tbl.rds")

# Input directory
input_dir <- "C:/ArcGIS/Data/R_Input/EXP"
baea_covars_crop <- "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters/Covars_Crop"

# SSF directories
covars_crop_dir <- file.path(ssf_raster_dir, "Covars_Crop")
ssf_value_dir <- file.path(ssf_raster_dir, "Step_Types")
ssf_prob_dir <- file.path(ssf_raster_dir, "Step_Types_Prob")
map_temp_dir <- "C:/TEMP/SSF_Maps"

# Maine files
wilson_base_file <- file.path(input_dir, "wilson_base_50km.tif")
wilson_raster_trim_file <- wilson_base_file

# Nests
nests_study_file <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/",
  "baea_ibm/Data/Nests/Nests_rds/nests_study.rds")

# Theme (for LaTeX font)
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

# Coordinate systems
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Move and Crop Covar Files ----------------------------------------------------

ssf_fits_best <- readRDS(fits_best_file)
ssf_fits_best %>% pluck("model_full") %>% unlist()

wilson_raster_trim <- raster(wilson_raster_trim_file)

# For each step_type fit covar, get BAEA covar_crop, crop to Wilson, add to
# Covars_Crop
for (i in 1:nrow(ssf_fits_best)){
  step_type_i <- ssf_fits_best %>% slice(i) %>% pull(step_type)
  writeLines(paste0("Creating SSF Layer for: ", step_type_i))
  covars_i <- ssf_fits_best %>% slice(i) %>% pluck("covar_fitted",
    1) %>% pull("covar_clean") %>% str_remove_all("\\^2") %>% unique()
  for (j in seq_along(covars_i)){
    covars_i_j <- covars_i[j]
    writeLines(paste0("covariates: ", covars_i_j))
    out_file <- file.path(covars_crop_dir, paste0(covars_i_j, ".tif"))
    if(!file.exists(out_file)){
      raster_file <- file.path(baea_covars_crop, paste0(covars_i_j, ".tif"))
      covar_raster <- raster(raster_file)
      covar_raster_crop <- crop(covar_raster, wilson_raster_trim)
      covar_raster_mask <- mask(covar_raster_crop, wilson_raster_trim)
        writeRaster(covar_raster_mask, out_file, format = "GTiff",
        overwrite = TRUE)
    }
  }
}

# Swap Out 'dist_turbine0' File ------------------------------------------------

dist_turbine_raster <- raster(file.path(input_dir, dist_turbine_raster_file))
out_file <- file.path(covars_crop_dir, paste0("dist_turbine0.tif"))
writeRaster(dist_turbine_raster, out_file, format = "GTiff", overwrite = TRUE)

# Create SSF Layers ------------------------------------------------------------

# Groups to update SSF_Raster/Covars_Crop layers (e.g. 1_1.tif)
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
  pull(step_type)

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
    writeRaster(ssf_prob_raster, file.path(ssf_prob_dir, step_type_i_numeric),
      format = "GTiff", overwrite = TRUE)
    rm(coefs_i, covars_brick, raster_file, ssf_formula, ssf_value_raster,
      ssf_prob_raster, step_type_i_numeric)
    gc()
  }
}

# Generate Maps for Nest by Step Type ------------------------------------------

tmap_mode("plot")

# Nests
nests_study <- st_as_sf(x = readRDS(nests_study_file), coords = c("long","lat"),
  crs = "+proj=longlat +datum=WGS84") %>% filter(!name %in%
    c("Davis", "Upper")) %>% st_transform(wgs84n19)
nests_sim <- nests_study %>% slice(c(5))

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
        frame = FALSE)
    if(FALSE) ssf_prob_i_nest_map
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

  #file.remove(file.path("C:/TEMP/SSF_Maps/",
  #  paste0("SSF_Probability_Maps_Nest_", model_id, "_", nest_j_name, ".svg")))

}

## Run RMarkdown report --------------------------------------------------------

nest_map_files <- list.files(path = "C:/TEMP/SSF_Maps/",  full.names = TRUE,
  pattern = paste0("SSF_Probability_Maps_Nest_", model_id, "*"))

rmarkdown::render(input = "R/RMarkdown/SSF_Map.Rmd",
  params = list(
    nest_map_files = nest_map_files),
  output_dir = "C:/TEMP/SSF_Maps", output_file = paste0("SSF_Fits_", model_id,
    ".pdf"), quiet = TRUE, clean = TRUE)

# Delete unneeded map files
for (i in seq_len(length(nest_map_files))){
  file.remove(nest_map_files[i])
}

# This script checks the sim run output and assesses the calibration metrics
# ---------------------------------------------------------------------------- #
# Load packages
pacman::p_load(plyr, dplyr, future, furrr, ggplot2, ggthemes, raster,
  stringr, tibble, tictoc, tidyr, whitebox)
whitebox::wbt_init() # required for WhiteboxTools to work
wbt_version() # check WhiteboxTools version
library(gisr)

# Set options
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE, chunksize=1e9,
  memfrac=.9)

############################ IMPORT RASTERS ####################################

# Ridgeline delineation arguments
agg_factor <- 5
rtp_filter <- 51
reclass_break <- .3
clump_cell_min <- 4
ridge_cell_min <- 2

# Convert arguments
clump_cell_min_area <- ((30*agg_factor)^2)*clump_cell_min
ridge_cell_min_area <- ((30*agg_factor)^2)*ridge_cell_min
reclass_break_str <- str_replace_all(reclass_break, "0\\.", "")

# Source data directories
file_dir <- "C:/ArcGIS/Data/R_Input/BAEA"

# Raster files
elev_file <- file.path(file_dir, "elev_30mc.tif")
elev_agg_file <- file.path(file_dir, paste0("Ridgelines/elev_agg_", agg_factor,
  ".tif"))

rtp_1_file <- file.path(file_dir, "Ridgelines", "rtp_1.tif")
rtp_2_file <- file.path(file_dir, "Ridgelines", "rtp_2.tif")
rtp_3_file <- file.path(file_dir, "Ridgelines", "rtp_3.tif")
rtp_4_file <- file.path(file_dir, "Ridgelines", "rtp_4.tif")
rtp_5_file <- file.path(file_dir, "Ridgelines", "rtp_5.tif")
rtp_6_file <- file.path(file_dir, "Ridgelines", "rtp_6.tif")
rtp_7_file <- file.path(file_dir, "Ridgelines", "rtp_7.tif")
rtp_final_file <- file.path(file_dir, "Ridgelines", paste0("rtp_",
  agg_factor, "_", rtp_filter, "_", reclass_break_str, "_clump_poly.tif"))

ridge_1_file <- file.path(file_dir, "Ridgelines", "ridge_1.tif")
ridge_2_file <- file.path(file_dir, "Ridgelines", "ridge_2.tif")
ridge_3_file <- file.path(file_dir, "Ridgelines", "ridge_3.tif")
ridge_4_file <- file.path(file_dir, "Ridgelines", "ridge_4.tif")
ridge_5_file <- file.path(file_dir, "Ridgelines", "ridge_5.tif")
ridge_6_file <- file.path(file_dir, "Ridgelines", "ridge_6.tif")
ridge_poly_file <- file.path(file_dir, "Ridgelines", "ridge_poly.shp")
ridge_line_file <- file.path(file_dir, "Ridgelines", "ridge_line.shp")

# Elevation raster aggregation
if(!file.exists(elev_agg_file)){
  wbt_aggregate_raster(elev_file, elev_agg_file, agg_factor = agg_factor,
    type = "mean")
}

# Relative Topographic Position ------------------------------------------------
# RTP calculations
wbt_relative_topographic_position(elev_agg_file, rtp_1_file,
  filterx = rtp_filter, filtery = rtp_filter)

# Reclass raster based on break point
wbt_reclass(rtp_1_file, rtp_2_file, paste0("0.0;-1.0;", reclass_break, ";1;",
  reclass_break, ";1.0"))

# Clump cells
wbt_clump(rtp_2_file, rtp_3_file, zero_back = TRUE)

# Calculate area (in cell size)
wbt_raster_area(rtp_3_file, rtp_4_file, out_text = FALSE, units = "map units",
  zero_back = TRUE)

# Reclass clumps by min cell size
clump_stats <- wbt_raster_summary_stats(rtp_4_file, verbose_mode = TRUE)
clump_max_value <- readr::parse_number(clump_stats[which(str_detect(clump_stats,
  "Image maximum"))])
wbt_reclass(rtp_4_file, rtp_5_file, reclass_vals = paste0("0;0;",
  clump_cell_min_area, ";1;", clump_cell_min_area, ";", clump_max_value + 100))

# Reclass clumps below threshold to NoData
wbt_set_nodata_value(input = rtp_5_file, output = rtp_final_file,
  back_value = "0.0")

# Clean up files
file.remove(rtp_1_file, rtp_2_file, rtp_3_file, rtp_4_file, rtp_5_file)

# Find Ridges ------------------------------------------------------------------
# Find ridges
wbt_find_ridges(elev_agg_file, ridge_1_file, line_thin = FALSE)

# Find ridges within clumps
wbt_multiply(ridge_1_file, rtp_final_file, ridge_2_file)

# Clump ridge cells
wbt_clump(ridge_2_file, ridge_3_file, zero_back = TRUE)

# Calculate area of ridge cells
wbt_raster_area(ridge_3_file, ridge_4_file, out_text = FALSE,
  units = "map units", zero_back = TRUE)

ridge_stats <- wbt_raster_summary_stats(ridge_4_file, verbose_mode = TRUE)
ridge_max_value <- readr::parse_number(ridge_stats[which(str_detect(ridge_stats,
  "Image maximum"))])

# Reclass clumps by min cell size
wbt_reclass(ridge_4_file, ridge_5_file, reclass_vals = paste0("0;0;",
  ridge_cell_min_area, ";1;", ridge_cell_min_area, ";", ridge_max_value + 100))

# Reclass clumps below threshold to NoData
wbt_set_nodata_value(ridge_5_file, ridge_6_file, back_value = "0.0")

# Convert to polygons (for easier spatial analysis regarding flight lines)
wbt_raster_to_vector_polygons(ridge_6_file, ridge_poly_file, wd = NULL,
  verbose_mode = FALSE)

# Clean up ridge files
file.remove(ridge_1_file, ridge_2_file, ridge_3_file, ridge_4_file,
  ridge_5_file, ridge_6_file)

# Set crs for ridge polys
elev <- raster(elev_file)
ridge_poly <- sf::read_sf(ridge_poly_file)
sf::st_crs(ridge_poly) <- crs(elev)
sf::st_write(ridge_poly, ridge_poly_file, append=FALSE)



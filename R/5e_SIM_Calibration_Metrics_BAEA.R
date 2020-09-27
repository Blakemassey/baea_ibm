########################### CALIBRATION CHECK ##################################
# This script checks the baea data and assesses the calibration metrics
# ---------------------------------------------------------------------------- #

# Load packages
pacman::p_load(cartography,ctmm, dplyr, fasterize, ggplot2, ggthemes, ggpubr,
  grid, leaflet, lubridate, magick, mapview, move, OpenStreetMap, plotly,
  prettymapr, purrr, raster, rosm, rsvg, sf, s2, tmap, tmaptools, viridis,
  units, webshot, zoo)
pacman::p_load(baear, gisr, ibmr)
suppressMessages(extrafont::loadfonts(device="win"))
set_thin_PROJ6_warnings(TRUE)

# Set options
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE, chunksize=1e9,
  memfrac=.9)

# Plot themes
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))
theme_update(plot.title = element_text(hjust = 0.5))

# Coordinate systems
wgs84 <- 4326 # WGS84 Lat/Long
wgs84n19 <- 32619 # WGS84 UTM 19N

# Functions
MakeLines <- function(x, y, x_end, y_end) {
  st_linestring(matrix(c(x, x_end, y, y_end), 2, 2))
}

# Behavior colors
behavior_colors <- CreateColorsByMetadata(file = file.path("Data/Assets",
  "behavior_colors.csv"), metadata_id = "behavior")

# Input file and Calibration directory
input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
calibration_dir <- "Output/Sim/Calibration"

############################## EMPIRICAL DATA ##################################

## Hydro Distance --------------------------------------------------------------

# Hydro Dist file
hydro_dist_ras <- raster(file.path(input_dir, "hydro_dist_30mc.tif"))

# Create Spatialdataframe of baea w/'Perch' behavior
baea_behavior_org <- readRDS("Data/BAEA/baea_behavior.rds")

baea_behavior <- baea_behavior_org %>%
  dplyr::select(id, datetime, behavior, time_proportion, long_utm, lat_utm,
    nest_site)
baea_perch <- baea_behavior %>% filter(behavior == "Perch")
baea_perch_xy <- baea_perch %>% dplyr::select(long_utm, lat_utm)
baea_perch_sp <- SpatialPointsDataFrame(baea_perch_xy, baea_perch,
  proj4string = CRS(SRS_string = paste0("EPSG:", wgs84n19)), match.ID = TRUE)

# Extract hydro_dist from baea_perch
hydro_dist_crop <- crop(hydro_dist_ras, as(st_as_sfc(st_bbox(baea_perch_sp)),
  "Spatial"), snap = "out")
extent(hydro_dist_crop)

hydro_dist <- raster::extract(hydro_dist_crop, baea_perch_sp, df = FALSE)
baea_perch_dist <- cbind(baea_perch, hydro_dist)

# Save baea_perch_dist file
baea_perch_dist
saveRDS(baea_perch_dist, file.path(calibration_dir, "baea_perch_dist.rds"))

for (i in unique(baea_perch_dist$id)){
  baea_perch_dist_i <- baea_perch_dist %>%
    filter(id == i)
  gg_hydro_dist_i <- ggplot(baea_perch_dist_i) +
    geom_histogram(aes(x = hydro_dist, y =  after_stat(count/sum(count))),
      boundary = 0, binwidth = 30, color = "black",
      fill = behavior_colors["Perch"]) +
    ggtitle(paste0(i , " Perching Sites")) +
    xlab("Hydro Distance Metric (m)") +
    ylab("Proportion of Locations") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(panel.grid.minor.x = element_blank())
  print(gg_hydro_dist_i)
}

gg_baea_dist <- ggplot(baea_perch_dist) +
  geom_histogram(aes(x = hydro_dist, y = after_stat(count/sum(count))),
    boundary = 0, binwidth = 30, color = "black",
    fill = behavior_colors["Perch"]) +
  ggtitle("Empirical Data Perch Locations") +
  xlab("Hydro Distance Metric (m)") +
  ylab("Proportion of Locations") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid.minor.x = element_blank())
gg_baea_dist

saveRDS(gg_baea_dist, file.path(calibration_dir, "gg_baea_dist.rds"))

rm(baea_behavior, baea_perch, baea_perch_xy, baea_perch_sp, hydro_dist_crop,
  hydro_dist, baea_perch_dist_i, gg_hydro_dist_i, gg_baea_dist)

## Ridgeline Flights -----------------------------------------------------------

# Source data directories
file_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
ridge_poly_file <- file.path(file_dir, "Ridgelines", "ridge_poly.shp")
ridge_line_file <- file.path(file_dir, "Ridgelines", "ridge_line.shp")

# Ridgeline Data
ridge_poly <- read_sf(ridge_poly_file) %>%
  st_transform(., crs = CRS(SRS_string = paste0("EPSG:", wgs84n19))) %>%
  st_set_crs(wgs84n19)
#mapview(ridge_poly)

# Import original data
baea_behavior_org <- readRDS("Data/BAEA/baea_behavior.rds")
baea_movements_org <- readRDS("Data/BAEA/baea_movements.rds")

# Get the territorial step data
terr_intervals <- list(
  interval(ymd("2015-03-15"), ymd("2015-08-15")),
  interval(ymd("2016-03-15"), ymd("2016-08-15")),
  interval(ymd("2017-03-15"), ymd("2017-08-15")),
  interval(ymd("2018-03-15"), ymd("2018-08-15")))

# Data = behavioral data of territorial birds during 3-15 to 8-15
baea_movements <- baea_behavior_org %>%
  filter(id %in% c("Ellis", "Sandy", "Musquash", "Hebron")) %>%
  filter(datetime %within% terr_intervals) %>%
  arrange(id, datetime) %>%
  group_by(id) %>%  #  slice(1:150) %>%
  mutate(x_end = lead(long_utm),
         y_end = lead(lat_utm)) %>%
  mutate(behavior_next = lead(behavior)) %>%
  mutate(behavior_behavior = paste(behavior, "->", behavior_next)) %>%
  mutate(step_time2 = lead(datetime) - datetime) %>%
  ungroup(.) %>%
  filter(step_time2 <= 20)  %>%
  filter(!is.na(turn_angle)) %>%
  filter(step_length > 42.43) %>%
  filter(behavior_next != "Nest",
      behavior_behavior != "Roost -> Roost",
      behavior_behavior != "Cruise -> Roost",
      behavior_behavior != "Roost -> Cruise") %>%
  rename(x = long_utm,
    y = lat_utm) %>%
  dplyr::select(id, datetime, behavior, x, y, x_end, y_end, nest_site)

# Create baea_steps and baea_lines (sf objects) for each of the birds
for (i in unique(baea_movements$id)){
  if (i == unique(baea_movements$id)[1]){
    baea_ridge_sum <- baea_movements %>%
      group_by(id) %>%
      summarize(nest_id = last(nest_site), .groups = "drop") %>%
      mutate(total_steps_n = NA_integer_, ridge_steps_n = NA_integer_,
        ridge_steps_prop = NA_real_, quant_05 = NA_real_, quant_95 = NA_real_)
  }
  row_n <- which(baea_ridge_sum$id == i)
  print(paste0("Starting: ", i, " (", row_n, " of ",
    length(unique(baea_movements$id)), ")"))

  baea_steps_i <- baea_movements %>%
    filter(id == i)
  baea_lines_i <- baea_steps_i %>%
    filter(id == i) %>%
    dplyr::select(x, y, x_end, y_end) %>%
    pmap(.f = MakeLines) %>%
    st_as_sfc(crs = wgs84n19) %>%
    {cbind(baea_steps_i, geom = .)} %>%
    st_sf(.) %>%  #slice(1:100) %>%
    tibble::rowid_to_column("row_id")

  baea_lines_i_bb_sf <- st_as_sfc(bb(baea_lines_i, relative = TRUE, height = 4,
      width = 4))

  ridge_poly_crop <- suppressWarnings(st_crop(st_buffer(ridge_poly, dist = 0),
    st_buffer(baea_lines_i_bb_sf, dist = 0))) # buffers fix known topo problems

  # mapview::mapview(baea_lines_i) +
  # mapview::mapview(ridge_poly_crop)

  baea_lines_intersect_df <- st_intersects(baea_lines_i, ridge_poly,
      sparse = TRUE) %>%
    as.data.frame(.) %>%
    as_tibble(.) %>%
    rename(row_id = row.id,
      intersect_poly_id = col.id)

  baea_lines_intersect_sf <- baea_lines_i %>%
    left_join(., baea_lines_intersect_df, by = "row_id") %>%
    mutate(ridge_intersect = !is.na(intersect_poly_id))

  # mapview::mapview(baea_lines_intersect_sf, zcol = "ridge_intersect",
  #   burst = TRUE) +
  # mapview::mapview(ridge_poly_crop)

  intersect_tbl <- baea_lines_intersect_sf %>%
    as_tibble(.)

  intersect_sum <- intersect_tbl %>%
    summarise(total_steps_n = n(),
      ridge_steps_n = sum(!is.na(intersect_poly_id))) %>%
    mutate(id = i) %>%
    mutate(ridge_steps_prop = ridge_steps_n/total_steps_n)

  intersect_prop_random <- rerun(1000, intersect_tbl %>%
    sample_frac(., size = 1, replace = TRUE) %>%
    summarise(total_steps_n = n(),
      ridge_steps_n = sum(!is.na(intersect_poly_id))) %>%
    mutate(id = i) %>%
    mutate(ridge_steps_prop_rand = ridge_steps_n/total_steps_n) %>%
    pull(ridge_steps_prop_rand)) %>%
    unlist(.) %>%
    enframe(.) %>%
    pull(value)

  baea_ridge_sum[row_n, "total_steps_n"] <- intersect_sum %>%
    pull(total_steps_n)
  baea_ridge_sum[row_n, "ridge_steps_n"] <- intersect_sum %>%
    pull(ridge_steps_n)
  baea_ridge_sum[row_n, "ridge_steps_prop"] <- intersect_sum %>%
    pull(ridge_steps_prop)
  baea_ridge_sum[row_n, "quant_05"] <- quantile(intersect_prop_random,
    probs = 0.05)
  baea_ridge_sum[row_n, "quant_95"] <- quantile(intersect_prop_random,
    probs = 0.95)

  rm(row_n, baea_steps_i, baea_lines_i, baea_lines_i_bb_sf, ridge_poly_crop,
    baea_lines_intersect_df, baea_lines_intersect_sf, intersect_sum)

}

# Save ridge-crossing summary data
baea_ridge_sum
saveRDS(baea_ridge_sum, "Output/Sim/Calibration/baea_ridge_sum.rds")

# Clean up objects
rm(file_dir, ridge_poly_file, ridge_line_file, ridge_poly, baea_steps_org,
  baea_behavior_org, baea_movements_org, baea_movements, baea_id_nest,
  terr_intervals)

# Graph ridge-crossing summary data
ggplot() +
  geom_errorbar(data = baea_ridge_sum,
    aes(x = nest_id, y = ridge_steps_prop, ymin = quant_05, ymax = quant_95,
      width = .1)) +
  geom_point(data = baea_ridge_sum, aes(x = nest_id, y = ridge_steps_prop)) +
  theme_minimal() +
  xlab("Nest ID") + ylab("Proportion of Ridge-Crossing Steps")

## Behavior Data ---------------------------------------------------------------

# Import original data
baea_steps_org <- readRDS("Data/BAEA/baea_steps.rds")
baea_behavior_org <- readRDS("Data/BAEA/baea_behavior.rds")

# Get the territorial behavior data
terr_intervals <- list(
  interval(ymd("2015-03-15"), ymd("2015-08-15")),
  interval(ymd("2016-03-15"), ymd("2016-08-15")),
  interval(ymd("2017-03-15"), ymd("2017-08-15")),
  interval(ymd("2018-03-15"), ymd("2018-08-15")))

# Data = behavioral data of territorial birds during 3-15 to 8-15
baea_behavior <- baea_behavior_org %>%
  filter(datetime %within% terr_intervals) %>%
  dplyr::select(id, sex, datetime, time_proportion, behavior)

unique(baea_behavior$sex)
unique(baea_behavior$id)

any(is.na(baea_behavior)) # Should be FALSE

## Consecutive Steps --------------

# Summarize behaviors' consecutive lengths
baea_behavior
cruise <- table(data.frame(unclass(rle(baea_behavior$behavior))) %>%
    filter(values == "Cruise")) %>% as_tibble(.) %>% rename(behavior = values)
flight <- table(data.frame(unclass(rle(baea_behavior$behavior))) %>%
    filter(values == "Flight")) %>% as_tibble(.) %>% rename(behavior =values)
nest <- table(data.frame(unclass(rle(baea_behavior$behavior))) %>%
    filter(values == "Nest")) %>% as_tibble(.) %>% rename(behavior = values)
perch <- table(data.frame(unclass(rle(baea_behavior$behavior))) %>%
    filter(values == "Perch")) %>% as_tibble(.) %>% rename(behavior = values)
roost <- table(data.frame(unclass(rle(baea_behavior$behavior))) %>%
    filter(values == "Roost")) %>% as_tibble(.) %>% rename(behavior = values)
baea_behavior_consecutive <- bind_rows(cruise, flight, nest, perch, roost) %>%
  group_by(behavior) %>%
  mutate(prop = n/sum(n)) %>%
  mutate(total_n = sum(n))

baea_behavior_consecutive

saveRDS(baea_behavior_consecutive, file.path(calibration_dir,
  "baea_behavior_consecutive.rds"))

# Graph behaviors' consecutive lengths
gg_baea_behavior_consecutive <- ggplot(baea_behavior_consecutive) +
    geom_col(aes(x = as.integer(lengths), y = prop, fill = behavior),
      color= "black", width = 1) +
    facet_grid(cols = vars(behavior), scales = "free") +
    scale_fill_manual(values = behavior_colors, name = "Behavior") +
    xlab("Consecutive Behavior State") +
    ylab("Proportion") +
    ggtitle(NULL) +
    theme_minimal() + theme_latex +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 5),
      expand = expansion(mult = c(0, .05)))  +
    theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
    theme(axis.text = element_text(size = 7)) +
    theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
    theme(plot.title = element_text(size = 8, vjust = -2, hjust = 0.5))

gg_baea_behavior_consecutive

## Time Proportions -----------------

# Summarize daily behavior by time_proportion
breaks = 20
baea_behavior_sum <- baea_behavior %>%
  mutate(behavior = factor(behavior)) %>%
  mutate(bins = CutProportion(time_proportion, breaks)) %>%
  mutate(bins_mid = factor(CutProportionMid(time_proportion, breaks))) %>%
  group_by(bins_mid) %>% #  group_by(sex, bins_mid) %>%
  count(behavior) %>%
  mutate(value = n/sum(n)) %>%
  mutate(bins_mid = as.numeric(as.character(bins_mid))) %>%
  ungroup(.) %>%
  arrange(bins_mid) #  arrange(sex, bins_mid)

saveRDS(baea_behavior_sum, file.path(calibration_dir,
  "baea_behavior_sum.rds"))

# Make Plot
point_size = 2; space_legend = .7
gg_baea_behavior_prop <- ggplot(baea_behavior_sum, aes(x = bins_mid, y = value,
    ymax = 1, fill = behavior)) +
  #facet_grid(~ sex, labeller = labeller(sex = Capitalize)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = behavior_colors, name = "Behavior") +
  labs(x = "Daily Period", y = "Behavior Proportion", title = "") +
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
behavior_legend <- get_legend(gg_baea_behavior_prop)
gg_behavior_legend <- as_ggplot(behavior_legend)

### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###

# library(sf)
#
# library(tidyverse)
# library(sf)
# df <- tibble::tribble(~ID,  ~X,  ~Y, ~prevXval, ~prevYval,
#                       1,    -0,  0,     2,    2,
#                       2,   2.5, 2.5,  3,   3)
#
# make_line <- function(X, Y, prevXval, prevYval) {
#     st_linestring(matrix(c(X, prevXval, Y, prevYval), 2, 2))
# }
#
# lines <- df %>%
#     select(X, Y, prevXval, prevYval) %>%
#     pmap(make_line)
#
# pol = st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0))))
# mapview::mapview(pts) +
# mapview::mapview(pol)
#
# (lst = st_intersects(pts, pol))
#
# #> Sparse geometry binary predicate list of length 3, where the predicate was `intersects'
# #>  1: 1
# #>  2: 1
# #>  3: (empty)
# (mat = st_intersects(pts, pol, sparse = FALSE))
# #>       [,1]
# #> [1,]  TRUE
# #> [2,]  TRUE
# #> [3,] FALSE
# # which points fall inside a polygon?
# apply(mat, 1, any)
# #> [1]  TRUE  TRUE FALSE
# lengths(lst) > 0
# #> [1]  TRUE  TRUE FALSE
# # which points fall inside the first polygon?
# st_intersects(pol, pts)[[1]]
# #> [1] 1 2

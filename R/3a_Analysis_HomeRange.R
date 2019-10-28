#------------------------- ANALYSIS HOME RANGES -------------------------------#
# This script is for identifying home ranges and calculating metrics
#------------------------------------------------------------------------------#

## Load Packages, Scripts, etc. ------------------------------------------------
pacman::p_load(ctmm, devtools, fitdistrplus, ggplot2, ggrepel, ggthemes,
  lubridate, mapview, move, tidyverse, raster, reshape2, sf, tmaptools, units,
  zoo)
options(stringsAsFactors = FALSE)
theme_update(plot.title = element_text(hjust = 0.5))

library(baear)
library(gisr)
library(ibmr)
#devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/gisr")

# Coordinate systems
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Landcover rasters
base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")
developed <- raster("C:/ArcGIS/Data/R_Input/BAEA/developed_30mc.tif")
forest <- raster("C:/ArcGIS/Data/R_Input/BAEA/forest_30mc.tif")
open_water <- raster("C:/ArcGIS/Data/R_Input/BAEA/open_water_30mc.tif")
pasture <- raster("C:/ArcGIS/Data/R_Input/BAEA/pasture_30mc.tif")
shrub_herb <- raster("C:/ArcGIS/Data/R_Input/BAEA/shrub_herb_30mc.tif")
wetland <- raster("C:/ArcGIS/Data/R_Input/BAEA/wetland_30mc.tif")

# Elevation raster
elev <- raster("C:/ArcGIS/Data/R_Input/BAEA/elev_30mc.tif")

# Directories
baea_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Data/BAEA"
nests_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Nests/Nests_rds")
turbine_dir <- file.path("C:/ArcGIS/Data/R_Input/BAEA")

wind_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Wind")
maps_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Products/Maps"
report_maps_dir <- file.path("C:/Users/blake/Documents/PhD Program/Reports",
  "BAEA Project - 2018/Maps")

## Get BAEA Data ---------------------------------------------------------------
baea <- readRDS(file="Data/BAEA/baea.rds")

# Remove all non-territorial birds
baea_terr <- baea %>% filter(!id %in% c("Cherryfield", "Davis", "Madagascal",
  "Webb"))

# Table of id and year
table(baea_terr$id, baea_terr$year) # Determine available individual/year combos
sum(table(baea_terr$id, baea_terr$year) > 0) # Total number of indiv/yr combos

# Table of duration and start/end dates of birds' location data
baea_dates <- baea_terr %>%
  group_by(id) %>%
  summarize(start_date = first(date), last_date = last(date), locs = n()) %>%
  mutate(date_period = as.period(interval(start_date, last_date),
    unit = "months")) %>%
  ungroup() %>% as.data.frame()
baea_dates

# Select id and year
table(baea_terr$id, baea_terr$year) # Determine available individual/year combos

# Create tibble of the ids and date ranges to assess for homerange analysis
# yr = year, sd = start_date, ed = end_date
hr <- tibble(id = character(), yr = integer(), sd = character(),
  ed = character())

## Determine territorial periods -----------------------------------------------

# i <- "Branch"
# j <- 2015
# baea_hr_k <- baea_hr %>% filter(id == i & year == k)
# ExportKMLTelemetryBAEA(df = baea_hr_k, file = paste0(i, "_", k, ".kmz"))

# Date ranges for individuals, based on a visual assessment of GPS data
# Branch  - 2015 (1) Starts 2015-07-22, territorial until 2015-08-15
hr <- add_row(hr, id = "Branch", yr = 2015, sd = "07-22", ed = "08-15")
# Branch  - 2016 (1) Territorial between 2016-03-20 // 2016-05-05
hr <- add_row(hr, id = "Branch", yr = 2016, sd = "03-20", ed = "05-05")
# Branch  - 2017 (0) Never territorial.
# Branch  - 2018 (0) Never territorial.
# Folsom  - 2013 (0) Starts 2013-09-23 (too late in year)
# Folsom  - 2014 (0) Ends 2014-03-03 (too early in year)
# Crooked - 2015 (1) Starts 2015-06-23. 07-15 to 08-08
hr <- add_row(hr, id = "Crooked", yr = 2015, sd = "07-15", ed = "08-08")
# Crooked - 2016 (0) Never territorial.
# Ellis   - 2015 (1) 2015-06-08 to 2015-07-21
hr <- add_row(hr, id = "Ellis", yr = 2015, sd = "06-08", ed = "07-21")
# Ellis   - 2016 (1) 2016-03-15 to 2016-08-15 (One night away)
hr <- add_row(hr, id = "Ellis", yr = 2016, sd = "03-15", ed = "08-15")
# Ellis   - 2017 (1) 2017-03-15 to 2017-04-22
hr <- add_row(hr, id = "Ellis", yr = 2017, sd = "03-15", ed = "04-22")
# Ellis   - 2018 (0) Never territorial.
# Ellis   - 2019 (0) Never territorial.
# Eskutas - 2015 (1) 2015-06-05 to 06-11 // 06-13 to 06-23 // 07-23 to 08-14
hr <- add_row(hr, id = "Eskutassis", yr = 2015, sd = "06-05", ed = "06-11")
hr <- add_row(hr, id = "Eskutassis", yr = 2015, sd = "06-13", ed = "06-23")
hr <- add_row(hr, id = "Eskutassis", yr = 2015, sd = "07-23", ed = "08-14")
# Eskutas - 2016 (1) 2016-04-04 to 2015-04-12
hr <- add_row(hr, id = "Eskutassis", yr = 2016, sd = "04-04", ed = "04-12")
# Hebron  - 2015 (1) 2015-08-04 to 2015-08-13
hr <- add_row(hr, id = "Hebron", yr = 2015, sd = "08-04", ed = "08-13")
# Hebron  - 2016 (1) 2016-03-15 to 2016-05-25 // 2016-06-10 to 2016-07-01
hr <- add_row(hr, id = "Hebron", yr = 2016, sd = "03-15", ed = "05-25")
hr <- add_row(hr, id = "Hebron", yr = 2016, sd = "06-10", ed = "07-01")
# Musquash- 2015 (1) 2015-06-26 to 07-25 // 07-31 to 08-07 // 8-11 to 08-15
hr <- add_row(hr, id = "Musquash", yr = 2015, sd = "06-26", ed = "07-25")
hr <- add_row(hr, id = "Musquash", yr = 2015, sd = "07-31", ed = "08-07")
hr <- add_row(hr, id = "Musquash", yr = 2015, sd = "08-11", ed = "08-15")
# Musquash- 2016 (1) 2016-03-15 to 04-29 // 05-09 to 05-27 // 06-01 to 06-07
#   // 06-15 to 06-20 // 06-22 to 07-18 // 07-22 to 07-29 // 08-02 to 08-15
hr <- add_row(hr, id = "Musquash", yr = 2016, sd = "03-15", ed = "04-29")
hr <- add_row(hr, id = "Musquash", yr = 2016, sd = "05-09", ed = "05-27")
hr <- add_row(hr, id = "Musquash", yr = 2016, sd = "06-01", ed = "06-07")
hr <- add_row(hr, id = "Musquash", yr = 2016, sd = "06-15", ed = "06-20")
hr <- add_row(hr, id = "Musquash", yr = 2016,  sd = "06-23", ed = "07-18")
hr <- add_row(hr, id = "Musquash", yr = 2016, sd = "07-22", ed = "07-29")
hr <- add_row(hr, id = "Musquash", yr = 2016, sd = "08-02", ed = "08-15")
# Musquash- 2017 (1) 2017-03-15 to 03-18 // 03-23 to 05-15 // 05-17 to 05-22
#   // 05-24 to 05-29 // 06-06 to 06-13 // 06-16 to 07-27 // 07-31 to 08-15
hr <- add_row(hr, id = "Musquash", yr = 2017, sd = "03-15", ed = "03-18")
hr <- add_row(hr, id = "Musquash", yr = 2017, sd = "03-23", ed = "05-15")
hr <- add_row(hr, id = "Musquash", yr = 2017, sd = "05-17", ed = "05-22")
hr <- add_row(hr, id = "Musquash", yr = 2017, sd = "05-24", ed = "05-29")
hr <- add_row(hr, id = "Musquash", yr = 2017, sd = "06-06", ed = "06-13")
hr <- add_row(hr, id = "Musquash", yr = 2017, sd = "06-16", ed = "07-27")
hr <- add_row(hr, id = "Musquash", yr = 2017, sd = "07-31", ed = "08-15")
# Musquash- 2018 (1) 2018-03-15 to 06-07 // 06-09 to 06-20 // 06-26 to 08-15
hr <- add_row(hr, id = "Musquash", yr = 2018, sd = "03-15", ed = "06-07")
hr <- add_row(hr, id = "Musquash", yr = 2018, sd = "06-10", ed = "06-20")
hr <- add_row(hr, id = "Musquash", yr = 2018, sd = "06-26", ed = "08-15")
# Musquash- 2019 (0) Data ends too early
# Norway  - 2015 (1) Starts 2015-05-15, goes to 08-15
hr <- add_row(hr, id = "Norway", yr = 2015, sd = "05-15", ed = "08-15")
# Onawa   - 2015 (0) Data starts too late
# Onawa   - 2016 (0) Data ends too early
# Philips - 2015 (1) 2015-05-20 to 2015-06-06 / 07-04 to 07-07 / 08-08 to 08-10
hr <- add_row(hr, id = "Phillips", yr = 2015, sd = "05-20", ed = "06-06")
hr <- add_row(hr, id = "Phillips", yr = 2015, sd = "07-04", ed = "07-07")
hr <- add_row(hr, id = "Phillips", yr = 2015, sd = "08-08", ed = "08-10")
# Sandy   - 2015 (1) Starts 2015-06-19 to 07-24 // 07-28 to 08-06 //
#  08-08 to 08-10
hr <- add_row(hr, id = "Sandy", yr = 2015, sd = "06-19", ed = "07-24")
hr <- add_row(hr, id = "Sandy", yr = 2015, sd = "07-28", ed = "08-06")
hr <- add_row(hr, id = "Sandy", yr = 2015, sd = "08-08", ed = "08-10")
# Sandy   - 2016 (1) 2016-03-15 to 08-15
hr <- add_row(hr, id = "Sandy", yr = 2016, sd = "03-15", ed = "08-15")
# Sandy   - 2017 (1) 2017-03-15 to 08-15
hr <- add_row(hr, id = "Sandy", yr = 2017, sd = "03-15", ed = "08-15")
# Sandy   - 2018 (1) 2018-03-15 to 03-24 // 03-27 to 08-15
hr <- add_row(hr, id = "Sandy", yr = 2018, sd = "03-15", ed = "03-24")
hr <- add_row(hr, id = "Sandy", yr = 2018, sd = "03-27", ed = "08-15")
# Sandy   - 2019 (1) 2019-03-15 to 04-25 // 05-26 to 08-15
hr <- add_row(hr, id = "Sandy", yr = 2019, sd = "03-15", ed = "04-25")
hr <- add_row(hr, id = "Sandy", yr = 2019, sd = "05-26", ed = "08-15")
# Sheepscot-2015 (1) Starts 2015-06-20, most nights Branch Pond, goes to 08-15
hr <- add_row(hr, id = "Sheepscot", yr = 2015, sd = "06-20", ed = "08-15")
# Sheepscot-2016 (0) No data after 2016-02
# Three     2015 (1) Starts 2015-06-20, goes to 08-15
hr <- add_row(hr, id = "Three", yr = 2015, sd = "06-20", ed = "08-15")
# Three     2016 (0) Not territorial
# Wilson    2015 (1) Starts 2015-06-05, goes to 08-15
hr <- add_row(hr, id = "Wilson", yr = 2015, sd = "06-05", ed = "08-15")

hr_dates <- hr %>%
  rename(year = yr, start_date = sd, end_date = ed)

for (i in seq_len(nrow(hr_dates))){
  if (i == 1) baea_hr <- baea %>% slice(0)
  hr_dates_i <- hr_dates %>% slice(i)
  id_i <- hr_dates_i %>% pull(id)
  year_i <- hr_dates_i %>% pull(year)
  start_date_i <- hr_dates_i %>% pull(start_date)
  end_date_i <- hr_dates_i %>% pull(end_date)
  baea_i <- baea %>%
    filter(id == id_i) %>%
    filter(year == year_i) %>%
    filter(date >=  ymd(paste0(year_i, "-03-15"))) %>%
    filter(date <=  ymd(paste0(year_i, "-08-15"))) %>%
    filter(date >=  ymd(paste0(year_i, "_", start_date_i))) %>%
    filter(date <=  ymd(paste0(year_i, "_", end_date_i)))
  print(paste0(id_i, " (", year_i, "-", start_date_i, " to ", year_i,
    "-", end_date_i, ")"))
  baea_hr <- baea_hr %>% bind_rows(baea_i)
  rm(hr_dates_i, id_i, year_i, start_date_i, end_date_i, baea_i)
}
saveRDS(baea_hr, "Data/BAEA/baea_homerange.rds")

## Run Spatial Analysis --------------------------------------------------------

table(baea_hr$id, baea_hr$year)
i <- "Branch"; j <- 2015

for (i in unique(baea_hr$id)){
  baea_hr_i <- baea_hr %>% filter(id == i) %>% arrange(datetime)
  for (j in unique(baea_hr_i$year)){
    baea_hr_k <- baea_hr_i %>% filter(year == j)
    print(paste0("ID:", i, "; ", "Year:", j))
    move_k <- move(x = baea_hr_k$long_utm, y = baea_hr_k$lat_utm,
      time = as.POSIXct(baea_hr_k$datetime, format = "%Y-%m-%d %H:%M:%S",
      tz = "NYC"), data = baea_hr_k, proj = wgs84n19, animal = i,
      sensor = "GPS")
    # Compute movement models
    telemetry_k <- as.telemetry(move_k)
    guess_k <- ctmm.guess(telemetry_k, interactive = FALSE)
    fit_k <- ctmm.fit(telemetry_k, guess_k)
    # Compute akde object
    akde_k <- akde(telemetry_k, fit_k)
    ud_95_sf_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.95,
      level = 0.95) %>% st_as_sf(.) %>% slice(2) # extract ML estimate
    ud_50_sf_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.5,
      level = 0.95) %>% st_as_sf(.) %>% slice(2) # extract ML estimate
    baea_sf_k <- st_as_sf(baea_hr_k, coords = c("long_utm", "lat_utm"),
      crs = 32619, agr = "constant")
    # Map of Utilization Distributions
    # mapview(list(ud_95_sf_k, ud_50_sf_k, baea_sf_k),
    #     zcol = list(NULL, NULL, NULL),
    #     legend = list(TRUE, FALSE, FALSE),
    #     homebutton = list(FALSE, TRUE, TRUE))
    baea_k_bb_sf <- st_as_sfc(bb(baea_sf_k, relative = TRUE, height = 3,
      width = 2))
    developed_k_poly <- crop(developed, as_Spatial(baea_sf_k)) %>%
      as(., 'SpatialPolygonsDataFrame') %>% st_as_sf(.) %>%
      filter(developed_30mc == 1) %>%
      group_by(developed_30mc) %>% summarise(cells_n = n()) %>%
      st_buffer(., dist = 0)
    forest_k_poly <- crop(forest, as_Spatial(baea_sf_k)) %>%
      as(., 'SpatialPolygonsDataFrame') %>% st_as_sf(.) %>%
      filter(forest_30mc == 1) %>%
      group_by(forest_30mc) %>% summarise(cells_n = n()) %>%
      st_buffer(., dist = 0)
    open_water_k_poly <- crop(open_water, as_Spatial(baea_sf_k)) %>%
      as(., 'SpatialPolygonsDataFrame') %>% st_as_sf(.) %>%
      filter(open_water_30mc == 1) %>%
      group_by(open_water_30mc) %>% summarise(cells_n = n()) %>%
      st_buffer(., dist = 0)
    pasture_k_poly <- crop(pasture, as_Spatial(baea_sf_k)) %>%
      as(., 'SpatialPolygonsDataFrame') %>% st_as_sf(.) %>%
      filter(pasture_30mc == 1) %>%
      group_by(pasture_30mc) %>% summarise(cells_n = n()) %>%
      st_buffer(., dist = 0)
    shrub_herb_k_poly <- crop(shrub_herb, as_Spatial(baea_sf_k)) %>%
      as(., 'SpatialPolygonsDataFrame') %>% st_as_sf(.) %>%
      filter(shrub_herb_30mc == 1) %>%
      group_by(shrub_herb_30mc) %>% summarise(cells_n = n())  %>%
      st_buffer(., dist = 0)
    wetland_k_poly <- crop(wetland, as_Spatial(baea_sf_k)) %>%
      as(., 'SpatialPolygonsDataFrame') %>% st_as_sf(.) %>%
      filter(wetland_30mc == 1) %>%
      group_by(wetland_30mc) %>% summarise(cells_n = n()) %>%
      st_buffer(., dist = 0)
    # Map of homerange and landcover polygon
    #mapview(open_water_k_poly) + mapview(ud_95_sf_k)
    #mapview(developed_k_poly) + mapview(ud_95_sf_k)
    # Find intersection of homerange and landcover types
    developed_k_intersect <- st_intersection(ud_95_sf_k, developed_k_poly)
    forest_k_intersect <- st_intersection(ud_95_sf_k, forest_k_poly)
    open_water_k_intersect <- st_intersection(ud_95_sf_k, open_water_k_poly)
    pasture_k_intersect <- st_intersection(ud_95_sf_k, pasture_k_poly)
    shrub_herb_k_intersect <- st_intersection(ud_95_sf_k, shrub_herb_k_poly)
    wetland_k_intersect <- st_intersection(ud_95_sf_k, wetland_k_poly)
    # Calculate terrain metrics
    elev_95_k <- crop(elev, as_Spatial(baea_sf_k)) %>%
      mask(., as_Spatial(ud_95_sf_k), inverse = FALSE)
    tpi_95_k <- CalculateTerrainMetric(elev_95_k, size = 3, metric = "tpi")
    tri_95_k <- CalculateTerrainMetric(elev_95_k, size = 3, metric = "tri")
    roughness_95_k <- CalculateTerrainMetric(elev_95_k, size = 3,
      metric ="roughness")
    elev_50_k <- crop(elev, as_Spatial(baea_sf_k)) %>%
      mask(., as_Spatial(ud_50_sf_k), inverse = FALSE)
    tpi_50_k <- CalculateTerrainMetric(elev_50_k, size = 3, metric = "tpi")
    tri_50_k <- CalculateTerrainMetric(elev_50_k, size = 3, metric = "tri")
    roughness_50_k <- CalculateTerrainMetric(elev_50_k, size = 3,
      metric ="roughness")
    # Map of Terrain metrics
    #raster::plot(elev_k);raster::plot(tpi_k);raster::plot(tri_k)
    #raster::plot(roughness_k)
    # Calculate landcover metrics
    hr_land_metrics_k <- tibble(
      id = i, year = j,
      locs = nrow(baea_hr_k),
      ud_95_total = st_area(ud_95_sf_k),
      ud_50_total = st_area(ud_50_sf_k),
      ud_95_developed = GetIntersectionArea(ud_95_sf_k, developed_k_poly),
      ud_95_forest = GetIntersectionArea(ud_95_sf_k, forest_k_poly),
      ud_95_open_water = GetIntersectionArea(ud_95_sf_k,open_water_k_poly),
      ud_95_pasture = GetIntersectionArea(ud_95_sf_k, pasture_k_poly),
      ud_95_shrub_herb = GetIntersectionArea(ud_95_sf_k,shrub_herb_k_poly),
      ud_95_wetland = GetIntersectionArea(ud_95_sf_k, wetland_k_poly),
      ud_50_developed = GetIntersectionArea(ud_50_sf_k, developed_k_poly),
      ud_50_forest = GetIntersectionArea(ud_50_sf_k, forest_k_poly),
      ud_50_open_water = GetIntersectionArea(ud_50_sf_k, open_water_k_poly),
      ud_50_pasture = GetIntersectionArea(ud_50_sf_k, pasture_k_poly),
      ud_50_shrub_herb = GetIntersectionArea(ud_50_sf_k,shrub_herb_k_poly),
      ud_50_wetland = GetIntersectionArea(ud_50_sf_k, wetland_k_poly),
      ud_95_tpi_mean = cellStats(tpi_95_k, stat = "mean"),
      ud_95_tri_mean = cellStats(tri_95_k, stat = "mean"),
      ud_95_roughness_mean = cellStats(roughness_95_k, stat = "mean"),
      ud_95_tpi_sd = cellStats(tpi_95_k, stat = "sd"),
      ud_95_tri_sd = cellStats(tri_95_k, stat = "sd"),
      ud_95_roughness_sd = cellStats(roughness_95_k, stat = "sd"),
      ud_50_tpi_mean = cellStats(tpi_50_k, stat = "mean"),
      ud_50_tri_mean = cellStats(tri_50_k, stat = "mean"),
      ud_50_roughness_mean = cellStats(roughness_50_k, stat = "mean"),
      ud_50_tpi_sd = cellStats(tpi_50_k, stat = "sd"),
      ud_50_tri_sd = cellStats(tri_50_k, stat = "sd"),
      ud_50_roughness_sd = cellStats(roughness_50_k, stat = "sd")) %>%
      mutate_if(function(col) class(col) == 'units', units::set_units,
        value = "km^2") %>%
      mutate_if(function(col) class(col) == 'units', as.numeric) %>%
      mutate(ud_95_developed_prop = ud_95_developed/ud_95_total) %>%
      mutate(ud_95_forest_prop = ud_95_forest/ud_95_total) %>%
      mutate(ud_95_open_water_prop = ud_95_open_water/ud_95_total) %>%
      mutate(ud_95_pasture_prop = ud_95_pasture/ud_95_total) %>%
      mutate(ud_95_shrub_herb_prop = ud_95_shrub_herb/ud_95_total) %>%
      mutate(ud_95_wetland_prop = ud_95_wetland/ud_95_total) %>%
      mutate(ud_50_developed_prop = ud_50_developed/ud_50_total) %>%
      mutate(ud_50_forest_prop = ud_50_forest/ud_50_total) %>%
      mutate(ud_50_open_water_prop = ud_50_open_water/ud_50_total) %>%
      mutate(ud_50_pasture_prop = ud_50_pasture/ud_50_total) %>%
      mutate(ud_50_shrub_herb_prop = ud_50_shrub_herb/ud_50_total) %>%
      mutate(ud_50_wetland_prop = ud_50_wetland/ud_50_total)
    if(i == unique(baea_hr$id)[1] && j == unique(baea_hr_i$year)[1]) {
      hr_land_metrics <- hr_land_metrics_k %>% slice(0)
      tibble(baea = akde_k)
      hr_akde <- tibble(id = NA, year = NA, hr_akde = list(akde_k)) %>%
        slice(0)
      print("Created hr_land_metrics and hr_akde")
    }
    hr_land_metrics <- bind_rows(hr_land_metrics, hr_land_metrics_k)
    hr_akde <- bind_rows(hr_akde, tibble(id = i, year = j,
      hr_akde = list(akde_k)))
    rm(baea_hr_k, move_k, guess_k, fit_k, telemetry_k,
      ud_50_sf_k, ud_95_sf_k, akde_k,
      developed_k_poly, forest_k_poly, open_water_k_poly, pasture_k_poly,
      shrub_herb_k_poly, wetland_k_poly,
      developed_k_intersect, forest_k_intersect, open_water_k_intersect,
      pasture_k_intersect, shrub_herb_k_intersect, wetland_k_intersect,
      elev_95_k, tpi_95_k, tri_95_k, roughness_95_k,
      elev_50_k, tpi_50_k, tri_50_k, roughness_50_k,
      hr_land_metrics_k)
  }
  rm(baea_hr_i)
}
saveRDS(hr_land_metrics,"Output/Analysis/Homerange/hr_land_metrics.rds")
saveRDS(hr_akde, "Output/Analysis/Homerange/hr_akde.rds")


# Hydro Features ---------------------------------------------------------------

## Input Files
nhd_file <- "C:/ArcGIS/Data/Hydrology/NHDH_ME.gdb"
hr_akde <- readRDS(file.path("Output/Analysis/Homerange",
  "hr_akde.rds"))
baea_hr <- readRDS("Data/BAEA/baea_homerange.rds")

waterbody <- st_read(dsn = nhd_file, layer = "NHDWaterbody") %>%
  st_transform(wgs84n19)
waterarea <- st_read(dsn = nhd_file, layer = "NHDArea") %>%
  filter(!FType %in% c(336, 343)) %>%
  st_transform(wgs84n19)

# i <- "Branch"; j <- 2015  # for testing
for (i in unique(hr_akde$id)){
  hr_akde_i <- hr_akde %>% filter(id == i)
  baea_hr_i <- baea_hr %>% filter(id == i)
  for (j in unique(hr_akde_i$year)){
    print(paste0("ID:", i, " - ", "Year:", j))
    hr_akde_k <- hr_akde_i %>% filter(year == j)
    akde_k <- hr_akde_k %>% pull(hr_akde) %>% pluck(1)
    ud_95_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.95,
      level = 0.95)
    ud_95_sf_k <- st_as_sf(ud_95_sp_k) %>% slice(2)
    ud_50_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.5,
      level = 0.95)
    ud_50_sf_k <- st_as_sf(ud_50_sp_k) %>% slice(2)
    waterbody_ud50_rows <- st_intersects(ud_50_sf_k, waterbody) %>% unlist()
    waterbody_ud50 <- waterbody %>% slice(waterbody_ud50_rows)
    waterbody_ud95_rows <- st_intersects(ud_95_sf_k, waterbody) %>% unlist()
    waterbody_ud95 <- waterbody %>% slice(waterbody_ud95_rows)
    waterarea_ud50_rows <- st_intersects(ud_50_sf_k, waterarea) %>% unlist()
    waterarea_ud50 <- waterarea %>% slice(waterarea_ud50_rows)
    waterarea_ud95_rows <- st_intersects(ud_95_sf_k, waterarea) %>% unlist()
    waterarea_ud95 <- waterarea %>% slice(waterarea_ud95_rows)
    # mapview(list(ud_50_sf_k, waterbody_ud50), zcol = list(NULL, NULL),
    #     legend = list(TRUE, TRUE), homebutton = list(TRUE, TRUE))
    # mapview(list(ud_95_sf_k, waterbody_ud95), zcol = list(NULL, NULL),
    #     legend = list(TRUE, TRUE), homebutton = list(TRUE, TRUE))
    # mapview(list(ud_50_sf_k, waterarea_ud50), zcol = list(NULL, NULL),
    #     legend = list(TRUE, TRUE), homebutton = list(TRUE, TRUE))
    # mapview(list(ud_95_sf_k, waterarea_ud95), zcol = list(NULL, NULL),
    #     legend = list(TRUE, TRUE), homebutton = list(TRUE, TRUE))
    waterbody_ud50_area <- waterbody_ud50 %>% st_drop_geometry(.) %>%
      dplyr::summarise(ud_50_waterbody_area = sum(AreaSqKm)) %>%
      slice(1) %>% pull(ud_50_waterbody_area)
    waterbody_ud50_n <- waterbody_ud50 %>% st_drop_geometry(.) %>%
      dplyr::summarise(ud_50_waterbody_n = dplyr::n()) %>%
      slice(1) %>% pull(ud_50_waterbody_n)
    waterbody_ud95_area <- waterbody_ud95 %>% st_drop_geometry(.) %>%
      dplyr::summarise(ud_95_waterbody_area = sum(AreaSqKm)) %>%
      slice(1) %>% pull(ud_95_waterbody_area)
    waterbody_ud95_n <- waterbody_ud95 %>% st_drop_geometry(.) %>%
      dplyr::summarise(ud_95_waterbody_n = dplyr::n()) %>%
      slice(1) %>% pull(ud_95_waterbody_n)
    waterarea_ud50_area <- waterarea_ud50 %>% st_drop_geometry(.) %>%
      dplyr::summarise(ud_50_waterarea_area = sum(AreaSqKm)) %>%
      slice(1) %>% pull(ud_50_waterarea_area)
    waterarea_ud50_n <- waterarea_ud50 %>% st_drop_geometry(.) %>%
      dplyr::summarise(ud_50_waterarea_n = dplyr::n()) %>%
      slice(1) %>% pull(ud_50_waterarea_n)
    waterarea_ud95_area <- waterarea_ud95 %>% st_drop_geometry(.) %>%
      dplyr::summarise(ud_95_waterarea_area = sum(AreaSqKm)) %>%
      slice(1) %>% pull(ud_95_waterarea_area)
    waterarea_ud95_n <- waterarea_ud95 %>% st_drop_geometry(.) %>%
      dplyr::summarise(ud_95_waterarea_n = dplyr::n()) %>%
      slice(1) %>% pull(ud_95_waterarea_n)
    hr_hydro_metrics_k <- tibble(
      id = i,
      year = j,
      ud_50_waterbody_area = waterbody_ud50_area,
      ud_50_waterbody_n = waterbody_ud50_n,
      ud_95_waterbody_area = waterbody_ud95_area,
      ud_95_waterbody_n = waterbody_ud95_n,
      ud_50_waterarea_area = waterarea_ud50_area,
      ud_50_waterarea_n = waterarea_ud50_n,
      ud_95_waterarea_area = waterarea_ud95_area,
      ud_95_waterarea_n = waterarea_ud95_n)
    if(i == unique(baea_hr$id)[1] && j == unique(baea_hr_i$year)[1]) {
      hr_hydro_metrics <- hr_hydro_metrics_k %>% slice(0)
      print("Created hr_hydro_metrics")
    }
    hr_hydro_metrics <- bind_rows(hr_hydro_metrics, hr_hydro_metrics_k)
  }
}
saveRDS(hr_hydro_metrics, "Output/Analysis/Homerange/hr_hydro_metrics.rds")


# Data Review ------------------------------------------------------------------

hr_land_metrics <- readRDS(file.path("Output/Analysis/Homerange",
  "hr_land_metrics.rds"))
hr_hydro_metrics <- readRDS(file.path("Output/Analysis/Homerange",
  "hr_hydro_metrics.rds"))

hr_all_metrics <- left_join(hr_land_metrics, hr_hydro_metrics,
  by = c("id", "year"))

# Home Range Size
hr_area_sum <- hr_all_metrics %>%
  group_by(year, id) %>%
  dplyr::summarize(ud_50_total, ud_95_total, locs = sum(locs))

View(hr_area_sum)
# Eskutassis appears to be an outlier for 50% and 95% UD
remove_list <- c("Eskutassis")

library(dplyr)
hr_area_sum_multi_yr <- hr_area_sum %>%
  filter(!id %in% c(remove_list)) %>%
  group_by(id) %>%
  filter(n() > 1)

mod_50_area_locs <- glm(ud_50_total ~ locs + id, data = hr_area_sum_multi_yr)
summary(mod_50_area_locs)

mod_95_area_locs <- glm(ud_95_total ~ locs + id, data = hr_area_sum_multi_yr)
summary(mod_95_area_locs)

# Check for relationship btwn # GPS locs and UD size
# 50% UD
ggplot(hr_area_sum %>% filter(!id %in% c(remove_list)),
    aes(x = locs, y = ud_50_total, color = id, label = year)) +
  geom_point(size = 2) +
  geom_line(size = 1.5) +
  geom_label_repel( fill = NA, label.size = NA, nudge_y = 0.5) +
  scale_color_brewer(type = "qual", palette = "Paired") +
  guides(color = guide_legend(title = "Eagle\nID")) +
  ggtitle("50% UD")
# No consistent pattern

# 95% UD
ggplot(hr_area_sum %>% filter(!id %in% c(remove_list)),
    aes(x = locs, y = ud_95_total, color = id, label = year)) +
  geom_point(size = 2) +
  geom_line(size = 1.5) +
  geom_label_repel( fill = NA, label.size = NA, nudge_y = 0.5) +
  scale_color_brewer(type = "qual", palette = "Paired") +
  guides(color = guide_legend(title = "Eagle\nID")) +
  ggtitle("95% UD")
# Generally positive correlation

# Range of 50% UD values by ID
ggplot(hr_area_sum %>% filter(!id %in% c(remove_list)),
    aes(x = id, y = ud_50_total, color = id, label = year)) +
  geom_boxplot() +
  geom_point(aes(group=id), position = position_dodge(width=0.75)) +
  scale_color_brewer(type = "qual", palette = "Paired") +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  ggtitle("")

# Range of 95% UD values by ID
ggplot(hr_area_sum %>% filter(!id %in% c(remove_list)),
    aes(x = id, y = ud_95_total, color = id, label = year)) +
  geom_boxplot() +
  geom_point(aes(group=id), position = position_dodge(width=0.75)) +
  scale_color_brewer(type = "qual", palette = "Paired") +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  ggtitle("")

#
hr_all_metrics_sum <- hr_all_metrics %>%
  group_by(id) %>%
  dplyr::summarize(ud_50 = mean(ud_50_total),
            ud_95 = mean(ud_95_total),
            ud_50_developed = mean(ud_50_developed_prop),
            ud_50_forest = mean(ud_50_forest_prop),
            ud_50_open_water = mean(ud_50_open_water_prop),
            ud_50_pasture = mean(ud_50_pasture_prop),
            ud_50_shrub_herb = mean(ud_50_shrub_herb_prop),
            ud_50_waterbody_area = mean(ud_50_waterbody_area),
            ud_50_waterbody_n = mean(ud_50_waterbody_n),
            ud_50_waterarea_area = mean(ud_50_waterarea_area),
            ud_50_waterarea_n = mean(ud_50_waterarea_n),
            ud_50_wetland = mean(ud_50_wetland_prop),
            ud_95_developed = mean(ud_95_developed_prop),
            ud_95_forest = mean(ud_95_forest_prop),
            ud_95_open_water = mean(ud_95_open_water_prop),
            ud_95_pasture = mean(ud_95_pasture_prop),
            ud_95_shrub_herb = mean(ud_95_shrub_herb_prop),
            ud_95_wetland = mean(ud_95_wetland_prop),
            ud_95_waterbody_area = mean(ud_95_waterbody_area),
            ud_95_waterbody_n = mean(ud_95_waterbody_n),
            ud_95_waterarea_area = mean(ud_95_waterarea_area),
            ud_95_waterarea_n = mean(ud_95_waterarea_n),
            locs = mean(locs),
            years = n()) %>%
  mutate(ud_50_prop_sum = ud_50_developed + ud_50_forest + ud_50_open_water +
           + ud_50_pasture + + ud_50_shrub_herb + ud_50_wetland) %>%
  mutate(ud_95_prop_sum = ud_95_developed + ud_95_forest + ud_95_open_water +
           + ud_95_pasture + + ud_95_shrub_herb + ud_95_wetland)

# Variables by UD (50% and 95%)
hr_metrics_50 <- hr_all_metrics %>%
  dplyr::select(-contains("ud_95")) %>%
  set_names(~ str_to_lower(.) %>% str_replace_all("ud_50_", "")) %>%
  melt(., id.var = "id") %>%
  mutate(ud = "50%") %>% map_if(is.factor, as.character)
hr_metrics_95 <- hr_all_metrics %>%
  dplyr::select(-contains("ud_50")) %>%
  set_names(~ str_to_lower(.) %>% str_replace_all("ud_95_", "")) %>%
  melt(., id.var = "id") %>%
  mutate(ud = "95%") %>%
  map_if(is.factor, as.character)

hr_metrics_ud <- bind_rows(hr_metrics_50, hr_metrics_95)
unique(hr_metrics_ud$variable)

# Cover class proportion in home ranges
ggplot(data = hr_metrics_ud %>% filter(str_detect(variable, "_prop"))) +
  geom_boxplot(aes(x = factor(0), y = value, fill = as.factor(ud))) +
  facet_wrap( ~ variable, scales = "free") +
  scale_fill_viridis_d(option = "C", direction = -1) +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  ggtitle("")


saveRDS(hr_all_metrics, file.path("Output/Analysis/Homerange",
  "hr_all_metrics.rds"))

# Mapping Home Ranges ----------------------------------------------------------

hr_akde <- readRDS(file.path("Output/Analysis/Homerange",
  "hr_akde.rds"))
baea_hr <- readRDS("Data/BAEA/baea_homerange.rds")

i <- "Musquash"
j <- 2018

for (i in unique(hr_akde$id)){
  hr_akde_i <- hr_akde %>% filter(id == i)
  baea_hr_i <- baea_hr %>% filter(id == i)
  for (j in unique(hr_akde_i$year)){
    hr_akde_k <- hr_akde_i %>% filter(year == j)
    baea_hr_k <- baea_hr_i %>% filter(year == j)
    akde_k <- hr_akde_k %>% pull(hr_akde) %>% pluck(1)
    ud_95_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.95,
      level = 0.95)
    ud_95_sf_k <- st_as_sf(ud_95_sp_k) %>% slice(2)
    ud_50_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.5,
      level = 0.95)
    ud_50_sf_k <- st_as_sf(ud_50_sp_k) %>% slice(2)
    baea_sf_k <- st_as_sf(baea_hr_k, coords = c("long_utm", "lat_utm"),
      crs = 32619, agr = "constant")
    mapview(list(ud_95_sf_k, ud_50_sf_k, baea_sf_k),
        zcol = list(NULL, NULL, NULL),
        legend = list(TRUE, FALSE, FALSE),
        homebutton = list(FALSE, TRUE, TRUE))
  }
}

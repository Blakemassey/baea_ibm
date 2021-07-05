## This script is for importing GIS datalayers and converting them to the
## proper coordinate reference system (NAD83 UTM N19), extent, and resolution.
## -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

# Load packages
pacman::p_load(dplyr, fasterize, foreign, mapview, plyr, plotKML, raster,
  readtext, rgdal, sf, sp, stringr, whitebox)
whitebox::wbt_init() # required for WhiteboxTools to work
wbt_version() # check WhiteboxTools version
library(gisr)

## ------------------------- DSL LANDCOVER LAYER -------------------  ##########

## Input Files
# Rasters
base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
land_file <- "C:/ArcGIS/Data/DSLland/DSLland_2010_v3.0.tif"

## Output files
# Rasters
landcover_output <- "C:/ArcGIS/Data/R_Input/BAEA/landcover_30mc.tif"

# Legends
landcover_legend_csv <- "C:/ArcGIS/Data/R_Input/BAEA/landcover_legend.csv"

# Colormaps
landcover_clr <- "C:/ArcGIS/Data/R_Input/BAEA/landcover_colormap.clr"

# CRS
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

## Import Raster layers
base <- raster(base_file)
land <- raster(land_file)

# Classification table
land_classify <-  land@data@attributes[[1]]

## Clip, reproject, and crop

# Full Maine extent (for the LCC layers)
xmin = 1900000
xmax = 2268265
ymin = 2484000
ymax = 3023046

# Small subset of Maine extent (for testing purposes)
# xmin = 2100000
# xmax = 2130000
# ymin = 2800000
# ymax = 2830000

maine_extent <- extent(c(xmin, xmax, ymin, ymax))

## Clip 'layer' to Maine extent in the LCC projection
land_clip <- crop(land, maine_extent, progress="text")
rm(maine_extent, xmin, xmax, ymin, ymax)

## Reproject 'layer_clip' to 'base' crs
land_reproject <- projectRaster(land_clip, base, progress="text", method='ngb')

# Crop to extent of 'base' layer
land_30m <- crop(land_reproject, extent(base))

## Reclassify layers
# Reclassifying the 'land' layer by 'formation' (but calling it landcover)
land_classify <- land_classify %>%
  mutate(value = as.integer(FORMATION))
landcover_30m <- subs(land_30m, land_classify, by="ID", which="value",
  progress = "text")

## Mask and Write Rasters
# Mask to extent of the 'base' layer
landcover_30mc <- mask(landcover_30m, base)

## Set crs object (Needed to ensure the whole crs is written to .tif)
crs(landcover_30mc) <- wgs84n19

## Write raster of layer_30mc
writeRaster(landcover_30mc, landcover_output, progress="text", datatype='INT2U',
  overwrite=TRUE)

## Create legend tables and colormaps
## Landcover
# Reclassifying the land layer info into 'landcover_legend'
landcover_legend <- land_classify %>%
  mutate(value = as.integer(FORMATION)) %>%
  mutate(name = as.character(FORMATION)) %>%
  group_by(value) %>%
  summarize(value = unique(value), name = unique(name)) %>%
  mutate(red = NA, green = NA, blue = NA) %>%
  dplyr::select(value, red, green, blue, name) %>%
  arrange(value)

## CAREFUL! Do not overwrite the following file unless absolutely necessary.
## The colors would have to manually entered again.
# write.csv(landcover_legend, landcover_legend_csv, row.names = FALSE)

## At this point, I manually transferred all the RGB colors from the ArcGIS
## symbology layer into the .csv
landcover_legend <- read.csv(landcover_legend_csv, stringsAsFactors=FALSE)

## Create hexidecimal colors from RGB colors and save file
landcover_legend$hex <- rgb(landcover_legend$red, landcover_legend$green,
  landcover_legend$blue, maxColorValue=255)
#write.csv(landcover_legend, landcover_legend_csv, row.names = FALSE)

## Create a "Colormap" for ArcGIS. The only way that a .tif can use
## a colormap is to create an attribute table for the raster
landcover_colormap <- landcover_legend %>%
  dplyr::select(value, red, green, blue)
write.table(landcover_colormap, landcover_clr, sep= " ", col.names=FALSE,
  row.names=FALSE)

## Plot Testing

raster_30mc <- landcover_30mc
legend <- landcover_legend
legend_name <- "Landcover"

library(rasterVis)
library(ggplot2)

# For testing purposes - clip to smaller area
ext <- extent(c(510000, 560000, 5025000, 5065000))
raster_30mc <- crop(raster_30mc, ext, snap='near')

value_vec <- freq(raster_30mc)[,1]
legend_sub <- legend %>% dplyr::filter(value %in% value_vec)

gplot(raster_30mc, maxpixel = 500000) +
  geom_raster(aes(fill=factor(value)), alpha=0.8) + coord_equal() +
    scale_fill_manual(values = legend_sub$hex, labels=legend_sub$name) +
  guides(fill=guide_legend(title=legend_name))

# For maps that have a lot of categorical data, the "maxpixel" value needs to be
# high (>= 500000) or else some of the categories aren't included in the drawing
# of the map and it an can mess up the relationship between the mapped values
# and the legend's name and hex values

######## ------------------ LANDCOVER LAYERS -------------------- ##############

## Input Files
landcover_input <-"C:/ArcGIS/Data/R_Input/BAEA/lc_30mc.tif"
ext <- extent(335000, 668000, 4750000, 5257000)

## Output files
developed_output <- "C:/ArcGIS/Data/R_Input/BAEA/developed_30mc.tif"
forest_output <- "C:/ArcGIS/Data/R_Input/BAEA/forest_30mc.tif"
open_water_output <- "C:/ArcGIS/Data/R_Input/BAEA/open_water_30mc.tif"
pasture_output <- "C:/ArcGIS/Data/R_Input/BAEA/pasture_30mc.tif"
shrub_herb_output <- "C:/ArcGIS/Data/R_Input/BAEA/shrub_herb_30mc.tif"
wetland_output <- "C:/ArcGIS/Data/R_Input/BAEA/wetland_30mc.tif"

# Import rasters and create lc_vx
lc_crop <- crop(raster(landcover_input), ext)
extent(lc_crop)
lc_vx <- velox(lc_crop)

# Create specific landcover layers by coverting type to 1 and all others to 0

## Developed ----
lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% 21:24] <- 0  ## Developed
lc_matrix[lc_matrix >= 1] <- 1
developed <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(developed, developed_output, progress="text", datatype='INT2U',
  overwrite=TRUE, template = ext)
rm(developed)

## Forest ----
lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% 41:43] <- 0  ## Forest
lc_matrix[lc_matrix >= 1] <- 1
forest <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(forest, forest_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
rm(forest)

## Open Water ----
lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% 11] <- 0  ## Open Water
lc_matrix[lc_matrix >= 1] <- 1
open_water <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(open_water, open_water_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
rm(open_water)

## Pasture/Crop ----
lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% c(81, 82)] <- 0  ## Pasture/Crop
lc_matrix[lc_matrix >= 1] <- 1
pasture <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(pasture, pasture_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
rm(pasture)

## Shrub/Herb ----
lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% c(52, 71)] <- 0  ## Shrub/Herb
lc_matrix[lc_matrix >= 1] <- 1
shrub_herb <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(shrub_herb, shrub_herb_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
rm(shrub_herb)

## Wetland ----
lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% c(90, 95)] <- 0  ## Wetland
lc_matrix[lc_matrix >= 1] <- 1
wetland <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(wetland, wetland_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
rm(wetland)

## Distance to Developed ----
developed_30mc <- raster(developed_output)
develop_fun <- function(x) {x[x>=1] <- 1; x[x==0] <- NA; return(x)}
developed_30mc_calc <- calc(developed_30mc, develop_fun)
mapview(developed_30mc_calc)

## Output Files
developed_dist_output <- "C:/ArcGIS/Data/R_Input/BAEA/dist_developed_30mc.tif"

# tic()
# developed_dist_30mc <- distance(developed_30mc_calc, doEdge = TRUE)
# toc()
# Ended up doing above calcuation in ArcGIS - cannot allocate vector length of
# sufficent size

# Rescale max distance to 5000m (New step aadded in 2021-06)
developed_dist_30mc <- raster(developed_dist_output)
developed_dist_30mc[developed_dist_30mc > 5000] <- 5000

# Write raster
writeRaster(developed_dist_30mc, developed_dist_output, progress = "text",
  datatype = 'INT2U', overwrite = TRUE)

## ---------------------------- HYDRO LAYERS ------------------------------ ####

## Input Files
base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
outline_file <- "C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp"
nhd_file <- "C:/ArcGIS/Data/Hydrology/NHDH_ME.gdb"

wgs84n19 <- 32619 # WGS84 UTM 19N
ext <- extent(335000, 668000, 4750000, 5257000)

# Create smaller ext (for testing)
# x_buffer <- 150000
# y_buffer <- 210000
# ext <- extent(335000 + x_buffer, 668000 - x_buffer, 4750000 + y_buffer,
#   5257000 - y_buffer)

## Output Files
hydro_output <- "C:/ArcGIS/Data/R_Input/BAEA/hydro_30mc.tif"

## Import Raster layers
base <- raster(base_file)
#ogrListLayers(nhd_file)

## Water ----

# The 'MaineOcean' feature was created in ArcGIS by using
# BlankPolygon/MaineOutline to create a polygon of the 'negative image' of the
# state outline in the SE corner of the rectangular study area
ocean <- st_read(dsn = nhd_file, layer = "MaineOcean") %>%
  st_transform(wgs84n19)
waterbody <- st_read(dsn = nhd_file, layer = "NHDWaterbody") %>%
  st_transform(wgs84n19)
waterarea <- st_read(dsn = nhd_file, layer = "NHDArea") %>%
  filter(!FType %in% c(336, 343)) %>%
  st_transform(wgs84n19)
# USGS FType classification (For reference on which features to remove)
table(waterbody$FType)
# 390 = Lake/Pond, 436 = Resevoir, 466 = Swamp/Marsh, 493 = Estuary
table(waterarea$FType)
# 312 = Bay/Inlet, 336 = Canal, 343 = Dam/Weir, 364 = Foreshore
# 431 = Rapids, 445 = Sea/Ocean, 460 = Stream/River

ocean_30mc <- fasterize(ocean, base, field = "FCODE")
waterbody_30mc <- fasterize(waterbody, base, field = "FCode")
waterarea_30mc <- fasterize(waterarea, base, field = "FCode")

#plot(ocean_30mc)
rm(base, waterbody, waterarea, ocean)

# using a RasterStack as input
water_stack <- stack(waterbody_30mc, waterarea_30mc, ocean_30mc)
# return a RasterLayer
rm(waterbody_30mc, waterarea_30mc, ocean_30mc)
gc()

hydro_sum <- calc(water_stack, sum, na.rm = TRUE)
rm(water_stack)

water_fun <- function(x) {x[x>=1] <- 1; x[x==0] <- NA; return(x)}
hydro_calc <- calc(hydro_sum, water_fun)
rm(hydro_sum)
plot(hydro_calc)

hydro_30mc <- crop(hydro_calc, ext)
plot(hydro_30mc)

## Write raster of layer_30mc
writeRaster(hydro_30mc, hydro_output, progress = "text", datatype = 'INT2U',
  overwrite = TRUE)

## Distance to Water ----

## Output Files
hydro_dist_output <- "C:/ArcGIS/Data/R_Input/BAEA/dist_hydro_30mc.tif"

# tic()
# hydro_dist_30mc <- distance(develop_dist_30mc, doEdge = TRUE)
# toc()
# Ended up doing above calcuation in ArcGIS - cannot allocate vector length of
# sufficent size

# Rescale max distance to 5000m (New step added in 2021-06)
hydro_dist_30mc <- raster(hydro_dist_output)
hydro_dist_30mc[hydro_dist_30mc > 5000] <- 5000

# Write raster
writeRaster(hydro_dist_30mc, hydro_dist_output, progress = "text",
  datatype = 'INT2U', overwrite = TRUE)

## Land Layer ----
hydro_dist_file <- "C:/ArcGIS/Data/R_Input/BAEA/hydro_dist_30mc.tif"
land_file <- "C:/ArcGIS/Data/R_Input/BAEA/land_30mc.tif"

wbt_reclass(hydro_dist_file, land_file, "0.0;0.0;0.0;1.0;0.1;100000000000",
  verbose_mode = TRUE)

## ----------------------------- WIND LAYERS ------------------------------ ####

## Wind Classes ----

# Input
wind_class <- st_read(file.path("C:/ArcGIS/Data/Wind",
  "/Maine_Wind_High_Resolution/maine_50mwind.shp")) %>%
  st_transform(wgs84n19)

# Output File
wind_class_output <- "C:/ArcGIS/Data/R_Input/BAEA/wind_class_30mc.tif"

wind_class_30mc <- fasterize(wind_class, base, field ="WPC")
#wind_class_30mc <- raster(wind_class, template = base)
writeRaster(wind_class_30mc, wind_class_output, progress = "text",
  datatype = 'INT2U', overwrite = TRUE)

wind_class_30mc2 <- crop(wind_class_30mc,
  extent(c(xmin(wind_class_30mc) + 120000,
           xmax(wind_class_30mc) - 120000,
           ymin(wind_class_30mc) + 220000,
           ymax(wind_class_30mc) - 220000)))
mapview(wind_class_30mc2)

## Wind Turbines ----

# Input Files
uswtdb_file <- file.path("C:/ArcGIS/Data/Wind/USGS_Wind_Turbine_2018-12-21",
  "uswtdb_v1_2_20181001.shp")

# Output Files
wt_shp_output <- "C:/ArcGIS/Data/R_Input/BAEA/wind_turbines.shp"

# Import Shapefile and Subset Data
uswtdb <- st_read(uswtdb_file)
wt <- uswtdb %>% filter(t_state == "ME") %>% st_transform(crs = wgs84n19)
table(wt$p_year)
mapview(wt)

st_write(wt, wt_shp_output, delete_layer = TRUE) # overwrites

## Wind Turbines Distance ----

# Output File
wt_rast_output <- "C:/ArcGIS/Data/R_Input/BAEA/windturb_30mc.tif"
wt_dist_output <- "C:/ArcGIS/Data/R_Input/BAEA/dist_turbine_30mc.tif"

wind_turbines_30mc <- rasterize(wt, raster = base, field = 'case_id')
writeRaster(wind_turbines_30mc, wt_rast_output, progress = "text",
  datatype = 'INT2U', overwrite = TRUE)

# Calcuation done in ArcGIS - cannot allocate vector length of sufficent size
# tic()
# turbine_dist_30mc <- distance(wind_turbines_30mc, doEdge = TRUE)
# toc()

# Rescale max distance to 20km (New step added in 2021-06)
turbine_dist_30mc <- raster(wt_dist_output)
turbine_dist_30mc[turbine_dist_30mc > 20000] <- 20000
turbine_dist_30mc[is.na(turbine_dist_30mc[])] <- 20000
plot(turbine_dist_30mc)

# Write raster
writeRaster(turbine_dist_30mc, wt_dist_output, progress = "text",
  datatype = 'INT2U', overwrite = TRUE)

######## ---------------------- TOPOGRAPHIC LAYERS ----------------- ###########

suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(spatialEco))
suppressPackageStartupMessages(library(stringr))

## Elevation ----

# Input Files
# Original ELEVATION DATA (1/3 ArcSecond) COMPILATION WAS DONE in Python
# Script (C:\Work\Python\Scripts\gispy\Create_Elevation_30mc) with arcpy tools
base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
elev_file <- "C:/ArcGIS/Data/Elevation/Elevation/elev_30mc.tif"
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N
ext <- extent(335000, 668000, 4750000, 5257000)

# Output Files
elev_output <- "C:/ArcGIS/Data/R_Input/BAEA/elev_30mc.tif"

# Import Raster Layer
elev <- raster(elev_file)

# Reproject 'layer_clip' to 'base' crs
elev_reproject <- projectRaster(elev, crs=wgs84n19, progress="text",
  method='ngb', filename = elev_output)
elev_ <- raster(elev_output)

# Crop to extent of 'base' layer
elev_30mc <- crop(elev_reproject, ext)

## Write raster of layer_30mc
writeRaster(elev_30mc, elev_output, progress = "text", datatype = 'INT2U',
  overwrite = TRUE)

## Slope ----

# Output Files
slope_output <- "C:/ArcGIS/Data/R_Input/BAEA/slope_30mc.tif"

elev_30mc <- raster::raster(elev_output)
slope_30mc <- raster::terrain(elev_30mc, opt = "slope", unit = "degrees")

## Write raster of layer_30mc
writeRaster(slope_30mc, slope_output, progress = "text", datatype = 'INT2U',
  overwrite = TRUE)

## Aspect ----

# Output Files
aspect_output <- "C:/ArcGIS/Data/R_Input/BAEA/aspect_30mc.tif"

elev_30mc <- raster(elev_output)
aspect_30mc <- terrain(elev_30mc, opt = "aspect", unit = "radians")

## Write raster of layer_30mc
writeRaster(aspect_30mc, aspect_output, progress = "text", datatype = 'INT2U',
  overwrite = TRUE)

## Eastness / Northness ----

# Output Files
eastness_output <- "C:/ArcGIS/Data/R_Input/BAEA/eastness_30mc.tif"
northness_output <- "C:/ArcGIS/Data/R_Input/BAEA/northness_30mc.tif"

east_fun <- function(x) {sin_x <- sin(x); return(sin_x)}
eastness_30mc <- calc(aspect_30mc, east_fun)

north_fun <- function(x) {cos_x <- cos(x); return(cos_x)}
northness_30mc <- calc(aspect_30mc, north_fun)

eastness_30mc <- raster(eastness_output)
eastness_30mc2 <- crop(eastness_30mc,
  extent(c(xmin(eastness_30mc) + 120000,
           xmax(eastness_30mc) - 120000,
           ymin(eastness_30mc) + 220000,
           ymax(eastness_30mc) - 220000)))
mapview(eastness_30mc2)

northness_30mc2 <- crop(northness_30mc,
  extent(c(xmin(northness_30mc) + 120000,
           xmax(northness_30mc) - 120000,
           ymin(northness_30mc) + 220000,
           ymax(northness_30mc) - 220000)))
mapview(northness_30mc2)

## Write raster of layer_30mc
writeRaster(eastness_30mc, eastness_output, progress = "text",
  datatype = 'FLT4S', overwrite = TRUE)
writeRaster(northness_30mc, northness_output, progress = "text",
  datatype = 'FLT4S', overwrite = TRUE)

## Roughness, TPI, TRI ----
# These files are NOT used directly in the analysis - they are only used for
# visualization purposes. The metrics are calculated during in ModelFit_SSF.R

# Roughness: difference between the maximum and the minimum value of a cell
#  and its surrounding cells.
# Topographic Position Index (TPI): is the difference between the value of a
#  cell and the mean value of its surrounding cells.
# Terrain Ruggedness Index (TRI): the mean of the absolute differences between
#  the value of a cell and the value of its surrounding cells.

rough_dir <- "C:/ArcGIS/Data/Elevation/Terrain_Metrics/Roughness"
tpi_dir <-"C:/ArcGIS/Data/Elevation/Terrain_Metrics/Topographic_Postition_Index"
tri_dir <- "C:/ArcGIS/Data/Elevation/Terrain_Metrics/Terrain_Ruggedness_Index"
r_input <- "C:/ArcGIS/Data/R_Input/BAEA"

tri_scale_meters <- c(30, 60) #seq(30, 300, by = 30)
tri_windows <- ((tri_scale_meters/30)*2)+1

# Needed to create 3 over-lapping rasters for initial data analysis (cannot
# allocate vector sufficient for an analysis run on the entire state)

x_min <- xmin(elev)
x_max <- xmax(elev)
y_min <- ymin(elev)
y_max <- ymax(elev)
(y_half <- (ymax(elev) - ymin(elev))*(1/2))
(y_third <- (ymax(elev) - ymin(elev))*(1/3))

elev_1 <- crop(elev, extent(x_min, x_max, y_min, y_max - y_half))
elev_2 <- crop(elev, extent(x_min, x_max, y_min + y_third,
  y_min + y_third + y_half))
elev_3 <- crop(elev, extent(x_min, x_max, y_min + y_half, y_max))
rm(elev)

for(i in seq_along(tri_windows)){
  tri1_name <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_1.tif"))
  print(paste0("Working on: ", basename(tri1_name)))
  tri_1 <- CalculateTerrainMetric(elev_1, tri_windows[i], metric = "tri")
  writeRaster(tri_1, file.name = tri1_name, overwrite = TRUE)
  rm(tri_1, tri1_name)
}
rm(elev_1)

for(i in seq_along(tri_windows)){
  tri2_name <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_2.tif"))
  print(paste0("Working on: ", basename(tri2_name)))
  tri_2 <- CalculateTerrainMetric(elev_2, tri_windows[i], metric = "tri")
  writeRaster(tri_2, file.name = tri1_name, overwrite = TRUE)
  rm(tri_2, tri2_name)
}
rm(elev_2)

for(i in seq_along(tri_windows)){
  tri3_name <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_3.tif"))
  print(paste0("Working on: ", basename(tri3_name)))
  tri_3 <- CalculateTerrainMetric(elev_3, tri_windows[i], metric = "tri")
  writeRaster(tri_3, file.name = tri1_name, overwrite = TRUE)
  rm(tri_3, tri3_name)
}
rm(elev_3)

for(i in seq_along(tri_windows)){
  tri_1 <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_1.tif"))
  tri_2 <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_2.tif"))
  tri_3 <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_3.tif"))
  tri_30mc <-  file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_30mc.tif"))
  merge_132 <- merge(raster(tri_1), raster(tri_3), raster(tri_2),
    filename=tri_30mc, overwrite = TRUE)
}

## ---------------------------- ROADS LAYERS ------------------------------ ####

## Input Files
base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
outline_file <- "C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp"
roads_file <- "C:/ArcGIS/Data/Transportation/Maine_State_GDB.gdb"

wgs84n19 <- 32619 # WGS84 UTM 19N
ext <- extent(335000, 668000, 4750000, 5257000)

# Create smaller ext (for testing)
# x_buffer <- 150000
# y_buffer <- 210000
# ext <- extent(335000 + x_buffer, 668000 - x_buffer, 4750000 + y_buffer,
#   5257000 - y_buffer)

## Output Files
road_output <- "C:/ArcGIS/Data/R_Input/BAEA/road_30mc.tif"
road_dist_output <- "C:/ArcGIS/Data/R_Input/BAEA/dist_road_30mc.tif"

## Import Raster layers
base <- raster(base_file)
ogrListLayers(roads_file)

## Roads ----
roads <- st_read(dsn = roads_file, layer = "Trans_RoadSegment") %>%
  filter(TNMFRC %in% c(1, 2, 3, 4)) %>%
  st_transform(wgs84n19) %>%
  dplyr::select(TNMFRC, FULL_STREET_NAME) %>%
  mutate(road = 1)

st_write(roads, file.path(dirname(roads_file), "roads.shp"), overwrite = TRUE)

wbt_vector_lines_to_raster(file.path(dirname(roads_file), "roads.shp"),
  road_output, field = "road", nodata = 0, base = base_file,
  verbose_mode = TRUE)

## Distance to Roads ----
wbt_euclidean_distance(road_output, road_dist_output, verbose_mode = FALSE)

# Rescale max distance to 20km (New step added in 2021-06)
road_dist_30mc <- raster(road_dist_output)
road_dist_30mc[road_dist_30mc > 5000] <- 5000

# Write raster
writeRaster(road_dist_30mc, road_dist_output, progress = "text",
  datatype = 'INT2U', overwrite = TRUE)

######## ----------------------- RIDGE LINES ----------------------- ###########
# Set options
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE, chunksize=1e9,
  memfrac=.9)

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
rtp_final_file <- file.path(file_dir, "Ridgelines", paste0("rtp_",
  agg_factor, "_", rtp_filter, "_", reclass_break_str, "_clump_poly.tif"))

ridge_1_file <- file.path(file_dir, "Ridgelines", "ridge_1.tif")
ridge_2_file <- file.path(file_dir, "Ridgelines", "ridge_2.tif")
ridge_3_file <- file.path(file_dir, "Ridgelines", "ridge_3.tif")
ridge_4_file <- file.path(file_dir, "Ridgelines", "ridge_4.tif")
ridge_5_file <- file.path(file_dir, "Ridgelines", "ridge_5.tif")
ridge_6_file <- file.path(file_dir, "Ridgelines", "ridge_6.tif")
ridge_poly_file <- file.path(file_dir, "Ridgelines", "ridge_poly.shp")
ridge_poly_kml_file <- file.path(file_dir, "Ridgelines", "KMLs",
  "ridge_poly.kml")
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

# Write to KML
ridge_poly_wgs84 <- ridge_poly %>%
  st_transform(4326) %>%
  select(Description = FID)

ridge_poly_kml_file <- file.path(file_dir, "Ridgelines", "KMLs",
  "ridge_poly.kml")

st_write(ridge_poly_wgs84, ridge_poly_kml_file, driver = "kml", append = FALSE)

# Update KML Colors (remove outline and fill with yellow)
kml_raw <- readtext(ridge_poly_kml_file)
kml_raw_line <- str_replace_all(kml_raw,
  "<Style><LineStyle><color>ff0000ff</color></LineStyle>",
  "<Style><LineStyle><width>0</width><color>ffffffff</color></LineStyle>")
kml_raw_poly <- str_replace_all(kml_raw_line,
  "<PolyStyle><fill>0</fill></PolyStyle></Style>",
  "<PolyStyle><color>ff00ffff</color><fill>1</fill></PolyStyle></Style>")
fileConn <- ridge_poly_kml_file
writeLines(kml_raw_poly, fileConn)
close(fileConn)

# IMPORTANT NOTE ----
# The colors of the KML can also be reconfigured in Google Earth under the kml's
# 'properties' drop-down menu and then saved as a new file.


################################  OLD CODE  ####################################

# maine_buff <- st_read(outline_file) %>% st_transform(wgs84n19)
# maine_crop <- st_crop(maine_buff, ocean)
# rm(maine_buff)
# maine_raster <- fasterize(maine_crop, ocean_raster)
# rm(maine_crop)
# ocean_30mc <- mask(ocean_raster, maine_raster, inverse = TRUE)
# rm(maine_raster)
#
## ------------------------------ LCC LAYERS ------------------------------ ###
#
# ## Input Files
# # Rasters
# base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
# elu_file <- "C:/ArcGIS/Data/Ecological_Land_Units/elu30reg1108"
# habitat_file <- "C:/ArcGIS/Data/Terrestrial_Habitat/ne-ca_hab815"
# iei_file <- "C:/ArcGIS/Data/Index_Ecological_Integrity/iei-r_2010_v3.1.tif"
# land_file <- "C:/ArcGIS/Data/DSLland/DSLland_2010_v3.0.tif"
#
# ## Output files
# # Rasters
# eluform_output <- "C:/ArcGIS/Data/R_Input/BAEA/eluform_30mc.tif"
# habitat_output <- "C:/ArcGIS/Data/R_Input/BAEA/habitat_30mc.tif"
# iei_output <- "C:/ArcGIS/Data/R_Input/BAEA/iei_30mc.tif"
# landcover_output <- "C:/ArcGIS/Data/R_Input/BAEA/landcover_30mc.tif"
#
# # Legends
# eluform_legend_csv <- "C:/ArcGIS/Data/R_Input/BAEA/eluform_legend.csv"
# habitat_legend_csv <- "C:/ArcGIS/Data/R_Input/BAEA/habitat_legend.csv"
# iei_legend_csv <- "C:/ArcGIS/Data/R_Input/BAEA/iei_legend.csv"
# landcover_legend_csv <- "C:/ArcGIS/Data/R_Input/BAEA/landcover_legend.csv"
#
# # Colormaps
# eluform_clr <- "C:/ArcGIS/Data/R_Input/BAEA/eluform_colormap.clr"
# habitat_clr <- "C:/ArcGIS/Data/R_Input/BAEA/habitat_colormap.clr"
# iei_clr <- "C:/ArcGIS/Data/R_Input/BAEA/iei_colormap.clr"
# landcover_clr <- "C:/ArcGIS/Data/R_Input/BAEA/landcover_colormap.clr"
#
# # CRS
# wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N
#
# ## Import Raster layers
# base <- raster(base_file)
# elu <- raster(elu_file)
# habitat <- raster(habitat_file)
# iei <- raster(iei_file)
# land <- raster(land_file)
#
# # Classification table
# elu_classify <- elu@data@attributes[[1]]
# habitat_classify <- habitat@data@attributes[[1]]
# land_classify <-  land@data@attributes[[1]]
#
# ## Clip, reproject, and crop
#
# # Full Maine extent (for the LCC layers)
# xmin = 1900000
# xmax = 2268265
# ymin = 2484000
# ymax = 3023046
#
# # Small subset of Maine extent (for testing purposes)
# # xmin = 2100000
# # xmax = 2130000
# # ymin = 2800000
# # ymax = 2830000
#
# maine_extent <- extent(c(xmin, xmax, ymin, ymax))
#
# ## Clip 'layer' to Maine extent in the LCC projection
# elu_clip <- crop(elu, maine_extent, progress="text")
# habitat_clip <- crop(habitat, maine_extent, progress="text")
# iei_clip <- crop(iei, maine_extent, progress="text")
# land_clip <- crop(land, maine_extent, progress="text")
# rm(maine_extent, xmin, xmax, ymin, ymax)
#
# ## Reproject 'layer_clip' to 'base' crs
# elu_reproject <- projectRaster(elu_clip, base, progress="text", method='ngb')
# habitat_reproject<-projectRaster(habitat_clip,base,progress="text",method='ngb')
# iei_reproject <- projectRaster(iei_clip, base,progress="text",method='bilinear')
# land_reproject <- projectRaster(land_clip, base, progress="text", method='ngb')
#
# # Crop to extent of 'base' layer
# elu_30m <- crop(elu_reproject, extent(base))
# habitat_30m <- crop(habitat_reproject, extent(base))
# iei_30m <- crop(iei_reproject, extent(base))
# land_30m <- crop(land_reproject, extent(base))
#
# ## Reclassify layers
# # Reclassifying the 'elu' layer by 'formation'
# elu_classify <- elu_classify %>%
#   mutate(value = as.integer(LF_TYPE))
# eluform_30m <- subs(elu_30m, elu_classify, by="ID", which="value",
#   progress = "text")
#
# # Reclassifying the 'habitat' layer by 'macrogroup'
# habitat_classify <- habitat_classify %>%
#   mutate(value = as.integer(MACR_2015))
# habitat_30m <- subs(habitat_30m, habitat_classify, by="ID", which="value",
#   progress = "text")
#
# # Reclassifying the 'land' layer by 'formation' (but calling it landcover)
# land_classify <- land_classify %>%
#   mutate(value = as.integer(FORMATION))
# landcover_30m <- subs(land_30m, land_classify, by="ID", which="value",
#   progress = "text")
#
# ## Mask and Write Rasters
# # Mask to extent of the 'base' layer
# eluform_30mc <- mask(eluform_30m, base)
# habitat_30mc <- mask(habitat_30m, base)
# iei_30mc <- mask(iei_30m, base)
# landcover_30mc <- mask(landcover_30m, base)
#
# ## Set crs object (Needed to ensure the whole crs is written to .tif)
# crs(eluform_30mc) <- wgs84n19
# crs(habitat_30mc) <- wgs84n19
# crs(iei_30mc) <- wgs84n19
# crs(landcover_30mc) <- wgs84n19
#
# ## Write raster of layer_30mc
# writeRaster(eluform_30mc, eluform_output, progress="text", datatype='INT2U',
#   overwrite=TRUE)
# writeRaster(habitat_30mc, habitat_output, progress="text", datatype='INT2U',
#   overwrite=TRUE)
# writeRaster(iei_30mc, iei_output, progress="text", datatype='FLT4S',
#   overwrite=TRUE)
# writeRaster(landcover_30mc, landcover_output, progress="text", datatype='INT2U',
#   overwrite=TRUE)
#
# ## Create legend tables and colormaps
# # Ecological_Land_Units
# # Reclassifying the land layer info into 'eluform_legend'
# eluform_legend <- elu_classify %>%
#   mutate(value = as.integer(LF_TYPE)) %>%
#   mutate(name = as.character(LFTYPE_DESC)) %>%
#   group_by(value) %>%
#   summarize(value = unique(value), name = unique(name)) %>%
#   mutate(red = NA, green = NA, blue = NA) %>%
#   dplyr::select(value, red, green, blue, name) %>%
#   arrange(value)
#
# ## CAREFUL! Do not overwrite the following file unless absolutely necessary.
# ## The colors would have to be manually entered again.
# #write.csv(eluform_legend, eluform_legend_csv, row.names = FALSE)
#
# ## At this point, I manually transferred all the RGB colors from the ArcGIS
# ## symbology layer into the .csv
# eluform_legend <- read.csv(eluform_legend_csv, stringsAsFactors=FALSE)
#
# # ## Create hexidecimal colors from RGB colors and save file
# eluform_legend$hex <- rgb(eluform_legend$red,
#   eluform_legend$green, eluform_legend$blue, maxColorValue=255)
# #write.csv(eluform_legend, eluform_legend_csv, row.names = FALSE)
#
# ## This is to create a "Colormap" for ArcGIS. The only way that a .tif can use
# ## a Colormap if to create an attribute table for the raster (Google it)
# eluform_colormap <- eluform_legend %>%
#   dplyr::select(value, red, green, blue)
# write.table(eluform_colormap, eluform_clr, sep= " ", col.names=FALSE,
#    row.names=FALSE)
#
# ## Habitat
# # Reclassifying the habitat layer info into 'habitat_legend'
# habitat_legend <- habitat_classify %>%
#   mutate(value = as.integer(MACR_2015)) %>%
#   mutate(name = as.character(MACR_2015)) %>%
#   group_by(value) %>%
#   summarize(value = unique(value), name = unique(name)) %>%
#   mutate(red = NA, green = NA, blue = NA) %>%
#   dplyr::select(value, red, green, blue, name) %>%
#   arrange(value)
#
# ## CAREFUL! Do not overwrite the following file unless absolutely necessary.
# ## The colors would have to manually entered again.
# #write.csv(habitat_legend, habitat_legend_csv, row.names = FALSE)
#
# ## At this point, I manually transferred all the RGB colors from the ArcGIS
# ## symbology layer into the .csv
# habitat_legend <- read.csv(habitat_legend_csv, stringsAsFactors=FALSE)
#
# ## Create hexidecimal colors from RGB colors and save file
# habitat_legend$hex <- rgb(habitat_legend$red, habitat_legend$green,
#   habitat_legend$blue, maxColorValue=255)
# #write.csv(habitat_legend, habitat_legend_csv, row.names = FALSE)
#
# ## Create a "Colormap" for ArcGIS. The only way that a .tif can use
# ## a colormap is to create an attribute table for the raster
# habitat_colormap <- habitat_legend %>%
#   dplyr::select(value, red, green, blue)
# write.table(habitat_colormap, habitat_clr, sep= " ", col.names=FALSE,
#   row.names=FALSE)
#
# ## Landcover
# # Reclassifying the land layer info into 'landcover_legend'
# landcover_legend <- land_classify %>%
#   mutate(value = as.integer(FORMATION)) %>%
#   mutate(name = as.character(FORMATION)) %>%
#   group_by(value) %>%
#   summarize(value = unique(value), name = unique(name)) %>%
#   mutate(red = NA, green = NA, blue = NA) %>%
#   dplyr::select(value, red, green, blue, name) %>%
#   arrange(value)
#
# ## CAREFUL! Do not overwrite the following file unless absolutely necessary.
# ## The colors would have to manually entered again.
# # write.csv(landcover_legend, landcover_legend_csv, row.names = FALSE)
#
# ## At this point, I manually transferred all the RGB colors from the ArcGIS
# ## symbology layer into the .csv
# landcover_legend <- read.csv(landcover_legend_csv, stringsAsFactors=FALSE)
#
# ## Create hexidecimal colors from RGB colors and save file
# landcover_legend$hex <- rgb(landcover_legend$red, landcover_legend$green,
#   landcover_legend$blue, maxColorValue=255)
# #write.csv(landcover_legend, landcover_legend_csv, row.names = FALSE)
#
# ## Create a "Colormap" for ArcGIS. The only way that a .tif can use
# ## a colormap is to create an attribute table for the raster
# landcover_colormap <- landcover_legend %>%
#   dplyr::select(value, red, green, blue)
# write.table(landcover_colormap, landcover_clr, sep= " ", col.names=FALSE,
#   row.names=FALSE)
#
# ## Plot Testing
# raster_30mc <- eluform_30mc
# legend <- eluform_legend
# legend_name <- "Formation"
#
# raster_30mc <- habitat_30mc
# legend <- habitat_legend
# legend_name <- "Habitat"
#
# raster_30mc <- landcover_30mc
# legend <- landcover_legend
# legend_name <- "Landcover"
#
# library(rasterVis)
# library(ggplot2)
#
# # For testing purposes - clip to smaller area
# ext <- extent(c(510000, 560000, 5025000, 5065000))
# raster_30mc <- crop(raster_30mc, ext, snap='near')
#
# value_vec <- freq(raster_30mc)[,1]
# legend_sub <- legend %>% dplyr::filter(value %in% value_vec)
#
# gplot(raster_30mc, maxpixel = 500000) +
#   geom_raster(aes(fill=factor(value)), alpha=0.8) + coord_equal() +
#     scale_fill_manual(values = legend_sub$hex, labels=legend_sub$name) +
#   guides(fill=guide_legend(title=legend_name))
#
# # For maps that have a lot of categorical data, the "maxpixel" value needs to be
# # high (>= 500000) or else some of the categories aren't included in the drawing
# # of the map and it an can mess up the relationship between the mapped values
# # and the legend's name and hex values
#

# Classify Elevation Raster into Landform Slopes (devised by Jennes)
# @param elev raster layer
# @param elev_agg integer, for aggrevation
# @param fine_scale integer, for fine scale kernel
# @param course_scale integer, for course scale kernel
# @param wd working directory
#
# @import whitebox
# @return
# @export
# @details This function is based on the landfClass() function that was part
#     of the GmAMisc package but was removed for some reason.
#
# ClassifySlopes <- function(elev,
#                            elev_agg = 1,
#                            fine_scale = 10,
#                            course_scale = 50,
#                            wd = getwd()) {
#
#   # Check arguments
#   is_odd <- function(x) {
#     assertthat::assert_that(is.numeric(x), length(x) == 1)
#     x %% 2 == 1
#   }
#
#   assertthat::assert_that(is_odd(elev_agg))
#   assertthat::assert_that(fine_scale %% 1 == 0)
#   assertthat::assert_that(course_scale %% 1 == 0)
#
#   # Set and create function's working directory
#   fun_wd <- file.path(wd, "ClassifyLandformFiles")
#   suppressWarnings(if(!exists(fun_wd)) dir.create(fun_wd))
#
#   # File Names
#   elev_file <- file.path(fun_wd, paste0("ElevOrg.tif"))
#   elev_agg_file <- file.path(fun_wd, paste0("ElevAgg_", elev_agg, ".tif"))
#   elev_std_file <- file.path(fun_wd, paste0("ElevStand_", elev_agg, ".tif"))
#   slope_file <- file.path(fun_wd, "Slope.tiff")
#   fine_file <- file.path(fun_wd, paste0("RelTopoPos_", fine_scale, ".tif"))
#   course_file <- file.path(fun_wd, paste0("RelTopoPos_", course_scale, ".tif"))
#   fine_file2 <- file.path(fun_wd, paste0("RelTopoPos_", fine_scale, "a.tif"))
#   course_file2 <- file.path(fun_wd, paste0("RelTopoPos_", course_scale,"a.tif"))
#
#   # Elev raster aggregation
#   if (elev_agg == 1){
#     raster::writeRaster(elev, elev_agg_file, format = "GTiff", overwrite = TRUE)
#   } else {
#     raster::writeRaster(elev, elev_file, format = "GTiff", overwrite = TRUE)
#     wbt_aggregate_raster(elev_file, elev_agg_file, agg_factor = elev_agg,
#       type = "mean", verbose_mode = TRUE)
#   }
#
#   file.exists(elev_agg_file)
#   # Standardize elevation
#   wbt_z_scores(input = elev_agg_file, output = elev_std_file)
#
#   # RTP calculations
#   wbt_relative_topographic_position(elev_std_file, fine_file,
#     filterx = fine_scale, filtery = fine_scale)
#   wbt_relative_topographic_position(elev_std_file, course_file,
#     filterx = course_scale, filtery = course_scale)
#
#   wbt_dev_from_mean_elev(elev_agg_file, fine_file2,
#     filterx = fine_scale, filtery = fine_scale)
#   wbt_dev_from_mean_elev(elev_agg_file, course_file2,
#     filterx = course_scale, filtery = course_scale)
#
#   # Slope Calculations
#   wbt_slope(elev_agg_file, slope_file, zfactor = 1.0, units="degrees")
#
#   # Extract Rasters
#   fs <- raster::raster(fine_file)
#   cs <- raster::raster(course_file)
#   slp <- raster::raster(slope_file)
#
#   # Define the ten classes on the basis of thresholds of sn, sl, and slope
#   canyons <- (fs <= -1) & (cs <= -1)
#   #canyons[na.omit(canyons)] <- 1
#
#   midslope.dr <- (fs <= -1) & (cs> -1 & cs < 1)
#   #midslope.dr[na.omit(midslope.dr)] <- 2
#
#   upland.dr <-  (fs <= -1) & (cs >= 1)
#   #upland.dr[na.omit(upland.dr)] <- 3
#
#   us.valley <-  (fs > -1 & fs< 1) & (cs <=-1)
#   #us.valley[na.omit(us.valley)] <- 4
#
#   plains <- (fs > -1 & fs < 1) & (cs > -1 & cs < 1) & (slp <= 5)
#   #plains[na.omit(plains)] <- 5
#
#   open.slp <-  (fs > -1 & fs < 1) & (cs > -1 & cs < 1) & (slp > 5)
#   #open.slp[na.omit(open.slp)] <- 6
#
#   upper.slp <- (fs > -1 & fs < 1) & (cs >= 1)
#   #upper.slp[na.omit(upper.slp)] <- 7
#
#   local.rdg <- (fs >= 1) & (cs <= -1)
#   #local.rdg[na.omit(local.rdg)] <- 8
#
#   midslp.rdg <- (fs >= 1) & (cs > -1 & cs < 1)
#   #midslp.rdg[na.omit(midslp.rdg)] <- 9
#
#   mount.top <- (fs >= 1) & (cs >=1)
#   #mount.top[na.omit(mount.top)] <- 10
#
#   slope_brick <- brick(upper.slp, midslp.rdg, mount.top)
#   slope_sum <- calc(slope_brick, sum)
#   return(slope_sum)
# }

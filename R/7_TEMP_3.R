# ------------------------- TERRITORIAL ANALYSIS ----------------------------- #
# This script is for calculating the territorial distribution of eagles by
# calculating their position relative to their nest and their neighbors nests
#------------------------------------------------------------------------------#

## Load Packages, Scripts, etc. ------------------------------------------------
pacman::p_load(ggthemes, ggmap, tidyverse, maps, raster, RColorBrewer, rgeos,
  tools)
options(stringsAsFactors = FALSE)
theme_update(plot.title = element_text(hjust = 0.5))

library(baear) #devtools::reload(file.path(package_dir, "baear"))
library(gisr) #devtools::reload(file.path(package_dir, "gisr"))
library(ibmr) #devtools::reload(file.path(package_dir, "ibmr"))
package_dir <- "C:/Users/blake/OneDrive/Work/R/Packages"

# GIS arguments
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Plotting arguments
id_colors <- CreateColorsByAny(by="id", output=TRUE)

############################  IMPORT FILES  ####################################

## Import Baea, Nests, and Base ------------------------------------------------
baea_hr <- readRDS("Data/BAEA/baea_homerange.rds")

nests_active <- readRDS(file="Data/Nests/Nests_rds/nests_active.RDS")
base = raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

## Filter BAEA Data ------------------------------------------------------------

individuals = c("Ellis", "Sandy", "Musquash", "Hebron")

baea_terr <- FilterLocations(df = baea_hr, id = "id", individual = individuals,
  start = "2016-05-01", end = "2016-09-01")

table(baea_terr$id)
table(baea_terr$nest_site)

baea_terr_spdf <- SpatialPointsDataFrame(baea_terr[c("long_utm", "lat_utm")],
  bbox = NULL, data = baea_terr, proj4string = wgs84n19)

baea_terr$x <- baea_terr$long_utm
baea_terr$y <- baea_terr$lat_utm
baea_terr <- CenterXYWithBase(baea_terr, base)
baea_terr$long_utm <- baea_terr$x
baea_terr$lat_utm <- baea_terr$y

saveRDS(baea_terr, "Data/BAEA/baea_terr.rds")

############## CREATE NEST AND CONSPECIFIC DISTANCE RASTERS ####################

## Calculate Homerange Distance ------------------------------------------------

homerange_distances <- AddHomeConEdgeDistanceBAEA(baea_terr, nests_active, base,
  output_dir = "Output/Analysis/Territorial", max_r = 30000,
  write_home_dist = FALSE, write_con_dist = FALSE, write_con_dist_nest = TRUE,
  write_edge_dist = FALSE, write_terr_edge = FALSE)

#---------------------------- Create BAEA -------------------------------------#
# This script is for adding fields related to sunrise, sunset, time steps,
# direction, distance, angle, elevation, etc. to the "deployed" dataset
# create the "BAEA" dataset.
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------

# Load packages
pacman::p_load(sp)
library(gisr)
library(baear)

# Variables
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Import Data and Convert ------------------------------------------------------

# Import deployed data
deployed_all <- readRDS(file = "Data/CTT/Deployed/Deployed.rds")
baea <- ImportBAEA(existing = deployed_all, import = FALSE)
baea <- AddNestData(baea)
baea <- AddSeason(baea)
baea <- AddElevationData(baea)

# Save Data --------------------------------------------------------------------

# Save baea.rds
saveRDS(baea, "Data/BAEA/baea.rds")

# Save baea SpatialPointsDataFrame
baea_spdf <- SpatialPointsDataFrame(baea[c("long_utm", "lat_utm")],
  bbox = NULL, data = baea, proj4string = wgs84n19)
saveRDS(baea_spdf, file = "Data/BAEA/baea_spdf.rds")

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

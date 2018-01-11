### This script is for adding fields related to sunrise, sunset, time steps,
### direction, distance, angle, elevation, etc. to the "deployed" dataset
### create the "BAEA" dataset.

library(gisr)
library(baear)

# Import Deployed Data and Convert to BAEA -------------------------------------
deployed_all <- readRDS(file="Data/CTT/Deployed/Deployed.rds")
baea <- ImportBAEA(existing=deployed_all, import=FALSE)
baea <- AddNestData(baea)
baea <- AddSeason(baea)
baea <- AddElevationData(baea)

# Save BAEA.rds ----------------------------------------------------------------
saveRDS(baea, "Data/BAEA/baea.rds")

# Save BAEA SpatialPointsDataFrame ---------------------------------------------
suppressPackageStartupMessages(library(sp))
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N
baea_spdf <- SpatialPointsDataFrame(baea[c("long_utm", "lat_utm")],
  bbox=NULL, data=baea, proj4string=wgs84n19)

saveRDS(baea_spdf, file="Data/BAEA/baea_spdf.rds")

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

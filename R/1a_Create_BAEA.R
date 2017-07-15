### This script is for downloading telemetry data of deployed transmitters from
### the CTT website, compiling the data, creating summary stats and plots, and
### exporting KMLs and Shapefiles.

library(gisr)
library(baear)

devtools::reload("C:/Work/R/Packages/baear")
devtools::reload("C:/Work/R/Packages/gisr")
devtools::reload("C:/Work/R/Packages/ibmr")

# Import Deployed Data and Convert to BAEA -------------------------------------

deployed_all <- ImportUnits(units="deployed", import=TRUE)
baea <- ImportBAEA(existing=deployed_all, import=FALSE)

# Save BAEA.rds ----------------------------------------------------------------
saveRDS(baea,"C:/Work/R/Projects/baea_ibm/Data/BAEA/baea.rds")

# Save BAEA SpatialPointsDataFrame ---------------------------------------------
library(sp)
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N
baea_spdf <- SpatialPointsDataFrame(baea[c("long_utm", "lat_utm")],
  bbox=NULL, data=baea, proj4string=wgs84n19)

saveRDS(baea_spdf, file="Data/BAEA/baea_spdf.rds")

################################################################################
################################ OLD CODE ######################################
################################################################################

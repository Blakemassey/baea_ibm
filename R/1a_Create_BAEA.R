### This script is for downloading telemetry data of deployed transmitters from
### the CTT website, compiling the data, creating summary stats and plots, and
### exporting KMLs and Shapefiles.

library(gisr)
library(baear)
library(sp)

## Download RECENT Deployed Data -----------------------------------------------
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Import Deployed Data and Convert to BAEA -------------------------------------

devtools::reload("C:/Work/R/Packages/baear")

deployed_all <- CompileDownloads(units="deployed", compile="all")
baea <- ImportBAEA(existing=deployed_all, import=TRUE, tz="Etc/GMT+5")

# Save BAEA.csv ----------------------------------------------------------------
write.csv(baea, file="C:/Work/R/Data/BAEA/BAEA.csv", row.names=FALSE)

# Save BAEA SpatialPointsDataFrame ---------------------------------------------
baea_spdf <- SpatialPointsDataFrame(baea[c("long_utm", "lat_utm")],
  bbox=NULL, data=baea, proj4string=wgs84n19)

save(baea, file="C:/Work/R/Data/R_Input/BAEA/baea.RData")
save(baea_spdf, file="C:/Work/R/Data/R_Input/BAEA/baea_spdf.RData")



################################################################################
################################ OLD CODE ######################################
################################################################################

# Join Landscape Raster Values to BAEA locations -------------------------------
# Maine_stack <- ImportLandscapeRasterStack()
# PrintRasterNames(Maine_stack)
# baea_land <- AddLandscapeValues(baea, Maine_stack, clean_up = TRUE)

# Add Nest Use Data ------------------------------------------------------------
# baea_nest <- AddNestData(baea_land)
#
# write.csv(baea_nest, file="C:/Work/R/Data/BAEA/BAEA.csv", row.names=FALSE)
# save(baea, file="baea.RData")

# ## Import Deployed Data --------------------------------------------------------
# deployed_all <- ImportUnits(units="deployed", existing=NULL,
#   import=TRUE)
#
# ## Plot Daily Location Count ---------------------------------------------------
# PlotLocationCount(df=deployed, id="id", individual="",
#   color_factor="id", start="", end=NA, breaks="2 days",tz="Etc/GMT+5",
#   wrap=FALSE)
#
# # Summary Statistics -----------------------------------------------------------
# SummarizeLocations(df=deployed, pdf=FALSE)
#
# # Create ArcGIS and PDF Maps ---------------------------------------------------
# CreateArcGISandPDFMaps(df=deployed)
#
# # Write Shapefile of Deployed --------------------------------------------------
# ExportShapefile(df=deployed, layer="deployed", folder="C:/Work/R/Data/Output",
#   overwrite_layer = TRUE)
#
# # Plot Movements with Move------------------------------------------------------
# move <- CreateMove(df=deployed)
# PlotMove(move=move)
#
# # Create 3D Plots --------------------------------------------------------------
# Plot3DInteractive(df=deployed, x="long", y="lat", z="alt")
#
# # Add Nest and Landscape Attributes --------------------------------------------


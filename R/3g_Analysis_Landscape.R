########### CREATE SPECIFIC RASTER LAYERS FOR SSF/RSF ANALYSIS #################

### This script is for importing GIS datalayers and converting/subsetting them
### Original data files should all come frm R_Input/BAEA
# --------------------------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(foreign))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(velox))

########################### SET LOCAL VARIABLES ################################

### Input Files ----------------------------------------------------------------

landcover_input <-"C:/ArcGIS/Data/R_Input/BAEA/lc_30mc.tif"
ext <- extent(335000, 668000, 4750000, 5257000)

### Output files ---------------------------------------------------------------

# Rasters
developed_output <- "C:/ArcGIS/Data/R_Input/BAEA/developed_30mc.tif"
forest_output <- "C:/ArcGIS/Data/R_Input/BAEA/forest_30mc.tif"
open_water_output <- "C:/ArcGIS/Data/R_Input/BAEA/open_water_30mc.tif"
pasture_output <- "C:/ArcGIS/Data/R_Input/BAEA/pasture_30mc.tif"
shrub_herb_output <- "C:/ArcGIS/Data/R_Input/BAEA/shrub_herb_30mc.tif"
wetland_output <- "C:/ArcGIS/Data/R_Input/BAEA/wetland_30mc.tif"

############ CREATE SPECIFIC LAYERS FOR SSF AND RSF ############################

# Import rasters and create lc_vx
lc_crop <- crop(raster(landcover_input), ext)
extent(lc_crop)
lc_vx <- velox(lc_crop)

# Create specific landcover layers by coverting type to 1 and all others to 0

lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% 21:24] <- 0  ## Developed
lc_matrix[lc_matrix >= 1] <- 1
developed <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(developed, developed_output, progress="text", datatype='INT2U',
  overwrite=TRUE, template = ext)
rm(developed)

lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% 41:43] <- 0  ## Forest
lc_matrix[lc_matrix >= 1] <- 1
forest <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(forest, forest_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
rm(forest)

lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% 11] <- 0  ## Open Water
lc_matrix[lc_matrix >= 1] <- 1
open_water <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(open_water, open_water_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
rm(open_water)

lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% c(81, 82)] <- 0  ## Pasture/Crop
lc_matrix[lc_matrix >= 1] <- 1
pasture <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(pasture, pasture_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
rm(pasture)

lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% c(52, 71)] <- 0  ## Shrub/Herb
lc_matrix[lc_matrix >= 1] <- 1
shrub_herb <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(shrub_herb, shrub_herb_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
rm(shrub_herb)

lc_matrix <- lc_vx$as.matrix(band = 1)
lc_matrix[!lc_matrix %in% c(90, 95)] <- 0  ## Wetland
lc_matrix[lc_matrix >= 1] <- 1
wetland <- raster(lc_matrix, template=lc_30mc)
rm(lc_matrix)
writeRaster(wetland, wetland_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
rm(wetland)

######################### PLOT LANDSCAPE RASTER ################################

plot(lc_crop, axes=TRUE, main="Landcover")
#SavePlot(file="Map_All_Color.jpg")

par(mfrow = c(2, 3))
developed <- raster(developed_output)
plot(developed, main="Developed",
  col=colorRampPalette(c("white","#ED0000"))(100))
forest <- raster(forest_output)
plot(forest, main="Forest",
  col=colorRampPalette(c("white","#B5C98E"))(100))
open_water <- raster(open_water_output)
plot(open_water, main="Open Water",
  col=colorRampPalette(c("white","#476BA0"))(100))
pasture <- raster(pasture_output)
plot(pasture, main = "Pasture",
    col=colorRampPalette(c("white","#DBD83C"))(100))
shrub_herb <- raster(shrub_herb_output)
plot(shrub_herb, main = "Shrub and Herbaceous",
  col=colorRampPalette(c("white","#CCBA7C"))(100))
wetland <- raster(wetland_output)
plot(wetland, main="Wetland",
    col=colorRampPalette(c("white","#BAD8EA"))(2))
#SavePlot(file="Map_All_Color.jpg")
par(mfrow = c(1,1))

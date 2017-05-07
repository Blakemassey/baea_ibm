### This script is for importing GIS datalayers and converting them to the
### proper coordinate reference system (NAD83 UTM N19), extent, and resolution.
--------------------------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(foreign))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(plotKML))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rgdal))

########################### SET LOCAL VARIABLES ################################

### Input Files ----------------------------------------------------------------

# Rasters
base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
elu_file <- "C:/ArcGIS/Data/Ecological_Land_Units/elu30reg1108"
habitat_file <- "C:/ArcGIS/Data/Terrestrial_Habitat/ne-ca_hab815"
iei_file <- "C:/ArcGIS/Data/Index_Ecological_Integrity/iei-r_2010_v3.1.tif"
land_file <- "C:/ArcGIS/Data/DSLland/DSLland_2010_v3.0.tif"

### Output files ---------------------------------------------------------------

# Rasters
eluform_output <- "C:/ArcGIS/Data/R_Input/BAEA/eluform_30mc.tif"
habitat_output <- "C:/ArcGIS/Data/R_Input/BAEA/habitat_30mc.tif"
iei_output <- "C:/ArcGIS/Data/R_Input/BAEA/iei_30mc.tif"
landcover_output <- "C:/ArcGIS/Data/R_Input/BAEA/landcover_30mc.tif"

# Legends
eluform_legend_csv <- "C:/ArcGIS/Data/R_Input/BAEA/eluform_legend.csv"
habitat_legend_csv <- "C:/ArcGIS/Data/R_Input/BAEA/habitat_legend.csv"
iei_legend_csv <- "C:/ArcGIS/Data/R_Input/BAEA/iei_legend.csv"
landcover_legend_csv <- "C:/ArcGIS/Data/R_Input/BAEA/landcover_legend.csv"

# Colormaps
eluform_clr <- "C:/ArcGIS/Data/R_Input/BAEA/eluform_colormap.clr"
habitat_clr <- "C:/ArcGIS/Data/R_Input/BAEA/habitat_colormap.clr"
iei_clr <- "C:/ArcGIS/Data/R_Input/BAEA/iei_colormap.clr"
landcover_clr <- "C:/ArcGIS/Data/R_Input/BAEA/landcover_colormap.clr"

# CRS
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

######################### IMPORT RASTER LAYERS #################################

base <- raster(base_file)
elu <- raster(elu_file)
habitat <- raster(habitat_file)
iei <- raster(iei_file)
land <- raster(land_file)

# Classification table
elu_classify <- elu@data@attributes[[1]]
habitat_classify <- habitat@data@attributes[[1]]
land_classify <-  land@data@attributes[[1]]

########################## CLIP, REPROJECT, AND CROP ###########################

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
elu_clip <- crop(elu, maine_extent, progress="text")
habitat_clip <- crop(habitat, maine_extent, progress="text")
iei_clip <- crop(iei, maine_extent, progress="text")
land_clip <- crop(land, maine_extent, progress="text")
rm(maine_extent, xmin, xmax, ymin, ymax)

## Reproject 'layer_clip' to 'base' crs
elu_reproject <- projectRaster(elu_clip, base, progress="text", method='ngb')
habitat_reproject<-projectRaster(habitat_clip,base,progress="text",method='ngb')
iei_reproject <- projectRaster(iei_clip, base,progress="text",method='bilinear')
land_reproject <- projectRaster(land_clip, base, progress="text", method='ngb')

# Crop to extent of 'base' layer
elu_30m <- crop(elu_reproject, extent(base))
habitat_30m <- crop(habitat_reproject, extent(base))
iei_30m <- crop(iei_reproject, extent(base))
land_30m <- crop(land_reproject, extent(base))

############################## RECLASSIFY LAYERS ###############################

## Reclassifying the 'elu' layer by 'formation'
elu_classify <- elu_classify %>%
  mutate(value = as.integer(LF_TYPE))
eluform_30m <- subs(elu_30m, elu_classify, by="ID", which="value",
  progress = "text")

## Reclassifying the 'habitat' layer by 'macrogroup'
habitat_classify <- habitat_classify %>%
  mutate(value = as.integer(MACR_2015))
habitat_30m <- subs(habitat_30m, habitat_classify, by="ID", which="value",
  progress = "text")

## Reclassifying the 'land' layer by 'formation' (but calling it landcover)
land_classify <- land_classify %>%
  mutate(value = as.integer(FORMATION))
landcover_30m <- subs(land_30m, land_classify, by="ID", which="value",
  progress = "text")

########################## MASK AND WRITE RASTERS ##############################

## Mask to extent of the 'base' layer
eluform_30mc <- mask(eluform_30m, base)
habitat_30mc <- mask(habitat_30m, base)
iei_30mc <- mask(iei_30m, base)
landcover_30mc <- mask(landcover_30m, base)

## Set crs object (Needed to ensure the whole crs is written to .tif)
crs(eluform_30mc) <- wgs84n19
crs(habitat_30mc) <- wgs84n19
crs(iei_30mc) <- wgs84n19
crs(landcover_30mc) <- wgs84n19

## Write raster of layer_30mc
writeRaster(eluform_30mc, eluform_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
writeRaster(habitat_30mc, habitat_output, progress="text", datatype='INT2U',
  overwrite=TRUE)
writeRaster(iei_30mc, iei_output, progress="text", datatype='FLT4S',
  overwrite=TRUE)
writeRaster(landcover_30mc, landcover_output, progress="text", datatype='INT2U',
  overwrite=TRUE)

################### CREATE LEGEND TABLES AND COLORMAPS #########################

## Ecological_Land_Units -------------------------------------------------------
# Reclassifying the land layer info into 'eluform_legend'
eluform_legend <- elu_classify %>%
  mutate(value = as.integer(LF_TYPE)) %>%
  mutate(name = as.character(LFTYPE_DESC)) %>%
  group_by(value) %>%
  summarize(value = unique(value), name = unique(name)) %>%
  mutate(red = NA, green = NA, blue = NA) %>%
  dplyr::select(value, red, green, blue, name) %>%
  arrange(value)

## CAREFUL! Don't overwrite the following file unless absolutely necessary.
## The colors would have to be manually entered again.
#write.csv(eluform_legend, eluform_legend_csv, row.names = FALSE)

## At this point, I manually transferred all the RGB colors from the ArcGIS
## symbology layer into the .csv
eluform_legend <- read.csv(eluform_legend_csv, stringsAsFactors=FALSE)

# ## Create hexidecimal colors from RGB colors and save file
eluform_legend$hex <- rgb(eluform_legend$red,
  eluform_legend$green, eluform_legend$blue, maxColorValue=255)
#write.csv(eluform_legend, eluform_legend_csv, row.names = FALSE)

## This is to create a "Colormap" for ArcGIS. The only way that a .tif can use
## a Colormap if to create an attribute table for the raster (Google it)
eluform_colormap <- eluform_legend %>%
  dplyr::select(value, red, green, blue)
write.table(eluform_colormap, eluform_clr, sep= " ", col.names=FALSE,
   row.names=FALSE)

## Habitat ---------------------------------------------------------------------
# Reclassifying the habitat layer info into 'habitat_legend'
habitat_legend <- habitat_classify %>%
  mutate(value = as.integer(MACR_2015)) %>%
  mutate(name = as.character(MACR_2015)) %>%
  group_by(value) %>%
  summarize(value = unique(value), name = unique(name)) %>%
  mutate(red = NA, green = NA, blue = NA) %>%
  dplyr::select(value, red, green, blue, name) %>%
  arrange(value)

## CAREFUL! Don't overwrite the following file unless absolutely necessary.
## The colors would have to manually entered again.
#write.csv(habitat_legend, habitat_legend_csv, row.names = FALSE)

## At this point, I manually transferred all the RGB colors from the ArcGIS
## symbology layer into the .csv
habitat_legend <- read.csv(habitat_legend_csv, stringsAsFactors=FALSE)

## Create hexidecimal colors from RGB colors and save file
habitat_legend$hex <- rgb(habitat_legend$red, habitat_legend$green,
  habitat_legend$blue, maxColorValue=255)
#write.csv(habitat_legend, habitat_legend_csv, row.names = FALSE)

## Create a "Colormap" for ArcGIS. The only way that a .tif can use
## a colormap is to create an attribute table for the raster
habitat_colormap <- habitat_legend %>%
  dplyr::select(value, red, green, blue)
write.table(habitat_colormap, habitat_clr, sep= " ", col.names=FALSE,
  row.names=FALSE)

## Landcover -------------------------------------------------------------------
# Reclassifying the land layer info into 'landcover_legend'
landcover_legend <- land_classify %>%
  mutate(value = as.integer(FORMATION)) %>%
  mutate(name = as.character(FORMATION)) %>%
  group_by(value) %>%
  summarize(value = unique(value), name = unique(name)) %>%
  mutate(red = NA, green = NA, blue = NA) %>%
  dplyr::select(value, red, green, blue, name) %>%
  arrange(value)

## CAREFUL! Don't overwrite the following file unless absolutely necessary.
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

############################# PLOT TESTING #####################################

raster_30mc <- eluform_30mc
legend <- eluform_legend
legend_name <- "Formation"

raster_30mc <- habitat_30mc
legend <- habitat_legend
legend_name <- "Habitat"

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
    scale_fill_manual(values = legend_sub$hex,
      labels=legend_sub$name) + guides(fill=guide_legend(title=legend_name)

# For maps that have a lot of categorical data, the "maxpixel" value needs to be
# high (>= 500000) or else some of the categories aren't included in the drawing
# of the map and it an can mess up the relationship between the mapped values
# and the legend's name and hex values

################################################################################
#################################   OLD CODE   #################################
################################################################################

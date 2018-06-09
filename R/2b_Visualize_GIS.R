### This script is for importing GIS datalayers and converting them to the
### proper coordinate reference system (NAD83 UTM N19), extent, and resolution.
--------------------------------------------------------------------------------

## Import libraries ------------------------------------------------------------
suppressPackageStartupMessages(library(foreign))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotKML))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rasterVis))
suppressPackageStartupMessages(library(rgdal))
options(stringsAsFactors=FALSE) # CRITICAL!!! Otherwise, map colors are wrong.
library(gisr)
library(baear)

## Import Raster Layers --------------------------------------------------------

eluform <- raster("C:/ArcGIS/Data/R_Input/BAEA/eluform_30mc.tif")
habitat <- raster("C:/ArcGIS/Data/R_Input/BAEA/habitat_30mc.tif")
iei <- raster("C:/ArcGIS/Data/R_Input/BAEA/iei_30mc.tif")
landcover <- raster("C:/ArcGIS/Data/R_Input/BAEA/landcover_30mc.tif")

eluform_colors <- read.csv("C:/ArcGIS/Data/R_Input/BAEA/eluform_legend.csv")
habitat_colors <- read.csv("C:/ArcGIS/Data/R_Input/BAEA/habitat_legend.csv")
iei_colors <- colorRampPalette(c("darkblue", "green", "yellow", "red"))(50)
landcover_colors <- read.csv("C:/ArcGIS/Data/R_Input/BAEA/landcover_legend.csv")


## Subset Layers ---------------------------------------------------------------

# For testing purposes - clip to smaller area
ext <- extent(c(510000, 560000, 4995000, 5035000))
ext <- extent(c(510000, 513000, 4995000, 4998000))

eluform2 <- crop(eluform, ext, snap='near')
habitat2 <- crop(habitat, ext, snap='near')
iei2 <- crop(iei, ext, snap='near')
landcover2 <- crop(landcover, ext, snap='near')


## Mapping Raster Layers -------------------------------------------------------

# Mapping of eluform_30mc. Demostrates the use of a 'color' file in conjuntion
# w/gplot for plotting rasters in the ggplot2 format
value_vec <- freq(eluform)[,1]
eluform_colors_sub <- eluform_colors %>% dplyr::filter(value %in% value_vec)
options(scipen = 1000) # prevents the use of scientific notation in labels
gplot(eluform, maxpixel = 200000) +
  geom_tile(aes(fill=factor(value)), alpha=0.8)  +
  scale_fill_manual(values = eluform_colors_sub$hex,
    labels=eluform_colors_sub$name, name="Formation") + coord_equal() +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude")

SaveGGPlot(paste0("ELU form.jpeg"), file.path(image_output,
  "Raster Layers"), bg = "white")

# For testing purposes - clip to smaller area
value_vec2 <- freq(eluform2)[,1]
eluform_colors_sub2 <- eluform_colors %>%
  dplyr::filter(value %in% value_vec2)
gplot(eluform2, maxpixel = 200000) +
  geom_tile(aes(fill=factor(value)), alpha=0.8)  +
  scale_fill_manual(values = eluform_colors_sub2$hex,
    labels=eluform_colors_sub2$name, name="Formation") + coord_equal() +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude")
SaveGGPlot("ELU form.jpeg",
  "Results/Analysis/GIS", bg = "white")


# Mapping of habitat_30mc. Demostrates the use of a 'color' file in conjuntion
# w/gplot for plotting rasters in the ggplot2 format
value_vec <- freq(habitat)[,1]
habitat_colors_sub <- habitat_colors %>% dplyr::filter(value %in% value_vec)
options(scipen = 1000) # prevents the use of scientific notation in labels
gplot(habitat, maxpixel = 500000) +
  geom_tile(aes(fill=factor(value)), alpha=0.8)  +
  scale_fill_manual(values = habitat_colors_sub$hex,
    labels=habitat_colors_sub$name, name="Habitat") + coord_equal() +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude")

SaveGGPlot(paste0("Habitat.jpeg"), file.path(image_output,
  "Raster Layers"), bg = "white")

# For testing purposes - clip to smaller area
value_vec2 <- freq(habitat2)[,1]
habitat_colors_sub2 <- habitat_colors %>% dplyr::filter(value %in% value_vec2)
gplot(habitat2, maxpixel = 200000) +
  geom_tile(aes(fill=factor(value)), alpha=0.8)  +
  scale_fill_manual(values = habitat_colors_sub2$hex,
    labels=habitat_colors_sub2$name, name="Habitat") + coord_equal() +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude")
SaveGGPlot("Habitat.jpeg",
  "Results/Analysis/GIS", bg = "white")


# Mapping of landcover_30mc. Demostrates the use of a 'color' file in conjuntion
# w/gplot for plotting rasters in the ggplot2 format
value_vec <- freq(landcover)[,1]
landcover_colors_sub <- landcover_colors %>% dplyr::filter(value %in% value_vec)
options(scipen = 1000) # prevents the use of scientific notation in labels
gplot(landcover, maxpixel = 500000) +
  geom_tile(aes(fill=factor(value)), alpha=0.8)  +
  scale_fill_manual(values = landcover_colors_sub$hex,
    labels=landcover_colors_sub$name, name="Landcover") + coord_equal() +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude")



# For testing purposes - clip to smaller area
value_vec2 <- freq(landcover2)[,1]
landcover_colors_sub2 <- landcover_colors %>% dplyr::filter(value%in%value_vec2)
gplot(landcover2, maxpixel = 200000) +
  geom_tile(aes(fill=factor(value)), alpha=0.8) + coord_equal() +
  scale_fill_manual(values = landcover_colors_sub2$hex,
    labels=landcover_colors_sub2$name, name="Landcover") +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude")
SaveGGPlot("LC.jpeg",
  "Results/Analysis/GIS", bg = "white")



# Mapping of iei_30mc.
gplot(iei, maxpixel = 500000) +
  geom_tile(aes(fill=value)) + coord_equal() +
  scale_fill_gradientn(colours = iei_colors, na.value = "transparent",
    name="IEI Index", limits=c(minValue(iei), maxValue(iei)),
    breaks = c(.25, .5, .75)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude")

# For testing purposes - clip to smaller area
gplot(iei2, maxpixel = 500000) +
  geom_tile(aes(fill=value)) + coord_equal() +
  scale_fill_gradientn(colours = iei_colors, na.value = "transparent",
    name="IEI Index", limits=c(minValue(iei), maxValue(iei)),
    breaks = c(.25, .5, .75)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude")

# Could change breaks to c(0,.25,.5,.75,1), BUT would need to change name to
# 'IEI Index\n' which adds an extra space between the title and colorbar

# To check color palette:
source('C:/Work/R/Functions/gen.R')
PlotColorPie(colorRampPalette(c("darkblue", "green", "yellow", "red"))(50))




################################################################################
#################################   OLD CODE   #################################
################################################################################

source('C:/Work/R/Functions/gis.R')
Maine_stack <- ImportLandscapeRasterStack()
PrintRasterNames(Maine_stack)

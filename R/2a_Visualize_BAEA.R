#---------------------------- Visualize BAEA ----------------------------------#
# This script is for importing baea data (.csv) and nest data (.csv) to create
# raster layers of home dist and con dist
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------

# Load packages
pacman::p_load(dplyr, ggplot2, ggthemes, lubridate)
pacman::p_load(baear, gisr)

# Import and Filter Data -------------------------------------------------------

# Import deployed BAEA data
load(file="Data/BAEA/baea.rds")
load(file="Data/BAEA/baea_spdf.rds")

# Filter BAEA data
week_ago <- as.character(floor_date(now() - period(1, "week"), "day"))
baea <- FilterLocations(df=baea, id="id", individual="",
  start="", end="")

wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N
baea_spdf <- SpatialPointsDataFrame(baea[c("long_utm", "lat_utm")],
  bbox=NULL, data=baea, proj4string=wgs84n19)

# Create 100 points for each eagle. Useful for symbology tests

baea_100 <- baea %>%
  group_by(serial) %>%
  slice(1:100) %>%
  ungroup %>%
  as.data.frame(.)

# Create a .csv file to be used for symbology of BAEA an ArcGIS layer
baea_rgb_colors <- read.csv("C:/Work/R/Data/BAEA/GPS_Deployments.csv") %>%
  filter(!is.na(deployed)) %>%
  select(serial,deploy_location, icon_color) %>%
  arrange(desc(deploy_location))

for (i in 1:nrow(baea_rgb_colors)){
  icon_color <- baea_rgb_colors$icon_color[i]
  baea_rgb_colors$red[i] = col2rgb(icon_color, alpha = FALSE)[1]
  baea_rgb_colors$green[i] = col2rgb(icon_color, alpha = FALSE)[2]
  baea_rgb_colors$blue[i] = col2rgb(icon_color, alpha = FALSE)[3]
}

ExportShapefileFromPoints(df=baea_100, name="baea",
  folder="C:/ArcGIS/Data/Temporary/BAEA_100", overwrite=TRUE) # WGS84

ExportKMLTelemetryBAEA(baea_100, file = "BAEA_100.kml",
  output_dir = "C:/Users/Blake/Desktop")

# Tests Plots ------------------------------------------------------------------

# Plotting 100 points

# Colors for each individual
baea_colors <- CreateColorsByAny("id", baea)

# Simple plot
ggplot() +
  geom_point(data=baea_100 %>% filter(speed < 1000),
    aes(x=long_utm, y=lat_utm, color=id), shape=19, alpha=.9,
    size=.75, stroke=1, na.rm=TRUE) +
    scale_color_manual(values=baea_colors)

# Plot with Maine as background
maine <- fortify(map_data("state") %>% filter(region == "maine"),
  region = "region")
ggplot(data = maine) +
  geom_map(map = maine, aes(x = long, y = lat, map_id = region,
    group = group), fill = "white", color = "black", size = 0.25) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  geom_point(data=baea_100, aes(x=long, y=lat, color=id), shape=19, alpha=.9,
    size=.75, stroke=1, na.rm=TRUE) +
  scale_color_manual(values=baea_colors, name = "ID") +
  theme_map() + theme(legend.position = c(1, .1))

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

## Import Base
# base = raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))
#
# ## Extract raster values and put into baea spatial points dataframe ------- ##
#
# baea$edge_dist <- extract(edge_dist, baea_spdf)
# baea$home_dist <- extract(home_dist, baea_spdf)
#
# baea_terr <- baea %>% dplyr::filter(edge_dist < 0)
#
# hist(baea_terr$edge_dist)
# source('C:/Work/R/Functions/all.R')
# source('C:/Work/R/Functions/sim/move.R')
# wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N
#
# Join Landscape Raster Values to BAEA locations ---------------------------- ##
# Maine_stack <- ImportLandscapeRasterStack()
# PrintRasterNames(Maine_stack)
# baea_land <- AddLandscapeValues(baea, Maine_stack, clean_up = TRUE)
#
# Add Nest Use Data --------------------------------------------------------- ##
# baea_nest <- AddNestData(baea_land)

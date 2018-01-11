# ------------------------- TERRIROTIRAL ANALYSIS -----------------------------#
# This script is for importing baea data (.csv) and nest data (.RData) to
# create raster layers of home dist and con dist, perform analyses, and plot.
# -----------------------------------------------------------------------------#

################################# SET UP #######################################

# Load packages and input parameters  ------------------------------------------
library(baear)
library(gisr)
library(ibmr)

suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(maps))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(rgeos))
suppressPackageStartupMessages(library(tools))
options(stringsAsFactors=FALSE)

# GIS arguments
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Plotting arguments
id_colors <- CreateColorsByAny(by="id", output=TRUE)

theme_legend <- theme(plot.title=element_text(size=24, face="bold", vjust=1.5,
    hjust = 0.5))+
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(colour="black")) +
  theme(axis.text.x=element_text(size=16, angle=50, vjust=0.5)) +
  theme(axis.text.y=element_text(size=16, vjust=0.5)) +
  theme(strip.text.x = element_text(size = 12, colour = "darkblue"))
theme_no_legend <- theme_legend + theme(legend.position="none")

# Image directory
image_output <- file.path("C:/Users/blake/Documents/PhD Program",
  "McGarigal Lab Presentations/Lab Presentation - 2017.10/Images")

############################  IMPORT FILES  ####################################

## Import Baea, Nests, and Base ------------------------------------------------
baea <- readRDS(file="Data/BAEA/baea.rds")
nests_active <- readRDS(file="Data/Nests/nests_active.RDS")
base = raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

## Filter BAEA Data ------------------------------------------------------------

individuals = c("Ellis","Sandy", "Musquash", "Hebron")

baea <- FilterLocations(df=baea, id="id", individual=individuals,
  start="2016-05-01", end="2016-09-01")
baea <- AddNestData(df=baea)

table(baea$id)
table(baea$nest_site)

baea_spdf <- SpatialPointsDataFrame(baea[c("long_utm", "lat_utm")],
  bbox=NULL, data=baea, proj4string=wgs84n19)

#ExportShapefileFromPoints(baea, name = "Baea2016", folder = getwd())

baea$x <- baea$long_utm
baea$y <- baea$lat_utm
baea <- CenterXYWithBase(baea, base)
baea$long_utm <- baea$x
baea$lat_utm <- baea$y

############## CREATE NEST AND CONSPECIFIC DISTANCE RASTERS ####################

## Calculate Homerange Distance ------------------------------------------------

devtools::reload("C:/Work/R/Packages/baear")
devtools::reload("C:/Work/R/Packages/gisr")
devtools::reload("C:/Work/R/Packages/ibmr")

homerange_distances <- AddHomeConEdgeDistanceBAEA(baea, nests_active, base,
  output_dir = "Output/GIS/Analysis_Territorial", max_r = 30000,
  write_home_dist = TRUE, write_con_dist = FALSE, write_con_dist_nest = TRUE,
  write_edge_dist = FALSE, write_terr_edge = FALSE)

## Merge ConDist Rasters -------------------------------------------------------

con_dist_files <- list.files("Output/GIS/Analysis_Territorial",
  pattern="^ConDist_+.+tif$", full.names=TRUE, recursive = TRUE)
con_dist <- list()
for(i in 1:length(con_dist_files)){con_dist[[i]] <- raster(con_dist_files[i])}
con_dist <- do.call(merge, con_dist)
writeRaster(con_dist, filename=file.path("Output/GIS/Analysis_Territorial",
  "ConDist_All.tif"), format="GTiff", overwrite=TRUE)
plot(con_dist)

## Merge ConDistNest Rasters ---------------------------------------------------

con_dist_nest_files <- list.files("Output/GIS/Analysis_Territorial",
  pattern="^ConDistNest_+.+tif$", full.names=TRUE, recursive = TRUE)
con_dist_nest <- list()
for(i in 1:length(con_dist_nest_files)){con_dist_nest[[i]] <-
  raster(con_dist_nest_files[i])}
con_dist_nest <- do.call(merge, con_dist_nest)
writeRaster(con_dist_nest, filename=file.path("Output/GIS/Analysis_Territorial",
  "ConDistNest_All.tif"), format="GTiff", overwrite=TRUE)
plot(con_dist_nest)

## Merge EdgeDist Rasters ------------------------------------------------------

edge_dist_files <- list.files("Output/GIS/Analysis_Territorial",
  pattern="^EdgeDist_+.+tif$", full.names=TRUE, recursive = TRUE)
edge_dist <- list()
for(i in 1:length(edge_dist_files)){edge_dist[[i]] <-raster(edge_dist_files[i])}
edge_dist <- do.call(merge, edge_dist)
writeRaster(edge_dist, filename=file.path("Output/GIS/Analysis_Territorial",
  "EdgeDist_All.tif"), format="GTiff", overwrite=TRUE)
plot(edge_dist)

## Merge EdgeShiftDist Rasters -------------------------------------------------

edge_dist_shift_files <- list.files("Output/GIS/Analysis_Territorial",
  pattern="^EdgeDistShift_+.+tif$", full.names=TRUE, recursive = TRUE)
edge_dist_shift <- list()
for(i in 1:length(edge_dist_shift_files)){edge_dist_shift[[i]] <-
  raster(edge_dist_shift_files[i])}
edge_dist_shift <- do.call(merge, edge_dist_shift)
writeRaster(edge_dist_shift, filename=file.path("Output/GIS",
  "Analysis_Territorial/EdgeDistShift_All.tif"), format="GTiff", overwrite=TRUE)
plot(edge_dist_shift)

## Merge HomeDist Rasters ------------------------------------------------------

home_dist_files <- list.files("Output/GIS/Analysis_Territorial",
  pattern="^HomeDist_+.+tif$", full.names=TRUE, recursive = TRUE)
home_dist <- list()
for(i in 1:length(home_dist_files)){home_dist[[i]] <- raster(home_dist_files[i])}
home_dist <- do.call(merge, home_dist)
plot(home_dist)
writeRaster(home_dist, filename=file.path("Output/GIS/Analysis_Territorial",
  "HomeDist_All.tif"), format="GTiff", overwrite=TRUE)

## Merge TerrEdge Rasters ------------------------------------------------------

terr_edge_dir <- "Output/GIS/Analysis_Territorial/TerrEdge_Shapefile"
terr_edge_files <- list.files(terr_edge_dir, pattern = ".shp$",
  full.names = FALSE, recursive = TRUE)
terr_edge <- list()
for(i in 1:length(terr_edge_files)){terr_edge[[i]] <- readOGR(dsn=terr_edge_dir,
  layer=file_path_sans_ext(terr_edge_files[i]))}
terr_edge <- do.call(bind, terr_edge)
terr_edge_ll <- spTransform(terr_edge, wgs84)
terr_edge_frt <- fortify(terr_edge_ll)
ggplot() + geom_path(data = terr_edge_ll, aes(x=long, y=lat, group=group),
  color='red', size=.2)

## Create and Merge Con_Nest Rasters -------------------------------------------

con_nest <- overlay(home_dist, con_dist_nest, fun=function(x,y){round(x+y)})
writeRaster(con_nest, filename=file.path("C:/Work/R/Workspace",
  "2016_Nests_Rasters/ConNest_All.tif"), format="GTiff", overwrite=TRUE)
plot(con_nest)
summary(con_nest)

################ LOAD NEST AND CONSPECIFIC DISTANCE RASTERS ####################

con_dist <- raster("C:/Work/R/Workspace/2016_Nests_Rasters/ConDist_All.tif")
con_dist_nest <- raster(file.path("C:/Work/R/Workspace/2016_Nests_Rasters",
  "ConDistNest_All.tif"))
edge_dist <- raster("C:/Work/R/Workspace/2016_Nests_Rasters/EdgeDist_All.tif")
edge_dist_shift <- raster(file.path("C:/Work/R/Workspace/2016_Nests_Rasters",
  "EdgeDistShift_All.tif"))
home_dist <- raster("C:/Work/R/Workspace/2016_Nests_Rasters/HomeDist_All.tif")
con_nest <- raster("C:/Work/R/Workspace/2016_Nests_Rasters/ConNest_All.tif")

######################## EXTRACT DISTANCE DATA #################################

## Extract raster values and put into baea

baea$con_dist <- extract(con_dist, baea[,c("long_utm", "lat_utm")])
baea$con_dist_nest <- extract(con_dist_nest, baea[,c("long_utm", "lat_utm")])
baea$edge_dist <- extract(edge_dist, baea[,c("long_utm", "lat_utm")])
baea$edge_dist_shift <- extract(edge_dist_shift, baea[,c("long_utm","lat_utm")])
baea$home_dist <- extract(home_dist, baea[,c("long_utm", "lat_utm")])
baea$con_nest <- extract(con_nest, baea[,c("long_utm", "lat_utm")])

hist(baea$con_dist)
hist(baea$con_dist_nest)
hist(baea$edge_dist)
hist(baea$edge_dist_shift)
hist(baea$home_dist)
hist(baea$con_nest)

head(sort(unique(baea$con_dist)),5)
head(sort(unique(baea$con_dist_nest)),5)
head(sort(unique(baea$edge_dist)),5)
head(sort(unique(baea$edge_dist_shift)),5)
head(sort(unique(baea$home_dist)),5)
head(sort(unique(baea$con_nest)),5)
head(table(baea$con_nest),10)

## Determine how many location are outside territories
library(dplyr)
baea_terr <- baea %>% dplyr::filter(edge_dist < 0)

# Only 4% of data is outside territorial edge
nrow(baea %>% dplyr::filter(edge_dist < 0))/nrow(baea) #0.9625802

#################### FITTING CON_NEST ##########################################

library(fitdistrplus)
library(dplyr)
library(extraDistr)
library(texmex)

# Fitting with speed <= 2 & con_nest > 75

baea01 <- baea %>%
  filter(speed <= 2) %>%
  filter(con_nest > 75) %>%
#  dplyr::select(con_nest, id, long_utm, lat_utm, lat, long) %>%
  filter(!is.na(con_nest)) %>%
  filter(con_nest != 0) %>%
  mutate(con_nest = con_nest/1000)

ExportKMLTelemetryBAEA(baea01, file="BAEA_01_2016.kmz", output_dir=output_dir)

descdist(baea01$con_nest, boot=100)

fits_baea01 <- list(
  exponential = fitdist(baea01$con_nest, "exp"),
  halfnorm = fitdist(baea01$con_nest, "hnorm", start=list(sigma= sqrt(pi))),
  gamma = fitdist(baea01$con_nest, "gamma"),
  pareto = fitdist(baea01$con_nest, "gpd", start=list(sigma=5, xi=5)),
  weibull = fitdist(baea01$con_nest, "weibull")
)

save(fits_baea01, file = "Output/Data/fits_baea01.RData")

sapply(fits_baea01, function(i) summary(i))
sapply(fits_baea01, function(i) coef(i))

plot(fits_baea01$exponential)
plot(fits_baea01$halfnorm)
plot(fits_baea01$gamma)
plot(fits_baea01$pareto)
plot(fits_baea01$weibull)


fit_name <- "Con_Nest Distance"

plot(fits_baea01$exponential)
mtext(paste(fit_name, "-", "Exponential"), font=2, line = 4.5)
  ## The placement of the text depends on how plot screen is sized - 4 is
  ## right at the top of the graph when the plot window is 1/4 of screen
SavePlot(paste0(fit_name, " - Exponential.jpeg"), image_output)

plot(fits_baea01$gamma)
mtext(paste(fit_name, "-", "Gamma"), font=2, line = 4.5)
SavePlot(paste0(fit_name, " - Gamma.jpeg"), image_output)

plot(fits_baea01$halfnorm)
mtext(paste(fit_name, "-", "Half Normal"), font=2, line = 4.5)
SavePlot(paste0(fit_name, " - Half Normal.jpeg"), image_output)

plot(fits_baea01$pareto)
mtext(paste(fit_name, "-", "Pareto"), font=2, line = 4.5)
SavePlot(paste0(fit_name, " - Pareto.jpeg"), image_output)

plot(fits_baea01$weibull)
mtext(paste(fit_name, "-", "Weibull"), font=2, line = 4.5)
SavePlot(paste0(fit_name, " - Weibull.jpeg"), image_output)


baea01_lines <- data.frame(x=baea01$con_nest,
  HalfNorm=dhnorm(baea01$con_nest, fits_baea01$halfnorm$estimate["sigma"]),
  Exponential=dexp(baea01$con_nest, fits_baea01$exponential$estimate["rate"]),
  Pareto=texmex::dgpd(baea01$con_nest, fits_baea01$pareto$estimate["sigma"],
    fits_baea01$pareto$estimate["xi"]),
  Gamma=dgamma(baea01$con_nest, fits_baea01$gamma$estimate["shape"],
    fits_baea01$gamma$estimate["rate"]),
  Weibull=stats::dweibull(baea01$con_nest,fits_baea01$weibull$estimate["shape"],
    fits_baea01$weibull$estimate["scale"]))

ggplot(baea01) +
  geom_histogram(aes(x=con_nest, y=..density..), color = "black", fill = "grey",
    breaks = seq(0, max(baea01$con_nest), by=.5)) +
  ggtitle("Con/Nest Distance") +
  xlab("Kilometers") + ylab("Density") + theme_legend +
  geom_line(data = baea01_lines, aes(x, HalfNorm, color = "HalfNorm"), size = 1.1) +
  geom_line(data = baea01_lines, aes(x, Exponential, color = "Exponential"), size = 1.1) +
  geom_line(data = baea01_lines, aes(x, Gamma, color = "Gamma"), size = 1.1) +
  geom_line(data = baea01_lines, aes(x, Weibull, color = "Weibull"), size = 1.1) +
  geom_line(data = baea01_lines, aes(x, Pareto,  color="Pareto"), size = 1.1) +
  scale_color_manual(name = "Fitted \nDistributions",
    values = c("Exponential" = "green", "HalfNorm" = "darkorchid1",
      "Pareto" = "red1", "Gamma" = "blue1",
      "Weibull" = "yellow"))

SaveGGPlot(paste0("Con_Nest Distance Distribution Fits.png"),
  file.path(image_output), bg = "black")

### Map Con, Nest and Con_Nest Distances ---------------------------------------

wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

## Import Base
base = raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

nests_2016 <- nests_active %>%
  filter(active_2016 == TRUE) %>%
  transmute(long = long_utm, lat = lat_utm)

nests <- baea %>% group_by(id) %>% slice(1) %>%
    dplyr::select(nest_long_utm, nest_lat_utm)  %>%
    transmute(long = nest_long_utm, lat = nest_lat_utm)

home_dist_gg <- ConvertRasterForGGPlot(home_dist)

ggplot(home_dist_gg, aes(x, y)) +
    geom_raster(aes(fill = value), interpolate=TRUE) +
    coord_fixed(ratio = 1) +
    scale_fill_distiller(breaks = seq(0,40000, by=5000), name="Meters",
      palette = "Blues", direction=-1) +
    geom_point(data = nests_2016, aes(long, lat), shape=24, alpha=.9,
      color="red", fill= "black", size=2, stroke=2) +
    geom_point(data = nests, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "floralwhite", size=2, stroke=2) +
    geom_point(data = baea01, aes(long_utm, lat_utm), shape=4, alpha=.9,
      color="yellow", size=1, stroke=1.5) +
    geom_point(data = nests, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "floralwhite", size=2, stroke=2) +
    theme_legend +
    ggtitle(paste("Stationary Locations")) + xlab("Longitude") + ylab("Latitude")
  SaveGGPlot("Stationary Locations.png", image_output, bg = "white")



for (i in unique(baea$id)){
  con_dist_i <- raster(file.path("C:/Work/R/Workspace",
    "2016_Nests_Rasters/2016",paste0("ConDist_", i, ".tif")))

  con_dist_nest_i <- raster(file.path("C:/Work/R/Workspace",
    "2016_Nests_Rasters/2016",paste0("ConDistNest_", i, ".tif")))
  home_dist_i <- raster(file.path("C:/Work/R/Workspace/2016_Nests_Rasters",
    "2016", paste0("HomeDist_", i ,".tif")))
  con_nest_i <- overlay(home_dist_i, con_dist_nest_i,
    fun=function(x,y){round(x+y)})
  nest_i <-  baea %>% filter(id == i) %>% slice(1) %>%
    dplyr::select(nest_long_utm, nest_lat_utm)  %>%
    transmute(long = nest_long_utm, lat = nest_lat_utm)

  nests_2016_i <- nests_2016 %>%
    filter(long >= xmin(con_dist_nest_i) & long <= xmax(con_dist_nest_i)) %>%
    filter(lat >= ymin(con_dist_nest_i) & lat <= ymax(con_dist_nest_i))

  home_dist_i_gg <- ConvertRasterForGGPlot(home_dist_i)
  ggplot(home_dist_i_gg, aes(x, y)) +
    coord_fixed(ratio = 1) +
    geom_raster(aes(fill = value), interpolate=TRUE) + theme_legend +
    scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
    scale_fill_distiller(breaks = seq(0,40000, by=5000), name="Meters",
      palette = "Blues", direction=-1) +
    ggtitle(paste(i, "- Home Distance")) +
    xlab("Longitude") + ylab("Latitude") +
    geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
      color="red", fill= "black", size=2, stroke=2) +
    geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "white", size=2, stroke=2)
  SaveGGPlot(paste0(i, " - Home Distance.png"),
    file.path(image_output), bg = NA)

  con_dist_nest_i_gg <- ConvertRasterForGGPlot(con_dist_nest_i)
  ggplot(con_dist_nest_i_gg, aes(x, y)) +
    coord_fixed(ratio = 1) +
    geom_raster(aes(fill = value)) + theme_legend +
    scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
    scale_fill_gradient2(name="Meters", low="white", mid="grey", high="tan4") +
    ggtitle(paste(i, "- Conspecific Distance")) +
    xlab("Longitude") + ylab("Latitude")  +
    geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
      color="red", fill= "black", size=2, stroke=2) +
    geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "white", size=2, stroke=2)
  SaveGGPlot(paste0(i, " - Conspecific Distance.png"),
    file.path(image_output), bg = NA)

  con_dist_i_gg <- ConvertRasterForGGPlot(con_dist_i)
  ggplot(con_dist_i_gg, aes(x, y)) +
    coord_fixed(ratio = 1) +
    geom_raster(aes(fill = value)) + theme_legend +
    scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
    scale_fill_gradient2(name="Meters", high="white", mid="grey", low="tan4",
      midpoint=round((range(con_dist_i_gg$value)[2] -
          range(con_dist_i_gg$value)[1])/2)) +
    ggtitle(paste(i, "- Conspecific (actual) Distance")) +
    xlab("Longitude") + ylab("Latitude")  +
    geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
      color="red", fill= "black", size=2, stroke=2) +
    geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "white", size=2, stroke=2)
  SaveGGPlot(paste0(i, " - Conspecific (actual) Distance.png"),
    file.path(image_output), bg = NA)

  con_nest_i_gg <- ConvertRasterForGGPlot(con_nest_i)
  ggplot(con_nest_i_gg, aes(x, y)) +
    coord_fixed(ratio = 1) +
    geom_raster(aes(fill = value)) + theme_legend +
    scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
    scale_fill_gradientn(name = "Meters", colours = terrain.colors(10)) +
    ggtitle(paste(i, "- Con/Nest Distance")) +
    xlab("Longitude") + ylab("Latitude")  +
    geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
      color="red", fill= "black", size=2, stroke=2) +
    geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "white", size=2, stroke=2) +
  SaveGGPlot(paste0(i, " - Con_Nest Distance.png"), file.path(image_output),
    bg = NA)

  baea01_i <- baea01 %>% filter(id == i)
  ggplot(con_nest_i_gg, aes(x, y)) +
    coord_fixed(ratio = 1) +
    geom_raster(aes(fill = value)) + theme_legend +
    scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
    scale_fill_gradientn(name = "Meters", colours = terrain.colors(10)) +
    ggtitle(paste(i, "- Con/Nest Distance")) +
    xlab("Longitude") + ylab("Latitude")  +
    geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
      color="red", fill= "black", size=2, stroke=2) +
    geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "white", size=2, stroke=2) +
    geom_point(data = baea01_i, aes(long_utm, lat_utm), shape=4, alpha=.9,
      color="black", size=2, stroke = 1.2)
  SaveGGPlot(paste0(i, " - Con_Nest Distance with GPS Locations.png"),
    file.path(image_output), bg = NA)
}

### TESTING USING SANDY --------------------------------------------------------

nestcon_gamma_shape = fits_baea01$gamma$estimate["shape"]
nestcon_gamma_rate = fits_baea01$gamma$estimate["rate"]

nest_Sandy <- baea %>% filter(id == "Sandy") %>% slice(1) %>%
  dplyr::select(nest_long_utm, nest_lat_utm)  %>%
  transmute(long = nest_long_utm, lat = nest_lat_utm)
nest_Sandy <- as.numeric(nest_Sandy[1,])

con_dist_nest_Sandy <- raster(file.path("C:/Work/R/Workspace",
  "2016_Nests_Rasters/2016/ConDistNest_Sandy.tif"))
home_dist_Sandy <- raster(file.path("C:/Work/R/Workspace/2016_Nests_Rasters",
  "2016/HomeDist_Sandy.tif"))

plot(home_dist_Sandy, col=terrain.colors(255), main= "Sandy")
points(nest_Sandy[1], nest_Sandy[2], pch=20, col="blue")
plot(con_dist_nest_Sandy, col=terrain.colors(255), main= "Sandy")
points(nest_Sandy[1], nest_Sandy[2], pch=20, col="blue")

con_nest_Sandy <- overlay(home_dist_Sandy, con_dist_nest_Sandy,
  fun=function(x,y){round(x+y)})

plot(con_nest_Sandy, col=terrain.colors(255), main= "Sandy - ConNest Distance")
#  legend.args=list(text="Con D", cex=1, side=3, line=1))
points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")

loc_pts <- data.frame(
  x = c(500000, 475000),
  y = c(4920000, 4930000),
  title = c("Near Edge", "Above Nest"))
points(loc_pts$x, loc_pts$y, size=2, pch=4, lwd=2, col="black")

SavePlot("Sandy with Points.jpeg", image_output)

step_max_r <- 15000 #15km
cellsize <- res(base)[1]
con_nest_raster <- con_nest_Sandy

for (i in 1:nrow(loc_pts)){
  x <- loc_pts[i, "x"]
  y <- loc_pts[i, "y"]
  title <- loc_pts[i, "title"]
  max_r_cells <- ceiling(max_r/cellsize)
  xmin <- xmin(base)
  ymin <- ymin(base)
  xy <- CenterXYInCell(x, y, xmin, ymin, cellsize)  # May be unnecessary
  cell_extent <- extent(xy[1]-(cellsize/2), xy[1]+(cellsize/2), xy[2]-
    (cellsize/2), xy[2]+(cellsize/2))
  cell <- setValues(raster(cell_extent, crs=projection(base), res=cellsize),1)
  movement_kernel <- extend(cell, c(max_r_cells, max_r_cells), value=NA)
  con_nest_crop <- crop(con_nest_raster, movement_kernel, snap='in')
  plot(con_nest_crop, col=terrain.colors(255), main = paste0(title,
    " - ConNest Distance"))
  points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
  points(x, y, size=2, pch=4, lwd=2, col="black")
  SavePlot(paste0("Sandy - ", title , ".jpeg"), image_output)
  xy_pt <- data.frame(x = xy[1], y = xy[2])
  xy_con_nest <- extract(con_nest_crop, xy_pt)
  con_nest_adjust <- calc(con_nest_crop, fun=function(x){(x - xy_con_nest)/1000})
  plot(con_nest_adjust, col=terrain.colors(255), main = paste0(title,
    " - ConNest Distance (Shifted)"))
  points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
  points(x, y, size=2, pch=4, lwd=2, col="black")
  (extract(con_nest_adjust, xy_pt)) #should be zero
  SavePlot(paste0("Sandy - ", title ," Shifted.jpeg"), image_output)
  xy_log_scale <- NonlinearRangeRescaleGamma(x=(xy_con_nest/1000),
    shape=nestcon_gamma_shape, rate=nestcon_gamma_rate,  min=NULL, max=NULL,
    lowBound=1, upBound=NULL, movement_kernel=movement_kernel, negative=TRUE)
  curve(LogisticByInflection(x, inflection=0, scale=xy_log_scale), -15, 15,
    main = paste0("Logistic - ", title, "; Scale = ", round(xy_log_scale, 2)),
    ylab = "Probability")
  SavePlot(paste0("Sandy - ", title ," - Logistic Curve.jpeg"), image_output)
  LogisticByInflection2 <- function(x){
    x <- LogisticByInflection(x, inflection=0, scale=xy_log_scale)
  }
  con_nest_rescale <- calc(con_nest_adjust, fun=LogisticByInflection2)
  plot(con_nest_rescale, main = paste0(title, " - Rescaled Probability"))
  points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
  points(x, y, size=2, pch=4, lwd=2, col="black")
  SavePlot(paste0("Sandy - ", title ," - Rescaled .jpeg"), image_output)
}

NonlinearRangeRescaleGamma <- function(x,
                                       shape = shape,
                                       rate = rate,
                                       min = NULL, #e.g., .001; pgamma
                                       max = NULL, #e.g., .99; pgamma
                                       lowBound = 1,
                                       upBound = NULL,
                                       movement_kernel = movement_kernel,
                                       negative = TRUE){
  if(is.null(min)){
    max_distance <- qgamma(0.999, shape=shape, rate=rate)
    min <- pgamma(max_distance, shape=shape, rate=rate, lower.tail=FALSE)
  }
  if(is.null(max)){
    max <- pgamma(.075, shape=shape, rate=rate, lower.tail=FALSE)
  }
  if(is.null(upBound)){
    upBound <- sqrt((xmin(movement_kernel)-xmax(movement_kernel))^2+
                      (ymin(movement_kernel)-ymax(movement_kernel))^2)/1000
  }
  # Get predicted y (cummulative gamma) for x
  y_pred <- pgamma(x, shape=shape, rate=rate, lower.tail=FALSE)
  rescale <- lowBound + (((y_pred-min)/(max-min)) * (upBound-lowBound))
  if(negative==TRUE){
    rescale <- rescale*-1
  }
  return(rescale)
}

LogisticByInflection <- function(x,
                                 inflection,
                                 scale) {
  y <- (1/(exp((-(x - inflection)) / scale) + 1))
  return(y)
}


################################################################################
################################ OLD CODE ######################################
################################################################################



## Plotting Distance Histograms ------------------------------------------------

library(dplyr)

# Individuals' "Con Nest" Distance (All Distances)
ggplot(baea %>% filter(speed <= 2)) +
  stat_bin(aes(x=(con_nest/1000), y=..density..*1, fill=id), color="black",
    breaks = seq(0, max(baea$con_nest, na.rm =TRUE)/1000, by=.5)) +
  scale_fill_manual(values = id_colors) +
  ggtitle("Distance to Con (Shifted) and Nest") +
  xlab("Kilometers") + ylab("Density") + theme_no_legend + facet_wrap(~id)

# Individuals' "Con Dist Shifted to Zero at Nest" Distance (All Distances)
ggplot(baea %>% filter(speed <= 2)) +
  stat_bin(aes(x=(con_dist_nest/1000), y=..density..*1, fill=id), color="black",
    breaks = seq(0, max(baea$con_nest, na.rm =TRUE)/1000, by=.5)) +
  scale_fill_manual(values = id_colors) +
  ggtitle("Distance to Conspecific (shifted to zero at nest)") +
  xlab("Kilometers") + ylab("Density") + theme_no_legend + facet_wrap(~id)

# Individuals "Conspecific" Distance from Conspecific (All Distances)
ggplot(baea %>% filter(speed <= 2)) +
  stat_bin(aes(x=(con_dist/1000), y=..density..*5, fill=id), color="black",
    breaks = seq(0, max(baea$con_dist, na.rm =TRUE)/1000, by=.5)) +
  scale_fill_manual(values = id_colors) +
  ggtitle("Distance to Conspecific") +
  xlab("Kilometers") + ylab("Density") + theme_no_legend  + facet_wrap(~id)

SaveGGPlot(paste0("Distance from Conspecifics.jpeg"), file.path(image_output,
  "Distance Histograms"), bg = "black")

# Individual's Distance from Home Nest (All Distances)
ggplot(baea %>% filter(speed <= 2)) +
  stat_bin(aes(x=(nest_dist/1000), y=..density..*5, fill=id), color="black",
    breaks = seq(0, max(baea$nest_dist)/1000, by=1)) +
  scale_fill_manual(values = id_colors) +
  ggtitle("Distance from Nest") + xlab("Kilometers") +
  ylab("Density") + theme_no_legend  + facet_wrap(~id)

SaveGGPlot(paste0("Distance from Nest.jpeg"), file.path(image_output,
  "Distance Histograms"), bg = "black")

# Individual's Distance from Home Nest (<35km)
ggplot(baea) +
  stat_bin(aes(x=(nest_dist/1000), y=..density.., fill=id), color="black",
    breaks = seq(0, max(baea$nest_dist)/1000, by=1)) +
  scale_fill_manual(values = id_colors) +
  ggtitle("Distance from Nest (<35km)") + xlab("Kilometers") +
  ylab("Density") + theme_no_legend  + facet_wrap(~id) + xlim(0,35)

SaveGGPlot(paste0("Distance from Nest 35km.jpeg"), file.path(image_output,
  "Distance Histograms"), bg = "black")

# Individual's Distance from Home Nest (<10km)
ggplot(baea) +
  stat_bin(aes(x=(nest_dist/1000), y=..density..*.25, fill=id), color="black",
    breaks = seq(0, max(baea$nest_dist)/1000, by=.25)) +
  scale_fill_manual(values = id_colors) +
  ggtitle("Distance from Nest") + xlab("Kilometers") +
  ylab("Density") + theme_no_legend  + facet_wrap(~id) + xlim(0,10)

SaveGGPlot(paste0("Distance from Home Nest 10km.jpeg"), file.path(image_output,
  "Distance Histograms"), bg = "black")

# All Distance from Home Nest (<10km)
ggplot(baea) +
  stat_bin(aes(x=(nest_dist/1000), y=..density..*.08, fill=id), color="black",
    breaks = seq(0, max(baea$nest_dist)/1000, by=.25)) +
  scale_fill_manual(values = id_colors) +
  ggtitle("Distance from Nest") + xlab("Kilometers") +
  ylab("Density") + theme_no_legend  + xlim(0,10)

SaveGGPlot(paste0("Distance from Home Nest 10km (All).jpeg"),
  file.path(image_output, "Distance Histograms"), bg = "black")

# Calculate Individual's Minimum Distance from Territorial Edge
baea_edge_sum <- baea %>%
  group_by(id) %>%
  dplyr::summarize(min_edge_dist = min(edge_dist, na.rm = TRUE)) %>%
  ungroup()

# Individual's Distance from Territorial Edge
ggplot(baea) +
  stat_bin(aes(x=(edge_dist/1000), y=..density.., fill=id), color="black",
    binwidth = 1) +
  scale_fill_manual(values = id_colors) +
  ggtitle("Distance from Territorial Edge") + xlab("Kilometers") +
  ylab("Density") + theme_no_legend  + facet_wrap(~id) +
  geom_vline(xintercept=0, color="red", size=1) +
  geom_vline(aes(xintercept=min_edge_dist/1000), color="purple",
    data=baea_edge_sum, size=1)

SaveGGPlot(paste0("Distance from Territorial Edge.jpeg"),
  file.path(image_output, "Distance Histograms"), bg = "black")

# Create Inverse Cumulative Distributions

baea_edge <- baea %>%
  filter(edge_dist < 2000) %>%
  arrange(desc(edge_dist)) %>%
  mutate(cum_edge_dist = 1 - cume_dist(edge_dist))

baea_edge_shift <- baea %>%
  filter(edge_dist_shift < 15000) %>%
  arrange(desc(edge_dist_shift)) %>%
  mutate(cum_edge_dist = 1 - cume_dist(edge_dist_shift))

# All Territorial Edge (Shifted) Distances
ggplot(baea_edge_shift) +
  stat_bin(aes(x=(edge_dist_shift/1000), y=..density..*.08, fill=id),
    breaks=seq(0, max(baea_edge_shift$edge_dist_shift)/1000, by=.25),
    color="black") +
  scale_fill_manual(values = id_colors) +
  ggtitle("Distance from Territorial Edge (Shifted)") + xlab("Kilometers") +
  ylab("Density") + theme_legend




?CenterXY

dweibull((1/1000), 0.4192814, 0.4268249)


mtext(paste(con_nest_name, "-", "Weibull"), font=2, line = 5.5)
SavePlot(paste0("Con_Nest Fit - Weibull.jpeg"), file.path(image_output))

    shape     scale
0.4192814 0.4268249

PlotWeibullPDF(0.4192814, 0.4268249, 30)

hist(con_nest)


fun <- function(x){return(dweibull((x/1000), 0.4192814, 0.4268249))}
con_nest_weibull_prob <- calc(con_nest, fun)
plot(con_nest_weibull_prob)
summary(con_nest_weibull_prob)

writeRaster(con_nest_weibull_prob, filename=file.path("C:/Work/R/Workspace",
  "2016_Nests_Rasters/ConNestWeibull_All.tif"), format="GTiff", overwrite=TRUE)

dweibull((.944/1000), 0.4192814, 0.4268249)

#    shape     scale
#0.4192814 0.4268249

dweibull(1, 0.4192814, 0.4268249)














library(fitdistrplus)

baea$con_nest2 <- baea$con_nest/1000
con_nest2 <- ifelse(con_nest2 < .015, .015, con_nest2)
con_nest2 <- con_nest2[!is.na(con_nest2) & con_nest2 > 0.015]
range(con_nest2, na.rm=T)
descdist(con_nest2, boot=1000)
hist(con_nest2)

library(texmex)
fits_con_nest <- list(
  exponential = fitdist(con_nest2, "exp"),
#  halfnorm = fitdist(home_dist, "halfnorm", start=list(theta=
#    sqrt(pi/2))),
  gamma = fitdist(con_nest2, "gamma"),
  pareto = fitdist(con_nest2, "gpd", start=list(sigma=1, xi=1)),
  weibull = fitdist(con_nest2, "weibull")
)

sapply(fits_con_nest, function(i) summary(i))
sapply(fits_con_nest, function(i) coef(i))

save(fits_con_nest, file = file.path("C:/Work/R/Workspace",
  "fits_con_nest.RData"))

con_nest_name <- "Conspecific + Nest Distance"

plot(fits_con_nest$exponential)
plot(fits_con_nest$weibull)
plot(fits_con_nest$pareto)

mtext(paste(con_nest_name, "-", "Pareto"), font=2, line = 4)
SavePlot(paste0("Home_Dist Fit - Pareto.jpeg"), file.path(image_output,
  "Home Dist Fits"))


    shape     scale
0.4192814 0.4268249

PlotWeibullPDF(0.4192814, 0.4268249, 30)

hist(con_nest)


#################### FITTING HOME_DIST_SHIFT ###################################
library(fitdistrplus)

home_dist <- baea$home_dist/1000
home_dist <- ifelse(home_dist == 0, .001, home_dist)
home_dist <- home_dist[!is.na(home_dist) & home_dist <= 10000]
range(home_dist)
#descdist(baea_sub$edge_dist_shift, boot=1000)

library(texmex)
fits_home_dist <- list(
  exponential = fitdist(home_dist, "exp"),
#  halfnorm = fitdist(home_dist, "halfnorm", start=list(theta=
#    sqrt(pi/2))),
  gamma = fitdist(home_dist, "gamma"),
  pareto = fitdist(home_dist, "gpd", start=list(sigma=1, xi=1)),
  weibull = fitdist(home_dist, "weibull")
)

sapply(fits_home_dist, function(i) summary(i))
sapply(fits_home_dist, function(i) coef(i))

save(fits_home_dist, file = file.path("C:/Work/R/Workspace",
  "fits_home_dist.RData"))

home_dist_name <- "Home Distance"

plot(fits_home_dist$exponential)
mtext(paste(home_dist_name, "-", "Exponential"), font=2, line = 4)
  ## The placement of the text depends on how plot screen is sized - 4 is
  ## right at the top of the graph when the plot window is 1/4 of screen
SavePlot(paste0("Home_Dist Fit - Exponential.jpeg"),
  file.path(image_output, "Home Dist Fits"))

plot(fits_home_dist$gamma)
mtext(paste(home_dist_name, "-", "Gamma"), font=2, line = 4)
SavePlot(paste0("Home_Dist Fit - Gamma.jpeg"), file.path(image_output,
  "Home Dist Fits"))

#plot(fits_edge_dist_shift$halfnorm)
#mtext(paste(edge_dist_shift_name, "-", "Half Normal"), font=2, line = 4)
#SavePlot(paste0("Edge_Dist_Shift Fit - Half Normal.jpeg"),
#  file.path(image_output, "Edge_Dist_Shift"))

plot(fits_home_dist$pareto)
mtext(paste(home_dist_name, "-", "Pareto"), font=2, line = 4)
SavePlot(paste0("Home_Dist Fit - Pareto.jpeg"), file.path(image_output,
  "Home Dist Fits"))

plot(fits_home_dist$weibull)
mtext(paste(home_dist_name, "-", "Weibull"), font=2, line = 4)
SavePlot(paste0("Home_Dist Fit - Weibull.jpeg"), file.path(image_output,
  "Home Dist Fits"))

ggplot(baea_edge_shift) +
  stat_bin(aes(x=(edge_dist_shift/1000), y=..density..*.08, fill=id),
    color="black", breaks = seq(0, max(baea_edge_shift$edge_dist_shift)/1000,
      by=.25))

ggplot(baea_edge_shift) +
  stat_bin(aes(x=(edge_dist_shift/1000), y=..density..),
    color="black", breaks = seq(0, max(baea_edge_shift$edge_dist_shift)/1000, by=1)) +  #facet_wrap(~id) +
#  scale_fill_manual(values = id_colors) +
  geom_step(aes(x=(edge_dist_shift/1000), y=cum_edge_dist), color="black",size=1)+
  ggtitle("Distance from Territorial Edge (Shifted)") + xlab("Kilometers") +
  ylab("Density") + theme_legend

ggplot(baea_edge_shift) +
  stat_bin(aes(x=(edge_dist_shift/1000), y=(..density..)*0.25, fill=id),
    color="black", breaks = seq(0, max(baea_edge_shift$edge_dist_shift)/1000, by=.25)) +  facet_wrap(~id) +
  scale_fill_manual(values = id_colors) +
  geom_step(aes(x=(edge_dist_shift/1000), y=cum_edge_dist), color="black",size=1)+
  ggtitle("Distance from Territorial Edge (Shifted)") + xlab("Kilometers") +
  ylab("Density") + theme_legend

m + geom_histogram(binwidth = 0.5, aes(y = (..density..)*0.5))

ggplot(baea_terr) +
  stat_bin(aes(x=(edge_dist/1000), y=..density.., fill=id),
    color="black", binwidth = .5)  + facet_wrap(~id) +
  scale_fill_manual(values = id_colors) +
#  geom_step(aes(x=(edge_dist/1000), y=cum_edge_dist), color="black",size=1.25)+
  ggtitle("Distance from Territorial Edge") + xlab("Kilometers") +
  ylab("Density") + theme_legend +
  geom_vline(aes(xintercept=min_edge_dist/1000, color=id),
    data=baea_edge_dist_sum, size=1.5) +
  scale_color_manual(values = id_colors)

ggplot(baea_terr) +
  stat_bin(aes(x=(edge_dist/1000), y=..density.., fill=id),
    color="black", binwidth = .5) + # + facet_wrap(~id) +
  scale_fill_manual(values = id_colors) +
#  geom_step(aes(x=(edge_dist/1000), y=cum_edge_dist), color="black",size=1.25)+
  ggtitle("Distance from Territorial Edge") + xlab("Kilometers") +
  ylab("Density") + theme_legend +
  geom_vline(aes(xintercept=min_edge_dist/1000, color=id),
    data=baea_edge_dist_sum, size=1.5) +
  scale_color_manual(values = id_colors)

#################################### PLOTTING ##################################

# Input Variables --------------------------------------------------------------
ptspermm <- 2.83464567
scalebar.length=10
maine <- fortify( map_data("state") %>% filter(region == "maine"),
  region = "region")

# Manage and Reproject Data ----------------------------------------------------

study_nests <- nests_active %>% filter(nest_site %in% unique(baea$nest_site))
study_nests_spdf <- SpatialPointsDataFrame(study_nests[c("long_utm","lat_utm")],
  bbox=NULL, data=study_nests, proj4string=wgs84n19)

nests_30km <- gBuffer(study_nests_spdf, width=30000, byid=TRUE)
nests_30km_ll <- spTransform(nests_30km, CRS("+proj=longlat +datum=WGS84"))
nests_30km_frt <- fortify(nests_30km_ll)

con_dist_gg <- ConvertRasterForGGPlot(projectRaster(con_dist, crs=wgs84))
edge_dist_gg <- ConvertRasterForGGPlot(projectRaster(edge_dist, crs=wgs84))
edge_dist_shift_gg <- ConvertRasterForGGPlot(projectRaster(edge_dist_shift,
  crs=wgs84))
home_dist_gg <- ConvertRasterForGGPlot(projectRaster(home_dist, crs=wgs84))

# Set Color Palettes -----------------------------------------------------------

baea_colors <- CreateColorsByAny("id", baea)
display.brewer.all()

con_dist_pal <- colorRampPalette(brewer.pal(9, "OrRd"))
scale_home_dist <- scale_fill_gradientn(colours = con_dist_pal(100),
  limits=c(min(con_dist_gg$value), max(con_dist_gg$value)), name="Con Dist")
PlotColorPie(con_dist_pal(100), names=FALSE)

edge_dist_pal <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
scale_edge_dist <- scale_fill_gradientn(colours = edge_dist_pal(100),
  limits=c(min(edge_dist_gg$value), max(edge_dist_gg$value)), name="Edge Dist")
PlotColorPie(edge_dist_pal(100), names=FALSE)

edge_dist_shift_pal <- colorRampPalette(rev(brewer.pal(11, "PiYG")))
scale_edge_dist_shift <- scale_fill_gradientn(colours=edge_dist_shift_pal(100),
  limits=c(min(edge_dist_shift_gg$value), max(edge_dist_shift_gg$value)),
  name= "Edge Dist (Shifted)")
PlotColorPie(edge_dist_shift_pal(100), names=FALSE)

home_dist_pal <- colorRampPalette(brewer.pal(9, "BuPu"))
scale_home_dist <- scale_fill_gradientn(colours = home_dist_pal(100),
  limits=c(min(home_dist_gg$value), max(home_dist_gg$value)), name="Home Dist")
PlotColorPie(home_dist_pal(100), names=FALSE)

# Plotting All Individuals with Maine outline ----------------------------------
maine <- map_data("state") %>% filter(region == "maine")
maine <- fortify(us, region="region")

# All locations, non nests
ggplot() +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  geom_map(data=maine, map=maine, aes(x=long, y=lat, map_id=region,
    group=group), fill="white", color="black", size=0.25) +
  geom_path(data=nests_30km_frt, aes(x=long, y=lat, group=group),
    color='darkslateblue', size=1)  +
  geom_point(data=baea %>% filter(speed < 1000), aes(x=long, y=lat, color=id),
    shape=19, alpha=.9, size=.75, stroke=1, na.rm=TRUE) +
  scale_color_manual(values=baea_colors, name = "Eagle") +
  theme_tufte(base_family="Arial") + xlab("Longitude") + ylab("Latitude") +
  guides(colour = guide_legend(override.aes = list(size=3,linetype=0)))

SaveGGPlot(paste0("GPS Map - Outline.jpeg"), file.path(image_output,
  "All GPS Location Maps"), bg = "white")

# All locations, no nests
ggplot() +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  geom_map(data=maine, map=maine, aes(x=long, y=lat, map_id=region,
    group=group), fill="white", color="black", size=0.25) +
  geom_path(data=nests_30km_frt, aes(x=long, y=lat, group=group),
    color='darkslateblue', size=1)  +
  geom_point(data=baea %>% filter(speed < 1000), aes(x=long, y=lat, color=id),
    shape=19, alpha=.9, size=.75, stroke=1, na.rm=TRUE) +
  scale_color_manual(values=baea_colors, name = "Eagle") +
  geom_point(data=nests_active %>% filter(active_2016 == TRUE),
    aes(x=long, y=lat), shape=24, alpha=.9, color="blue", fill= "purple",
    size=2, stroke=2, na.rm=TRUE) + # Active Nests
  theme_tufte(base_family="Arial") + xlab("Longitude") + ylab("Latitude") +
  guides(colour = guide_legend(override.aes = list(size=3,linetype=0)))

SaveGGPlot(paste0("GPS and Nests - Outline.jpeg"), file.path(image_output,
  "All GPS Location Maps"), bg = "white")

# All locations, nests, Voronoi
ggplot() +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  geom_map(data=maine, map=maine, aes(x=long, y=lat, map_id=region,
    group=group), fill="white", color="black", size=0.25) +
  geom_path(data=nests_30km_frt, aes(x=long, y=lat, group=group),
    color='darkslateblue', size=1)  +
  geom_point(data=baea %>% filter(speed < 1000), aes(x=long, y=lat, color=id),
    shape=19, alpha=.9, size=.75, stroke=1, na.rm=TRUE) +
  scale_color_manual(values=baea_colors, name = "Eagle") +
  geom_point(data=nests_active %>% filter(active_2016 == TRUE),
    aes(x=long, y=lat), shape=24, alpha=.9, color="blue", fill= "purple",
    size=2, stroke=2, na.rm=TRUE) + # Active Nests
  geom_path(data = terr_edge_ll, aes(x=long, y=lat, group=group),
    color='red', size=1) +
  theme_tufte(base_family="Arial") + xlab("Longitude") + ylab("Latitude") +
  guides(colour = guide_legend(override.aes = list(size=3,linetype=0)))

SaveGGPlot(paste0("GPS, Nests, and Voronoi - Outline.jpeg"),
  file.path(image_output, "All GPS Location Maps"), bg = "white")


# Test Plot (GGMap, Points, Polygon (fortified), Raster) -----------------------

maine_map <- get_map(location = "Bangor, Maine", color="color", source="google",
  maptype="hybrid", zoom=7)
maine_ggmap <- ggmap(maine_map, legend="right") +
  theme_tufte(base_family="Arial") + xlab("Longitude") + ylab("Latitude")
maine_ggmap +
#  geom_tile(data=con_dist_gg, aes(x=x, y=y, fill=value), alpha=.8) +
#    scale_con_dist +
#  geom_tile(data=edge_dist_gg, aes(x=x, y=y, fill=value), alpha=.8) +
#    scale_edge_dist +
#  geom_tile(data=edge_dist_shift_gg, aes(x=x, y=y, fill=value), alpha=.8) +
#    scale_edge_dist_shift +
#  geom_tile(data=home_dist_gg, aes(x=x, y=y, fill=value), alpha=.8) +
#    scale_home_dist +
  geom_path(data = terr_edge_ll, aes(x=long, y=lat, group=group),
    color='red', size=1) +
  geom_path(data = nests_30km_frt, aes(x=long, y=lat, group=group),
    color = 'darkslateblue', size = 1)  +
  geom_point(data=baea %>% filter(speed < 1000), aes(x=long, y=lat, color=id),
    shape=19, alpha=.9, size=.75, stroke=1, na.rm=TRUE) +
  scale_color_manual(values=baea_colors, name="Eagle")  +
  guides(colour = guide_legend(override.aes = list(size=3,linetype=0)))

SaveGGPlot(paste0("Overview Map - Google Map.jpeg"), file.path(image_output,
  "All GPS Location Maps"), bg = "white")


# Plotting Individual's GPS Points, Nests, and Territorial Edges w/Google Maps -
zoom_levels <- c(10)
for (i in 1:length(individuals)){
  ind <- individuals[i]
  for (j in 1:length(zoom_levels)){
    zoom_level <- zoom_levels[j]
    baea_ind <- baea_spdf[baea_spdf$id == ind,]
    nest_ind <- nests_active %>%
      dplyr::filter(nest_site == unique(baea_ind$nest_site))
    nest_map <- get_map(location = c(lon=nest_ind$long[1], lat=nest_ind$lat[1]),
      color="color", source="google", maptype="hybrid", zoom=zoom_level)
    nest_ggmap <- ggmap(nest_map, legend="none") +
      theme_tufte(base_family="Arial") + xlab("Longitude") + ylab("Latitude")
    bb <- attr(nest_map, "bb")
    sbar <- data.frame(long_start = c(bb$ll.lon + 0.1*(bb$ur.lon - bb$ll.lon)),
                       long_end = c(bb$ll.lon + 0.25*(bb$ur.lon - bb$ll.lon)),
                       lat_start = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                       lat_end = c(bb$ll.lat + 0.1*(bb$ur.lat - bb$ll.lat)),
                       bb_diag = bb$ur.lat - bb$ll.lat,
                       scalebar.length = scalebar.length)
     sbar$distance <- geosphere::distVincentyEllipsoid(c(sbar$long_start,
      sbar$lat_start), c(sbar$long_end,sbar$lat_end))
    sbar$long_end <- sbar$long_start +
      ((sbar$long_end-sbar$long_start)/sbar$distance) * scalebar.length*1000
    map_title <- cbind.data.frame(name=ind, x=(bb$ll.lon + bb$ur.lon)/2,
      y=bb$ur.lat - 0.02)
    gg <- nest_ggmap +
      stat_density2d(geom="tile", data = baea_ind@data, aes(x = long, y = lat,
        fill=..density..), show.legend = FALSE, contour=FALSE) +
      scale_fill_gradient(name="Density", low = "sienna1",
        high="sienna4", limits = c(1,NA), na.value = NA) +
      geom_point(data=baea_ind@data %>% filter(speed < 1000), aes(x=long, y=lat),
        shape=19, alpha=.9, color="yellow", size=.75, stroke=1, na.rm=TRUE) +
#      scale_color_manual(values=baea_colors, name="Eagle") +
      geom_path(data = nests_30km_frt, aes(x=long, y=lat, group=group),
        color = 'darkslateblue', size = 1)  +
      geom_path(data = terr_edge_ll, aes(x=long, y=lat, group=group),
        color='red', size=1) + # Voronoi Lines
      geom_point(data=nests_active %>% filter(active_2016 == TRUE),
        aes(x=long, y=lat), shape=24, alpha=.9, color="blue", fill= "purple",
        size=2, stroke=2, na.rm=TRUE) + # Active Nests
      geom_segment(data = sbar, aes(x=long_start, xend=long_end, y=lat_start,
        yend=lat_end), size=1.25, arrow=arrow(angle=90, length=unit(0.2, "cm"),
        ends="both", type="open"), color="white") + # Distance Bar
      geom_text(data = sbar, aes(x = (long_start + long_end)/2, y = lat_start +
        0.025*(bb_diag), label=paste(format(scalebar.length),'km')),
        hjust=0.5, vjust=.45, size=18/ptspermm, color="white")  +
      geom_text(data = map_title, aes(x=x, y=y, label=name), size=24/ptspermm,
        color="white")
    print(gg)
      SaveGGPlot(paste0(ind," - Zoom ",zoom_level," and Density.jpeg"),
        file.path(image_output, "Individual GPS Location Maps"), bg = "white")
  }
}



# Test plot --------------------------------------------------------------------

ggplot() +
  geom_tile(data=dat, aes(x = x, y = y, fill=value))    +
  geom_path(data = nests_30km_frt, aes(x=long, y=lat, group=group),
      color = 'red', size = .2) +
  geom_point(data=baea %>% filter(speed < 1000),
    aes(x=long, y=lat, color=id), shape=19, alpha=.9,
    size=.75, stroke=1, na.rm=TRUE) +
    scale_color_manual(values=baea_colors, name = "ID") +
  theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude") +
    theme(legend.justification=c(1,1), legend.position=c(1,.65))  +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) #+


myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(0, 8000))

ggplot(subset(mtcars, am==0), aes(x=wt, y=mpg, colour=carb)) +
  geom_point(size=6) + sc


# Reproject Rasters to Lat/Long (BA Values are
plot(edge_dist)
head(edge_dist)
NAvalue(edge_dist)
edge_dist_ll <- projectRaster(edge_dist, crs = wgs84)
head(edge_dist_ll)
plot(edge_dist_ll)
NAvalue(edge_dist_ll)



x <- sampleRegular(edge_dist_ll, 10000, asRaster = TRUE)
coords <- xyFromCell(x, seq_len(ncell(x)))
dat <- stack(as.data.frame(getValues(x)))
names(dat) <- c("value", "variable")
dat <- cbind(coords, dat)
dat <- dat %>% filter(!is.na(value))
nest_ggmap +
  geom_tile(data=dat, aes(x = x, y = y, fill=value), alpha = .8)


plot(edge_dist_ll)
edge_dist_shift_ll <- projectRaster(edge_dist_shift, crs = wgs84)
iei_ll <- projectRaster(iei, crs = wgs84)

extent(edge_dist_ll)
plot(edge_dist_ll)

for i in 1:length(individuals){
  ind <- individuals[1]
  baea_ind <- baea_spdf[baea_spdf$id == ind,]
  nest_ind <- nests_active %>%
    dplyr::filter(nest_site == unique(baea_ind$nest_site))
  nest_map <- get_map(location = c(lon=nest_ind$long[1], lat=nest_ind$lat[1]),
    color="color", source="google", maptype="hybrid", zoom=11)
  ext_map <- extent(unlist(c(attr(nest_map, "bb")[c(2,4,1,3)])))
  edge_dist_crop <- crop(edge_dist_ll, ext_map, snap='out')
  x <- sampleRegular(edge_dist_crop, 200000, asRaster = TRUE)
  coords <- xyFromCell(x, seq_len(ncell(x)))
  dat <- stack(as.data.frame(getValues(x)))
  names(dat) <- c("value", "variable")
  dat <- cbind(coords, dat)
  nest_ggmap <- ggmap(nest_map, extent="device", ylab="Latitude",
    xlab="Longitude", legend="right")
  nest_ggmap +
#    geom_raster(data=dat, aes(x = x, y = y, fill=value), alpha = .8) +
    geom_point(data=baea_ind@data, aes(x=long, y=lat), shape=19, alpha=.9,
      color="red", size=.75, stroke=1, na.rm=TRUE) + ggtitle(ind) +
    theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude")


for i in 1:length(individuals){
  ind <- individuals[1]
  baea_ind <- baea_spdf[baea_spdf$id == ind,]
  nest_ind <- nests_active %>%
    dplyr::filter(nest_site == unique(baea_ind$nest_site))
  q_plot()



  nest_ggmap <- ggmap(nest_map, extent="device", ylab="Latitude",
    xlab="Longitude", legend="right")
  nest_ggmap +
    geom_point(data=baea_spdf_ind@data, aes(x=long, y=lat), shape=19, alpha=.9,
      color="red", size=.75, stroke=1, na.rm=TRUE) +
  gplot(iei2, maxpixel = 500000) +
  geom_tile(data = iei2, aes(fill=value)) + coord_equal() +
  scale_fill_gradientn(colours = iei_colors, na.value = "transparent",
    name="IEI Index", limits=c(minValue(iei), maxValue(iei)),
    breaks = c(.25, .5, .75)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude")



     geom_tile(data = edge_dist_shift)

   gg <-
    nest_ggmap +
    geom_polygon(data=poly_wgs84, aes(x=long, y=lat), alpha=0.8,
      fill="dodgerblue1", color="black", size=0.2) +
    geom_path(data=as.data.frame(coordinates(mid_ends_wgs84)), aes(x=long_utm,
      y=lat_utm), colour="grey80", size=1, linetype=2) +
    geom_point(data=points1_wgs84, aes(x=long, y=lat), shape=19, alpha=.9,
      color="red", size=.75, stroke=1, na.rm=TRUE) +
    geom_point(data=points1_inside_wgs84, aes(x=long_utm,y=lat_utm), shape=19,
      alpha=.75, color="yellow", size=2, stroke=1, na.rm=TRUE) +
   geom_segment(data = sbar, aes(x=long_start, xend=long_end, y=lat_start,
      yend=lat_end), size=1.25, arrow=arrow(angle=90, length=unit(0.2, "cm"),
      ends="both", type="open"), color="white") +
    geom_text(data = sbar, aes(x = (long_start + long_end)/2, y = lat_start +
      0.025*(bb_diag), label=paste(format(scalebar.length),'km')),
      hjust=0.5, vjust=.7, size=18/ptspermm, color="white")  +
    geom_text(data = map_title, aes(x=x, y=y, label=name), size=24/ptspermm,
      color="white") +
    theme(legend.justification=c(1,1), legend.position=c(1,1))
  print(gg)
  }

gplot(iei, maxpixel = 500000) +
  geom_tile(aes(fill=value)) + coord_equal() +
  scale_fill_gradientn(colours = iei_colors, na.value = "transparent",
    name="IEI Index", limits=c(minValue(iei), maxValue(iei)),
    breaks = c(.25, .5, .75)) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  theme_tufte(base_size = 15) + xlab("Longitude") + ylab("Latitude")

#con_dist <- reclassify(con_dist, c(-Inf,0,1,1,Inf,0)) # Useful for any raster
#con_dist[is.na(con_dist)] <- 0 # Useful for any raster

## Import Deployed BAEA Data ---------------------------------------------------
deployed_all <- ImportUnits(units="deployed", existing=NULL,
  import=TRUE)

wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

## Import Base
base = raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

## Import Nests ----------------------------------------------------------------

nest_set <- read.csv2(file=file.path("C:/Work/R/Data/BAEA",
  "Nests/Nests_Study_Intact_Active.csv"), header=TRUE, sep=",",
  stringsAsFactors=FALSE)
nest_set$long_utm <- as.numeric(nest_set$long_utm)
nest_set$lat_utm <- as.numeric(nest_set$lat_utm)

############## CREATE NEST AND CONSPECIFIC DISTANCE RASTERS ####################

baea <- FilterLocations(df=deployed_all, id="id",
  individual=c("Ellis","Sandy", "Musquash", "Hebron"),
  start="2016-05-01", end="2016-09-01")
baea <- AddNestData(df=baea)
ExportShapefileFromPoints(df = baea, lat = "lat", long = "long",
  name = "baea", folder = "C:/Work/R/Workspace/2016_GPS_Data",
  crs = "+proj=longlat +datum=WGS84", overwrite = TRUE)

baea_spdf <- SpatialPointsDataFrame(baea[c("long_utm", "lat_utm")],
  bbox=NULL, data=baea, proj4string=wgs84n19)
plot(baea_spdf)

## Calculate Homerange Distance ------------------------------------------------

homerange_distances <- AddHomeConEdgeDistanceBAEA(baea, nest_set, base,
  output_dir = "C:/Work/R/Workspace/2016_Nests_Rasters", max_r = 30000,
  write_home_dist = TRUE, write_con_dist = TRUE, write_edge_dist = TRUE,
  write_terr_edge = TRUE)

## Import "Nests_Study_Intact_Active.csv" Data ---------------------------------
nests_study_intact_active <- read.csv2(file=file.path("C:/Work/R/Data/BAEA",
  "Nests/Nests_Study_Intact_Active.csv"), header=TRUE, sep=",",
  stringsAsFactors=FALSE)
nests_study_intact_active$lat_utm <- as.numeric(nests_study_intact_active$lat_utm)
nests_study_intact_active$long_utm <- as.numeric(nests_study_intact_active$long_utm)

## Import "Nests_Study.csv" Data -----------------------------------------------
nests_study <- read.csv2("C:/Work/R/Data/BAEA/Nests/Nests_Study.csv",
  header=TRUE, sep=",", stringsAsFactors = FALSE)
nests_study$eagle_id <- nests_study$name

## Import "Nests_Study_Use_Dates.csv" Data -------------------------------------
nests_use_dates <- read.csv2(file.path("C:/Work/R/Data/BAEA/Nests",
  "Nests_Study_Use_Dates.csv"), header=TRUE, sep=",", stringsAsFactors=FALSE)

############## CREATE NEST AND CONSPECIFIC DISTANCE RASTERS ####################

source('C:/Work/R/Functions/baea.R')
source('C:/Work/R/Functions/all.R')
years <- 2016  # these must be unique
study_nests <- nests_study
all_nests <- nests_study_intact_active
nest_con_dist <- CreateRasterNestConDist(years, 1000, study_nests, all_nests)

## Export all rasters by nest_id and year name ---------------------------------
source('C:/Work/R/Functions/gis.R')
ExportRasterNestConDist(nest_con_dist, dir="C:/Work/R/Workspace")

########  EXTRACT NEST AND CONSPECIFIC DISTANCES FOR LOCATION DATA  ############

## Import BAEA and Make into a SpatialPointsDataframe --------------------------
source('C:/Work/R/Functions/baea.R')
source('C:/Work/R/Functions/gis.R')
baea <- ImportBAEA(existing=NULL, import=TRUE, tz="Etc/GMT+5")
baea <- AddNestData(baea)
baea <- AddNestConDist(baea, nest_con_dist)
ExportShapefileFromPoints(baea, lat="lat_utm", long="long_utm",
  name="BAEA2", folder="C:/ArcGIS/Data/BAEA/Telemetry/BAEA",
  crs="+proj=utm +zone=19 +datum=NAD83")


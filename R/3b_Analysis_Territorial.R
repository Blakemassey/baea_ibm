# ------------------------- TERRITORIAL ANALYSIS ----------------------------- #
# This script is for calculating the territorial distribution of eagles by
# calculating their position relative to their nest and their neighbors nests
#------------------------------------------------------------------------------#

## Load Packages, Scripts, etc. ------------------------------------------------
pacman::p_load(ggthemes, ggmap, tidyverse, maps, raster, RColorBrewer, rgeos,
  tools)
options(stringsAsFactors = FALSE)
theme_update(plot.title = element_text(hjust = 0.5))
package_dir <- "C:/Users/blake/OneDrive/Work/R/Packages"

library(baear)
devtools::reload(file.path(package_dir, "baear"))
library(gisr)  # devtools::reload(file.path(package_dir, "gisr"))
library(ibmr)  # devtools::reload(file.path(package_dir, "ibmr"))


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
image_output <- file.path("C:/TEMP")

############################  IMPORT FILES  ####################################

## Import Baea, Nests, and Base ------------------------------------------------
baea_hr <- readRDS("Data/BAEA/baea_homerange.rds")

nests_active <- readRDS(file="Data/Nests/Nests_rds/nests_active.RDS")
base = raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

## Filter BAEA Data ------------------------------------------------------------

baea_terr <- baea_hr %>%
  filter(id %in% c("Ellis", "Sandy", "Musquash", "Hebron")) %>%
  filter(date >= as.Date("2016-03-15") & date <= as.Date("2016-08-15"))
table(baea_terr$id)
table(baea_terr$nest_site)

baea_terr <- baea_terr %>% mutate(x = long_utm, y = lat_utm)
baea_terr <- CenterXYWithBase(baea_terr, base)
baea_terr <- baea_terr %>% mutate(long_utm = x, lat_utm = y)

saveRDS(baea_terr, "Data/BAEA/baea_terr.rds")

############## CREATE NEST AND CONSPECIFIC DISTANCE RASTERS ####################

## Calculate Homerange Distance ------------------------------------------------

con_nest <- CreateConNestDistRasters(baea_terr, nests_active, base,
  output_dir = "Output/Analysis/Territorial", max_r = 30000,
  write_con_nest_all = TRUE)

################ LOAD NEST AND CONSPECIFIC DISTANCE RASTERS ####################

con_nest <- raster("Output/Analysis/Territorial/ConNest_All.tif")
plot(con_nest)

######################## EXTRACT DISTANCE DATA #################################

baea_terr <- readRDS("Data/BAEA/baea_terr.rds")

## Extract raster values and put into baea
baea_terr$con_nest <- raster::extract(con_nest, baea_terr[,c("long_utm",
  "lat_utm")])

table(baea_terr$id)

baea_dist <- baea_terr %>% mutate(con_nest_km = con_nest/1000)
sum(is.na(baea_dist$con_nest)) # should be 0

saveRDS(baea_dist, "Data/BAEA/baea_dist.rds")

#ExportKMLTelemetryBAEA(baea_dist, file = "baea_dist.kmz")

hist(baea_dist$con_nest_km, breaks = 0:max(ceiling(baea_dist$con_nest_km)))
sum(is.na(baea_dist$con_nest_km))

head(sort(unique(baea_dist$con_nest_km)),5)
head(table(baea_dist$con_nest_km),10)

#################### FITTING CON_NEST ##########################################

library(fitdistrplus)
library(dplyr)
library(extraDistr)
library(VGAM)
set.seed(2019)

baea_dist <- readRDS("Data/BAEA/baea_dist.rds")

# Fitting with con_nest > 75
baea_dist_samp <- readRDS("Data/BAEA/baea_dist.rds") %>%
  filter(!is.na(con_nest)) %>%
  filter(con_nest > 0) %>%
  dplyr::select(id, con_nest, con_nest_km, long_utm, lat_utm, lat, long) %>%
  group_by(id) %>%
  sample_n(2000, replace = TRUE) %>%
  ungroup()
# No need to resave baea_dist_samp (originally done on 2019-10-23)
#saveRDS(baea_dist_samp, "Data/BAEA/baea_dist_samp.rds")

baea_dist_samp <- readRDS("Data/BAEA/baea_dist_samp.rds")
descdist(baea_dist_samp$con_nest_km, boot = 100)

fits_baea_dist <- list(
  exponential = fitdist(baea_dist_samp$con_nest_km, "exp"),
  halfnorm = fitdist(baea_dist_samp$con_nest_km, "hnorm",
    start = list(sigma = sqrt(pi))),
  gamma = fitdist(baea_dist_samp$con_nest_km, "gamma"),
  pareto = fitdist(baea_dist_samp$con_nest_km, "gpd",
    start = list(scale = 5, shape = 1)),
  weibull = fitdist(baea_dist_samp$con_nest_km, "weibull")
)
saveRDS(fits_baea_dist, file = "Output/Analysis/Territorial/fits_baea_dist.rds")

sapply(fits_baea_dist, function(i) summary(i))
sapply(fits_baea_dist, function(i) coef(i))

plot(fits_baea_dist$exponential)
plot(fits_baea_dist$halfnorm)
plot(fits_baea_dist$gamma)
plot(fits_baea_dist$pareto)
plot(fits_baea_dist$weibull)

fit_name <- "Con_Nest Distance"

plot(fits_baea_dist$exponential)
mtext(paste(fit_name, "-", "Exponential"), font=2, line = 4.5)
  ## The placement of the text depends on how plot screen is sized - 4 is
  ## right at the top of the graph when the plot window is 1/4 of screen
SavePlot(paste0(fit_name, " - Exponential.jpeg"), image_output)

plot(fits_baea_dist$gamma)
mtext(paste(fit_name, "-", "Gamma"), font=2, line = 4.5)
SavePlot(paste0(fit_name, " - Gamma.jpeg"), image_output)

plot(fits_baea_dist$halfnorm)
mtext(paste(fit_name, "-", "Half Normal"), font=2, line = 4.5)
SavePlot(paste0(fit_name, " - Half Normal.jpeg"), image_output)

plot(fits_baea_dist$pareto)
mtext(paste(fit_name, "-", "Pareto"), font=2, line = 4.5)
SavePlot(paste0(fit_name, " - Pareto.jpeg"), image_output)

plot(fits_baea_dist$weibull)
mtext(paste(fit_name, "-", "Weibull"), font=2, line = 4.5)
SavePlot(paste0(fit_name, " - Weibull.jpeg"), image_output)

# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #
#
#
# Map Con, Nest and Con_Nest Distances --------------------------------------- #
#
# wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N
#
# ## Import Base
# base = raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))
#
# nests_2016 <- nests_active %>%
#   filter(active_2016 == TRUE) %>%
#   transmute(long = long_utm, lat = lat_utm)
#
# nests <- baea_terr %>% group_by(id) %>% slice(1) %>%
#     dplyr::select(nest_long_utm, nest_lat_utm)  %>%
#     transmute(long = nest_long_utm, lat = nest_lat_utm)
#
# home_dist_gg <- ConvertRasterForGGPlot(home_dist)
#
# ggplot(home_dist_gg, aes(x, y)) +
#   geom_tile(aes(fill = value), interpolate=TRUE) +
#   coord_fixed(ratio = 1) +
#   scale_fill_distiller(breaks = seq(0,40000, by=5000), name="Meters",
#     palette = "Blues", direction=-1) +
#   geom_point(data = nests_2016, aes(long, lat), shape=24, alpha=.9,
#     color="red", fill= "black", size=2, stroke=2) +
#   geom_point(data = nests, aes(long, lat), shape=24, alpha=.9,
#     color="blue", fill= "floralwhite", size=2, stroke=2) +
#   geom_point(data = baea_dist, aes(long_utm, lat_utm), shape=4, alpha=.9,
#     color="yellow", size=1, stroke=1.5) +
#   geom_point(data = nests, aes(long, lat), shape=24, alpha=.9,
#     color="blue", fill= "floralwhite", size=2, stroke=2) +
#   theme_legend +
#   ggtitle(paste("Stationary Locations")) + xlab("Longitude") +
#   ylab("Latitude")
#
# SaveGGPlot("Stationary Locations.png", image_output, bg = "white")
#
# for (i in unique(baea$id)){
#   con_dist_i <- raster(file.path("C:/Work/R/Workspace",
#     "2016_Nests_Rasters/2016",paste0("ConDist_", i, ".tif")))
#
#   con_dist_nest_i <- raster(file.path("C:/Work/R/Workspace",
#     "2016_Nests_Rasters/2016",paste0("ConDistNest_", i, ".tif")))
#   home_dist_i <- raster(file.path("C:/Work/R/Workspace/2016_Nests_Rasters",
#     "2016", paste0("HomeDist_", i ,".tif")))
#   con_nest_i <- overlay(home_dist_i, con_dist_nest_i,
#     fun=function(x,y){round(x+y)})
#   nest_i <-  baea %>% filter(id == i) %>% slice(1) %>%
#     dplyr::select(nest_long_utm, nest_lat_utm)  %>%
#     transmute(long = nest_long_utm, lat = nest_lat_utm)
#
#   nests_2016_i <- nests_2016 %>%
#     filter(long >= xmin(con_dist_nest_i) & long <= xmax(con_dist_nest_i)) %>%
#     filter(lat >= ymin(con_dist_nest_i) & lat <= ymax(con_dist_nest_i))
#
#   home_dist_i_gg <- ConvertRasterForGGPlot(home_dist_i)
#   ggplot(home_dist_i_gg, aes(x, y)) +
#     coord_fixed(ratio = 1) +
#     geom_raster(aes(fill = value), interpolate=TRUE) + theme_legend +
#     scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
#     scale_fill_distiller(breaks = seq(0,40000, by=5000), name="Meters",
#       palette = "Blues", direction=-1) +
#     ggtitle(paste(i, "- Home Distance")) +
#     xlab("Longitude") + ylab("Latitude") +
#     geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
#       color="red", fill= "black", size=2, stroke=2) +
#     geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
#       color="blue", fill= "white", size=2, stroke=2)
#   SaveGGPlot(paste0(i, " - Home Distance.png"),
#     file.path(image_output), bg = NA)
#
#   con_dist_nest_i_gg <- ConvertRasterForGGPlot(con_dist_nest_i)
#   ggplot(con_dist_nest_i_gg, aes(x, y)) +
#     coord_fixed(ratio = 1) +
#     geom_raster(aes(fill = value)) + theme_legend +
#     scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
#     scale_fill_gradient2(name="Meters", low="white", mid="grey", high="tan4") +
#     ggtitle(paste(i, "- Conspecific Distance")) +
#     xlab("Longitude") + ylab("Latitude")  +
#     geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
#       color="red", fill= "black", size=2, stroke=2) +
#     geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
#       color="blue", fill= "white", size=2, stroke=2)
#   SaveGGPlot(paste0(i, " - Conspecific Distance.png"),
#     file.path(image_output), bg = NA)
#
#   con_dist_i_gg <- ConvertRasterForGGPlot(con_dist_i)
#   ggplot(con_dist_i_gg, aes(x, y)) +
#     coord_fixed(ratio = 1) +
#     geom_raster(aes(fill = value)) + theme_legend +
#     scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
#     scale_fill_gradient2(name="Meters", high="white", mid="grey", low="tan4",
#       midpoint=round((range(con_dist_i_gg$value)[2] -
#           range(con_dist_i_gg$value)[1])/2)) +
#     ggtitle(paste(i, "- Conspecific (actual) Distance")) +
#     xlab("Longitude") + ylab("Latitude")  +
#     geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
#       color="red", fill= "black", size=2, stroke=2) +
#     geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
#       color="blue", fill= "white", size=2, stroke=2)
#   SaveGGPlot(paste0(i, " - Conspecific (actual) Distance.png"),
#     file.path(image_output), bg = NA)
#
#   con_nest_i_gg <- ConvertRasterForGGPlot(con_nest_i)
#   ggplot(con_nest_i_gg, aes(x, y)) +
#     coord_fixed(ratio = 1) +
#     geom_raster(aes(fill = value)) + theme_legend +
#     scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
#     scale_fill_gradientn(name = "Meters", colours = terrain.colors(10)) +
#     ggtitle(paste(i, "- Con/Nest Distance")) +
#     xlab("Longitude") + ylab("Latitude")  +
#     geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
#       color="red", fill= "black", size=2, stroke=2) +
#     geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
#       color="blue", fill= "white", size=2, stroke=2) +
#   SaveGGPlot(paste0(i, " - Con_Nest Distance.png"), file.path(image_output),
#     bg = NA)
#
#   baea_dist_i <- baea_dist %>% filter(id == i)
#   ggplot(con_nest_i_gg, aes(x, y)) +
#     coord_fixed(ratio = 1) +
#     geom_raster(aes(fill = value)) + theme_legend +
#     scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
#     scale_fill_gradientn(name = "Meters", colours = terrain.colors(10)) +
#     ggtitle(paste(i, "- Con/Nest Distance")) +
#     xlab("Longitude") + ylab("Latitude")  +
#     geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
#       color="red", fill= "black", size=2, stroke=2) +
#     geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
#       color="blue", fill= "white", size=2, stroke=2) +
#     geom_point(data = baea_dist_i, aes(long_utm, lat_utm), shape=4, alpha=.9,
#       color="black", size=2, stroke = 1.2)
#   SaveGGPlot(paste0(i, " - Con_Nest Distance with GPS Locations.png"),
#     file.path(image_output), bg = NA)
# }
#
# ### TESTING USING SANDY ----------------------------------------------------#
#
# nestcon_gamma_shape = fits_baea_dist$gamma$estimate["shape"]
# nestcon_gamma_rate = fits_baea_dist$gamma$estimate["rate"]
#
# nest_Sandy <- baea %>% filter(id == "Sandy") %>% slice(1) %>%
#   dplyr::select(nest_long_utm, nest_lat_utm)  %>%
#   transmute(long = nest_long_utm, lat = nest_lat_utm)
# nest_Sandy <- as.numeric(nest_Sandy[1,])
#
# con_dist_nest_Sandy <- raster(file.path("C:/Work/R/Workspace",
#   "2016_Nests_Rasters/2016/ConDistNest_Sandy.tif"))
# home_dist_Sandy <- raster(file.path("C:/Work/R/Workspace/2016_Nests_Rasters",
#   "2016/HomeDist_Sandy.tif"))
#
# plot(home_dist_Sandy, col=terrain.colors(255), main= "Sandy")
# points(nest_Sandy[1], nest_Sandy[2], pch=20, col="blue")
# plot(con_dist_nest_Sandy, col=terrain.colors(255), main= "Sandy")
# points(nest_Sandy[1], nest_Sandy[2], pch=20, col="blue")
#
# con_nest_Sandy <- overlay(home_dist_Sandy, con_dist_nest_Sandy,
#   fun=function(x,y){round(x+y)})
#
# plot(con_nest_Sandy, col=terrain.colors(255), main= "Sandy - ConNest Distance")
# #  legend.args=list(text="Con D", cex=1, side=3, line=1))
# points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
#
# loc_pts <- data.frame(
#   x = c(500000, 475000),
#   y = c(4920000, 4930000),
#   title = c("Near Edge", "Above Nest"))
# points(loc_pts$x, loc_pts$y, size=2, pch=4, lwd=2, col="black")
#
# SavePlot("Sandy with Points.jpeg", image_output)
#
# step_max_r <- 15000 #15km
# cellsize <- res(base)[1]
# con_nest_raster <- con_nest_Sandy
#
# for (i in 1:nrow(loc_pts)){
#   x <- loc_pts[i, "x"]
#   y <- loc_pts[i, "y"]
#   title <- loc_pts[i, "title"]
#   max_r_cells <- ceiling(max_r/cellsize)
#   xmin <- xmin(base)
#   ymin <- ymin(base)
#   xy <- CenterXYInCell(x, y, xmin, ymin, cellsize)  # May be unnecessary
#   cell_extent <- extent(xy[1]-(cellsize/2), xy[1]+(cellsize/2), xy[2]-
#     (cellsize/2), xy[2]+(cellsize/2))
#   cell <- setValues(raster(cell_extent, crs=projection(base), res=cellsize),1)
#   movement_kernel <- extend(cell, c(max_r_cells, max_r_cells), value=NA)
#   con_nest_crop <- crop(con_nest_raster, movement_kernel, snap='in')
#   plot(con_nest_crop, col=terrain.colors(255), main = paste0(title,
#     " - ConNest Distance"))
#   points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
#   points(x, y, size=2, pch=4, lwd=2, col="black")
#   SavePlot(paste0("Sandy - ", title , ".jpeg"), image_output)
#   xy_pt <- data.frame(x = xy[1], y = xy[2])
#   xy_con_nest <- extract(con_nest_crop, xy_pt)
#   con_nest_adjust <- calc(con_nest_crop, fun=function(x){(x - xy_con_nest)/1000})
#   plot(con_nest_adjust, col=terrain.colors(255), main = paste0(title,
#     " - ConNest Distance (Shifted)"))
#   points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
#   points(x, y, size=2, pch=4, lwd=2, col="black")
#   (extract(con_nest_adjust, xy_pt)) #should be zero
#   SavePlot(paste0("Sandy - ", title ," Shifted.jpeg"), image_output)
#   xy_log_scale <- NonlinearRangeRescaleGamma(x=(xy_con_nest/1000),
#     shape=nestcon_gamma_shape, rate=nestcon_gamma_rate,  min=NULL, max=NULL,
#     lowBound=1, upBound=NULL, movement_kernel=movement_kernel, negative=TRUE)
#   curve(LogisticByInflection(x, inflection=0, scale=xy_log_scale), -15, 15,
#     main = paste0("Logistic - ", title, "; Scale = ", round(xy_log_scale, 2)),
#     ylab = "Probability")
#   SavePlot(paste0("Sandy - ", title ," - Logistic Curve.jpeg"), image_output)
#   LogisticByInflection2 <- function(x){
#     x <- LogisticByInflection(x, inflection=0, scale=xy_log_scale)
#   }
#   con_nest_rescale <- calc(con_nest_adjust, fun=LogisticByInflection2)
#   plot(con_nest_rescale, main = paste0(title, " - Rescaled Probability"))
#   points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
#   points(x, y, size=2, pch=4, lwd=2, col="black")
#   SavePlot(paste0("Sandy - ", title ," - Rescaled .jpeg"), image_output)
# }
#
# NonlinearRangeRescaleGamma <- function(x,
#                                        shape = shape,
#                                        rate = rate,
#                                        min = NULL, #e.g., .001; pgamma
#                                        max = NULL, #e.g., .99; pgamma
#                                        lowBound = 1,
#                                        upBound = NULL,
#                                        movement_kernel = movement_kernel,
#                                        negative = TRUE){
#   if(is.null(min)){
#     max_distance <- qgamma(0.999, shape=shape, rate=rate)
#     min <- pgamma(max_distance, shape=shape, rate=rate, lower.tail=FALSE)
#   }
#   if(is.null(max)){
#     max <- pgamma(.075, shape=shape, rate=rate, lower.tail=FALSE)
#   }
#   if(is.null(upBound)){
#     upBound <- sqrt((xmin(movement_kernel)-xmax(movement_kernel))^2+
#                       (ymin(movement_kernel)-ymax(movement_kernel))^2)/1000
#   }
#   # Get predicted y (cummulative gamma) for x
#   y_pred <- pgamma(x, shape=shape, rate=rate, lower.tail=FALSE)
#   rescale <- lowBound + (((y_pred-min)/(max-min)) * (upBound-lowBound))
#   if(negative==TRUE){
#     rescale <- rescale*-1
#   }
#   return(rescale)
# }
#
# LogisticByInflection <- function(x,
#                                  inflection,
#                                  scale) {
#   y <- (1/(exp((-(x - inflection)) / scale) + 1))
#   return(y)
# }
#
#
#
# # This code should be re-written so the table is placed in "Products"
# suppressPackageStartupMessages(library(fitdistrplus))
# suppressPackageStartupMessages(library(xtable))
# getwd()
# load("Output/fits_movements.RData")
# options(xtable.comment = FALSE)
#
# fits_df <- baear::SummarizeFitdist(fits_movements)
# print(xtable(fits_df),latex.environments = "", include.rownames=FALSE)
#
#
#
#

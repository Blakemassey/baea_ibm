--------------------------------------------------------------------------------
### This script is for importing baea data (.csv) and nest data (.RData) to
### create raster layers of home dist and con dist, perform analyses, and plot
--------------------------------------------------------------------------------

# Load libraries, scripts, and input parameters
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(maps))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(rgeos))
source('C:/Work/R/Functions/all.R')
source('C:/Work/R/Functions/sim/move.R')
options(stringsAsFactors=FALSE)

wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N
image_output <- file.path("C:/Users/blake/Documents/PhD Program",
  "McGarigal Lab Presentations/Lab Presentation - 2017.04.25/Images")
output_dir = "C:/Users/blake/Desktop"

############################  IMPORT FILES  ####################################

## Import Baea, Nests, and Base ------------------------------------------------
load(file="C:/Work/R/Data/R_Input/BAEA/baea.RData")
load(file="C:/Work/R/Data/R_Input/BAEA/nests_active.RData")
base = raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

## Filter BAEA Data ------------------------------------------------------------

individuals = c("Ellis","Sandy", "Musquash", "Hebron")

baea <- FilterLocations(df=baea, id="id", individual=individuals,
  start="2016-05-01", end="2016-09-01")
baea <- AddNestData(df=baea)

ExportKMLTelemetryBAEA(baea, file="BAEA_2016.kmz", output_dir=output_dir)

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

#################### FITTING CON_NEST ##########################################

library(fitdistrplus)
library(dplyr)
library(extraDistr)
library(texmex)

movements <- baea %>% group_by("id") %>%
  filter(step_length > 42.5) %>%
  filter(step_time <= 20) %>%
  mutate(step_length = step_length/1000)
  ungroup()

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
    geom_point(data = movements, aes(long_utm, lat_utm), shape=4, alpha=.9,
      color="chartreuse2", size=1, stroke=2) +
    geom_point(data = nests, aes(long, lat), shape=24, alpha=.9,
      color="blue", fill= "floralwhite", size=2, stroke=2) +
    theme_legend +
    ggtitle(paste("Movement Locations")) + xlab("Longitude") + ylab("Latitude")
  SaveGGPlot("Movement Locations.png", image_output, bg = "white")


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


descdist(movements$step_length, boot=100)

fits_movements <- list(
  exponential = fitdist(movements$step_length, "exp"),
  halfnorm = fitdist(movements$step_length, "hnorm", start=list(sigma=
    sqrt(pi/2))),
  gamma = fitdist(movements$step_length, "gamma"),
  pareto = fitdist(movements$step_length, "gpd", start=list(sigma=2, xi=2)),
  weibull = fitdist(movements$step_length, "weibull")
)

save(fits_movements, file = "Output/fits_movements.RData")

sapply(fits_movements, function(i) summary(i))
sapply(fits_movements, function(i) coef(i))

plot(fits_movements$exponential)
plot(fits_movements$halfnorm)
plot(fits_movements$gamma)
plot(fits_movements$pareto)
plot(fits_movements$weibull)

ggplot(movements, aes(step_length)) + stat_ecdf(geom = "step") +
  xlab("Step Length Distance (km)") + ylab("ECD") + theme_no_legend

movements_lines <- data.frame(x=movements$step_length,
  HalfNorm=dhnorm(movements$step_length, fits_movements$halfnorm$estimate["sigma"]),
  Exponential=dexp(movements$step_length, fits_movements$exponential$estimate["rate"]),
  Pareto=texmex::dgpd(movements$step_length, fits_movements$pareto$estimate["sigma"],
    fits_movements$pareto$estimate["xi"]),
  Gamma=dgamma(movements$step_length,
    fits_movements$gamma$estimate["shape"], fits_movements$gamma$estimate["rate"]),
  Weibull=stats::dweibull(movements$step_length,
    fits_movements$weibull$estimate["shape"], fits_movements$weibull$estimate["scale"]))

ggplot(movements) +
  geom_histogram(aes(x=step_length, y=..density..), color="black", fill="grey",
    breaks = seq(0, max(movements$step_length), by=.25)) +
  ggtitle("Step Length (km)") +
  xlab("Kilometers") + ylab("Density") + theme_legend +
#  geom_line(data = movements_lines, aes(x, HalfNorm, color = "HalfNorm"), size = 1.1) +
#  geom_line(data = movements_lines, aes(x, Exponential, color = "Exponential"), size = 1.1) +
#  geom_line(data = movements_lines, aes(x, Gamma, color = "Gamma"), size = 1.1) +
  geom_line(data = movements_lines, aes(x, Weibull, color = "Weibull"), size = 1.1) +
#  geom_line(data = movements_lines, aes(x, Pareto,  color="Pareto"), size = 1.1) +
  scale_color_manual(name = "Fitted \nDistributions",
    values = c("Exponential" = "green", "HalfNorm" = "darkorchid1",
      "Pareto" = "red1", "Gamma" = "blue1",
      "Weibull" = "yellow"))

SaveGGPlot(paste0("Movements Distribution Fits.png"),
  file.path(image_output), bg = "black")

##  Using Weibull Fit ----------------------------------------------------------

max_r <- qweibull(.995, fits_movements$weibull$estimate["shape"],
  fits_movements$weibull$estimate["scale"]) * 1000

nestcon_gamma_shape <- 1.1
nestcon_gamma_rate <- 0.495

(step_weibull_scale = fits_movements$weibull$estimate["scale"])
(step_weibull_shape = fits_movements$weibull$estimate["shape"])


con_nest_Sandy <- overlay(home_dist_Sandy, con_dist_nest_Sandy,
  fun=function(x,y){round(x+y)})

plot(con_nest_Sandy, col=terrain.colors(255), main= "Sandy - ConNest Distance")
#  legend.args=list(text="Con D", cex=1, side=3, line=1))
points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")


loc_pts <- data.frame(
  x = c(500015, 474995),
  y = c(4919965, 4930015),
  title = c("Near Edge", "Above Nest"))
points(loc_pts$x, loc_pts$y, size=2, pch=4, lwd=2, col="black")


CenterXYInCell(x = c(500000, 475000),y = c(4920000, 4930000), xmin(base), ymin(base), 30)

step_max_r = 15000

cellsize = 30
mu = 0
rho = .5
scale = 1.172086
shape = 0.7081443

redist <- CreateRedistKernelWeibull(max_r=step_max_r, cellsize=cellsize,
  mu=mu, rho=rho, shape=shape, scale=scale)

r <- (cellsize*((nrow(redist)-1)/2))+(cellsize/2)
redist_raster <- raster::raster(redist, xmn=-r, xmx=r, ymn=-r, ymx=r)
plot(redist_raster, main= "Movement Kernel")
SavePlot("Movement Kernel.jpeg", image_output)

i <- 2
redist_shift <- raster::shift(redist_raster, x=loc_pts$x[i],
  y=loc_pts$y[i])
plot(redist_shift, main= "Sandy - Movement Kernel")
points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
points(loc_pts$x[i], loc_pts$y[i], size=2, pch=4, lwd=2, col="black")
SavePlot(paste0("Sandy - Movement Kernel - ", i, ".jpeg"), image_output)

redist_shift <- raster::crop(redist_shift, base, snap="in")
con_nest <- CreateConNestProb(con_nest_raster = con_nest_Sandy,
  gamma_shape=nestcon_gamma_shape, gamma_rate=nestcon_gamma_rate,
  x=loc_pts$x[i], y=loc_pts$y[i], max_r=step_max_r, cellsize=cellsize,
  base=base)
plot(con_nest, main= "Sandy - ConNest Probability")
points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
points(loc_pts$x[i], loc_pts$y[i], size=2, pch=4, lwd=2, col="black")
SavePlot(paste0("Sandy - ConNest Probability - ", i, ".jpeg"), image_output)

con_nest_crop <- raster::crop(con_nest, redist_shift, snap="out")
redist_shift_crop <- raster::crop(redist_shift, con_nest, snap="out")

redist_shift_crop
con_nest_crop

prob_raster <- raster::overlay(redist_shift_crop, con_nest_crop,
  fun=function(a,b){return(a*b)}, recycle=FALSE)

prob_raster <- prob_raster/raster::cellStats(prob_raster, stat="sum")
plot(prob_raster, main= "Sandy - Redistribution Kernel")
points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
points(loc_pts$x[i], loc_pts$y[i], size=2, pch=4, lwd=2, col="black")
SavePlot(paste0("Sandy - Redistribution Kernel - ", i, ".jpeg"), image_output)

print("prob_min:", raster::minValue(prob_raster))
raster::crs(prob_raster) <- raster::crs(sim$spatial$base)

CreateConNestProb











CreateRedistKernelWeibull <- function(max_r = 300,
                                      cellsize = 30,
                                      mu,
                                      rho,
                                      shape,
                                      scale,
                                      ignore_cauchy = FALSE,
                                      ignore_weibull = FALSE) {

max_r <- qweibull(.99, fits_movements$weibull$estimate["shape"],
  fits_movements$weibull$estimate["scale"]) * 1000
#max_r <- 100
cellsize <- 30
mu = 0
rho = .5
scale = step_weibull_scale
shape = step_weibull_shape
ignore_cauchy = FALSE
ignore_weibull = FALSE

ptm <- proc.time()

  if (is.null(max_r)) max_r <- qweibull(.99, shape, scale) * 1000
  # Create the empty kernel objects
  max_r_cells <- ceiling(max_r/cellsize)
  size <- max_r_cells * 2 + 1
  center <- max_r_cells + 1
  angle_matrix <- row_matrix <- col_matrix <- new("matrix", 0, size, size)
  distance_matrix <- new("matrix", 0, size, size)
  i <- j <-  1:size
  row_matrix[] <- rep(i, times  = max(j))
  col_matrix <- t(row_matrix)
  dx <- row_matrix - center
  dy <- col_matrix - center
  abs_angle <- atan2(dx, dy)
  angle_matrix <- ifelse(abs_angle < 0, (2*pi) + abs_angle, abs_angle)
  wrpc_kernel <- suppressWarnings(dwrappedcauchy(angle_matrix, mu=mu, rho=rho))
  distance_matrix <- (sqrt((row_matrix - center)^2 + (col_matrix - center)^2) *
      cellsize) / 1000
  weibull_kernel <- dweibull(distance_matrix, scale=scale, shape=shape)
  weibull_kernel[center, center] <- 0  # Forces agent to move from current cell
  # This last part deletes the cells at the edge if they are all zero
  if (all(wrpc_kernel[1, ] == 0, wrpc_kernel[, 1] == 0,
    wrpc_kernel[nrow(wrpc_kernel),] == 0, wrpc_kernel[, ncol(wrpc_kernel)] ==0))
    wrpc_kernel <- wrpc_kernel[2:(nrow(wrpc_kernel) - 1), 2:(ncol(wrpc_kernel)
      - 1)]
  if (all(weibull_kernel[1, ] == 0, weibull_kernel[, 1] == 0,
    weibull_kernel[nrow(weibull_kernel),] == 0, weibull_kernel[,
      ncol(weibull_kernel)] == 0))
    weibull_kernel <- weibull_kernel[2:(nrow(weibull_kernel) - 1),
      2:(ncol(weibull_kernel) - 1)]
  # Multiply the two kernels together and re-normalize
  if (ignore_cauchy) wrpc_kernel <- 1
  if (ignore_weibull) weibull_kernel <- 1
  redist_kernel <- weibull_kernel*wrpc_kernel
  redist_kernel <- redist_kernel/sum(redist_kernel)
#  return(redist_kernel)

proc.time() - ptm

##
redist <- redist_kernel
r <- (cellsize*((nrow(redist)-1)/2))+(cellsize/2)

redist_raster <- raster(redist, xmn=-r, xmx=r, ymn=-r, ymx=r)
redist_shift <- shift(redist_raster, x=50000, y=50000)

plot(redist_shift)
Plot3DRaster(redist_shift, main="Redist Kernel", border=NA)



  ptm <- proc.time()
  for (i in 1:size) {
    for (j in 1:size) {
      r = (sqrt((i - center)^2 + (j - center)^2) * cellsize) / 1000
      b = AngleToPoint(center, center, j, i)
      if(r <= max_r){
#        distance_kernel[i, j] <- r
        wrpc_kernel[i, j] <- round(suppressWarnings(dwrappedcauchy(b,
          mu=mu, rho=rho)), 5)
        weibull_kernel[i, j] <- stats::dweibull(r, scale=scale, shape=shape,
          log=FALSE)
      }
    }
  }
  proc.time() - ptm
  wrpc_kernel <- apply(wrpc_kernel, 2, rev)
  weibull_kernel[center, center] <- 0  # Forces agent to move from current cell
  # This last part deletes the cells at the edge if they are all zero
  if (all(wrpc_kernel[1, ] == 0, wrpc_kernel[, 1] == 0,
    wrpc_kernel[nrow(wrpc_kernel),] == 0, wrpc_kernel[, ncol(wrpc_kernel)] ==0))
    wrpc_kernel <- wrpc_kernel[2:(nrow(wrpc_kernel) - 1), 2:(ncol(wrpc_kernel)
      - 1)]
  if (all(weibull_kernel[1, ] == 0, weibull_kernel[, 1] == 0,
    weibull_kernel[nrow(weibull_kernel),] == 0, weibull_kernel[,
      ncol(weibull_kernel)] == 0))
    weibull_kernel <- weibull_kernel[2:(nrow(weibull_kernel) - 1),
      2:(ncol(weibull_kernel) - 1)]
  # Multiply the two kernels together and re-normalize
  if (ignore_cauchy)
    wrpc_kernel <- 1
  if (ignore_weibull)
    weibull_kernel <- 1
  redist_kernel <- weibull_kernel*wrpc_kernel
  redist_kernel <- redist_kernel/sum(redist_kernel)
  return(redist_kernel)
}




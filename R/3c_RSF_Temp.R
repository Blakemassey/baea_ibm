########   Program R code and description. #####################################
## This R software code allows for the estimation of used habitat via
## a fixed-width buffer around each telemetry point, the estimation of
## available habitat via a Pareto-weighted kernel around each telemetry
## point. These data are then used in a conditional mixed-effects 
## logistic regression to model resource selection. 
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(VGAM))
source('C:/Work/R/Functions/home.R')

## Define the data projection -------------------------------------------------- 
dataproj <- "+proj=utm +zone=19 +datum=NAD83"

## Import point locations and filter to movements only -------------------------
source('C:/Work/R/Functions/baea.R')
baea <- ImportBAEA(existing=NULL, import=TRUE, tz="Etc/GMT+5")
source('C:/Work/R/Functions/gps.R')
df <- FilterLocations(df=baea, id="id", individual="",  start="2013-11-01", 
  end="2013-12-24") 
table (df$id)
#df <- ExtractMovements(df, min_step_length = 50, max_step_time = 20) 
table (df$id)
#df <- df[1:500,]
ExportKMLTelemetryBAEA(df)

## Read in raster landcover ----------------------------------------------------
habitat_full <- raster("C:/ArcGIS/Data/Landcover/Landcover/lc30m.tif")

## Create spatialpoints object -------------------------------------------------
df_xy <- df[,c("long_utm", "lat_utm")]
df_xy <- SpatialPoints(df_xy, crs(habitat_full))

## Crop habitat object to area around SpatialPoints ----------------------------
source('C:/Work/R/Functions/gis.R')
df_xy_buffer <- CreateExtentBuffer(df_xy, 3000)
habitat <- crop(habitat_full, df_xy_buffer)
#(uniques <- unique(habitat))
freq(habitat)
#plot(habitat)
habitat <- reclassify(habitat, c(20, 32, 20, 80, 83, 80))
(uniques <- unique(habitat))
#freq(habitat)
#plot(habitat)

## Extract used habitat from a 30m buffer around each point --------------------
usedhabitat <- extract(habitat, df_xy, buffer=30)

## Add Columns for used, available, and difference habitats --------------------
used_start <- ncol(df)
df[, c(paste0('used', (uniques)))] <- NA
avail_start <- ncol(df)
df[, c(paste0('avail', (uniques)))] <- NA
diff_start <- ncol(df)
df[, c(paste0('diff', (uniques)))] <- NA

## Add proportion used habitat for each point ----------------------------------
for (i in 1:length(usedhabitat)){
  for (k in uniques){
    col <- paste0("used",k) 
    df[i, col] <- length(which(usedhabitat[[i]]==k))/length(usedhabitat[[i]])
  }
}

## Create a RasterLayer of 0s and 1s for each unique habitat type -------------- 
plot(habitat)
habitatlist <- list()
for (i in uniques) {
  habitat_i <- habitat == i
  habitatlist <- c(habitatlist, habitat_i = habitat_i)
} 
names(habitatlist) <- paste0("habitat", uniques)

## Calcualte shape and scale parameters for the Pareto kernel ------------------
source('C:/Work/R/Functions/pars.R')
df2 <- df[1:800,]
PlotHistAndPareto(df2, var="step_length", by = NULL, pars = NULL,             
  fit_pareto = TRUE)
pareto_pars <- FitParetoParsToData(df2, var="step_length")
pareto_location <- as.numeric(pareto_pars["location"]) 
pareto_scale <- as.numeric(pareto_pars["scale"]) 
pareto_shape <- as.numeric(pareto_pars["shape"]) 

# Create Pareto kernel probability layer ---------------------------------------
(max_r = VGAM::qgpd(0.95, location=pareto_location, scale=pareto_scale, 
  shape=pareto_shape)) # check to be sure this is reasonable
max_r <- 300
cellsize <- res(habitat)[1]
pareto_kern <- CreateParetoKernel(scale=pareto_scale, shape=pareto_shape, 
  max_r=max_r, cellsize=cellsize)
r <- (cellsize*((nrow(pareto_kern)-1)/2))+(cellsize/2)
pareto_raster <- raster(pareto_kern, xmn=-r, xmx=r, ymn=-r, ymx=r)
xmin <- xmin(habitat)
ymin <- ymin(habitat)

## Calculate Pareto-weighted habitat availability for each point ---------------
source('C:/Work/R/Functions/home.R')
for (j in 1:nrow(df)){
  for (k in 1:length(habitatlist)){
    hab <- habitatlist[[k]] # note that this is a raster with 0s and 1s
    x <- CenterXYInCell(df$long_utm[j], df$lat_utm[j], xmin, ymin, cellsize)[1]
    y <- CenterXYInCell(df$long_utm[j], df$lat_utm[j], xmin, ymin, cellsize)[2]    
    pareto_raster_shift <- shift(pareto_raster, x=x, y=y)
    hab_crop <- crop(hab, pareto_raster_shift)
    df[j, (avail_start+k)] <- cellStats(overlay(hab_crop, pareto_raster_shift, 
      fun=function(x,y) {x*y}), "sum")
  }
}

## Difference used and available -----------------------------------------------
for (i in 1:length(uniques)) {
  diff_col <-  diff_start+i
  used_col <-  used_start+i
  avail_col <-  avail_start+i
  df[, diff_col] <- df[, used_col] - df[, avail_col]
}

## Remove factor levels in df$id------------------------------------------------
df$id <- as.character(df$id) 

# Simple Logisitic Regressions -------------------------------------------------
# GLM used to get starting values for mixed-effects model
df$status <- 1
mod11 <- glm(status ~ -1+diff11, data=df, family="binomial")
mod20 <- glm(status ~ -1+diff20, data=df, family="binomial")
mod41 <- glm(status ~ -1+diff41, data=df, family="binomial")
mod42 <- glm(status ~ -1+diff42, data=df, family="binomial")
mod43 <- glm(status ~ -1+diff43, data=df, family="binomial")
mod52 <- glm(status ~ -1+diff52, data=df, family="binomial")
mod71 <- glm(status ~ -1+diff71, data=df, family="binomial")
mod80 <- glm(status ~ -1+diff80, data=df, family="binomial")
mod90 <- glm(status ~ -1+diff90, data=df, family="binomial")
mod95 <- glm(status ~ -1+diff95, data=df, family="binomial")

## Mixed-effects Logisitic Regressions -----------------------------------------
f <- "binomial"
mod11 <- lmer(status ~ -1+diff11+(-1+diff11|id), data=df, family="binomial", start=mod11$coef)
mod20 <- lmer(status ~ -1+diff20+(-1+diff20|id), data=df, family="binomial", start=mod20$coef)
mod41 <- lmer(status ~ -1+diff41+(-1+diff41|id), data=df, family="binomial", start=mod41$coef)
mod42 <- lmer(status ~ -1+diff42+(-1+diff42|id), data=df, family="binomial", start=mod42$coef)
mod43 <- lmer(status ~ -1+diff43+(-1+diff43|id), data=df, family="binomial", start=mod43$coef)
mod52 <- lmer(status ~ -1+diff52+(-1+diff52|id), data=df, family="binomial", start=mod52$coef)
mod71 <- lmer(status ~ -1+diff71+(-1+diff71|id), data=df, family="binomial", start=mod71$coef)
mod80 <- lmer(status ~ -1+diff80+(-1+diff80|id), data=df, family="binomial", start=mod80$coef)
mod90 <- lmer(status ~ -1+diff90+(-1+diff90|id), data=df, family="binomial", start=mod90$coef)
mod95 <- lmer(status ~ -1+diff95+(-1+diff95|id), data=df, family="binomial", start=mod95$coef)
# ERROR "Error in mer_finalize(ans) : Downdated X'X is not positive definite, 1"

## CoxME

# Multiple Mixed-Effects Logisitic Regression ---------------------------------- 
# One habitat type was left out
model_full <- lmer(status ~ -1+diff11+diff20+diff41+diff42+diff43+diff52+diff71+
  diff80+diff95+(-1+diff11+diff20+diff41+diff42+diff43+diff52+diff71+diff80+
  diff95|id), data=df, family="binomial")
# ERROR "Error in mer_finalize(ans) : Downdated X'X is not positive definite, 1"

# Multiple Logisitic Regression ------------------------------------------------
model_full <- glm(status ~ -1+diff11+diff20+diff41+diff42+diff43+diff52+diff71+
  diff80+diff95+(-1+diff11+diff20+diff41+diff42+diff43+diff52+diff71+diff80+
  diff95), data=df, family="binomial")
# Working, but not sure this is producing what I want.
model_full

## Import "BAEA_nest.csv" ------------------------------------------------------
baea_nests <- read.csv2(file="C:/Work/R/Data/BAEA/BAEA_nests.csv", header=TRUE, 
  sep=",", stringsAsFactors = FALSE)
crooked <- subset(baea_nests, nest_site=="Crooked", 
  sel=c("nest_lat_utm", "nest_long_utm")) 
crooked$long_utm <- crooked$nest_long_utm; crooked$nest_long_utm <- NULL
crooked$lat_utm <- crooked$nest_lat_utm; crooked$nest_lat_utm <- NULL
crooked <- SpatialPoints(crooked, crs(habitat_full))

## Calculate 'used', 'available', and 'difference' for each habitat point ------
crooked_buffer <- CreateExtentBuffer(crooked, 6000)
habitat_layer <- crop(habitat_full, crooked_buffer)
habitat_points <- rasterToPoints(habitat_layer, fun=NULL, spatial=FALSE)
habitat_points_sp <- rasterToPoints(habitat_layer, fun=NULL, spatial=TRUE)
habitat_points <- as.data.frame(habitat_points)

## Extract used habitat from a 30m buffer around each point --------------------
usedhabitat <- extract(habitat, habitat_points_sp, buffer=30)

## Add Columns for used, available, and difference habitats --------------------
used_start <- ncol(habitat_points)
habitat_points[, c(paste0('used', (uniques)))] <- NA
avail_start <- ncol(habitat_points)
habitat_points[, c(paste0('avail', (uniques)))] <- NA
diff_start <- ncol(habitat_points)
habitat_points[, c(paste0('diff', (uniques)))] <- NA

## Add proportion used habitat for each point ----------------------------------
for (i in 1:length(usedhabitat)){
  for (k in uniques){
    col <- paste0("used",k) 
    habitat_points[i, col] <- length(which(usedhabitat[[i]]==k)) /
      length(usedhabitat[[i]])
  }
}

## Add proportion available habitat for each point -----------------------------
source('C:/Work/R/Functions/home.R')
for (j in 1:nrow(habitat_points)){
  for (k in 1:length(habitatlist)){
    hab <- habitatlist[[k]] # note that this is a raster with 0s and 1s
    x <- CenterXYInCell(habitat_points$x[j], habitat_points$y[j], xmin, ymin, 
      cellsize)[1]
    y <- CenterXYInCell(habitat_points$x[j], habitat_points$y[j], xmin, ymin, 
      cellsize)[2]    
    pareto_raster_shift <- shift(pareto_raster, x=x, y=y)
    hab_crop <- crop(hab, pareto_raster_shift)
    habitat_points[j, (avail_start+k)] <- cellStats(overlay(hab_crop, 
      pareto_raster_shift, fun=function(x,y) {x*y}), "sum")
  }
}

## Difference used and available -----------------------------------------------
for (i in 1:length(uniques)) {
  diff_col <-  diff_start+i
  used_col <-  used_start+i
  avail_col <-  avail_start+i
  habitat_points[, diff_col] <- habitat_points[, used_col] - 
    habitat_points[, avail_col]
}

# Predict probability of use for a surface -------------------------------------
habitat_points_sp$pred <- predict(model_full, habitat_points, type="response")
pred_raster <- rasterize(habitat_points_sp, habitat_layer, field="pred")
Plot3DRaster(pred_raster)
SavePlot("pred_raster.jpeg")
source('C:/Work/R/Functions/gis.R')
pred_range <- cellStats(pred_raster, "range")
ExportKMLRaster(pred_raster, outfile= "Habitat_RSF.kmz", 
  color_pal = gg.col(100), color_levels = 100, color_range = pred_range) 
ExportKMLRasterOverlay(pred_raster, color_pal = gg.col(100), 
  outfile = "Habitat_RSF_Overlay.kml") 
writeRaster(pred_raster, filename="pred_raster.tif", format="GTiff", 
  overwrite=TRUE)

# Load libraries, scripts, and input parameters
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(optimx))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(smoothie))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tictoc))
suppressPackageStartupMessages(library(velox))
library(baear)
library(gisr)
library(ibmr)
options(stringsAsFactors=FALSE)
theme_update(plot.title = element_text(hjust = 0.5))
xy_theme <- theme(panel.background = element_rect(fill = "grey90",
  colour = "black", size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
    colour = "white"),
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
    colour = "white"))

wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Landscape file locations
base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
lc_file <- "C:/ArcGIS/Data/Landcover/Landcover/lc_30mc.tif"
developed_file <- "C:/ArcGIS/Data/R_Input/BAEA/developed_30mc.tif"
forest_file <- "C:/ArcGIS/Data/R_Input/BAEA/forest_30mc.tif"
open_water_file <- "C:/ArcGIS/Data/R_Input/BAEA/open_water_30mc.tif"
pasture_file <- "C:/ArcGIS/Data/R_Input/BAEA/pasture_30mc.tif"
shrub_herb_file <- "C:/ArcGIS/Data/R_Input/BAEA/shrub_herb_30mc.tif"
wetland_file <- "C:/ArcGIS/Data/R_Input/BAEA/wetland_30mc.tif"

# BAEA and Movement Parameters
baea_steps_file = "Data/BAEA/baea_steps.rds"
move_pars_file = "Results/Analysis/Tables/move_pars.RDS"

############################  Create STEP TYPE #################################

base <- raster(base_file)
baea_steps <- readRDS(file = baea_steps_file) %>%
  mutate(x = long_utm, y = lat_utm) %>%
  CenterXYWithBase(., base) %>%
  mutate(long_utm = x, lat_utm = y)
move_pars <- readRDS(file = move_pars_file)

# Step type
unique(baea_steps$behavior_behavior)
step_type <- "Flight -> Flight"

# Subset baea_steps to step category
baea_steps_type <- baea_steps %>%
  filter(behavior_behavior == step_type) %>%
  tibble::rowid_to_column(., "step_id")

# Now have kernel and step_data
baea_steps_type_sp <- SpatialPointsDataFrame(baea_steps_type[,
  c("long_utm_end","lat_utm_end")], baea_steps_type,
  proj4string = CRS("+proj=utm +zone=19 ellps=WGS84"), bbox = NULL)

# Create Movement Kernel
move_pars_i <- readRDS(file = move_pars_file) %>%
  filter(behavior_behavior == step_type)
ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise", "Flight"),
  FALSE, TRUE)
kernel_i <- CreateMoveKernelWeibullVonMises(
  max_r = NULL,
  cellsize = 30,
  mu1 = move_pars_i$mvm_mu1[1],
  mu2 = move_pars_i$mvm_mu2[1],
  kappa1 = move_pars_i$mvm_kappa1[1],
  kappa2 = move_pars_i$mvm_kappa2[1],
  mix = move_pars_i$mvm_prop[1],
  shape = move_pars_i$weibull_shape[1],
  scale = move_pars_i$weibull_scale[1],
  ignore_von_mises = ignore_von_mises)
sum(kernel_i)

r <- (30*((nrow(kernel_i)-1)/2))+(30/2)
kernel_raster <- raster::raster(kernel_i, xmn=-r, xmx=r, ymn=-r, ymx=r)
prob_raster <- kernel_raster/raster::cellStats(kernel_raster, stat = "sum")
prob_raster[prob_raster <= .000001] <- 0
rm(kernel_i, kernel_raster, move_pars_i, ignore_von_mises, r)
rm(move_pars_file)

# SUBSET DATA - FOR TESTING!!!!
#baea_steps_type_sp <- baea_steps_type_sp[which(baea_steps_type_sp$id == "Sandy"), ]
plot(baea_steps_type_sp, axes = TRUE)

##############################  IMPORT RASTERS #################################

# Import rasters
lc_30mc <- raster("C:/ArcGIS/Data/Landcover/Landcover/lc_30mc.tif")
developed_30mc <- raster("C:/ArcGIS/Data/R_Input/BAEA/developed_30mc.tif")
forest_30mc <- raster("C:/ArcGIS/Data/R_Input/BAEA/forest_30mc.tif")
open_water_30mc <- raster("C:/ArcGIS/Data/R_Input/BAEA/open_water_30mc.tif")
pasture_30mc <- raster("C:/ArcGIS/Data/R_Input/BAEA/pasture_30mc.tif")
shrub_herb_30mc <- raster("C:/ArcGIS/Data/R_Input/BAEA/shrub_herb_30mc.tif")
wetland_30mc <- raster("C:/ArcGIS/Data/R_Input/BAEA/wetland_30mc.tif")
plot(lc_30mc)


## Make VeloxRaster
lc_vx <- velox(lc_30mc)
developed_vx <- velox(developed_30mc)
forest_vx <- velox(forest_30mc)
open_water_vx <- velox(open_water_30mc)
pasture_vx <- velox(pasture_30mc)
shrub_herb_vx <- velox(shrub_herb_30mc)
wetland_vx <- velox(wetland_30mc)

## Crop Extent
steps_extent <- extend(extent(alignExtent(baea_steps_type_sp, base,
  snap='out')), 10000)
lc_vx_crop <- lc_vx$crop(steps_extent)
developed <- developed_vx$crop(steps_extent)
forest <- forest_vx$crop(steps_extent)
open_water <- open_water_vx$crop(steps_extent)
pasture <- pasture_vx$crop(steps_extent)
shrub_herb <- shrub_herb_vx$crop(steps_extent)
wetland <- wetland_vx$crop(steps_extent)

# To visualize extent
lc_crop <- crop(lc_30mc, steps_extent, snap="in")
plot(lc_crop, axes = TRUE)

################### CALCULATE KERNEL-WEIGHTED VALUES ###########################

# Here is an example of looping through many different bandwidths,
# extracting the values at each point, and storing those values in a
# data frame. Note that the smoothed surfaces are NOT stored here.

cell_radius <- 15
bandwidths <- c(seq(0,750, by=15), seq(825, 3000, by=75)) # radius (meters)
lc_types <- c("developed", "forest", "open_water", "pasture", "shrub_herb",
  "wetland")

# Subset for testing
bandwidths <- c(seq(15,165, by=15)) # Subset for testing
lc_types <- c("developed", "forest", "open_water") # Subset for testing

####### CALCULATE KERNEL-WEIGHTED COVARIATE VALUES FOR AVAILABLE ###############

baea_steps_type_df <- baea_steps_type_sp@data %>% as.data.frame(.)

covariate_cols <- paste0(rep(lc_types, each=length(bandwidths)), rep(bandwidths,
  times=length(lc_types)))

avail_df <- cbind(baea_steps_type_sp@data,
  setNames(data.frame(matrix(ncol = length(covariate_cols),
    nrow = nrow(baea_steps_type_sp), NA)), covariate_cols))

prob_rasters_avail <- list(NA)
tic()
for (i in 1:nrow(avail_df)){
  print(paste("Working on:", i, "of", nrow(avail_df)))
  exp_angle <- avail_df[i,] %>% pull(exp_angle)
  prob_raster_rotated <- RotateRaster(prob_raster, Rad2Deg(exp_angle))
  prob_raster_rotated_resampled <- resample(prob_raster_rotated, prob_raster,
    method="bilinear")
  prob_raster_rotated_resampled[is.na(prob_raster_rotated_resampled)] <- 0
  # rowSums(!is.na(prob_raster_rotated_resampled))
  # rowSums(is.na(prob_raster_rotated_resampled))
  # sum(rowSums(is.na(prob_raster_rotated_resampled)))
  # sum(rowSums(is.na(prob_raster)))
  # plot(prob_raster)
  #  plot(prob_raster_rotated_resampled)
  #  prob_raster_rotated_resampled_cropped <- crop(prob_raster_rotated_resampled,
  #    extent(-165, 165, -165, 165))
  #  plot(prob_raster_rotated_resampled_cropped)
  prob_rasters_avail[i] <- prob_raster_rotated_resampled
}
toc()

for (i in seq(lc_types)){
  lc_type_i <- lc_types[i]
  for(j in 1:length(bandwidths)){
    bw_meters <- bandwidths[j]
    bw_pixel <- bandwidths[j]/cell_radius # smoothie needs units in pixels
    print(noquote(paste0("Calculating: '", lc_type_i, "' at bandwidth ", bw_meters)))
    covar_matrix <- as.matrix(eval(parse(text = lc_type_i)))
    covar_matrix_smooth <- kernel2dsmooth(covar_matrix, kernel.type="gauss",
      nx=nrow(lc_crop), ny=ncol(lc_crop), sigma = bw_pixel)
    covar_raster_smooth <- raster(covar_matrix_smooth, template=lc_crop)
    #plot(covar_raster_smooth)
    for (k in 1:nrow(avail_df)){
      avail_xy_k <- c(avail_df[i,] %>% pull(long_utm),
        avail_df[i,] %>% pull(lat_utm))
      prob_raster_avail_k <- prob_rasters_avail[[k]]
      prob_raster_avail_trim_k<- trim(prob_raster_avail_k, padding=0, values=0) # Could be done above??
      prob_raster_avail_shift_k <- shift(prob_raster_avail_trim_k, x = avail_xy_k[1],
        y = avail_xy_k[2])
      # plot(prob_raster_avail_k)
      # plot(prob_raster_avail_shift_k)
      # plot(covar_raster_smooth)
      covar_raster_smooth_crop <- crop(covar_raster_smooth, prob_raster_avail_shift_k)
      #covar_raster_smooth_crop
      #prob_raster_avail_shift_k
      covar_kernel_weighted_product <- covar_raster_smooth_crop *
        prob_raster_avail_shift_k
      #plot(covar_raster_smooth_crop)
      #plot(prob_raster_avail_shift_k)
      #plot(covar_kernel_weighted_product)
      covar_kernel_weighted_product_sum <- cellStats(covar_kernel_weighted_product,
        stat='sum', na.rm=TRUE)
      data_col <- which(colnames(avail_df) == paste0(lc_type_i, bw_meters))
      avail_df[k, data_col] <- covar_kernel_weighted_product_sum
    }
  }
}

########## CALCULATE KERNEL-WEIGHTED COVARIATE VALUES FOR USED #################

new_lc_cols <- paste0(rep(lc_types, each=length(bandwidths)), rep(bandwidths,
  times=length(lc_types)))
new_df <- as.data.frame(matrix(ncol=length(new_lc_cols),
  nrow=nrow(baea_steps_type_sp),NA))

colnames(new_df) <- new_lc_cols

used_data <- cbind(baea_steps_type_sp@data, new_df)
head(used_data)

system.time(
  for (i in seq(lc_types)){
    lc_type_i <- lc_types[i]
    for(j in 1:length(bandwidths)){
      bw <- bandwidths[j]/cell_radius # gotta be in units of pixels I think
      zmat <- as.matrix(eval(parse(text = lc_type_i)))
      f <- kernel2dsmooth(zmat, kernel.type="gauss", nx=nrow(lc_crop),
        ny=ncol(lc_crop), sigma = bw)
      lc_smooth <- lc_crop
      values(lc_smooth) <- f
      print(noquote(paste0("Working on: '", lc_type_i, "' at bandwidth ",
        (bw*15))))
      data_col <- which(colnames(used_data) == paste(lc_type_i, (bw*15), sep=""))
      used_data[, data_col] <- extract(lc_smooth, baea_steps_type_sp)
    }
  }
)
# Took 11407 seconds = 3hr 10min

save(ua_data, file=file.path(getwd(), "ua_data_lc_data.RData"))





ua_data$open_water0 <- extract(open_water, ua_data_sp)
ua_data$developed0 <- extract(developed, ua_data_sp)
ua_data$forest0 <- extract(forest, ua_data_sp)
ua_data$shrub_herb0 <- extract(shrub_herb, ua_data_sp)
ua_data$pasture0 <- extract(pasture, ua_data_sp)
ua_data$wetland0 <- extract(wetland, ua_data_sp)





################## CONVERT POINT DATA AND CROP LANDSCAPE RASTER ################




for (i in 1:nrow(baea_steps_type)){
  baea_steps_type_i <- baea_steps_type[i, ] %>% dplyr::select(step_id,
    long_utm, lat_utm, exp_angle)
  print(baea_steps_type_i)
}

# Plot Movement Kernel
plot(prob_raster)
plot(RotateRaster(prob_raster, 45))
rad2deg

library(circular)
circular::

## Make VeloxRaster with two bands
set.seed(0)
mat1 <- matrix(rnorm(100), 10, 10)
mat2 <- matrix(rnorm(100), 10, 10)
vx <- velox(list(mat1, mat2), extent=c(0,1,0,1), res=c(0.1,0.1),
crs="+proj=longlat +datum=WGS84 +no_defs")
## Make SpatialPoints
library(sp)
library(rgeos)
coord <- cbind(runif(10), runif(10))
spoint <- SpatialPoints(coords=coord)
## Extract
vx$extract_points(sp=spoint)










###################### CONVERT LANDSCAPE RASTER ################################

# Convert all non-zero values to 1's

developed <- subs(lc_crop, data.frame(id=c(21:24), v=rep(1, 4))) ## Developed
developed[is.na(developed)] <- 0

forest <- subs(lc_crop, data.frame(id=c(41:43), v=rep(1, 3))) ## Forest
forest[is.na(forest)] <- 0

open_water <- subs(lc_crop, data.frame(id=11, v=1)) ## Open Water
open_water[is.na(open_water)] <- 0

pasture <- subs(lc_crop, data.frame(id=c(81, 82), v=rep(1, 2))) ## Pasture/Crop
pasture[is.na(pasture)] <- 0

shrub_herb <- subs(lc_crop, data.frame(id=c(52, 71), v=rep(1, 2))) ## Shrub/Herb
shrub_herb[is.na(shrub_herb)] <- 0

wetland <- subs(lc_crop, data.frame(id=c(90, 95), v=rep(1, 2))) ## Wetland
wetland[is.na(wetland)] <- 0


###################### EXTRACT LC DATA ################################

cc_df <- cbind(cc_df, raster::extract(raster_stack, cc_df[, c("long_utm_end",
  "lat_utm_end")]))

cc_df$lc_30mc <- factor(cc_df$lc_30mc)

# need to make lc classes into individual layers !!!

mod1 <- survival::clogit(case ~ forest + hydro_dist_30mc + strata(step_id),
  data = cc_df)

## ssud ssf
p1_ud1 <- exp(coef(mod1)[1] * raster_stack[[1]] + coef(mod1)[2] * raster_stack[[2]])
p1_ud1 <- p1_ud1 / sum(p1_ud1[])

p1_ud2 <- exp(coef(mod1)[1] * raster_stack[[1]])

plot(p1_ud1)











tic()
for (i in 1:nrow(cc_df)){
  long_utm <- cc_df[i, "long_utm"]
  lat_utm <- cc_df[i, "lat_utm"]
  exp_angle <- cc_df[i, "exp_angle"]
  destination_cell <- suppressWarnings(sampling::strata(data=
    data.frame(cell=1:raster::ncell(prob_raster)), stratanames=NULL, size=1,
    method="systematic", pik=prob_raster@data@values))
  xy <- raster::xyFromCell(prob_raster, destination_cell[1,1])
  xy_rot <- as.data.frame(Rotation(xy, exp_angle))
  colnames(xy_rot) <- c("x", "y")
  xy_rot_centered <- CenterXYWithBase(xy_rot, base)
  cc_df[i, "dx"] <- xy[1]
  cc_df[i, "dy"] <- xy[2]
  cc_df[i, "dx_rot"] <- xy_rot[1]
  cc_df[i, "dy_rot"] <- xy_rot[2]
  cc_df[i, "long_utm_end"] <- long_utm + xy_rot_centered$x[1]
  cc_df[i, "lat_utm_end"] <- lat_utm + xy_rot_centered$y[1]
}
toc()




# Create "Control" destination cells based on Movement Kernel probability
n_cc <- 10 # number of FALSE cases per TRUE case

cc_df <- data.frame(id = rep(baea_steps_type$id, each = n_cc),
  datetime = rep(baea_steps_type$datetime, each = n_cc),
  step_id = rep(baea_steps_type$step_id, each = n_cc),
  long_utm = rep(baea_steps_type$long_utm, each = n_cc),
  lat_utm = rep(baea_steps_type$lat_utm, each = n_cc),
  exp_angle = rep(baea_steps_type$exp_angle, each = n_cc),
  dx = NA, dy = NA, long_utm_rot = NA, lat_utm_rot = NA,
    case=FALSE)

tic()
for (i in 1:nrow(cc_df)){
  long_utm <- cc_df[i, "long_utm"]
  lat_utm <- cc_df[i, "lat_utm"]
  exp_angle <- cc_df[i, "exp_angle"]
  destination_cell <- suppressWarnings(sampling::strata(data=
    data.frame(cell=1:raster::ncell(prob_raster)), stratanames=NULL, size=1,
    method="systematic", pik=prob_raster@data@values))
  xy <- raster::xyFromCell(prob_raster, destination_cell[1,1])
  xy_rot <- as.data.frame(Rotation(xy, exp_angle))
  colnames(xy_rot) <- c("x", "y")
  xy_rot_centered <- CenterXYWithBase(xy_rot, base)
  cc_df[i, "dx"] <- xy[1]
  cc_df[i, "dy"] <- xy[2]
  cc_df[i, "dx_rot"] <- xy_rot[1]
  cc_df[i, "dy_rot"] <- xy_rot[2]
  cc_df[i, "long_utm_end"] <- long_utm + xy_rot_centered$x[1]
  cc_df[i, "lat_utm_end"] <- lat_utm + xy_rot_centered$y[1]
}
toc()

# Plot destination_xy
xy_raster <- rasterize(as.matrix(cc_df[, c("dx", "dy")]), prob_raster,
  fun='count', background=0)
plot(xy_raster)

# Join step data and case control data
baea_steps_type_cc <- rbind.fill(baea_steps_type, cc_df) %>% arrange(step_id)

# Plot destination_xy with 2D density
options(stringsAsFactors = TRUE)
origin <- data.frame(x = 0, y = 0)
ggplot(baea_steps_type_cc %>% filter(case == FALSE), aes(x=dx, y=dy)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", contour=TRUE) +
  scale_fill_distiller(type = "seq", palette = "YlOrBr", direction = 1,
    guide = FALSE) +
  geom_point(size = .75, color = "blue2") +
  geom_point(data = origin, aes(x, y), stroke = 1.5, size = 4, colour="black",
    shape = 1) +
  geom_point(data = origin, aes(x, y), stroke = 1.5, colour="black", shape = 4)+
  ggtitle(paste(step_type, "Control Points")) +  xlab("Long") + ylab("Lat") +
  coord_fixed(ratio = 1) + xlim(-8000, 8000) + ylim(-8000, 8000) + xy_theme
SaveGGPlot(paste(str_replace_all(step_type, " ->", ""),
  "Control Points All.png"), "Results/Analysis/Plots/Step_Selection")
options(stringsAsFactors = FALSE)

origin <- data.frame(x = 0, y = 0)

ConvertUnitRadianToStandard <- function(x){
  x <- x
  if (x >=0 && x <= .5*pi) {
    x <- (.5*pi)-x
  } else if (x > .5*pi && x <= pi) {
    x <- (2*pi)-(x-(.5*pi))
  } else if (x > pi && x <= 1.5*pi) {
    x <- (1.5*pi)-pi-x
  } else if (x > 1.5*pi && x <= 2*pi) {
    x <- pi-(x-1.5*pi)
  }
  return(x)
}

baea_steps_type_cc_ex <- baea_steps_type_cc %>% filter(step_id == 2)

baea_steps_type_cc_ex_f <- baea_steps_type_cc_ex %>%
  filter(case == FALSE) %>%
  filter(step_id == 2) %>%
  mutate(
    rad_start1 = atan2(dy, dx),
    r = sqrt((dx^2) + (dy^2)),
    rad_end1 = (atan2(dy_rot, dx_rot))) %>%
  mutate(rad_start2 = if_else(rad_start1 <= 0, (2*pi) + rad_start1, rad_start1),
    rad_end2 = if_else(rad_end1 <= 0,(2*pi) + rad_end1, rad_end1)) %>%
  mutate(rad_start3 = ConvertUnitRadianToStandard(rad_start2),
    rad_end3 = ConvertUnitRadianToStandard(rad_end2)) %>%
  mutate(rad_start = if_else(rad_start3 > 2*pi, rad_start3 - (2*pi) , rad_start3),
    rad_end = if_else(rad_end3 > 2*pi,rad_end3 - (2*pi), rad_end3))


baea_steps_type_cc_ex_f <- baea_steps_type_cc_ex %>%
  filter(case == FALSE) %>%
  filter(step_id == 2) %>%
  mutate(
    rad_start1 = atan2(dy, dx),
    r = sqrt((dx^2) + (dy^2)),
    rad_end1 = rad_start1 + exp_angle) %>%
  mutate(rad_start2 = if_else(rad_start1 <= 0, (2*pi) + rad_start1, rad_start1),
    rad_end2 = if_else(rad_end1 <= 0,(2*pi) + rad_end1, rad_end1)) %>%
  mutate(rad_start3 = ConvertUnitRadianToStandard(rad_start2),
    rad_end3 = ConvertUnitRadianToStandard(rad_end2)) %>%
  mutate(rad_start = if_else(rad_start3 > 2*pi, rad_start3 - (2*pi) , rad_start3),
    rad_end = if_else(rad_end3 > 2*pi,rad_end3 - (2*pi), rad_end3))


# Plot Control Points Example
ggplot(baea_steps_type_cc_ex, aes(dx,dy)) +
  geom_point(size = 2, color = "blue2") +
  geom_point(data = origin, aes(x, y), stroke = 1.5, size = 4, colour="black",
    shape = 1) +
  geom_point(data = origin, aes(x, y), stroke = 1.5, colour="black", shape = 4)+
  geom_point(data = baea_steps_type_cc_ex %>% filter(case == TRUE),
    aes(dx, dy), stroke = 1.5, colour="orange", shape = 16) +
  ggtitle(paste(step_type, "Control Points")) +
  coord_fixed(ratio = 1) + xlim(-8000, 8000) + ylim(-8000, 8000) + xy_theme
SaveGGPlot(paste(str_replace_all(step_type, " ->", ""),
  "Control Points Example.png"), "Results/Analysis/Plots/Step_Selection")


# Plot Control Points Example Rotated
ggplot(baea_steps_type_cc_ex_f) +
  geom_point(aes(dx, dy), size = 2, color = "blue2") +
  geom_point(aes(dx_rot, dy_rot), size = 2, color = "darkgreen") +
  geom_arc(aes(x0 = 0, y0 = 0, r = r, start = rad_start, end = rad_end)) +
  geom_point(data = origin, aes(x, y), stroke = 1.5, size = 4,
    colour="black", shape = 1) +
  geom_point(data = origin, aes(x, y), stroke = 1.5, colour="black",
    shape = 4) + xlab("dx") + ylab("dy") +
  geom_point(data = baea_steps_type_cc_ex %>% filter(case == TRUE),
    aes(dx, dy), stroke = 1.5, colour="orange", shape = 16) +
  ggtitle(paste(step_type, "Control Points")) +
  coord_fixed(ratio = 1) + xlim(-8000, 8000) + ylim(-8000, 8000) + xy_theme
SaveGGPlot(paste(str_replace_all(step_type, " ->", ""),
  "Control Points Example Rotated.png"),"Results/Analysis/Plots/Step_Selection")


##############################  IMPORT RASTERS #################################

# Import rasters
base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")
lc_30mc <- raster("C:/ArcGIS/Data/Landcover/Landcover/lc_30mc.tif")

################## CONVERT POINT DATA AND CROP LANDSCAPE RASTER ################

ua_data_sp <- SpatialPointsDataFrame(cc_df[,c("long_utm_end","lat_utm_end")], cc_df,
  proj4string = CRS("+proj=utm +zone=19 ellps=WGS84"), bbox = NULL)
lc_crop <- crop(lc_30mc, extend(extent(ua_data_sp), 500))

###################### CONVERT LANDSCAPE RASTER ################################

# Convert all non-zero values to 1's

developed <- subs(lc_crop, data.frame(id=c(21:24), v=rep(1, 4))) ## Developed
developed[is.na(developed)] <- 0

forest <- subs(lc_crop, data.frame(id=c(41:43), v=rep(1, 3))) ## Forest
forest[is.na(forest)] <- 0

open_water <- subs(lc_crop, data.frame(id=11, v=1)) ## Open Water
open_water[is.na(open_water)] <- 0

pasture <- subs(lc_crop, data.frame(id=c(81, 82), v=rep(1, 2))) ## Pasture/Crop
pasture[is.na(pasture)] <- 0

shrub_herb <- subs(lc_crop, data.frame(id=c(52, 71), v=rep(1, 2))) ## Shrub/Herb
shrub_herb[is.na(shrub_herb)] <- 0

wetland <- subs(lc_crop, data.frame(id=c(90, 95), v=rep(1, 2))) ## Wetland
wetland[is.na(wetland)] <- 0


###################### EXTRACT LC DATA ################################

cc_df <- cbind(cc_df, raster::extract(raster_stack, cc_df[, c("long_utm_end",
  "lat_utm_end")]))

cc_df$lc_30mc <- factor(cc_df$lc_30mc)

# need to make lc classes into individual layers !!!

mod1 <- survival::clogit(case ~ forest + hydro_dist_30mc + strata(step_id),
  data = cc_df)

## ssud ssf
p1_ud1 <- exp(coef(mod1)[1] * raster_stack[[1]] + coef(mod1)[2] * raster_stack[[2]])
p1_ud1 <- p1_ud1 / sum(p1_ud1[])

p1_ud2 <- exp(coef(mod1)[1] * raster_stack[[1]])

plot(p1_ud1)





plot(rotate(selected_xy, 135))
set.seed(1)
### Create a set of coordinates

# Parameters for bivariate normal distribution
sample_n <- 20
rho <- 0
mu1 <- 50; s1 <- 20
mu2 <- 0; s2 <- 10
mu <- c(mu1, mu2) # Mean
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),
           2) # Covariance matrix
library(MASS)
coords <- mvrnorm(sample_n, mu = mu, Sigma = sigma) # from MASS package
colnames(bvn1) <- c("x","y")

### Create a series of angles
rad <- seq(0, 2*pi, length = 9)
deg <- rad2deg(rad)

rotation_plots <- vector("list", length(rad))
cols <- iwanthue(nrow(coords) , plot=FALSE)

for(i in 1:length(rad)){
	coords.rot <- Rotation(coords, rad[i])
	coords_df <- data.frame(x = coords.rot[,1], y = coords.rot[,2], lab = as.character(1:10))
	rotation_plot <- ggplot(coords_df, aes(x, y, color=lab)) + geom_point() +
	  scale_color_manual(values=cols, guide=FALSE) +
	  xlim(-100, 100) + ylim(-100, 100) + coord_fixed(ratio = 1) +
	  ggtitle(round(deg[i] , 3)) +
    theme(plot.title = element_text(size = 12, hjust = 0.5))
	rotation_plot
	rotation_plots[[i]] <- rotation_plot
}

plot_grid(rotation_plots[[1]], rotation_plots[[2]], rotation_plots[[3]],
  rotation_plots[[4]], rotation_plots[[5]], rotation_plots[[6]],
  rotation_plots[[7]], rotation_plots[[8]], rotation_plots[[9]])

### Rotate the coordinates by an angle of 90 degrees
coords.90 <- Rotation(coords,90*pi/180)
coords.90

plot(coords,xlim=range(rbind(coords.90,coords)[,1]),ylim=range(rbind(coords.90,coords)[,2]),asp=1)
points(coords.90,pch=19)
# }



degrees <- seq(0,345, by = 15)

x <- sin(degrees*(pi/180))
y <- cos(degrees*(pi/180))

plot(x,y)




sin(50*(pi∕180))*30
cos(50*(pi∕180))*30




RotateRaster <- function(raster,
                         angle=0,
                         resolution=res(raster)) {
  raster_i <- raster
  crs(raster_i) <- "+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=0"
  raster_rotated <- projectRaster(raster_i, res=resolution,
      crs=paste0("+proj=aeqd +ellps=sphere +lat_0=90 +lon_0=", -angle))
  crs(raster_rotated) <- crs(raster)
  return(raster_rotated)
}

x <- raster(matrix(1:(15*25), nrow=15), xmn=-1000, xmx=1000, ymn=-1000, ymx=1000)
plot(x, main="Original")
plot(RotateRaster(x, 30, resolution=10), main=paste("Rotated by 30 degrees"))
plot(RotateRaster(x, 75, resolution=10), main=paste("Rotated by 75 degrees"))
plot(RotateRaster(x, 180, resolution=10), main=paste("Rotated by 180 degrees"))
plot(RotateRaster(x, 300, resolution=10), main=paste("Rotated by 300 degrees"))

# Make a probability Raster??

ggplot(move_dens_i, aes(x = x, y = y)) +
  geom_raster(aes(fill = dens)) +
  labs(x = "X", y = "Y", title = paste(i)) +
  coord_fixed(ratio = 1) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Oranges")) +
  labs(fill = "Probability") +
  scale_x_continuous(expand = c(0.005, 0.005)) +
  scale_y_continuous(expand = c(0.005, 0.005)) +
  theme(title = element_text(size = 16)) +
  theme(axis.text = element_text(colour = "grey20", size = 12)) +
  theme(axis.title.y = element_text(angle=0, vjust=0.5)) +
  theme(panel.grid.major = element_blank())  +
  theme(panel.grid.minor = element_blank())  +
  theme(panel.background = element_rect(fill = NA, color = "black")) +
  theme(plot.margin = margin(15, 15, 15, 15, "pt"))


df <- baea_movements
behavior <- "Flight -> Flight"
n_control <- 10
resources <-

ssf_case_control <- function(df,
                             behavior,
                             n_control = 10,
                             resources) {

  ne <- nrow(df) - 1  # effective number of steps

  ## control points
  case_for_control <- rep(1:ne, each = n_control)
  df_xy <- df[case_for_control, ]

  xy_cc[, 1] <- xy_cc[, 1] + sl * cos(ta)
  xy_cc[, 2] <- xy_cc[, 2] + sl * sin(ta)

  cc_df <- do.call(rbind, list(
    data.frame(
      step_id = 1:ne,
      case = TRUE,
      xstart = x$xy[1:ne, 1],
      ystart = x$xy[1:ne, 2],
      xend = x$xy[2:ns, 1],
      yend = x$xy[2:ns, 2],
      sl = obs_lengths
    ),
    data.frame(
      step_id = rep(1:ne, each = n_control),
      case = FALSE,
      xstart = x$xy[case_for_control, 1],
      ystart = x$xy[case_for_control, 2],
      xend = xy_cc[, 1],
      yend = xy_cc[, 2],
      sl = sl
    )
  ))

  ## Extract covariates
  cc_df$sl[cc_df$sl == 0] <- 1/(2 * pi)
  cc_df$log_sl <- log(cc_df$sl)
  cc_df <- cbind(cc_df, raster::extract(resources, cc_df[, c("xend", "yend")]))
  list(cc = cc_df, gamma_dist = list(shape = shape, scale = scale))
}


#------------------------------------------------------------------------------#
################################## OLD CODE ####################################
#------------------------------------------------------------------------------#


if (testing == TRUE){
  i <- j <- k <- 1
}
for (i in seq(lc_types)){
  lc_type_i <- lc_types[i]
  for(j in 1:length(bandwidths)){
    bw_meters <- bandwidths[j]
    bw_pixel <- bandwidths[j]/cell_radius # smoothie needs units in pixels
    print(noquote(paste0("Calculating: '", lc_type_i, "' at bandwidth ", bw_meters)))
    covar_matrix <- eval(parse(text = lc_type_i))$as.matrix(band=1)
    covar_matrix_smooth <- kernel2dsmooth(covar_matrix, kernel.type="gauss",
      nx=nrow(covar_matrix), ny=ncol(covar_matrix), sigma = bw_pixel)
    covar_raster_smooth <- raster(covar_matrix_smooth, template=lc_crop)
    covar_raster_smooth <- velox(covar_matrix_smooth, extent=)
    #plot(covar_raster_smooth)
    for (k in 1:nrow(avail_steps_i)){
      xy <- c(avail_steps_i[i,] %>% pull(long_utm),
        avail_steps_i[i,] %>% pull(lat_utm))
      prob_raster_avail_k <- prob_rasters_avail[[k]]
      prob_raster_avail_trim_k<- trim(prob_raster_avail_k, padding=0, values=0) # Could be done above??
      prob_raster_avail_shift_k <- shift(prob_raster_avail_trim_k, x = avail_xy_k[1],
        y = avail_xy_k[2])
      # plot(prob_raster_avail_k)
      # plot(prob_raster_avail_shift_k)
      # plot(covar_raster_smooth)
      covar_raster_smooth_crop <- crop(covar_raster_smooth, prob_raster_avail_shift_k)
      #covar_raster_smooth_crop
      #prob_raster_avail_shift_k
      covar_kernel_weighted_product <- covar_raster_smooth_crop *
        prob_raster_avail_shift_k
      #plot(covar_raster_smooth_crop)
      #plot(prob_raster_avail_shift_k)
      #plot(covar_kernel_weighted_product)
      covar_kernel_weighted_product_sum <- cellStats(covar_kernel_weighted_product,
        stat='sum', na.rm=TRUE)
      data_col <- which(colnames(avail_df) == paste0(lc_type_i, bw_meters))
      avail_df[k, data_col] <- covar_kernel_weighted_product_sum
    }
  }
}





ggplot(baea_steps_type_cc_ex) +
  geom_point(aes(dx, dy), size = 2, color = "green2") +
  geom_point(aes(dx_rot, dy_rot), size = 2, color = "blue2") +
  geom_arc(aes(x0 = 0, y0 = 0, r = r, start = rad_start, end = rad_end)) +
  geom_point(data = origin, aes(x, y), stroke = 1.5, size = 4,
    colour="black", shape = 1) +
  geom_point(data = origin, aes(x, y), stroke = 1.5, colour="black",
    shape = 4) +
#  geom_point(data = baea_steps_type_cc_ex %>% filter(case == TRUE),
#    aes(dx, dy), stroke = 1.5, colour="orange", shape = 16) +
  ggtitle(paste(step_type, "Control Points")) +
  coord_fixed(ratio = 1) + xlim(-8000, 8000) + ylim(-8000, 8000) + xy_theme

SaveGGPlot(paste(str_replace_all(step_type, " ->", ""),
  "Control Points Example Rotated.png"),"Results/Analysis/Plots/Step_Selection")

ConvertRadTo2PiScale <- function(x){
  if(x <= 0){
    x <- (2*pi) + x
  }
  return(x)
}

ConvertRadTo2PiScale2 <- function(x){
  if(x > 2*pi){
    x <- x - (2*pi)
  }
  return(x)
}

test <- 1:5
ConvertRadTo2PiScale(1)

x <- atan2(0,1)
x
ConvertStandardRadianToUnit(x)
ConvertRadTo2PiScale()

baea_steps_type_cc_ex <- data.frame(dx = c(3000, 5000), dy = c(3000, 5000),
  dx_rot = c(-4242.641, 7071.068), dy_rot = c(0,0)) %>%
  mutate(
    rad_start1 = atan2(dy, dx),
    r = sqrt((dx^2) + (dy^2)),
    rad_end1 = (atan2(dy_rot, dx_rot))) %>%
  mutate(rad_start2 = if_else(rad_start1 <= 0, (2*pi) + rad_start1, rad_start1),
    rad_end2 = if_else(rad_end1 <= 0,(2*pi) + rad_end1, rad_end1)) %>%
  mutate(rad_start3 = ConvertUnitRadianToStandard(rad_start2),
    rad_end3 = ConvertUnitRadianToStandard(rad_end2)) %>%
  mutate(rad_start = if_else(rad_start3 > 2*pi, rad_start3 - (2*pi) , rad_start3),
    rad_end = if_else(rad_end3 > 2*pi,rad_end3 - (2*pi), rad_end3))

baea_steps_type_cc_ex$rad_start <- c(0, 0)
baea_steps_type_cc_ex$rad_end <- c(pi, pi*.5)


move_dens <- data.frame(grp=character(), x=numeric(), y=numeric(),
  dens=numeric())
for (i in 1:nrow(move_pars)){
  move_pars_i <- move_pars[i, ]
  ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise",
    "Flight"), FALSE, TRUE)
  kernel_i <- CreateMoveKernelWeibullVonMises(
      max_r = NULL,
      cellsize = 30,
      mu1 = move_pars_i$mvm_mu1[1],
      mu2 = move_pars_i$mvm_mu2[1],
      kappa1 = move_pars_i$mvm_kappa1[1],
      kappa2 = move_pars_i$mvm_kappa2[1],
      mix = move_pars_i$mvm_prop[1],
      shape = move_pars_i$weibull_shape[1],
      scale = move_pars_i$weibull_scale[1],
      ignore_von_mises = ignore_von_mises)
  r <- (30*((nrow(kernel_i)-1)/2))+(30/2)
  kernel_raster <- raster::raster(kernel_i, xmn=-r, xmx=r, ymn=-r, ymx=r)
  df <- data.frame(raster::rasterToPoints(kernel_raster))
  names(df)[3] <- "dens"
  df$behavior_behavior <- move_pars_i$behavior_behavior
  move_dens <- rbind(move_dens, df)
}

  move_shift <- raster::shift(move_kernel, x=step_data$x[i],
      y=step_data$y[i])
    move_shift <- raster::crop(move_shift, base, snap="in")
    prob_raster <- move_shift
    prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")

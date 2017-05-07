###################### RESEARCH LAB POWERPOINT - 2016.04.28  ###################

########################### LOAD PACKAGES AND DATA  ############################

suppressPackageStartupMessages(library(AICcmodavg))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(optimx))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(smoothie))
source('C:/Work/R/Functions/all.R')

id_colors <- CreateColorsByAny(by="id", output=TRUE)  
theme_legend <- theme(plot.title=element_text(size=24, face="bold", vjust=1.5))+
  theme(axis.title=element_text(size=20, face="bold")) + 
  theme(axis.text=element_text(colour="black")) +
  theme(axis.text.x=element_text(size=16, angle=50, vjust=0.5)) +
  theme(axis.text.y=element_text(size=16, vjust=0.5)) 
theme_no_legend <- theme_legend + theme(legend.position="none")

########################  MANAGE POINT DATA ####################################

keep <- c("Sheepscot", "Sandy", "Branch", "Phillips")
#remove <- c("Cherryfield", "Madagascal", "Webb", "Upper")

all_nests <- read.csv(file=file.path("C:/Work/R/Data/BAEA/Nests",
  "Nests_Study_Intact_Last.csv"), header=TRUE, stringsAsFactors=FALSE)
all_nests <- ConvertNestIdToNum(all_nests)

study_nests <- read.csv(file=file.path("C:/Work/R/Data/BAEA/Nests/",
    "Nests_Study.csv"), header=TRUE, stringsAsFactors=FALSE) %>% 
  dplyr::filter(name %in% keep)   
#  dplyr::filter(!name %in% remove) 

study_nests <- ConvertNestIdToNum(study_nests)
study_nests[, "nest_id"] <- study_nests[, "nest_site"]

intact_last <- read.csv("C:/Work/R/Data/BAEA/Nests/Nests_Study_Intact_Last.csv")
study_nests <- read.csv(file="C:/Work/R/Data/BAEA/Nests/Nests_Study.csv")

#intact_last_spdf <- SpatialPointsDataFrame(intact_last[c("long", "lat")], 
#  bbox=NULL, data=intact_last, proj4string = CRS("+proj=longlat +datum=WGS84"))
#intact_last_voronoi <- ConvertToVoronoi(intact_last_spdf)

all_nests_spdf <- SpatialPointsDataFrame(all_nests[c("long_utm", "lat_utm")], 
  bbox=NULL, data=all_nests, proj4string = crs(base))
all_nests_voronoi <- ConvertToVoronoi(all_nests_spdf)


baea_all <- read.csv("C:/Work/R/Data/BAEA/BAEA.csv", header=TRUE)
baea_sub <-  baea_all %>%
  dplyr::filter(agl <= 67) %>%
  dplyr::filter(speed <= 2) %>%  
  dplyr::filter(year == 2015) %>%
  dplyr::filter(id %in% keep)  
#  dplyr::filter(!id %in% remove)

used <- baea_sub %>%  
  mutate(x = long_utm) %>%
  mutate(y = lat_utm) %>%
  mutate(point = 1) %>%
  dplyr::select(x,y,point)

base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")
ua_data <- SampleRandomPointsInHomerange(df_all=all_nests, df_home=study_nests,
  used_pts=baea_sub, base, max_r = 30000, id = "nest_site", name = "nest_id",
  output_dir = getwd(), write_homerange = TRUE)
ua_data$id <- as.character(ua_data$id)
ua_data %>% count(id, point)
nrow(ua_data)/2
save(ua_data, file = file.path(getwd(), "ua_data.RData"))

####################  IMPORT POINT DATA AND RASTERS ############################

# Load point data
load(file=file.path(getwd(), "ua_data.RData"))

# Import rasters
base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")
lc_30mc <- raster("C:/ArcGIS/Data/Landcover/Landcover/lc_30mc.tif")

################## CONVERT POINT DATA AND CROP LANDSCAPE RASTER ################

ua_data_sp <- SpatialPointsDataFrame(ua_data[,c("x","y")], ua_data, 
  proj4string = CRS("+proj=utm +zone=19 ellps=WGS84"), bbox = NULL)
lc_crop <- crop(lc_30mc, extend(extent(ua_data_sp), 500))

####################### CONVERT UA_DATA TO SPDF ################################

ua_data_sp <- SpatialPointsDataFrame(ua_data[,c("x","y")], ua_data, 
  proj4string = CRS("+proj=utm +zone=19 ellps=WGS84"), bbox = NULL)
ua_data_sp <- spTransform(ua_data_sp, CRS("+proj=longlat +datum=WGS84"))

############################## MAP UA_DATA #####################################

ggplot(data = ua_data %>% filter(point == 1)) + ggtitle("Used Points") +
  geom_point(aes(x = x, y = y), 
    alpha = 1, size = 1, na.rm = FALSE, color = "red") + theme_legend +
    theme(axis.text.x=element_text(size=16, angle=0, vjust=0.5)) +
  geom_point(data = intact_last,aes(x = long_utm, y = lat_utm), shape = 17,
      alpha = 1, color = "blue", size = 2, stroke = 1.5, 
      na.rm = TRUE) +
  geom_polygon(data=all_nests_voronoi,aes(x=long, y=lat, map_id=id), 
    color="mediumvioletred", fill="#FFFFFF00", size=.75)  +
  coord_cartesian(xlim = c(460000, 540000), ylim = c(4905000, 4960000)) +
  ggtitle("Used Points") 
SaveGGPlot("Used Points.jpeg")


ggplot(data = ua_data %>% filter(point == 1)) +
  geom_point(aes(x = x, y = y), data = ua_data %>% filter(point == 0),
    alpha = 1, size = 1, na.rm = FALSE, color = "yellow")  +
  geom_point(aes(x = x, y = y), 
    alpha = 1, size = 1, na.rm = FALSE, color = "red") + theme_legend +
    theme(axis.text.x=element_text(size=16, angle=0, vjust=0.5)) +
  geom_point(data = intact_last,aes(x = long_utm, y = lat_utm), shape = 17,
      alpha = 1, color = "blue", size = 2, stroke = 1.5, 
      na.rm = TRUE) +
  geom_polygon(data=all_nests_voronoi,aes(x=long, y=lat, map_id=id), 
    color="mediumvioletred", fill="#FFFFFF00", size=.75)  +
  coord_cartesian(xlim = c(460000, 540000), ylim = c(4905000, 4960000)) +
  ggtitle("Used and Available Points") +
SaveGGPlot("Used and Available Points.jpeg")


ua_data$long <- coordinates(ua_data_sp)[,1]
ua_data$lat <- coordinates(ua_data_sp)[,2]

area_map <- get_map(location = ext,
  color = "color", source = "google", maptype = "hybrid", zoom = 9)
area_ggmap <- ggmap(area_map, extent = "device", ylab="Latitude", 
  xlab="Longitude", legend = "right") +
  geom_point(aes(x = long, y = lat), data = ua_data %>% filter(point == 0),
    alpha = 1, size = 1, na.rm = FALSE, color = "yellow")  +
  geom_point(aes(x = long, y = lat), data = ua_data %>% filter(point == 1),
    alpha = 1, size = 1, na.rm = FALSE, color = "red")  
print(area_ggmap)
SaveGGPlot("Used and Available Points - Map.jpeg", bg = "black")

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

ua_data$open_water0 <- extract(open_water, ua_data_sp)
ua_data$developed0 <- extract(developed, ua_data_sp)
ua_data$forest0 <- extract(forest, ua_data_sp)
ua_data$shrub_herb0 <- extract(shrub_herb, ua_data_sp)
ua_data$pasture0 <- extract(pasture, ua_data_sp)
ua_data$wetland0 <- extract(wetland, ua_data_sp)

######################### PLOT LANDSCAPE RASTER ################################

plot(lc_crop, axes=TRUE, main="Landcover")
SavePlot(file="Map_All_Color.jpg")

par(mfrow = c(2, 3))
plot(developed, main="Developed",
  col=colorRampPalette(c("white","#ED0000"))(100))
plot(forest, main="Forest", 
  col=colorRampPalette(c("white","#B5C98E"))(100))
plot(open_water, main="Open Water", 
  col=colorRampPalette(c("white","#476BA0"))(100))
plot(pasture, main = "Pasture", 
    col=colorRampPalette(c("white","#DBD83C"))(100))
plot(shrub_herb, main = "Shrub and Herbaceous", 
  col=colorRampPalette(c("white","#CCBA7C"))(100))
plot(wetland, main="Wetland", 
    col=colorRampPalette(c("white","#BAD8EA"))(100))
SavePlot(file="Map_All_Color.jpg")
par(mfrow = c(1,1))

# Convert the raster to points for plotting
lc_crop_pts <- rasterToPoints(lc_crop)
df <- data.frame(lc_crop_pts)
lc_colors_df <- read.csv(file="C:/Work/R/Data/Mapping/lc_50mc.csv")
lc_colors <- CreateColorsByMetadata("C:/Work/R/Data/Mapping/lc_50mc.csv")
col_range <- as.numeric(names(lc_colors))
df$cuts <- cut(df$lc_30mc, breaks = col_range, right = TRUE)
break_names <- names(table(df$cuts))
ggplot(aes(x = x, y = y), data = df) +
  geom_tile(aes(x, y, fill=cuts)) +
  scale_fill_manual(breaks=break_names,
                    values = c(paste0(lc_colors[-1], "FF")),
                    labels = lc_colors_df$Land_Cover[-1]) +
  theme(panel.background = element_blank()) +
  guides(fill = guide_legend(title = "Classifications")) +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
#  theme_legend +
  theme(axis.text=element_text(size=10)) + 
  xlab("Longitude") + ylab("Latitude") + ggtitle("Landcover")
SaveGGPlot(file="Map_lc_crop.jpeg")

ext <- as.vector(extent(projectExtent(wetland, 
  CRS("+proj=longlat +datum=WGS84")))[c(1,3,2,4)])
ext <- ext

library(colorspace)
library(rasterVis)
lc_stack <- stack(developed, forest, open_water, pasture, shrub_herb, wetland)
names(lc_stack) <- c("developed", "forest", "open_water", "pasture",
  "shrub_herb", "wetland")
myTheme <- rasterTheme(region=rev(colorRampPalette(c("darkgreen", "white"))(2)))
levelplot(lc_stack, par.settings = myTheme, colorkey=FALSE)
SavePlot(file="Map_All.jpg")
str(ua_data)

################### CALCULATE KERNEL-WEIGHTED VALUES ###########################

# Here is an example of looping through many different bandwidths,
# extracting the values at each point, and storing said values in a 
# data frame. Note that the smoothed surfaces are NOT stored here.
#h <- c(1:50, seq(55, 200, by=5))
bandwidths <- c(1:50, seq(55, 200, by=5))*15
bandwidths <- c(seq(15,750, by=15), seq(825, 3000, by=75))
cell_radius <- 15
lc_types <- c("developed", "forest", "open_water", "pasture", "shrub_herb",
  "wetland") 

new_lc_cols <- paste0(rep(lc_types, each=length(bandwidths)), rep(bandwidths, 
  times=length(lc_types)))
new_df <- as.data.frame(matrix(ncol=length(new_lc_cols), nrow=nrow(ua_data),NA))

colnames(new_df) <- new_lc_cols

ua_data <- cbind(ua_data, new_df)
head(ua_data)

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
      data_col <- which(colnames(ua_data) == paste(lc_type_i, (bw*15), sep=""))
      ua_data[, data_col] <- extract(lc_smooth, ua_data_sp)
    }
  }
)
# Took 11407 seconds = 3hr 10min

save(ua_data, file=file.path(getwd(), "ua_data_lc_data.RData"))

#################### FIT KERNEL-WEIGHTED VALUES ################################

load(file=file.path(getwd(), "ua_data_lc_data.RData"))
bandwidths <- c(0:50, seq(55, 200, by=5))*15
lc_types <- c("developed", "forest", "open_water", "pasture", "shrub_herb",
  "wetland") 

# Fit univariate models to find the "approximate" AIC-best bandwidth
# (i.e., stage-one of a two-stage analysis)

opt_mods <- cbind.data.frame(lc_type=lc_types)
all_mods <- data.frame()

for (i in 1:length(lc_types)){
  lc_mods <- list()
  lc_type_i <- lc_types[i]
  for (j in 1:length(bandwidths)){
    x_name <- paste(lc_type_i, bandwidths[j], sep="")
    fmla <- as.formula(paste("point ~ ", x_name))
    lc_mod = glm(fmla, data=ua_data, family="binomial")
    lc_mods[[length(lc_mods)+1]] <- lc_mod
    names(lc_mods)[[length(lc_mods)]] <-  as.character(bandwidths[j])
    mod <- data.frame(lc_type = lc_type_i, bw = bandwidths[j], aic = lc_mod$aic, 
      intercept = as.numeric(coef(lc_mod)[1]), 
      slope = as.numeric(coef(lc_mod)[2]), opt_bw = NA)
    all_mods <- rbind(all_mods, mod)
  }
  aic_table <- aictab(lc_mods, second.ord = FALSE)
  aic_table$lc_type <- lc_type_i
  aic_table$opt_bw <- as.numeric(as.character(aic_table[1,1]))
  opt_bw <- as.numeric(as.character(aic_table[1,1]))
  all_mods[all_mods$lc_type == lc_type_i, "opt_bw"] <- opt_bw
  opt_mods[opt_mods$lc_type == lc_type_i, "bw"] <- opt_bw 
  g <- ggplot(aic_table, aes(x=as.numeric(as.character(Modnames)), y = AIC)) + 
    geom_line(color="red") + geom_point() + xlab("Bandwidth") + theme_no_legend+
    ggtitle(lc_type_i) + geom_vline(xintercept = opt_bw, color="blue") +
    annotate("text", x = opt_bw+100, xmin = opt_bw, y = min(aic_table$AIC), 
    label = as.character(opt_bw), color = "blue")
  print(g)
  SaveGGPlot(file = paste0("AIC_BW_", lc_type_i, ".jpg"))
}

save(all_mods, file=file.path(getwd(), "all_mods.RData"))
save(opt_mods, file=file.path(getwd(), "opt_mods.RData"))

all_mods_sort <- all_mods %>% arrange(lc_type, aic)
all_mods_top5 <- all_mods_sort %>% group_by(lc_type) %>% slice(1:5)
write.csv(all_mods_top5, file = "all_mods_top5.csv")

# Plot all AIC by bandwidths in facet_wrap
g <- ggplot(all_mods, aes(x=bw, y = aic)) + 
  geom_line(color="red") + geom_point() + xlab("Bandwidth") + theme_no_legend +
  geom_vline(data = ddply(all_mods, "lc_type", summarize, 
    opt_bw_i = min(opt_bw)), aes(xintercept=opt_bw_i), color= "blue") +
  geom_text(data = ddply(all_mods, "lc_type", summarize, 
    opt_bw_i = min(opt_bw), min_aic = min(aic)), 
    mapping=aes(x = opt_bw_i + 300, y = min_aic, label = opt_bw_i), 
    size = 4, color = "blue") +
  facet_wrap(~lc_type, scales = "free_y") +
  theme(axis.title=element_text(size=12, face="bold")) + 
  theme(axis.text.x=element_text(size=8, angle=50, vjust=0.5)) +
  theme(axis.text.y=element_text(size=8, vjust=0.5)) 
print(g)  
SaveGGPlot(file = paste0("AIC_BW_all.jpg"))



######### PREDICT LOGISTIC FUNCTION AT OPTIMAL KERNEL-WEIGHTED VALUES ##########

predict_range <- with(ua_data, data.frame(developed150 = seq(from = 0, to = 1, 
  length.out = 100))) 
predicted <- cbind(predict_range, predict(all_lc_mods[["developed150"]], 
  newdata=predict_range, type="link", se=TRUE))
predicted <- within(predicted, {
  Predicted_Prob <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(predicted)

ggplot(predicted, aes(x = developed150, y = Predicted_Prob)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .2) +
  geom_line(size=1) + ylim(c(0,1))+ theme_no_legend

################ OPTIMIZATION PROCEDURE OF KERNEL BANDWIDTH ####################

load(file=file.path(getwd(), "all_mods.RData"))
load(file=file.path(getwd(), "opt_mods.RData"))

# Likelihood function
# z = a vector of zeros and ones corresponding to available and used points
# cell = a vector of the cell numbers of each point
# surface = a data frame with x/y coordinates and values of the raster surface

nll_kern_bw <- function(parms=parms, z=z, cell=cell, surface=surface){
  # Parameters
  b0 <- parms[1]         # intercept
  b1 <- parms[2]         # slope of the relationship
  b2 <- exp(parms[3])+1  # bw/spatial scale parameter
  # Create matrix
  r <- rasterFromXYZ(surface) # surface is a df with X|Y|VALUES
  zmat <- as.matrix(r)
  # Gaussian-weighted smooth of matrix
  f <- kernel2dsmooth(zmat,kernel.type="gauss", nx=nrow(r), ny=ncol(r),sigma=b2)
  values(r) <- f
  # Compute binomial success probabilities
  probs <- plogis(b0 + b1*(r[cell]))
  # Evaluate log of binomial pmf
  tmp <- dbinom(z, 1, probs, log=TRUE)
  ll <- -1*sum(tmp)
  return(ll)
}

opt_mods_coef <- dplyr::left_join(opt_mods, all_mods, by = c("lc_type", "bw"))

# testing potential for loop code
#  for (i in 1:length(lc_types)){
#   test <- eval(parse(text = lc_types[i]))
#   print(test)
#  }
  
# Parameters
developed_table = opt_mods_coef %>% filter(lc_type == "developed")
parms_developed <- c(developed_table$intercept, developed_table$slope, log(developed_table$opt_bw/15))

forest_table = opt_mods_coef %>% filter(lc_type == "forest")
parms_forest <- c(forest_table$intercept, forest_table$slope, log(forest_table$opt_bw/15))

open_water_table = opt_mods_coef %>% filter(lc_type == "open_water")
parms_open_water <- c(open_water_table$intercept, open_water_table$slope, log(open_water_table$opt_bw/15))

pasture_table = opt_mods_coef %>% filter(lc_type == "pasture")
parms_pasture <- c(pasture_table$intercept, pasture_table$slope, log(pasture_table$opt_bw/15))

shrub_herb_table = opt_mods_coef %>% filter(lc_type == "shrub_herb")
parms_shrub_herb <- c(shrub_herb_table$intercept, shrub_herb_table$slope, log(shrub_herb_table$opt_bw/15))

wetland_table = opt_mods_coef %>% filter(lc_type == "wetland")
parms_wetland <- c(wetland_table$intercept, wetland_table$slope, log(wetland_table$opt_bw/15))

# Extract cell numbers
cell_developed <- extract(developed, ua_data[,c("x","y")], cellnumbers=TRUE)[,1]
cell_forest <- extract(forest, ua_data[,c("x","y")], cellnumbers=TRUE)[,1]
cell_open_water <- extract(open_water, ua_data[,c("x","y")], cellnumbers=TRUE)[,1]
cell_pasture <- extract(pasture, ua_data[,c("x","y")], cellnumbers=TRUE)[,1]
cell_shrub_herb <- extract(shrub_herb, ua_data[,c("x","y")], cellnumbers=TRUE)[,1]
cell_wetland <- extract(wetland, ua_data[,c("x","y")], cellnumbers=TRUE)[,1]

# Convert raster to data frame
surface_developed <- data.frame(coordinates(developed), values(developed))
surface_forest <- data.frame(coordinates(forest), values(forest))
surface_open_water <- data.frame(coordinates(open_water), values(open_water))
surface_pasture <- data.frame(coordinates(pasture), values(pasture))
surface_shrub_herb <- data.frame(coordinates(shrub_herb), values(shrub_herb))
surface_wetland <- data.frame(coordinates(wetland), values(wetland))
# testing potential for loop code 
# surface <- data.frame(coordinates(eval(parse(text = lc_types[i]))), values(eval(parse(text = lc_types[i]))))

# OPTIMIZE!!!!!!!!!!!!

system.time(out_developed <- optimx(par=parms_developed, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_developed, surface=surface_developed, 
  control=list(trace=3)))
save(out_developed, file = "out_developed.RData")

system.time(out_forest <- optimx(par=parms_forest, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_forest, surface=surface_forest, 
  control=list(trace=3)))
save(out_forest, file = "out_forest.RData")

system.time(out_open_water <- optimx(par=parms_open_water, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_open_water, surface=surface_open_water, 
  control=list(trace=3)))
save(out_open_water, file = "out_open_water.RData")

system.time(out_pasture <- optimx(par=parms_pasture, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_pasture, surface=surface_pasture, 
  control=list(trace=3)))
save(out_pasture, file = "out_pasture.RData")

system.time(out_shrub_herb <- optimx(par=parms_shrub_herb, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_shrub_herb, surface=surface_shrub_herb, 
  control=list(trace=3)))
save(out_shrub_herb, file = "out_shrub_herb.RData")

system.time(out_wetland <- optimx(par=parms_wetland, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_wetland, surface=surface_wetland, 
  control=list(trace=3)))
save(out_wetland, file = "out_wetland.RData")



# FAILED
system.time(out <- nlm(nll_kern_bw,parms,print.level=2,hessian=TRUE))

# FAILED 
system.time(out <- optim(par=parms, fn=nll_kern_bw, method = "L-BFGS-B",
      lower = c(-100, -1000, log(0)), upper = rep(100, 1000, log(500)), 
      z=ua_data$point, cell=cell1, surface=surface, control = list(trace=3, 
        REPORT=1)))











for (i in 1:length(lc_types)){
  lc_type_i <- lc_types[i]
  ua_data_sub <- ua_data %>% dplyr::select(point, contains(lc_type_i))
  lc_mods <- list(
    "0"   = glm(ua_data_sub[,1]~ua_data_sub[,2], ua_data_sub, family="binomial"),
    "15"  = glm(ua_data_sub[,1]~ua_data_sub[,3], ua_data_sub, family="binomial"),
    "30"  = glm(ua_data_sub[,1]~ua_data_sub[,4], ua_data_sub, family="binomial"),
    "45"  = glm(ua_data_sub[,1]~ua_data_sub[,5], ua_data_sub, family="binomial"),
    "60"  = glm(ua_data_sub[,1]~ua_data_sub[,6], ua_data_sub, family="binomial"),
    "75"  = glm(ua_data_sub[,1]~ua_data_sub[,7], ua_data_sub, family="binomial"),
    "90"  = glm(ua_data_sub[,1]~ua_data_sub[,8], ua_data_sub, family="binomial"),
    "105"  = glm(ua_data_sub[,1]~ua_data_sub[,9], ua_data_sub, family="binomial"),
    "120"  = glm(ua_data_sub[,1]~ua_data_sub[,10], ua_data_sub, family="binomial"),
    "135"  = glm(ua_data_sub[,1]~ua_data_sub[,11], ua_data_sub, family="binomial"),
    "150"  = glm(ua_data_sub[,1]~ua_data_sub[,12], ua_data_sub, family="binomial"),
    "165"  = glm(ua_data_sub[,1]~ua_data_sub[,13], ua_data_sub, family="binomial"),
    "180"  = glm(ua_data_sub[,1]~ua_data_sub[,14], ua_data_sub, family="binomial"),
    "195"  = glm(ua_data_sub[,1]~ua_data_sub[,15], ua_data_sub, family="binomial"),
    "210"  = glm(ua_data_sub[,1]~ua_data_sub[,16], ua_data_sub, family="binomial"),
    "225"  = glm(ua_data_sub[,1]~ua_data_sub[,17], ua_data_sub, family="binomial"),
    "240"  = glm(ua_data_sub[,1]~ua_data_sub[,18], ua_data_sub, family="binomial"),
    "255"  = glm(ua_data_sub[,1]~ua_data_sub[,19], ua_data_sub, family="binomial"),
    "270"  = glm(ua_data_sub[,1]~ua_data_sub[,20], ua_data_sub, family="binomial"),
    "285"  = glm(ua_data_sub[,1]~ua_data_sub[,21], ua_data_sub, family="binomial"),
    "300"  = glm(ua_data_sub[,1]~ua_data_sub[,22], ua_data_sub, family="binomial"),  
    "315"  = glm(ua_data_sub[,1]~ua_data_sub[,23], ua_data_sub, family="binomial"),   
    "330"  = glm(ua_data_sub[,1]~ua_data_sub[,24], ua_data_sub, family="binomial"), 
    "345"  = glm(ua_data_sub[,1]~ua_data_sub[,25], ua_data_sub, family="binomial"),   
    "360"  = glm(ua_data_sub[,1]~ua_data_sub[,26], ua_data_sub, family="binomial"),   
    "375"  = glm(ua_data_sub[,1]~ua_data_sub[,27], ua_data_sub, family="binomial"),   
    "390"  = glm(ua_data_sub[,1]~ua_data_sub[,28], ua_data_sub, family="binomial"),   
    "405"  = glm(ua_data_sub[,1]~ua_data_sub[,29], ua_data_sub, family="binomial"),   
    "420"  = glm(ua_data_sub[,1]~ua_data_sub[,30], ua_data_sub, family="binomial"),   
    "435"  = glm(ua_data_sub[,1]~ua_data_sub[,31], ua_data_sub, family="binomial"),  
    "450"  = glm(ua_data_sub[,1]~ua_data_sub[,32], ua_data_sub, family="binomial"),   
    "465"  = glm(ua_data_sub[,1]~ua_data_sub[,33], ua_data_sub, family="binomial"),     
    "480"  = glm(ua_data_sub[,1]~ua_data_sub[,34], ua_data_sub, family="binomial"),     
    "495"  = glm(ua_data_sub[,1]~ua_data_sub[,35], ua_data_sub, family="binomial"),     
    "510"  = glm(ua_data_sub[,1]~ua_data_sub[,36], ua_data_sub, family="binomial"),     
    "525"  = glm(ua_data_sub[,1]~ua_data_sub[,37], ua_data_sub, family="binomial"),     
    "540"  = glm(ua_data_sub[,1]~ua_data_sub[,38], ua_data_sub, family="binomial"),     
    "555"  = glm(ua_data_sub[,1]~ua_data_sub[,39], ua_data_sub, family="binomial"),     
    "570"  = glm(ua_data_sub[,1]~ua_data_sub[,40], ua_data_sub, family="binomial"),     
    "585"  = glm(ua_data_sub[,1]~ua_data_sub[,41], ua_data_sub, family="binomial"),
    "600"  = glm(ua_data_sub[,1]~ua_data_sub[,42], ua_data_sub, family="binomial"),   
    "615"  = glm(ua_data_sub[,1]~ua_data_sub[,43], ua_data_sub, family="binomial"),     
    "630"  = glm(ua_data_sub[,1]~ua_data_sub[,44], ua_data_sub, family="binomial"),     
    "645"  = glm(ua_data_sub[,1]~ua_data_sub[,45], ua_data_sub, family="binomial"),     
    "660"  = glm(ua_data_sub[,1]~ua_data_sub[,46], ua_data_sub, family="binomial"),     
    "675"  = glm(ua_data_sub[,1]~ua_data_sub[,47], ua_data_sub, family="binomial"),     
    "690"  = glm(ua_data_sub[,1]~ua_data_sub[,48], ua_data_sub, family="binomial"),     
    "705"  = glm(ua_data_sub[,1]~ua_data_sub[,49], ua_data_sub, family="binomial"),     
    "720"  = glm(ua_data_sub[,1]~ua_data_sub[,50], ua_data_sub, family="binomial"),     
    "735"  = glm(ua_data_sub[,1]~ua_data_sub[,51], ua_data_sub, family="binomial"),
    "750"  = glm(ua_data_sub[,1]~ua_data_sub[,52], ua_data_sub, family="binomial"),
    "825"  = glm(ua_data_sub[,1]~ua_data_sub[,53], ua_data_sub, family="binomial"),
    "900"  = glm(ua_data_sub[,1]~ua_data_sub[,54], ua_data_sub, family="binomial"),
    "975"  = glm(ua_data_sub[,1]~ua_data_sub[,55], ua_data_sub, family="binomial"),
    "1050"  = glm(ua_data_sub[,1]~ua_data_sub[,56], ua_data_sub, family="binomial"),
    "1125"  = glm(ua_data_sub[,1]~ua_data_sub[,57], ua_data_sub, family="binomial"),
    "1200"  = glm(ua_data_sub[,1]~ua_data_sub[,58], ua_data_sub, family="binomial"),
    "1275"  = glm(ua_data_sub[,1]~ua_data_sub[,59], ua_data_sub, family="binomial"),  
    "1350"  = glm(ua_data_sub[,1]~ua_data_sub[,60], ua_data_sub, family="binomial"),
    "1425"  = glm(ua_data_sub[,1]~ua_data_sub[,61], ua_data_sub, family="binomial"),
    "1500"  = glm(ua_data_sub[,1]~ua_data_sub[,62], ua_data_sub, family="binomial")     
  )
  mod_table <- aictab(lc_mods, second.ord = FALSE)
  View(mod_table)
  optimum_bw <- as.numeric(as.character(mod_table[1,1]))
  g <- ggplot(mod_table, aes(x=as.numeric(as.character(Modnames)), y = AIC)) + 
    geom_line(color="red") + geom_point() + xlab("Bandwidth") + theme_no_legend +
    ggtitle(lc_type_i) + geom_vline(xintercept = optimum_bw, color="blue")
  print(g)
  SaveGGPlot(file = paste0("AIC_BW_", lc_type_i, ".jpg"))
}

lc_mods <- list(
  "0"   = glm(point~open_water0, ua_data, family="binomial"),
  "15"  = glm(point~open_water15, data=ua_data, family="binomial"),
  "30"  = glm(point~open_water30, data=ua_data, family="binomial"),
  "45"  = glm(point~open_water45, data=ua_data, family="binomial")
)

names(lc_mods)[[1]] <- as.character(100)

lc_mods <- list(
  "0"   = glm(point~LC,    ua_data, family="binomial"),
  "15"  = glm(point~LC15,  ua_data, family="binomial"),
  "30"  = glm(point~LC30,  ua_data, family="binomial"),
  "45"  = glm(point~LC45,  ua_data, family="binomial"),
  "60"  = glm(point~LC60,  ua_data, family="binomial"),
  "75"  = glm(point~LC75,  ua_data, family="binomial"),
  "90"  = glm(point~LC90,  ua_data, family="binomial"),
  "105" = glm(point~LC105, ua_data, family="binomial"),
  "120" = glm(point~LC120, ua_data, family="binomial"),
  "135" = glm(point~LC135, ua_data, family="binomial"),
  "150" = glm(point~LC150, ua_data, family="binomial"),
  "165" = glm(point~LC165, ua_data, family="binomial"),
  "180" = glm(point~LC180, ua_data, family="binomial"),
  "195" = glm(point~LC195, ua_data, family="binomial"),
  "210" = glm(point~LC210, ua_data, family="binomial"),
  "225" = glm(point~LC225, ua_data, family="binomial"),
  "240" = glm(point~LC240, ua_data, family="binomial"),
  "255" = glm(point~LC255, ua_data, family="binomial"),
  "270" = glm(point~LC270, ua_data, family="binomial"),
  "285" = glm(point~LC285, ua_data, family="binomial"),
  "300" = glm(point~LC300, ua_data, family="binomial")
)



predict_range <- with(ua_data, data.frame(open_water15 = seq(from = 0, to = 1, 
  length.out = 100)))



lc_mods <- list(
  "15"   = glm(point~open_water15,    ua_data, family="binomial")
  )

predicted <- cbind(predict_range, predict(lc_mods[["15"]], 
  newdata=predict_range, type="link", se=TRUE))
predicted <- within(predicted, {
  Predicted_Prob <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(predicted)

ggplot(predicted, aes(x = open_water15, y = Predicted_Prob)) +
  geom_ribbon(aes(ymin = LL, ymax = UL), alpha = .2) +
  geom_line(size=1) + ylim(c(0,1))+ theme_no_legend


# TESTING THE LIKELIHOOD FUNCTION ON TERRESTRIAL LAND COVER

#likelihood function
#z = a vector of zeros and ones corresponding to available and used points
#cell = a vector of the cell numbers of each point
#surface = a data frame with the x/y coordinates and values of the raster surface

parms <- c(coef(lc_mods[["15"]])[1], coef(lc_mods[["15"]])[2], log(1))

NllBandwidth <- function(parms=parms, z=z, cell=cell, surface){
    #Set some things up:
    npx <- nrow(surface)
    b0 <- parms[1]              #intercept
    b1 <- parms[2]              #slope of the relationship
    b2 <- exp(parms[3])+1              #bw/spatial scale parameter

    r <- rasterFromXYZ(surface) #surface is a df with X|Y|VALUES
    zmat <- as.matrix(r)
    f <- kernel2dsmooth(zmat,kernel.type="gauss", nx=nrow(r), ny=ncol(r), sigma = b2)
    values(r) <- f
    ### Compute binomial success probabilities
    probs <- plogis(b0 + b1*(r[cell]))
    #ll <- rep(0,length(z))

    ### evaluate log of binomial pmf
    tmp <- dbinom(z,1,probs,log=TRUE)
    ll <-  -1*sum(tmp)
    return(ll)
  }


# Extract cell numbers
cell1 <- extract(open_water, ua_data[,c("x","y")], cellnumbers=TRUE)[,1]

# Convert raster to data frame
surface <- data.frame(coordinates(open_water), values(open_water))

# OPTIMIZE!!!!!!!!!!!!
system.time(out <- nlm(nll_kern_bw,parms,print.level=2,
                       z=ua_data$point,cell=cell1,
                       surface=surface,hessian=TRUE))




lik <- function(parms=params, z=z, cell=cell, surface=surface){

    #Set some things up:
    npx <- nrow(surface)
    b0 <- parms[1]              #intercept
    b1 <- parms[2]              #slope of the relationship
    b2 <- exp(parms[3])+1              #bw/spatial scale parameter

    r <- rasterFromXYZ(surface) #surface is a df with X|Y|VALUES
    zmat <- as.matrix(r)
    f <- kernel2dsmooth(zmat,kernel.type="gauss", nx=nrow(r), ny=ncol(r), sigma = b2)
    values(r) <- f
    ### Compute binomial success probabilities
    probs <- plogis(b0 + b1*(r[cell]))
    #ll <- rep(0,length(z))

    ### evaluate log of binomial pmf
    tmp <- dbinom(z,1,probs,log=TRUE)
    ll <-  -1*sum(tmp)
    return(ll)
}

# Extract cell numbers
cell1 <- extract(terrestrial,UAdata[,c("x","y")],cellnumbers=TRUE)[,1]

# Convert raster to data frame
surface <- data.frame(coordinates(terrestrial),values(terrestrial))

# OPTIMIZE!!!!!!!!!!!!
system.time(out <- nlm(lik,c(-1.301,3.620, log(67)),print.level=2,
                       z=UAdata$point,cell=cell1,
                       surface=surface,hessian=TRUE))


system.time(lik(parms,z,surface))


################
# This is code from Chris Sutherland's original script,
# including the simulation example.
################

par(mfrow=c(2,2))

#original layer
plot(terrestrial)


#smoothed layer
bw <- 900 / 15
zmat <- as.matrix(r.terrestrial)
zmat[zmat>0] <- 1
f <- kernel2dsmooth(zmat,kernel.type="gauss", nx=nrow(r.terrestrial), ny=ncol(r.terrestrial), sigma = bw)
r.terrestrial.smooth <- r.terrestrial
values(r.terrestrial.smooth) <- f
dev.new()
plot(r.terrestrial.smooth)



#binary surface
r.lp <- r.terrestrial.smooth
r.z <- r.terrestrial.smooth
lin.pred <- 3 + -30 * values(r.terrestrial.smooth)
values(r.lp) <- plogis(lin.pred)
plot(r.lp)
z <- rbinom(length(values(r.z)), 1, values(r.lp))
values(r.z) <- z
plot(r.z)

# cut out some points and limit the analysis to a subset of locations
surface <- data.frame(coordinates(r.terrestrial),values(r.terrestrial))
pick <- 1:nrow(surface)#sample(1:nrow(surface), 1000, replace=FALSE)
system.time(out <- nlm(lik,c(-0.89747,2.323, bw),z=z[pick],surface=surface[pick,],hessian=TRUE))





## Making used and available points --------------------------------------------

# Read in the used points and an equal number of available points

#load(file=file.path(getwd(), "used.RData"))
#load(file=file.path(getwd(), "avail.RData"))

all_nests <- read.csv(file=file.path("C:/Work/R/Data/BAEA/Nests",
  "Nests_Study_Intact_Last.csv"), header=TRUE, stringsAsFactors=FALSE)
all_nests <- ConvertNestIdToNum(all_nests)

study_nests <- read.csv(file=file.path("C:/Work/R/Data/BAEA/Nests/",
    "Nests_Study.csv"), header=TRUE, stringsAsFactors=FALSE) %>% 
#  dplyr::filter(!name %in% c("Cherryfield", "Madagascal", "Webb", "Upper")) 
  dplyr::filter(name %in% c("Sheepscot", "Sandy"))   
study_nests <- ConvertNestIdToNum(study_nests)
study_nests[, "nest_id"] <- study_nests[, "nest_site"]

base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")

baea_all <- read.csv("C:/Work/R/Data/BAEA/BAEA.csv", header=TRUE)
baea_sub <-  baea_all %>%
  dplyr::filter(agl <= 67) %>%
  dplyr::filter(speed <= 2) %>%  
  dplyr::filter(year == 2015) %>%
#  dplyr::filter(!id %in% c("Cherryfield", "Madagascal", "Webb", "Upper"))
  dplyr::filter(id %in% c("Sheepscot", "Sandy"))  
  
used <- baea_sub %>%  
  mutate(x = long_utm) %>%
  mutate(y = lat_utm) %>%
  mutate(point = 1) %>%
  dplyr::select(x,y,point)
save(used, file = file.path(getwd(), "used.RData"))


source('C:/Work/R/Functions/gis.R')
ua_data <- SampleRandomPointsInHomerange(df_all=all_nests, df_home=study_nests,
  used_pts=baea_sub, base, max_r = 30000, id = "nest_site", name = "nest_id",
  output_dir = getwd(), write_homerange = TRUE)
#avail <-  avail1 %>%
#  mutate(x = long_utm) %>%
#  mutate(y = lat_utm) %>%
#  mutate(point = 0) %>%
#  dplyr::select(x,y,point)
save(ua_data, file = file.path(getwd(), "ua_data.RData"))

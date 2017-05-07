--------------------------------------------------------------------------------
### This script is for importing baea data (.csv) and nest data (.RData) to
### create raster layers of home dist and con dist
--------------------------------------------------------------------------------
source('C:/Work/R/Functions/all.R')
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

############################  IMPORT FILES  ####################################

## Import Nests ----------------------------------------------------------------
load(file="C:/Work/R/Data/R_Input/BAEA/nests_active_spdf.RData")

######################### FILTER NESTS #########################################

## Filter Nests ----------------------------------------------------------------
nests2016 <- nests_active_spdf[which(nests_active_spdf$active_2016 == TRUE),]

plot(nests2016)

maine <- fortify(map_data("state") %>% filter(region == "maine"),
  region = "region")
ggplot(data = maine) +
  geom_map(map = maine, aes(x = long, y = lat, map_id = region,
    group = group), fill = "white", color = "black", size = 0.25) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  geom_point(data=nests_active_spdf@data, aes(x=long, y=lat), shape=19, alpha=.9,
    size=.75, stroke=1, na.rm=TRUE) +
  theme_map() + theme(legend.position = c(1, .1))

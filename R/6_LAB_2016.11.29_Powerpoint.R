###################### RESEARCH LAB POWERPOINT - 2016.03.29  ###################

########################### LOAD PACKAGES AND DATA  ############################

#save(baea, file = file.path(sim_input, "baea.RData"))
#RemoveExcept("baea")
suppressPackageStartupMessages(library(broom))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggmap))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr)) # dplyr must be after plyr
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(rgeos))
suppressPackageStartupMessages(library(texmex))
source('C:/Work/R/Functions/all.R')
source('C:/Work/R/Functions/sim/move.R')
options(stringsAsFactors = FALSE)

############################## ORIGINAL DATA ###################################

sim_input <- "C:/Work/R/Data/Simulation"
image_output <- file.path("C:/Users/blake/Documents/PhD Program",
  "McGarigal Lab Presentations/Lab Presentation - 2016.11.29/Images")
setwd(file.path("C:/Work/R/Workspace"))
load(file.path(sim_input,"baea.RData"))
crs <- "+proj=utm +zone=19 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs_proj <- "+proj=longlat +datum=WGS84"

id_colors <- CreateColorsByAny(by="id", output=TRUE)  
theme_legend <- theme(plot.title=element_text(size=24, face="bold", vjust=1.5))+
  theme(axis.title=element_text(size=20, face="bold")) + 
  theme(axis.text=element_text(colour="black")) +
  theme(axis.text.x=element_text(size=16, angle=50, vjust=0.5)) +
  theme(axis.text.y=element_text(size=16, vjust=0.5)) 
theme_no_legend <- theme_legend + theme(legend.position="none")

##################### HOME_DIST PARETO FITTING DATA ############################
divisor <- 1000
by <- 1

# Only non "cruising" locations from breeding eagles
baea_sub <- baea %>%
  dplyr::filter(agl <= 67) %>%
  dplyr::filter(speed <= 2) %>%  
  dplyr::filter(year == 2015) %>%
  dplyr::filter(date <= "2015-09-01") %>%
  dplyr::filter(!is.na(edge_dist)) %>%
  mutate(con_dist = con_dist/divisor) %>% 
  mutate(home_dist = home_dist/divisor) %>%  
  mutate(edge_dist = edge_dist/divisor) %>%  
  mutate(edge_dist_shift = edge_dist_shift/divisor) %>% 
  mutate(home_dist = ifelse(home_dist==0, 0.001, home_dist)) %>%
  mutate(edge_dist = ifelse(edge_dist==0, 0.001, edge_dist))

null <- evm(home_dist, data=baea_sub, th=0, phi=~1, xi=~1)
summary(null)  # scale parameter is sigma; shape parameter is xi
               # phi = log(sigma)
coef(null)
phi = coef(null)[1]
xi = coef(null)[2]
sigma = exp(phi)
scale = sigma
shape = xi

PlotParetoPDF(location=0, scale=sigma, shape=xi)

########################## PAIR ANALYSIS DATA ##################################
library(texmex)
library(scales)
load('pair_analysis.RData')

mid_lengths <- seq(1000, 5000, by=1000)

pair_analysis <- pair_analysis %>% filter(mid_length %in% mid_lengths) # %>%
#  mutate(mid_length = mid_length/1000)

# Mid-length (wrapped)
g <- ggplot(pair_analysis) +
  stat_bin(aes(home_dist, ..density..), binwidth = .1, center = .05,
    col = "black") +
  ggtitle(paste0("Edge Length (km)")) +
  xlab("Home Distance (km)") + ylab("Density") +
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_y_continuous(breaks = pretty_breaks()) +   theme_legend 
g + facet_wrap( ~ mid_length)
SaveGGPlot(
  paste0("Home Distances by Edge Length (wrapped).jpeg"),
  file.path(image_output, "Home Dist - Histograms")
)
    
# By Mid-length
for (i in seq(mid_lengths)){
  mid_length_i <- mid_lengths[i]
  pair_analysis_i <- pair_analysis %>% filter(mid_length == mid_length_i)
  g <- ggplot(pair_analysis_i) + 
    stat_bin(aes(home_dist, ..density..), binwidth = .1, center=.05, 
      col = "black") +
      ggtitle(paste0("Edge Length: ", mid_length_i/1000, " km")) + 
      xlab("Home Distance (km)") + ylab("Density") + 
      theme_legend + scale_x_continuous(breaks= pretty_breaks()) +
      scale_y_continuous(breaks= pretty_breaks())
  print(g)  
  SaveGGPlot(paste0("Home Distances - ", mid_length_i/1000, " km.jpeg"), 
    file.path(image_output, "Home Dist - Histograms", "Mid-length"))
}

# By Mid-length and Pair Distance (wrapped)
mid_length_list <- seq(1000, 5000, by=1000)
for (i in seq(mid_length_list)){
  mid_length_i <- mid_length_list[i]
  pair_analysis_i <- pair_analysis %>% filter(mid_length == mid_length_i) %>%
    arrange(pair_dist)
  g <- ggplot(pair_analysis_i) + 
    stat_bin(aes(home_dist, ..density..), binwidth = .1, center=.05, 
      col = "black") + 
    geom_vline(aes(xintercept = pair_dist/2), lty=2, col = "grey50") +
    geom_vline(aes(xintercept = pair_dist), lty=2, col = "darkred") +
    ggtitle(paste0("Edge Length: ", mid_length_i/1000, " km"))+ 
    xlab("Home Distance (km)") + ylab("Density") + 
    scale_x_continuous(breaks= pretty_breaks()) +
    scale_y_continuous(breaks= pretty_breaks()) + theme_legend 
  g2 <- g + facet_wrap(~pair_dist)
  print(g2)  
  SaveGGPlot(paste0("Edge Length - ", mid_length_i/1000, " km"), 
    file.path(image_output, "Home Dist - Histograms"))
}


# By Mid-length and Pair Distance
mid_length_list <- seq(1000, 5000, by=1000)
for (i in seq(mid_length_list)){
  mid_length_i <- mid_length_list[i]
  pair_analysis_i <- pair_analysis %>% filter(mid_length == mid_length_i) %>%
    arrange(pair_dist)
  for (j in unique(pair_analysis_i$pair_dist)){
    pair_analysis_i_j <- pair_analysis_i %>% filter(pair_dist == j)
    g <- ggplot(pair_analysis_i_j) + 
      stat_bin(aes(home_dist, ..density..), binwidth = .1, center=.05, 
        col = "black") + geom_vline(aes(xintercept = j/2), lty=2, col = "grey50") +
      ggtitle(paste0("Edge Length: ", mid_length_i/1000, " km; Pair Distance: ",
        j, " km")) + xlab("Home Distance (km)") + ylab("Density") + 
      theme_legend + scale_x_continuous(breaks= pretty_breaks()) +
      scale_y_continuous(breaks= pretty_breaks()) 
    print(g)  
    SaveGGPlot(paste0("Home Distances - ", mid_length_i/1000, " km; ", j,
      " km.jpeg"), 
      file.path(image_output, "Home Dist - Histograms", 
        "Mid-length and Pair Distance"))
  }
}

pair_analysis_i <- pair_analysis %>% filter(mid_length == 5000)

null <- evm(home_dist, data=pair_analysis_i, th=0, phi=~1, xi=~1)
pair_dist <- evm(home_dist, data=pair_analysis_i, th=0, phi=~pair_dist, xi=~pair_dist)
pair_dist_no_xi <- evm(home_dist, data=pair_analysis_i, th=0, phi=~pair_dist, xi=~1)
pair_dist_no_phi <- evm(home_dist, data=pair_analysis_i, th=0, phi=~1, xi=~pair_dist)

summary(null)
summary(pair_dist)
summary(pair_dist_no_xi)
summary(pair_dist_no_phi)

null$coefficients
pair_dist$coefficients
pair_dist_no_xi$coefficients
pair_dist_no_phi$coefficients

par(mfrow=c(2, 2))
plot(null)
plot(pair_dist)
plot(pair_dist_no_xi)
plot(pair_dist_no_phi)

fit_pairs <- FitParetoParsToData(pair_analysis, "home_dist", by = "pair_dist", 
  location = 0, scale = 1, shape=0)


########################### SUBSET DATA  #######################################

divisor <- 1000
by <- 1

# Only non "cruising" locations from breeding eagles
baea_sub <- baea %>%
  dplyr::filter(agl <= 67) %>%
  dplyr::filter(speed <= 2) %>%  
  dplyr::filter(year == 2015) %>%
  dplyr::filter(date <= "2015-09-01") %>%
  dplyr::filter(!is.na(edge_dist)) %>%
  mutate(con_dist = con_dist/divisor) %>% 
  mutate(home_dist = home_dist/divisor) %>%  
  mutate(edge_dist = edge_dist/divisor) %>%  
  mutate(edge_dist_shift = edge_dist_shift/divisor) %>% 
  mutate(home_dist = ifelse(home_dist==0, 0.001, home_dist)) %>%
  mutate(edge_dist = ifelse(edge_dist==0, 0.001, edge_dist))

baea_sub_sp <- SpatialPointsDataFrame(baea_sub[c("long_utm", "lat_utm")], 
  bbox=NULL, data=baea_sub, proj4string=CRS(crs))

# for (i in unique(baea_sub$id)){
#   baea_sub_i <- baea_sub %>% filter(id == i) 
#   ExportKMLTelemetryBAEA(baea_sub_i, file = paste0(i, ".kml"), 
#     kml_folder = "C:/Work/R/Workspace/Baea_sub")
# }

############################### CREATE NEST DATA  ##############################

intact_last_df <- read.csv(file.path("C:/Work/R/Data/BAEA/Nests",
  "Nests_Study_Intact_Last.csv"))
intact_last <- SpatialPointsDataFrame(intact_last_df[c("long_utm", "lat_utm")], 
  bbox=NULL, data=intact_last_df, proj4string=CRS(crs))

study_nests_df <- read.csv(file="C:/Work/R/Data/BAEA/Nests/Nests_Study.csv")
#ExportKMLPoints(study_nests_df, file = "Study_Nests.kmz", id="name", lat="lat", 
#  long="long")
study_nests <- SpatialPointsDataFrame(study_nests_df[c("long_utm", "lat_utm")], 
  bbox=NULL, data=study_nests_df, proj4string=CRS(crs))

ifw_all_df <- read.csv(file.path("C:/Work/R/Data/BAEA/Nests",
  "Nests_IFW_All.csv"))
ifw_all <- SpatialPointsDataFrame(ifw_all_df[c("long_utm", "lat_utm")], 
  bbox=NULL, data=ifw_all_df, proj4string=CRS(crs))

# Pair-wise Comparisons
# Three (2012/2013) - maybe with Upper?
#                    - maybe with Madagascal?
# Ellis - maybe with Webb?
# Branch - Phillips      #1
#        - 393A (west)   #2
# Sandy - Sheepscot      #3
# Wilson - Onawa         #4
#        - Hebron        #5
# MusquahshE - NONE
# Phillips - 319B (west)         #6
#          - Branch (duplicate)
# Webb - NONE
# Crooked - Eskutassis           #7
#         - Upper                #8
# Madagascal - NONE
# Eskutassis - Upper             #9
#            - Crooked (duplicate)
# Hebron - Wilson (duplicate)
#        - Moxie                 #10
# Norway - NONE
# Sheepscot - Sandy (duplicate)

pair1a <- c("Branch", "Phillips")
pair1b <- c("Phillips", "Branch")
pair2 <- c("Branch", "Alamoosook") #393A
pair3a <- c("Sandy", "Sheepscot")
pair3b <- c("Sheepscot", "Sandy")
pair4 <- c("Wilson", "Onawa") 
pair5 <- c("Wilson", "Hebron")
pair6 <- c("Phillips", "Brewer") #319A
#pair7 <- c("Crooked", "Eskutassis")
#pair8 <- c("Crooked", "Upper")
pair9 <- c("Eskutassis","Upper")
pair10 <- c("Hebron", "Moxie") #704A


pair_list <- list(pair1a, pair1b, pair2, pair3a, pair3b, pair4, pair5, pair6, 
  pair9, pair10)

extra_nests_df <- ifw_all_df[ifw_all_df$nest_site %in% c("393A", "319A", 
  "704A"), ]

extra_nests_df$name <- NA
extra_nests_df$new_2015 <- NA
study_nests_extra_df <- rbind(study_nests_df, extra_nests_df)
study_nests_extra_df[study_nests_extra_df$nest_site == "393A", "name"] <- 
  "Alamoosook"
study_nests_extra_df[study_nests_extra_df$nest_site == "319A", "name"] <- 
  "Brewer"
study_nests_extra_df[study_nests_extra_df$nest_site == "704A", "name"] <- 
  "Moxie"
study_nests_extra <- SpatialPointsDataFrame(study_nests_extra_df[c("long_utm", 
  "lat_utm")], bbox=NULL, data=study_nests_extra_df, proj4string=CRS(crs))

#pair <- c("Crooked", "Upper")
nests <- study_nests_extra
points <- baea_sub_sp
mid_length_list <- seq(1000, 5000, by=1000)

pair_analysis <- data.frame()

source('C:/Work/R/Functions/all.R')

for (i in 1:length(mid_length_list)){
  mid_length <- mid_length_list[i]
  for (j in 1:length(pair_list)){
    pair <- pair_list[[j]]
    if (identical(pair, pair10) || identical(pair, pair5)){
      zoom = 11
    } else {
      zoom = 12
    } 
    pairs_j <- AnalyzePairLocations(pair, nests, points, mid_length, zoom=zoom)
    pair_analysis <- rbind(pair_analysis, pairs_j)
  }
}

pair_analysis <- output
save(pair_analysis, file = 'pair_analysis.RData')


########################### SIMPLE FIT PARETO ##################################
library(texmex)

# Subset 
divisor <- 1000
by <- 1
baea_sub <- baea %>%
  dplyr::filter(year == 2015) %>%
  dplyr::filter(date <= "2015-09-01") %>%
  dplyr::filter(!is.na(edge_dist)) %>%
  mutate(con_dist = con_dist/divisor) %>% 
  mutate(home_dist = home_dist/divisor) %>%  
  mutate(edge_dist = edge_dist/divisor) %>%  
  mutate(edge_dist_shift = edge_dist_shift/divisor) %>% 
  mutate(home_dist = ifelse(home_dist==0, 0.001, home_dist)) %>%
  mutate(edge_dist = ifelse(edge_dist==0, 0.001, edge_dist))

baea_sub_pareto_fit <- evm(home_dist, data=baea_sub, th=0.001)
summary(baea_sub_pareto_fit)
coef(baea_sub_pareto_fit)


PlotHistAndPareto(baea_sub, var="home_dist", by=NULL, pars= , xlim, bin_width, fit_pareto,
###           fit_color, hold_axes, labels, lines)


ggplot(baea_sub, aes(home_dist, ..density..,)) + 
  stat_bin(binwidth = 1, pad=TRUE, center=.5, col = "black")




################################## OLD CODE ####################################


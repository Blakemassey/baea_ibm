#-------------------------------------------------------------------------------
### This script is used to create a 'sim' list object, consisting of 'agents',
### 'pars', and 'spatial' lists.
#--------------------------------------------------------------------------------
pacman::p_load(baear, gisr, ibmr)

######################## SIMULATED AGENTS ######################################

pacman::p_load(raster)

## Set base area ---------------------------------------------------------------
x_min <- 0
x_max <- 100020
y_min <- 0
y_max <- 100020
cell_size <- 30
base <- raster(xmn=x_min, xmx=x_max, ymn=y_min, ymx=y_max, resolution=cell_size,
  vals=NULL)

## Set population size (number of nests = n/2) ---------------------------------
n <- 20

## Set random centroids --------------------------------------------------------
radius_range <- c(5000, 10000)  # range of radius values
circles <- cbind(sample(min(radius_range):max(radius_range), n/2), times=1)
nests_xy <- PackCircles(circles, extent=c(x_min, x_max, y_min, y_max),
  edge_buffer=15000, max_iter=1000, overlap=0, plot=TRUE)
all_nests <- cbind.data.frame(nests_xy, nest_id = paste0("nest_",
  sprintf("%03d", 1:nrow(nests_xy))), stringsAsFactors=FALSE)
study_nests <- all_nests
points(all_nests[,c("x", "y")], col="red", bg="red", pch=21)

## Create input ---------------------------------------------------------
id <- 1:n
sex <- rep(c("male", "female"), length.out = n)
age <- rep(5, n)
nest_id <- rep(study_nests[, "nest_id"], each=2)
start_x <- rep(study_nests[, "x"], each=2)
start_y <- rep(study_nests[, "y"], each=2)
input <- cbind.data.frame(id, sex, age, nest_id, start_x, start_y,
  stringsAsFactors = FALSE)

## Compile "agents" list -------------------------------------------------------
agents <- NamedList(input)

######################## REAL BAEA AGENTS ######################################

pacman::p_load(raster, rgdal)

base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")
all_nests <- read.csv(file=file.path("Data/Nests/Nests_csv",
  "Nests_Study_Intact_Last.csv"), header=TRUE, stringsAsFactors=FALSE)
all_nests <- ConvertNestIdToNum(all_nests)
#colnames(all_nests) <- c("nest_ifw", "nest_id", "status", "x", "y","territory")
for (i in 1:nrow(all_nests)){
  all_nests$x[i] <- CenterXYInCell(all_nests$long_utm[i],
    all_nests$y[i], xmin(base), ymin(base), res(base)[1])[1]
  all_nests$y[i] <- CenterXYInCell(all_nests$long_utm[i],
    all_nests$lat_utm[i], xmin(base), ymin(base), res(base)[1])[2]
  all_nests[i, "nest_id"] <- paste0("nest_", all_nests[i, "nest_id"])
}
study_nests <- read.csv(file=file.path("Data/Nests/Nests_csv",
  "Nests_Study.csv"), header=TRUE, stringsAsFactors=FALSE)
study_nests <- ConvertNestIdToNum(study_nests)
for (i in 1:nrow(study_nests)){
  study_nests$x[i] <- CenterXYInCell(study_nests$long_utm[i],
    study_nests$lat_utm[i], xmin(base), ymin(base), res(base)[1])[1]
  study_nests$y[i] <- CenterXYInCell(study_nests$long_utm[i],
    study_nests$lat_utm[i], xmin(base), ymin(base), res(base)[1])[2]
  study_nests[i, "nest_id"] <- paste0("nest_", study_nests[i, "nest_site"])
}

#global_dist <- raster(file.path("C:/ArcGIS/Data/BAEA/Nests/Distance_Rasters",
#  "global_dist_001.tif"))

##  Subset base and points -----------------------------------------------------

suppressPackageStartupMessages(library(ggplot2))
# g <- ggplot(data=study_nests) + geom_point(aes(x=x, y=y),
#   colour="dark green", size=10) + theme(legend.position="none") +
#   theme(plot.title=element_text(size=22)) + theme(text=element_text(size=20,
#   colour="black")) + theme(axis.text=element_text(colour="black"))
# g + labs(x='Long', y='Lat', title="Study Nests")
# SaveGGPlot("Study Nests.png")


## Create agents input ---------------------------------------------------------
n <- nrow(study_nests)*2
id <- 1:n
sex <- rep(c("male", "female"), length.out = n)
age <- sample(5:10, n, TRUE)
nest_id <- rep(study_nests[, "nest_id"], each=2)
start_x <- rep(study_nests[, "x"], each=2)
start_y <- rep(study_nests[, "y"], each=2)
input <- cbind.data.frame(id, sex, age, nest_id, start_x, start_y,
  stringsAsFactors = FALSE)

## Compile "agents" list -------------------------------------------------------
agents <- NamedList(input)

################################### PARS #######################################

## Create "global" parameters --------------------------------------------------
suppressPackageStartupMessages(require(lubridate))
sim_start <- as.POSIXct("2015-03-20", tz = "UTC")
sim_period <- NULL #period(4, "days")
sim_end <- as.POSIXct("2015-09-20", tz = "UTC")
rep_period <- period(1, "month")
rep_interval <- c("01Jan", "01Jul")
rep_interval_custom <- NULL

step_period <- period(1, "day")
time_step_period <- period(15, "minutes")

sim_seasons <- data.frame(start = c("01Mar", "01Sep"), season = c("breeding",
  "winter"), stringsAsFactors = FALSE)

birth_day = "01Sep"
input_age_period = "years"
report_age_period = "months"

global <- NamedList(sim_start, sim_period, sim_end, rep_period, rep_interval,
  rep_interval_custom, step_period, time_step_period, sim_seasons, birth_day,
  input_age_period, report_age_period)

## Create "Class" movement parameters ------------------------------------------

## Male ------------------------------------------------------------------------
## constant

library(broom)
library(stringr)
library(momentuHMM)
baea_hmm_full <- readRDS(file = "Data/Models/baea_hmm_full.rds")
baea_hmm_full$mle$beta
beta_est <- baea_hmm_full$mle$beta %>% as.data.frame(.)
rownames(beta_est) <- rownames(beta_est) %>%
  str_replace_all(c("\\(Intercept\\)" = "Intercept", "cosinor" = "")) %>%
  str_replace_all(c(", period = 365" = "", ", period = 1" = ""))
behavior_betas <- beta_est

move_pars <- readRDS(file="Output/Models/move_pars.rds") %>% as.data.frame(.)

fixed <- NamedList(move_pars, behavior_betas)
constant <- NamedList(fixed)

## season
breeding <- list(step_pareto_scale=300, step_pareto_shape=.2, step_max_r=100)
winter <- list(step_pareto_scale=100, step_pareto_shape=.1, step_max_r=100)
season <- NamedList(breeding, winter)

## julian
x <- 1:365
y <- sin(3*pi*x/365)*-.003 + runif(length(x),.005,.010)
julian <- data.frame(day = x)
julian$nest_return <- predict(loess(y ~ x, span=.75, data.frame(x=x, y=y)),
  data.frame(x=x))

male <- NamedList(constant, season, julian)

# g <- ggplot(data=julian, aes(x=day, y=nest_return)) + geom_line(data=julian,
#   colour="dark green", size=1.5) + theme(legend.position="none") +
#   theme(plot.title=element_text(size=22)) + theme(text=element_text(size=20,
#   colour="black")) + theme(axis.text=element_text(colour="black"))
# g + labs(x='Julian Date', y='nest_return',
#   title="Male - Nest Return Probability")
# SaveGGPlot("Male - Nests Return.png")

## Female ----------------------------------------------------------------------
## constant
fixed <- NamedList(move_pars, behavior_betas)
constant <- NamedList(fixed)
## season
breeding <- list(step_pareto_scale=300, step_pareto_shape=.2, step_max_r=100)
winter <- list(step_pareto_scale=100, step_pareto_shape=.1, step_max_r=100)
season <- NamedList(breeding, winter)
## julian
x <- 1:365
y <- sin(3*pi*x/365)*-.003 + runif(length(x), .005, .010)
julian <- data.frame(day = x)
julian$nest_return <- predict(loess(y ~ x, span=.75, data.frame(x=x, y=y)),
  data.frame(x=x))

female <- NamedList(constant, season, julian)

# g <- ggplot(data=julian, aes(x=day, y=nest_return)) + geom_line(data=julian,
#   colour="dark green", size=1.5) + theme(legend.position="none") +
#   theme(plot.title=element_text(size=22)) + theme(text=element_text(size=20,
#   colour="black")) + theme(axis.text=element_text(colour="black"))
# g + labs(x='Julian Date', y='nest_return',
#   title="Female - Nest Return Probability")
# SaveGGPlot("Female - Nests Return.png")

## Compile "pars" list ---------------------------------------------------------

classes <- NamedList(male, female)
pars <- NamedList(global, classes)

################################# SPATIAL ######################################

## Parameters for creating spatial layers --------------------------------------
pareto_scale <- 1.1 # 1.123367
pareto_shape <- 0.5 # 0.647878
gamma_scale <- 2 # 2.056299
gamma_shape <- 1.5 # 1.515315
max_r <- 45000
id = "name"
df_all <- all_nests
df_home <- study_nests
write_distance = FALSE
write_homerange = TRUE


## Create raster layers --------------------------------------------------------

homerange_kernels <- CreateHomeRangeKernelsParetoGamma(all_nests, study_nests,
  base, max_r, pareto_shape, pareto_scale, gamma_shape, gamma_scale,
  id, output_dir = getwd(), write_distance, write_homerange)

nests <- data.frame(study_nests[c("x","y")], data.frame(nest_id =
  study_nests["nest_id"]))
spatial <- NamedList(base, nests)#, homerange_kernels)

################################## SIM #########################################

sim <- NamedList(agents, pars, spatial)
RemoveExcept("sim")
save(sim, file="Data/Simulation/sim2.RData")
#sim <- readRDS("Data/Simulation/sim2.rds")

#plot(sim$spatial$homerange_kernels[[1]])
SavePlot("homerange_kernel_1.png", height = 5, width=5)

# Recompile sim
agents <- sim$agents
pars <- sim$pars
spatial <- sim$spatial
sim <- NamedList(agents, pars, spatial)
saveRDS(sim, file="Data/Simulation/sim2.rds")

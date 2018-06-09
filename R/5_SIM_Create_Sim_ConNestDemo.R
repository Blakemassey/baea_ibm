--------------------------------------------------------------------------------
### This script is used to create a 'sim' list object, consisting of 'agents',
### 'pars', and 'spatial' lists.
--------------------------------------------------------------------------------

library(baear)
library(gisr)
library(ibmr)

######################## REAL BAEA AGENTS ######################################

suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(dplyr))

base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")

study_nests <- baea %>%
  group_by(id) %>%
  slice(1) %>%
  dplyr::select(id, nest_site, nest_long_utm, nest_lat_utm) %>%
  mutate(nest_id = nest_site,
    x = NA,
    y= NA) %>%
  rename(name = id,
    long_utm = nest_long_utm,
    lat_utm = nest_lat_utm) %>%
  mutate(x = NA,
    y= NA) %>%
  ungroup()

#study_nests <- read.csv(file=file.path("C:/Work/R/Data/BAEA/Nests/",
#  "Nests_Study.csv"), header=TRUE, stringsAsFactors=FALSE)
study_nests <- ConvertNestIdToNum(study_nests)
for (i in 1:nrow(study_nests)){
  study_nests$x[i] <- CenterXYInCell(study_nests$long_utm[i],
    study_nests$lat_utm[i], xmin(base), ymin(base), res(base)[1])[1]
  study_nests$y[i] <- CenterXYInCell(study_nests$long_utm[i],
    study_nests$lat_utm[i], xmin(base), ymin(base), res(base)[1])[2]
  study_nests[i, "nest_id"] <- paste0("nest_", study_nests[i, "nest_site"])
}

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
nest_id <- rep(unlist(study_nests[, "nest_id"]), each=2)
start_x <- rep(unlist(study_nests[, "x"]), each=2)
start_y <- rep(unlist(study_nests[, "y"]), each=2)
input <- cbind.data.frame(id, sex, age, nest_id, start_x, start_y,
  stringsAsFactors = FALSE)

## Compile "agents" list -------------------------------------------------------
agents <- NamedList(input)

################################### PARS #######################################

## Create "global" parameters --------------------------------------------------
suppressPackageStartupMessages(require(lubridate))
sim_start <- as.POSIXct("2015-01-01", tz = "UTC")
sim_period <- period(5, "days")
sim_end <- NULL
rep_period <- period(1, "days")
rep_interval <- c("01Jan", "01Jul")
rep_interval_custom <- NULL

step_period <- period(1, "day")
time_step_period <- period(1, "hour")

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
step_cauchy_mu <- 0
step_cauchy_rho <- .5
nestcon_gamma_shape <- 1.1
nestcon_gamma_rate <- 0.495

fixed <- NamedList(step_cauchy_mu, step_cauchy_rho, nestcon_gamma_shape,
  nestcon_gamma_rate)
constant <- NamedList(fixed)
## season
breeding <- list(step_weibull_scale=1.172086, step_weibull_shape=0.7081443, step_max_r=12000)
winter <- list(step_weibull_scale=1.172086, step_weibull_shape=0.7081443, step_max_r=12000)
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
step_cauchy_mu <- 0
step_cauchy_rho <- .5
nestcon_gamma_shape <- 1.1
nestcon_gamma_rate <- 0.495

fixed <- NamedList(step_cauchy_mu, step_cauchy_rho, nestcon_gamma_shape,
  nestcon_gamma_rate)
constant <- NamedList(fixed)
## season
breeding <- list(step_weibull_scale=1.172086, step_weibull_shape=0.7081443, step_max_r=12000)
winter <- list(step_weibull_scale=1.172086, step_weibull_shape=0.7081443, step_max_r=12000)
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

base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")

## Parameters for creating spatial layers --------------------------------------

id = "name"
baea <- baea
nest_set <- nests_active
base <- base
max_r <- 45000
write_home_dist = FALSE
write_con_dist = FALSE
write_con_dist_nest = FALSE
write_con_nest = FALSE

## Create raster layers --------------------------------------------------------

con_nest_raster <- CreateConNestRasters(baea, nest_set, base=base,
  output_dir=getwd(), max_r,
  write_home_dist = FALSE,
  write_con_dist = FALSE,
  write_con_dist_nest = FALSE,
  write_con_nest = TRUE)

nests <- data.frame(study_nests[c("x","y")], data.frame(nest_id =
  study_nests["nest_id"]))
spatial <- NamedList(base, nests, con_nest_raster)

################################## SIM #########################################

sim <- NamedList(agents, pars, spatial)
#RemoveExcept("sim")
save(sim, file="C:/Work/R/Data/Simulation/sim.RData")

# Recompile sim
agents <- sim$agents
pars <- sim$pars
spatial <- sim$spatial
sim <- NamedList(agents, pars, spatial)
save(sim, file="C:/Work/R/Data/Simulation/sim.RData")

RemoveExcept(c("baea", "nest_set", "base", "max_r", "write_home_dist",
  "write_con_dist", "write_con_dist_nest", "write_con_dist"))

CreateConNestRasters <- function(baea,
                                 nest_set,
                                 base,
                                 output_dir = getwd(),
                                 max_r,
                                 write_home_dist = FALSE,
                                 write_con_dist = FALSE,
                                 write_con_dist_nest = FALSE,
                                 write_con_nest = FALSE){
  id <- "nest_site"
  name <- "name"
  if (output_dir != getwd()) unlink(output_dir, recursive=TRUE)
  if (!dir.exists(output_dir)) dir.create(output_dir)
  if (write_home_dist | write_con_dist | write_con_dist_nest | write_con_nest){
  for (k in sort(unique(baea$year))) dir.create(file.path(output_dir, k),
    showWarnings = FALSE)
  }
  cellsize <- res(base)[1]
  max_r_cells <- ceiling(max_r/cellsize)
  xmin <- raster::xmin(base)
  ymin <- raster::ymin(base)
  for (i in 1:nrow(nest_set)){
    nest_set[i, "x"] <- CenterXYInCell(nest_set[i,"long_utm"],
      nest_set[i,"lat_utm"], xmin, ymin, cellsize)[1]
    nest_set[i, "y"] <- CenterXYInCell(nest_set[i,"long_utm"],
      nest_set[i,"lat_utm"], xmin, ymin, cellsize)[2]
  }

  con_nest_rasters <- list()
  con_nest_rasters_names <- vector()

  for (i in sort(unique(baea$year))){
    baea_year <- baea %>% dplyr::filter(year == i)
    active_year <- paste0("active_", i)
    col <- which(colnames(nest_set) == active_year)
    df_all <- nest_set[which(nest_set[,col] == TRUE), ]
    year_nest <- unique(baea_year$nest_site)
    df_home <- df_all %>% dplyr::filter(nest_site %in% year_nest)
    df_all_sp <- SpatialPointsDataFrame(df_all[,c("x","y")], df_all,
      proj4string=crs(base))
    nest_ids <- df_home[,id]
    total <- length(nest_ids)
    for (j in 1:nrow(df_home)) {
      hr_nest_site <- df_home[j, id]
      id_year_xy <- baea %>%
        dplyr::filter(nest_site == hr_nest_site) %>%
        dplyr::filter(year == i) %>%
        dplyr::mutate(x = long_utm) %>%
        dplyr::mutate (y = lat_utm) %>%
        dplyr::select(x,y)
      sv <- baea$nest_site == hr_nest_site & baea$year == i
      sv <- ifelse(is.na(sv), FALSE, sv)
      home <- df_home[j,]
      ifelse(is.null(name), home_name <- j, home_name <- home[,name])
      writeLines(noquote(paste0("Calculating nest, con, and nestcon distances",
        " for ", i, ": ", home_name, " (", j, " of ", total, ").")))
      home_sp <- sp::SpatialPointsDataFrame(home[,c("x","y")], home,
        proj4string=crs(base))
      xy <- c(home_sp@coords[,"x"], home_sp@coords[,"y"])
      cell_extent <- raster::extent(xy[1]-(cellsize/2), xy[1]+(cellsize/2),
        xy[2]-(cellsize/2), xy[2]+(cellsize/2))
      cell <- setValues(raster(cell_extent,crs=projection(base),res=cellsize),j)
      home_ext <- raster::extend(cell, c(max_r_cells, max_r_cells), value=NA)
      home_dist <- raster::distanceFromPoints(home_ext, home[,c("x","y")])
      if (write_home_dist == TRUE) {
        filename <- file.path(output_dir, i, paste0("HomeDist_", home_name,
          ".tif"))
        raster::writeRaster(home_dist, filename=filename, format="GTiff",
          overwrite=TRUE)
        writeLines(noquote(paste("Writing:", filename)))
      }
      base_crop <- raster::raster(raster::extent(home_ext), resolution=30,
        crs=raster::crs(home_ext))
      rm(home_ext)
      base_crop <- raster::setValues(base_crop, runif(ncell(base_crop), 1, 10))
      con_dist <- raster::distanceFromPoints(base_crop,
        df_all_sp[which(df_all_sp$nest_area != home$nest_area),])
      # raster::plot(con_dist)
      if (write_con_dist == TRUE) {
        filename <- file.path(output_dir, i, paste0("ConDist_", home_name,
          ".tif"))
        writeLines(noquote(paste0("Writing: ", filename)))
        writeRaster(con_dist, filename=filename, format="GTiff",
          overwrite=TRUE)
      }
      fun <- function(x){ raster::extract(con_dist, home_sp) - x  }
      con_dist_nest <- raster::calc(con_dist, fun)
      if (write_con_dist_nest == TRUE) {
        filename <- file.path(output_dir, i, paste0("ConDistNest_", home_name,
            ".tif"))
        writeLines(noquote(paste0("Writing: ", filename)))
        raster::writeRaster(con_dist_nest, filename=filename, format="GTiff",
          overwrite=TRUE)
      }
      con_nest <- overlay(home_dist, con_dist_nest, fun=function(x,y){round(x+y)})
      if (write_con_nest == TRUE) {
        filename <- file.path(output_dir, i, paste0("ConNest_", home_name,
            ".tif"))
        writeLines(noquote(paste0("Writing: ", filename)))
        raster::writeRaster(con_nest, filename=filename, format="GTiff",
          overwrite=TRUE)
      }
      plot(con_nest, main=paste0(home_name, " (", df_home[j, id], ")"))
      con_nest_rasters[[j]] <- con_nest
      con_nest_rasters_names[j] <- paste0("nest_", df_home[j, id])
    }
    con_nest_rasters <- setNames(con_nest_rasters, con_nest_rasters_names)
  }
  return(con_nest_rasters)
}


###################### ModelFit_SSF_Optimize ###################################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, plyr, dplyr, future, furrr, optimx, ggplot2, ggthemes,
  optimx, raster, reproducible, rgdal, rgenoud, smoothie, stringr, survival,
  tictoc) #spatialfil
pacman::p_load(baear, gisr, ibmr)
options(stringsAsFactors=FALSE)

tic_msg <- function(tic, msg) {
  if (is.null(msg) || is.na(msg) || length(msg) == 0){
    outmsg <- paste(lubridate::duration(round(toc - tic)))
  } else {
    outmsg <- paste0("Starting: ", msg)
  }
}
toc_msg <- function(tic, toc, msg, info) {
  tt_duration <- lubridate::duration(round(toc - tic))
  if (is.null(msg) || is.na(msg) || length(msg) == 0) {
    outmsg <- paste0(tt_duration)
  } else {
    outmsg <- paste0(info,": ", tt_duration)
  }
}

################ OPTIMIZATION PROCEDURE OF KERNEL BANDWIDTH ####################

start = "roost"
end = "flight"

step_type <- paste0(start, "_", end)
ua_steps_i <- readRDS(file.path("Output/Analysis/Movements/SSF/UA_Data",
  paste0("ua_steps_", step_type, ".rds")))  %>%
  #dplyr::select(-c(datetime)) %>%
  dplyr::select(-c(step_id:lat_utm_end)) %>%
  select_if(function(x) !(all(is.na(x)) | all(x=="")))

colnames_alpha <- str_replace_all(colnames(ua_steps_i), "[0-9]", "")
colnames_num <- as.numeric(str_replace_all(colnames(ua_steps_i), "[^0-9]", ""))
covars <- unique(colnames_alpha[-1]) # list of covariates (minus case)

colnames_tbl <- tibble(colnames_alpha, colnames_num) %>%
  mutate(colnames_sigma = ifelse(!is.na(colnames_num), colnames_num/30, NA)) %>%
  mutate_all(~str_replace_na(., "")) %>%
  mutate(colnames_final = paste0(colnames_alpha, colnames_sigma))

colnames(ua_steps_i) <- colnames_tbl %>% pull(colnames_final)

domains <- tibble(covar = character(), covar_min = integer(),
  covar_max = integer(), covar_median = integer())

for (i in 1:length(covars)){
  covar <- covars[i]
  covar_i <- str_subset(colnames(ua_steps_i), covar)
  covar_i_bandwidths <- as.numeric(str_replace_all(covar_i, "[^0-9]", ""))
  domains[i, "covar"] <- covar
  domains[i, "covar_min"] <- min(covar_i_bandwidths)
  domains[i, "covar_max"] <- max(covar_i_bandwidths)
  domains[i, "covar_median"] <- quantile(covar_i_bandwidths, p = 0.5, type = 1)
}

sigma_domains <- domains %>%
  filter(!covar %in% c("hydro_dist"))

sigma_matrix <- sigma_domains %>%
  dplyr::select(covar_min, covar_max) %>%
  as.matrix(.)
sigma_variables <- sigma_domains %>% pull(covar)
sigma_starts <- sigma_domains %>% pull(covar_median)

# the domains matrix has to be equal to the number of scale-dependent variables

# FIND UNIQUE COLUMN NAMES -----------------------------------------------------
# FIND MIN, MAX, MEDIAN OF COLUMNS with that name

# all_mods <- readRDS(file.path("Output/Analysis/Movements/SSF/Models",
#   paste0("all_models_", step_type, ".rds")))
# opt_mods <- readRDS(file.path("Output/Analysis/Movements/SSF/Models",
#   paste0("opt_models_", step_type, ".rds")))


# Create Covar Sigma Rasters Brick ---------------------------------------------

# covar_brick <- brick(c(
#   tibble(sigma = opt_sigmas, covar = "covar1") %>% pmap(., SmoothRaster),
#   tibble(sigma = opt_sigmas, covar = "covar2") %>% pmap(., SmoothRaster),
#   tibble(sigma = opt_sigmas, covar = "covar3") %>% pmap(., SmoothRaster)
#   ))
#
# covar_matrix <- raster::as.matrix(covar_brick)
# covar_cols <-  setNames(seq_len(ncol(covar_matrix)), colnames(covar_matrix))
# covar_names <- c(names(covar1), names(covar2), names(covar3))
# rm(covar_brick)

# Fit Sigma Combo Models -------------------------------------------------------

# Make Clusters
plan(multiprocess)

ua_steps_i_lst <- tibble(number = 1, ua_data = list(ua_steps_i))

tic("Fit Optimization Models")
df_opt_fit0 <- ua_steps_i_lst %>%
  mutate(opt_fit = future_map2(number, ua_data, FitSigmaOpt_1_9,
    .progress = TRUE))
toc()
#saveRDS(df_opt_fit0, file.path(file_dir, paste0("df_opt_fit0_", today(),
#  ".rds")))

# Functions: Sigma Combos Model Fitting  ---------------------------------------

  domains = matrix(c(rep(min(opt_sigmas), 3), rep(max(opt_sigmas), 3)),
    ncol = 2)
  sigma3 <- sigma2 <- sigma1 <- median(opt_sigmas)
  starting_values <- c(sigma1, sigma2, sigma3)
  parms <- c(sigma1, sigma2, sigma3)
  sigmas <- sigma_starts

# For 1 scale-independent variable, 9 scale-dependent variables
FitSigmaOpt_1_9 <- function(number, ua_data){
  # domains are the min and max allowed sigma evalutated
  domains <- sigma_matrix[1:4,]
  # starting values for each covariate sigma evalutated
  starting_values <- sigma_starts[1:4]
  parms <- starting_values
  cases <- ua_data[, "case"]
  #cell_nums <- ua_data[, "cell_num"]
  opt_fit <- genoud(fn = AICSigmaOpt_1_9, nvars = 4, pop.size = 10000,
    starting.values = starting_values, optim.method = "SANN",
    max.generations = 2000, hard.generation.limit = FALSE, # was 200 max.gen
    wait.generations = 200, P5 = 0, P6 = 0, P7 = 0, P8 = 0,
    BFGSburnin = 500,
    print.level = 0, #cluster = cl,
    boundary.enforcement = 2, ua_data = ua_data,
    data.type.int = TRUE,  Domains = domains)
  return(opt_fit)
}

AICSigmaOpt_1_9 <- function(sigmas, ua_data){
  #cell_nums <- ua_data$cell_num
  df <- data.frame(case = ua_data$case,
    value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
    value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
    value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])],
    value4 = ua_data[, paste0(sigma_variables[4], sigmas[4])],
    value5 = ua_data[, "hydro_dist0"])
  model_logistic <- glm(case ~ value1 + value2 + value3 + value4 + value5,
    family = binomial(link = "logit"), data = df)
  model_aic = AIC(model_logistic)
  return(model_aic)
}


# Projection data
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Landscape file locations
      base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
# kernel class
 developed_file <- "C:/ArcGIS/Data/R_Input/BAEA/developed_30mc.tif"
    forest_file <- "C:/ArcGIS/Data/R_Input/BAEA/forest_30mc.tif"
open_water_file <- "C:/ArcGIS/Data/R_Input/BAEA/open_water_30mc.tif"
   pasture_file <- "C:/ArcGIS/Data/R_Input/BAEA/pasture_30mc.tif"
shrub_herb_file <- "C:/ArcGIS/Data/R_Input/BAEA/shrub_herb_30mc.tif"
   wetland_file <- "C:/ArcGIS/Data/R_Input/BAEA/wetland_30mc.tif"
# terrain class
      elev_file <- "C:/ArcGIS/Data/R_Input/BAEA/elev_30mc.tif"
# extract class
hydro_dist_file <- "C:/ArcGIS/Data/R_Input/BAEA/hydro_dist_30mc.tif"

# BAEA and Movement Parameters
baea_steps_file <- "Data/BAEA/baea_steps.rds"
move_pars_file <- "Output/Analysis/Movements/move_pars.rds"

# Subsetting Variables
subsetting_bandwidths <- FALSE
subsetting_covars <- FALSE
subsetting_ids <- FALSE
subsetting_step_types <- TRUE

##########  IMPORT BASE RASTER, STEPS DATA, AND MOVEMENT PARAMETERS ############

base <- raster(base_file)
baea_steps <- readRDS(file = baea_steps_file)
move_pars <- readRDS(file = move_pars_file)
rm(base_file, baea_steps_file, move_pars_file)

#######################  IMPORT COVARIATE RASTERS ##############################

# Import rasters
elev <- raster(elev_file) # all other layers' extent are set to this layer
developed <- raster(developed_file)
forest <- raster(forest_file)
open_water <- crop(raster(open_water_file), elev)
pasture <- crop(raster(pasture_file), elev)
shrub_herb <- crop(raster(shrub_herb_file), elev)
wetland <- crop(raster(wetland_file), elev)
hydro_dist <- raster(hydro_dist_file)

rm(developed_file, forest_file, open_water_file, pasture_file,
  shrub_herb_file, wetland_file, elev_file, hydro_dist_file)

################ OPTIMIZATION PROCEDURE OF KERNEL BANDWIDTH ####################

start = "nest"
end = "roost"

step_type <- paste0(start, "_", end)
ua_data <- readRDS(file.path("Output/Analysis/Movements/SSF/UA_Data",
  paste0("ua_steps_", step_type, ".rds")))
all_mods <- readRDS(file.path("Output/Analysis/Movements/SSF/Models",
  paste0("all_models_", step_type, ".rds")))
opt_mods <- readRDS(file.path("Output/Analysis/Movements/SSF/Models",
  paste0("opt_models_", step_type, ".rds")))


# Create Covar Sigma Rasters Brick ---------------------------------------------

covar_brick <- brick(c(
  tibble(sigma = opt_sigmas, covar = "covar1") %>% pmap(., SmoothRaster),
  tibble(sigma = opt_sigmas, covar = "covar2") %>% pmap(., SmoothRaster),
  tibble(sigma = opt_sigmas, covar = "covar3") %>% pmap(., SmoothRaster)
  ))

covar_matrix <- raster::as.matrix(covar_brick)
covar_cols <-  setNames(seq_len(ncol(covar_matrix)), colnames(covar_matrix))
covar_names <- c(names(covar1), names(covar2), names(covar3))
rm(covar_brick)

# Fit Sigma Combo Models -------------------------------------------------------

# Make Clusters
plan(multiprocess)

tic("Fit Optimization Models 0")
df_opt_fit0 <- df_pa_data %>% slice(1:1000) %>%
  mutate(opt_fit = future_map2(number, pa_data, FitSigmaOpt3, .progress = TRUE))
toc()
saveRDS(df_opt_fit0, file.path(file_dir, paste0("df_opt_fit0_", today(),
  ".rds")))
rm(df_opt_fit0)


# Functions: Sigma Combos Model Fitting  ---------------------------------------

FitSigmaOpt3 <- function(number, pa_data){
  domains = matrix(c(rep(min(opt_sigmas), 3), rep(max(opt_sigmas), 3)),
    ncol = 2)
  sigma3 <- sigma2 <- sigma1 <- median(opt_sigmas)
  starting_values <- c(sigma1, sigma2, sigma3)
  parms <- c(sigma1, sigma2, sigma3)
  cases <- pa_data[, "case"]
  cell_nums <- pa_data[, "cell_num"]
  opt_fit <- genoud(fn = AICSigmaOpt3, nvars = 3, pop.size = 10000,
    starting.values = starting_values, optim.method = "SANN",
    max.generations = 2000, hard.generation.limit = FALSE, # was 200 max.gen
    wait.generations = 200, P5 = 0, P6 = 0, P7 = 0, P8 = 0,
    BFGSburnin = 500,
    print.level = 0, #cluster = cl,
    boundary.enforcement = 2, pa_data = pa_data,
    data.type.int = TRUE,  Domains = domains)
  return(opt_fit)
}

AICSigmaOpt3 <- function(sigmas, pa_data){
  cell_nums <- pa_data$cell_num
  df <- data.frame(case = pa_data$case,
    value1 = covar_matrix[cell_nums, covar_cols[paste0(covar_names[1],
      sigmas[1])]],
    value2 = covar_matrix[cell_nums, covar_cols[paste0(covar_names[2],
      sigmas[2])]],
    value3 = covar_matrix[cell_nums, covar_cols[paste0(covar_names[3],
      sigmas[3])]])
  model_logistic <- glm(case ~ value1 + value2 + value3,
    family = binomial(link = "logit"), data = df)
  model_aic = AIC(model_logistic)
  return(model_aic)
}



## -------------------------------------------------------------------------- ##
################# OLD CODE BELOW HERE ##########################################
## -------------------------------------------------------------------------- ##


# Likelihood function
# z = a vector of zeros and ones corresponding to available and used points
# cell = a vector of the cell numbers of each point
# surface = a data frame with x/y coordinates and values of the raster surface

r1 <- rasterFromXYZ(surface1)
r2 <- rasterFromXYZ(surface2) # surface is a df with X|Y|VALUES
#  zmat2 <- as.matrix(r2)

nll_kern_bw <- function(parms=parms, z=z, cell=cell, zmat1=zmat1, zmat2=zmat2){
  # Parameters
  b0 <- parms[1]         # intercept
  bx1 <- parms[2]         # slope of the relationship
  bwx1 <-  exp(parms[3])+1
  bx2 <- parms[4]  # bw/spatial scale parameter
  bwx2 <-  exp(parms[5])+1
  # Create matrix
  zmat1 <- as.matrix(r1)
  zmat2 <- as.matrix(r1)
  # Gaussian-weighted smooth of matrix
  f <- kernel2dsmooth(zmat1,kernel.type="gauss", nx=nrow(r), ny=ncol(r),sigma=bwx1)
  values(r1) <- f
  # Gaussian-weighted smooth of matrix
  f2 <- kernel2dsmooth(zmat2,kernel.type="gauss", nx=nrow(r), ny=ncol(r),sigma=bwx2)
  values(r2) <- f2
  # Compute binomial success probabilities
  probs <- plogis(b0 + bx1*(r[cell])+ bx2(r[cell]))
  # Evaluate log of binomial pmf
  tmp <- dbinom(z, 1, probs, log=TRUE)
  ll <- -1*sum(tmp)
  return(ll)
}

opt_mods_coef <- dplyr::left_join(opt_mods, all_mods, by = c("lc_type", "bw"))

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
# test: surface <- data.frame(coordinates(eval(parse(text = lc_types[i]))), values(eval(parse(text = lc_types[i]))))

# OPTIMIZE!!!!!!!!!!!!

system.time(out_developed <- optimx(par=parms_developed, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_developed, surface=surface_developed, control = list(trace=3)))
save(out_developed, file = "out_developed.RData")

system.time(out_forest <- optimx(par=parms_forest, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_forest, surface=surface_forest, control = list(trace=3)))
save(out_forest, file = "out_forest.RData")

system.time(out_open_water <- optimx(par=parms_open_water, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_open_water, surface=surface_open_water, control = list(trace=3)))
save(out_open_water, file = "out_open_water.RData")

system.time(out_pasture <- optimx(par=parms_pasture, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_pasture, surface=surface_pasture, control = list(trace=3)))
save(out_pasture, file = "out_pasture.RData")

system.time(out_shrub_herb <- optimx(par=parms_shrub_herb, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_shrub_herb, surface=surface_shrub_herb, control = list(trace=3)))
save(out_shrub_herb, file = "out_shrub_herb.RData")

system.time(out_wetland <- optimx(par=parms_wetland, fn=nll_kern_bw,
  z=ua_data$point, cell=cell_wetland, surface=surface_wetland, control = list(trace=3)))
save(out_wetland, file = "out_wetland.RData")


# Load optimx results

load(file="out_developed.RData")
load(file="out_forest.RData")
load(file="out_open_water.RData")
load(file="out_pasture.RData")
load(file="out_shrub_herb.RData")
load(file="out_wetland.RData")

(exp(out_developed[1,3])+1)*15
(exp(out_forest[1,3])+1)*15
(exp(out_open_water[1,3])+1)*15
(exp(out_pasture[1,3])+1)*15
(exp(out_shrub_herb[1,3])+1)*15
(exp(out_wetland[1,3])+1)*15


(exp(out_developed[2,3])+1)*15
(exp(out_forest[2,3])+1)*15
(exp(out_open_water[2,3])+1)*15
(exp(out_pasture[2,3])+1)*15
(exp(out_shrub_herb[2,3])+1)*15
(exp(out_wetland[2,3])+1)*15

log(150)
exp(5.010635)


# FAILED
system.time(out <- optim(par=parms, fn=nll_kern_bw, method = "L-BFGS-B",
      lower = c(-Inf, -Inf, log(1)), upper = rep(Inf, Inf, log(500)),
      z=ua_data$point, cell=cell1, surface=surface, control = list(trace=3,
        REPORT=1)))
# FAILED
system.time(out <- nlm(nll_kern_bw,parms,print.level=2,hessian=TRUE))


################ MULTI LAYER OPTIMX ############################################

# Create surfaces and matrixes

r1 <- surface1
r2 <- surface2
zmat1 <- as.matrix(r1)
zmat2 <- as.matrix(r2)

# Likelihood function
# z = a vector of zeros and ones corresponding to available and used points
# cell = a vector of the cell numbers of each point
# surface = a data frame with x/y coordinates and values of the raster surface

nll_kern_bw <- function(parms=parms, z=z, cell=cell, r1=r1, r2=r2,
    zmat1=zmat1, zmat2=zmat2){
  # Parameters
  b0 <- parms[1]             # intercept
  beta1 <- parms[2]            # slope of the relationship, surface 1
  bw1 <-  exp(parms[3])+1   # bw/spatial scale parameter, surface 1
  beta2 <- parms[4]            # slope of the relationship, surface 2
  bw2 <-  exp(parms[5])+1   # bw/spatial scale parameter, surface 2
  # Gaussian-weighted smooth of matrix
  f1 <- kernel2dsmooth(zmat1, kernel.type="gauss", nx=nrow(r1), ny=ncol(r1), sigma=bw1)
  values(r1) <- f1
  # Gaussian-weighted smooth of matrix
  f2 <- kernel2dsmooth(zmat2, kernel.type="gauss", nx=nrow(r2), ny=ncol(r2), sigma=bw2)
  values(r2) <- f2
  # Compute binomial success probabilities
  probs <- plogis(b0 + beta1*(r1[cell]) + beta2(r2[cell]))
  # Evaluate log of binomial pmf
  tmp <- dbinom(z, 1, probs, log=TRUE)
  ll <- -1*sum(tmp)
  return(ll)
}

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


################################################################################
# This is code from Chris Sutherland's original script,
# including the simulation example.
################################################################################

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

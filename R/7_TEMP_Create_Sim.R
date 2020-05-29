########################## CREATE SIM ##########################################

#-------------------------------------------------------------------------------
### This script is used to create a 'sim' list object, consisting of 'agents',
### 'pars', and 'spatial' lists.
#-------------------------------------------------------------------------------
pacman::p_load(baear, gisr, ibmr)
pacman::p_load(momentuHMM, raster, rgdal, tidyverse, lubridate)

######################## REAL BAEA AGENTS ######################################

baea_terr <- readRDS("Data/BAEA/baea_terr.rds")
base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")

nests_agents <- baea_terr %>%
  group_by(id) %>%
  slice(1) %>%
  dplyr::select(id, nest_site, nest_long_utm, nest_lat_utm) %>%
  mutate(nest_id = nest_site) %>%
  rename(name = id, long_utm = nest_long_utm, lat_utm = nest_lat_utm) %>%
  ungroup() %>%
  ConvertNestIdToNum(.) %>%
  mutate(x = CenterXYInCell(long_utm, lat_utm, xmin(base), ymin(base),
    res(base)[1], "x")) %>%
  mutate(y = CenterXYInCell(long_utm, lat_utm, xmin(base), ymin(base),
    res(base)[1], "y"))

## Create agents input ---------------------------------------------------------
n <- nrow(nests_agents)*2
id <- 1:n
sex <- rep(c("male", "female"), length.out = n)
age <- sample(5:10, n, TRUE)

nest_id <- rep(unlist(nests_agents[, "nest_id"]), each=2)
start_x <- rep(unlist(nests_agents[, "x"]), each=2)
start_y <- rep(unlist(nests_agents[, "y"]), each=2)
input <- tibble(id, sex, age, nest_id, start_x, start_y)

## Compile "agents" list -------------------------------------------------------
agents <- NamedList(input)

## Clean up environment --------------------------------------------------------
rm(age, baea_terr, id, input, n, nest_id, sex, start_x, start_y)

################################### PARS #######################################

## Create "global" parameters --------------------------------------------------
sim_start <- as.POSIXct("2015-03-15", tz = "UTC")
sim_period <- NULL #period(4, "days")
sim_end <- as.POSIXct("2015-03-30", tz = "UTC")
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

## Clean up environment --------------------------------------------------------
rm(birth_day, input_age_period, rep_interval, rep_interval_custom, rep_period,
  report_age_period, sim_end, sim_period, sim_seasons, sim_start, step_period,
  time_step_period)

## Create "Class" movement parameters ------------------------------------------

## Male ------------------------------------------------------------------------
## constant
baea_hmm_full <- readRDS(file = "Output/Analysis/Transitions/baea_hmm_full.rds")
baea_hmm_full$mle$beta
beta_est <- baea_hmm_full$mle$beta %>% as.data.frame(.)
rownames(beta_est) <- rownames(beta_est) %>%
  str_replace_all(c("\\(Intercept\\)" = "Intercept", "cosinor" = "")) %>%
  str_replace_all(c(", period = 365" = "", ", period = 1" = ""))
behavior_betas <- beta_est

move_pars <- readRDS(file = "Output/Analysis/Movements/move_pars.rds") %>%
  as.data.frame(.)
con_nest_pars <- readRDS(file = "Output/Analysis/Territorial/con_nest_pars.rds")

fixed <- NamedList(con_nest_pars, move_pars, behavior_betas)
constant <- NamedList(fixed)

## season
breeding <- list(NA)
winter <- list(NA)
season <- NamedList(breeding, winter)

## julian
julian <- list(NA)

male <- NamedList(constant, season, julian)

## Female ----------------------------------------------------------------------
## constant
fixed <- NamedList(con_nest_pars, move_pars, behavior_betas) # All are the same as male
constant <- NamedList(fixed)
## season
breeding <- list(NA)
winter <- list(NA)
season <- NamedList(breeding, winter)
## julian
julian <- list(NA)

female <- NamedList(constant, season, julian)

## Compile "pars" list ---------------------------------------------------------

classes <- NamedList(male, female)
pars <- NamedList(global, classes)

## Clean up environment --------------------------------------------------------
RemoveExcept(c("agents", "pars", "nests_agents"))

################################# SPATIAL ######################################

base <- raster("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif")

## Create Nest and Conspecific Distance Rasters --------------------------------

nests_active <- readRDS("Data/Nests/Nests_rds/nests_active.rds")
fits_baea_dist = readRDS("Output/Analysis/Territorial/fits_baea_dist.rds")

## Calculate Con_Nest Distance -------------------------------------------------

con_nest_dist <- CreateSimConNestDistRasters(agents = agents,
  nest_set = nests_active, base = base, output_dir = "Output/Sim/Territorial",
  max_r = 45000, write_con_nest_all = FALSE)

nests <- data.frame(nests_agents[c("x","y")], data.frame(nest_id =
  nests_agents["nest_id"]))
spatial <- NamedList(base, nests, con_nest_dist)

## Clean up environment --------------------------------------------------------
RemoveExcept(c("agents", "pars", "spatial"))

################################## SIM #########################################

sim <- NamedList(agents, pars, spatial)
#RemoveExcept("sim")
saveRDS(sim, file="C:/Work/R/Data/Simulation/sim_01.rds")

# Recompile sim
sim <- readRDS(file="C:/Work/R/Data/Simulation/sim_01.rds")
agents <- sim$agents
pars <- sim$pars
spatial <- sim$spatial
sim <- NamedList(agents, pars, spatial)
saveRDS(sim, file="C:/Work/R/Data/Simulation/sim_01.rds")

RemoveExcept(c("baea", "nest_set", "base", "max_r", "write_home_dist",
  "write_con_dist", "write_con_dist_nest", "write_con_dist"))

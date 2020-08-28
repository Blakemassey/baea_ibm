pacman::p_load(baear, gisr, ibmr)
pacman::p_load(rgdal, tictoc, tidyverse, lubridate)
#devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/ibmr")
toc_msg <- function(tic, toc, msg, info){
  outmsg <- paste(seconds_to_period(round(toc - tic)))
}

source('R/5c_SIM_MovementSubmodelBAEA.R')

sim <- readRDS("C:/Work/R/Data/Simulation/sim_01.rds")

sim$pars$global$sim_end <- as.POSIXct("2015-05-15", tz = "UTC")
sim$agents$input <- sim$agents$input %>% slice(c(1,3))




init = TRUE


#
tic("Original version")
  sim <- sim
    spatial <- sim$spatial
    base <- spatial$base
    nests <- spatial$nests
    landscape <- spatial$landscape
    con_nest_dist <- spatial$con_nest_dist
    ssf_layers <- spatial$ssf_layers
    behavior_levels <- c("Cruise", "Flight", "Nest", "Perch", "Roost")
    # male and female same for now
    move_pars <- sim$pars$classes$male$constant$fixed$move_pars %>%
      mutate(
        behavior_num = as.numeric(factor(behavior, levels = behavior_levels)),
        behavior_next_num = as.numeric(factor(behavior_next,
          levels = behavior_levels))) %>%
      mutate(ids = paste0(behavior_num, "_", behavior_next_num))
    move_pars_ids <- move_pars$ids
    move_kernels <- as.list(setNames(rep(NA, nrow(move_pars)),
      move_pars_ids), move_pars_ids)
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
      move_kernels[[i]] <- kernel_raster
      names(move_kernels[[i]]) <- paste0(move_pars_i$behavior_behavior[1])
    }
    male <- NamedList(move_kernels)
    female <- NamedList(move_kernels)
    classes <- NamedList(male, female)
    spatial <- NamedList(base, nests, classes, con_nest_dist, landscape,
      ssf_layers)
    sim$spatial <- spatial
toc()


tic("Updated version")
    behavior_levels <- c("Cruise", "Flight", "Nest", "Perch", "Roost")
    # male and female same for now
    move_pars <- sim$pars$classes$male$constant$fixed$move_pars %>%
      mutate(
        behavior_num = as.numeric(factor(behavior, levels = behavior_levels)),
        behavior_next_num = as.numeric(factor(behavior_next,
          levels = behavior_levels))) %>%
      mutate(ids = paste0(behavior_num, "_", behavior_next_num))
    move_pars_ids <- move_pars$ids
    move_kernels <- as.list(setNames(rep(NA, nrow(move_pars)),
      move_pars_ids), move_pars_ids)
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
      move_kernels[[i]] <- kernel_raster
      names(move_kernels[[i]]) <- paste0(move_pars_i$behavior_behavior[1])
    }
    male <- NamedList(move_kernels)
    female <- NamedList(move_kernels)
    classes <- NamedList(male, female)
    sim$spatial[["classes"]] <- classes

toc()

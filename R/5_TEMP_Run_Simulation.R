#  i <- 1; j <- 1; k <- 1; m <- 1; n <- 1

### WORKING !!!! (with commented out codes)
RunSimulation <- function(sim = sim, 
                          runs = 1,
                          write = FALSE,
                          output_dir = getwd()) {
  sim <- sim
  runs <- CreateRunsList(runs)
  for (i in 1:length(runs)) {
    rep_intervals <- CreateReportIntervals(sim) 
    sim <- UpdateAgentStates(sim=sim, init=TRUE)
    sim <- UpdateAgentStepData(sim=sim, init=TRUE)
    sim <- UpdateAgentParsData(sim, init=TRUE)
    sim <- UpdateSpatial(sim=sim, init=TRUE)
    for (j in 1:length(rep_intervals)) {
      step_intervals <- CreateStepIntervals(rep_intervals[[j]])
      for (k in 1:length(step_intervals)) {
        time_steps <- CreateTimeSteps(step_intervals[[k]]) 
        for (m in 1:length(time_steps)) {
          step <- time_steps[[m]]
          alive_seq <- ReturnAliveSeq(sim)
          sim <- UpdateAgentParsData(sim)
          for (n in alive_seq) {
             agent_states <- sim$agents$all[[n]][["states"]] 
             step_data <- sim$agents$all[[n]][["step_data"]]           
             pars_data <- sim$agents$all[[n]][["pars_data"]]
             # START Submodels #
             agent_states <- AgingSubModel(agent_states, step_data, step)
             step_data <- MovementSubModel(sim, agent_states, step_data, step)
             agent_states <- SurvivalSubModel(agent_states, step_data)
             agent_states <- ReproductionSubModel(agent_states, step_data) 
     	       # END Submodels #
             sim$agents$all[[n]][["states"]] <- UpdateAgentStates(agent_states)
             sim$agents$all[[n]][["step_data"]] <- UpdateAgentStepData(step_data)
           } # end of alive_seq[[n]]
           print(paste("end of time_step:", time_steps[[m]]))
           sim <- UpdateSpatial(sim=sim)
         } # end of time_steps[[m]]
         print(paste("end of step_interval:", step_intervals[[k]])) 
       } # end of step_interval[[k]]
       sim$agents <- UpdateAgentsReport(sim=sim, rep_interval=rep_intervals[[j]],
         step_intervals = step_intervals)
       sim$agents <- UpdatePopReport(sim=sim, step_intervals=step_intervals, 
         rep_interval= rep_intervals[[j]])
       print(paste("end of rep_interval:", rep_intervals[[j]])) 
    } # end of rep_interval[[j]]
    print("LAST REP")
    runs[[i]] <- sim
      run = names(runs[i])
      file_path = file.path(output_dir, paste0("sim_", run, ".RData"))
      print(paste0("Writing: ", file_path))
      save(sim, file = file_path)       
#    WriteSimList(write, run = names(runs[i]), sim = sim, components = "agents")
  } # end of runs[[i]]
  return(runs)
}

# MovementSubModel <- function(agent_states = agent_states,
#                              step_data = step_data,
#                              step = step) {
#   suppressPackageStartupMessages(require(circular))
#   suppressPackageStartupMessages(require(ggplot2))
#   suppressPackageStartupMessages(require(grid))
#   suppressPackageStartupMessages(require(raster))
#   suppressPackageStartupMessages(require(rasterVis))
#   suppressPackageStartupMessages(require(sampling))
#   suppressPackageStartupMessages(require(VGAM))
#   source('C:/Work/R/Functions/gen.R')
#   source('C:/Work/R/Functions/gps.R')  
#   source('C:/Work/R/Functions/pars.R')
#   source('C:/Work/R/Functions/sim/home.R') 
#   cellsize <- res(sim$spatial$base)[1]
#   step_start <- int_start(step)
#   step_end <- int_end(step)
#   sex <- agent_states$sex
#   season <- FindSeasonFromDatetime(step_start, sim$pars$global$sim_seasons)
#   nest_return <- sim$pars$classes[[sex]]$julian[yday(int_start(step)), 
#     "nest_return"]
#   step_max_r <- sim$pars$classes[[sex]]$season[[season]]$step_max_r
#   step_cauchy_mu <- sim$pars$classes[[sex]]$constant$fixed$step_cauchy_mu
#   step_cauchy_rho <- sim$pars$classes[[sex]]$constant$fixed$step_cauchy_rho
#   step_pareto_shape <-sim$pars$classes[[sex]]$season[[season]]$step_pareto_shape
#   step_pareto_scale <-sim$pars$classes[[sex]]$season[[season]]$step_pareto_scale
#   homerange_kernel <- sim$spatial$homerange_kernel[[agent_states$nest_id]]
#   if (nrow(step_data) == 1) {
#     i <- 1
#     step_data[i+1, "datetime"] <- int_end(step)
#     step_data[1, "exp_angle"] <- sample(x=seq(from=0, to=(2*pi), by=(2*pi/360)), 
#       size=1) 
#     go_nest <- FALSE
#   } else {
#     i <- nrow(step_data)
#     step_data[i+1, "datetime"] <- int_end(step)
#     step_data$exp_angle[i] <- step_data$abs_angle[i-1]
#     go_nest <- rbinom(1, 1, nest_return) 
#   }
#   if (go_nest == TRUE) {
#     nests_df <- data.frame(sim$spatial$nests)
#     nests_xy <- nests_df[which(nests_df$nest_id == agent_states$nest_id),
#       c("x", "y")]
#     nest_xy <- CenterXYInCell(nests_xy[1], nests_xy[2], xmin(homerange_kernel), 
#       ymin(homerange_kernel), cellsize)
#     step_data$x[i+1] <- nest_xy[[1]]
#     step_data$y[i+1] <- nest_xy[[2]]        
#     step_data$abs_angle[i] <- CalculateAngleToPoint(step_data$x[i], 
#       step_data$y[i], step_data$x[i+1], step_data$y[i+1])    
#     step_data$step_length[i] <- as.integer(sqrt((step_data[i, "x"] - 
#       step_data[i+1, "x"])^2 + (step_data[i, "y"]-step_data[i+1, "y"])^2))
#   } else {
#     redist <- CreateRedistKernel(max_r=step_max_r, cellsize=cellsize, 
#       mu=step_data$exp_angle[i], rho=step_cauchy_rho, shape=step_pareto_shape, 
#       scale=step_pareto_scale)
#     r <- (cellsize*((nrow(redist)-1)/2))+(cellsize/2)
#     redist_raster <- raster(redist, xmn=-r, xmx=r, ymn=-r, ymx=r)
#     redist_shift <- shift(redist_raster, x=step_data$x[i], y=step_data$y[i])
# ### PLACE TO ADD IN OTHER PROBABILITY LAYERS
#                 
#     homerange_crop <- crop(homerange_kernel, redist_shift, snap="out")
#     prob_raster <- overlay(redist_shift, homerange_crop, fun=function(x,y) 
#       {return(x*y)}, recycle=FALSE) 
#                 
# ### END OF OTHER PROBABILITY LAYERS
#     destination_cell <- suppressWarnings(strata(data=data.frame(cell=
#       1:ncell(prob_raster)), stratanames=NULL, size=1, method="systematic", 
#       pik=prob_raster@data@values))
#     destination_xy <- xyFromCell(prob_raster, destination_cell[1,1])
#     step_data[i+1, "x"] <- destination_xy[1]
#     step_data[i+1, "y"] <- destination_xy[2]
#     step_data$abs_angle[i] <- CalculateAngleToPoint(step_data$x[i], 
#       step_data$y[i], step_data$x[i+1], step_data$y[i+1])
#     step_data$step_length[i] <- as.integer(sqrt((step_data[i, "x"] - 
#       step_data[i+1, "x"])^2 + (step_data[i, "y"]-step_data[i+1, "y"])^2))
#   } 
#   step_data[i+1, "id"] <- step_data[i, "id"]
#   return(step_data)
# }
# 

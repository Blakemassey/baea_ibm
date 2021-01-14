i <- 127
model_i <- tbl_models$model[i]

covars_scale <- GetCovarsScale(model_i)
covars_fixed <- GetCovarsFixed(model_i)
sigma_domains <- GetSigmaDomains(covars_scale)
sigma_starts <- GetSigmaStarts(covars_scale)

  OptimizeClogitSigma <- function(covars_scale, covars_fixed, sigma_domains,
    sigma_starts, pop_size, model, mod_num){
    ua_data <- ua_steps_i
    sigma_n <- length(covars_scale)
    domains <- sigma_domains # domains are min & max for each covariate
    domains_num <- mapply(as.numeric, domains) # Fixes a FRUSTRATING bug!
    dim(domains_num) <- dim(domains) # rgenoud NOW REQUIRES DOMAINS BE NUMERIC!
    starting_values <- sigma_starts # starting values for covariates
    parms <- starting_values
    if (length(covars_scale) == 0){ # not optimizing sigmas
      covars_fixed_0 <- paste0(covars_fixed, "0")
      preds <- paste(c(covars_fixed_0, "strata(step_id)"), collapse = " + ")
      clogit_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
      opt_fit <- tryCatch({
        suppressMessages(opt_fit <- clogit(clogit_model_formula,
          data = ua_steps_i, method = "efron", iter.max = iter_max))
      }, error = function(cond) {
        # message(cond)
        opt_fit <- NA
        return(opt_fit)
      }, warning = function(cond) {
        # message(cond)
        opt_fit <- NA
        return(opt_fit)
      },
        finally={})
    } else { # optimizing sigmas
      FitClogitSigma <- function(sigmas, ua_data){

        sigmas <- sigma_starts

        # NEW
        poly_vec <- which(covars_scale %in% poly_covars) # for polynomial covars
        mono_vec <- which(!covars_scale %in% poly_covars) # for monotopic covars

        covars_poly_sigmas <- paste0("poly(", covars_scale[poly_vec], sigmas[poly_vec], ", 2)")
        covars_mono_sigmas <- paste0(covars_scale[mono_vec], sigmas[mono_vec])
        covars_fixed_0 <- paste0(covars_fixed, "0")
        preds <- paste(c(covars_poly_sigmas, covars_mono_sigmas, covars_fixed_0,
          "strata(step_id)"), collapse = " + ")

preds1  <- "developed50 + I(developed50^2) + forest50 + I(forest50^2) +
eastness50 + northness50 + hydro_dist0 + turbine_dist0 + strata(step_id)"

preds2  <- "poly(developed50, 2, raw = TRUE) + poly(forest50, 2, raw = TRUE) +
eastness50 + northness50 + hydro_dist0 + turbine_dist0 + strata(step_id)"

preds3  <- "poly(developed50, 2, raw = FALSE) + poly(forest50, 2, raw = FALSE) +
eastness50 + northness50 + hydro_dist0 + turbine_dist0 + strata(step_id)"

clogit_model_formula1 <- as.formula(paste('case', preds1, sep = " ~ "))
clogit_fit1 <- clogit(clogit_model_formula1, data = ua_data, method = "efron",
  iter.max = iter_max)

clogit_model_formula2 <- as.formula(paste('case', preds2, sep = " ~ "))
clogit_fit2 <- clogit(clogit_model_formula2, data = ua_data, method = "efron",
  iter.max = iter_max)

clogit_model_formula3 <- as.formula(paste('case', preds3, sep = " ~ "))
clogit_fit3 <- clogit(clogit_model_formula3, data = ua_data, method = "efron",
  iter.max = iter_max)

clogit_fit1
clogit_fit2
clogit_fit3


        model_aicc = AICc(clogit_fit)
        model_aicc
        return(model_aicc)
      }




      opt_fit <- tryCatch({
        opt_fit <- genoud(fn = FitClogitSigma, nvars = sigma_n,
          pop.size = pop_size, starting.values = starting_values,
          optim.method = "SANN", max.generations = max_generations,
          hard.generation.limit = FALSE, wait.generations = wait_generations,
          solution.tolerance = 0.0001, P5 = 0, P6 = 0, P7 = 0, P8 = 0,
          BFGSburnin = burnin_generations, print.level = 0,
          boundary.enforcement = 2, ua_data = ua_data, data.type.int = TRUE,
          Domains = domains_num)
        return(opt_fit)
      }, error = function(cond) {
        message(cond)
        return(opt_fit)
      }, warning = function(cond) {
        opt_fit <- NA
        return(opt_fit)
      }, finally={})
    }
    toc()
    return(opt_fit)
  }




# Checking for issues with the step type Nest -> Perch
# Currently, all of the simulated birds seem to file to the edges/corners of the
# redistribution_kernel

base <- sim$spatial$base

# Create Move Kernels for 'Nest -> Perch' around each nest
for (i in c(1, 2)){
  print(i)
  start_x <- sim$agents$input$start_x[i]
  start_y <- sim$agents$input$start_y[i]
  move_org <- sim_out$run_1$spatial$classes$male$move_kernels$`3_4`
  plot(move_org)
  move_rotated <- suppressWarnings(RotateRaster(move_org, Rad2Deg(0),
    resolution=raster::res(base)))
  plot(move_rotated)
  move_crop <- raster::crop(move_rotated, move_org, snap = "near")
  plot(move_crop)
  move_resample <- raster::resample(move_rotated, move_org, method = "ngb")
  plot(move_resample)
  move_shift <- raster::shift(move_resample, dx = start_x,  dy = start_y)
  raster::crs(move_shift) <- raster::crs(base)
  move_kernel <- raster::crop(move_shift, base, snap="in")
  raster::writeRaster(move_kernel, paste0("C:/TEMP/Sim3/move_kernel", i,
    ".tif"), overwrite = TRUE)
  land <- sim$spatial$landscape$land[[sim$agents$input$nest_id[i]]]
  land_kernel <- raster::crop(land, move_kernel, snap = "in")
  land_kernel <- raster::extend(land_kernel, move_kernel, value = 0)
  land_kernel <- raster::mask(land_kernel, move_kernel)
  raster::writeRaster(land_kernel, paste0("C:/TEMP/Sim3/land_kernel", i,
    ".tif"), overwrite = TRUE)
}

# Create SSF Kernels for 'Nest -> Perch' around each nest
for (i in c(1, 3, 5, 7)){
  print(i)
  start_x <- sim$agents$input$start_x[i]
  start_y <- sim$agents$input$start_y[i]
  move_org <- sim_out$
  plot(move_org)
  move_rotated <- suppressWarnings(RotateRaster(move_org, Rad2Deg(0),
    resolution=raster::res(base)))
  plot(move_rotated)
  move_crop <- raster::crop(move_rotated, move_org, snap = "near")
  plot(move_crop)
  move_resample <- raster::resample(move_rotated, move_org, method = "ngb")
  plot(move_resample)
  move_shift <- raster::shift(move_resample, dx = start_x,  dy = start_y)
  raster::crs(move_shift) <- raster::crs(base)
  move_kernel <- raster::crop(move_shift, base, snap="in")
  raster::writeRaster(move_kernel, paste0("C:/TEMP/Sim2/ssf_kernel", i, ".tif"))
}

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

#pop_size = 20000
max_generations = 50
wait_generations = 10
burnin_generations = 10
iter_max = 100

GetDateTime <- function(){
  date_time <- paste0(date(now()), "_",
    str_pad(hour(now()), 2, "left", "0"),
    str_pad(minute(now()), 2, "left", "0"))
  return(date_time)
}

paste2 <- function(x, y){
  xy <- c(x[!is.na(x)], y[!is.na(y)])
  xy <- paste(xy[!is.na(xy)], collapse = " + ")
  out <- if_else(str_length(xy) > 0, xy, NA_character_)
  return(out)
}

paste3 <- function(x, y){
  xy <- paste0(x[!is.na(x)], y[!is.na(y)], collapse = " + ")
  out <- if_else(str_length(xy) > 0, xy, NA_character_)
  return(out)
}

# OptimizeClogitSigma_2_13 <- function(ua_data){
#   sigma_n <- 2
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_3_0, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeClogitSigma_3_0 <- function(ua_data){
#   sigma_n <- 3
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_3_0, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeClogitSigma_3_3 <- function(ua_data){
#   sigma_n <- 3
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_3_3, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeClogitSigma_4_3 <- function(ua_data){
#   sigma_n <- 4
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_4_3, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeClogitSigma_5_3 <- function(ua_data){
#   sigma_n <- 5
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_5_3, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeClogitSigma_5_0 <- function(ua_data){
#   sigma_n <- 5
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_5_, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeClogitSigma_6_3 <- function(ua_data){
#   sigma_n <- 6
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_6_3, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeClogitSigma_8_3 <- function(ua_data){
#   sigma_n <- 8
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_8_3, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeClogitSigma_7_3 <- function(ua_data){
#   sigma_n <- 7
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_7_3, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeClogitSigma_6_3 <- function(ua_data){
#   sigma_n <- 6
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_6_3, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeClogitSigma_9_3 <- function(ua_data){
#   sigma_n <- 9
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_9_3, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeClogitSigma_10_3 <- function(ua_data){
#   sigma_n <- 10
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = FitClogitSigma_10_3, nvars = sigma_n, pop.size = pop_size,
#     starting.values = starting_values, optim.method = "SANN",
#     max.generations = max_generations, hard.generation.limit = FALSE,
#     wait.generations = wait_generations, solution.tolerance = 0.0001,
#     P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# FitClogitSigma_1_0 <- function(sigmas, ua_data){
#   df <- data.frame(case = ua_data$case,
#     step_id = ua_data$step_id,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])])
#   clogit_fit <- clogit(case ~ value1 + strata(step_id),
#     data = df, method = "efron", iter.max = iter_max)
#   model_aic = AIC(clogit_fit)
#   return(model_aic)
# }
#
# FitClogitSigma_3_0 <- function(sigmas, ua_data){
#   df <- data.frame(case = ua_data$case,
#     step_id = ua_data$step_id,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
#     value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
#     value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])])
#   clogit_fit <- clogit(case ~ value1 + value2 + value3 +
#       strata(step_id),
#     data = df, method = "efron", iter.max = iter_max)
#   model_aic = AIC(clogit_fit)
#   return(model_aic)
# }
#
# FitClogitSigma_3_3 <- function(sigmas, ua_data){
#   df <- data.frame(case = ua_data$case,
#     step_id = ua_data$step_id,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
#     value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
#     value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])],
#     value13 = ua_data[, "developed_dist0"],
#     value14 = ua_data[, "hydro_dist0"],
#     value15 = ua_data[, "turbine_dist0"])
#   clogit_fit <- clogit(case ~ value1 + value2 + value3 +
#       value13 + value14 + value15 + strata(step_id),
#     data = df, method = "efron", iter.max = iter_max)
#   model_aic = AIC(clogit_fit)
#   return(model_aic)
# }
#
# FitClogitSigma_4_3 <- function(sigmas, ua_data){
#   df <- data.frame(case = ua_data$case,
#     step_id = ua_data$step_id,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
#     value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
#     value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])],
#     value4 = ua_data[, paste0(sigma_variables[4], sigmas[4])],
#     value13 = ua_data[, "developed_dist0"],
#     value14 = ua_data[, "hydro_dist0"],
#     value15 = ua_data[, "turbine_dist0"])
#   clogit_fit <- clogit(case ~ value1 + value2 + value3 + value4 +
#       value13 + value14 + value15 + strata(step_id),
#     data = df, method = "efron", iter.max = iter_max)
#   model_aic = AIC(clogit_fit)
#   return(model_aic)
# }
#
# FitClogitSigma_5_3 <- function(sigmas, ua_data){
#   df <- data.frame(case = ua_data$case,
#     step_id = ua_data$step_id,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
#     value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
#     value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])],
#     value4 = ua_data[, paste0(sigma_variables[4], sigmas[4])],
#     value5 = ua_data[, paste0(sigma_variables[5], sigmas[5])],
#     value13 = ua_data[, "developed_dist0"],
#     value14 = ua_data[, "hydro_dist0"],
#     value15 = ua_data[, "turbine_dist0"])
#   clogit_fit <- clogit(case ~ value1 + value2 + value3 + value4 + value5 +
#       value13 + value14 + value15 + strata(step_id),
#     data = df, method = "efron", iter.max = iter_max)
#   model_aic = AIC(clogit_fit)
#   return(model_aic)
# }
#
# FitClogitSigma_6_3 <- function(sigmas, ua_data){
#   df <- data.frame(case = ua_data$case,
#     step_id = ua_data$step_id,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
#     value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
#     value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])],
#     value4 = ua_data[, paste0(sigma_variables[4], sigmas[4])],
#     value5 = ua_data[, paste0(sigma_variables[5], sigmas[5])],
#     value6 = ua_data[, paste0(sigma_variables[6], sigmas[6])],
#     value13 = ua_data[, "developed_dist0"],
#     value14 = ua_data[, "hydro_dist0"],
#     value15 = ua_data[, "turbine_dist0"])
#   clogit_fit <- clogit(case ~ value1 + value2 + value3 + value4 + value5 +
#       value6 + value13 + value14 + value15 + strata(step_id),
#     data = df, method = "efron", iter.max = iter_max)
#   model_aic = AIC(clogit_fit)
#   return(model_aic)
# }
#
# FitClogitSigma_7_3 <- function(sigmas, ua_data){
#   df <- data.frame(case = ua_data$case,
#     step_id = ua_data$step_id,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
#     value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
#     value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])],
#     value4 = ua_data[, paste0(sigma_variables[4], sigmas[4])],
#     value5 = ua_data[, paste0(sigma_variables[5], sigmas[5])],
#     value6 = ua_data[, paste0(sigma_variables[6], sigmas[6])],
#     value7 = ua_data[, paste0(sigma_variables[7], sigmas[7])],
#     value13 = ua_data[, "developed_dist0"],
#     value14 = ua_data[, "hydro_dist0"],
#     value15 = ua_data[, "turbine_dist0"])
#   clogit_fit <- clogit(case ~ value1 + value2 + value3 + value4 + value5 +
#       value6 + value7 + value13 + value14 + value15 + strata(step_id),
#     data = df, method = "efron", iter.max = iter_max)
#   model_aic = AIC(clogit_fit)
#   return(model_aic)
# }
#
# FitClogitSigma_8_3 <- function(sigmas, ua_data){
#   df <- data.frame(case = ua_data$case,
#     step_id = ua_data$step_id,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
#     value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
#     value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])],
#     value4 = ua_data[, paste0(sigma_variables[4], sigmas[4])],
#     value5 = ua_data[, paste0(sigma_variables[5], sigmas[5])],
#     value6 = ua_data[, paste0(sigma_variables[6], sigmas[6])],
#     value7 = ua_data[, paste0(sigma_variables[7], sigmas[7])],
#     value8 = ua_data[, paste0(sigma_variables[8], sigmas[8])],
#     value13 = ua_data[, "developed_dist0"],
#     value14 = ua_data[, "hydro_dist0"],
#     value15 = ua_data[, "turbine_dist0"])
#   clogit_fit <- clogit(case ~ value1 + value2 + value3 + value4 + value5 +
#       value6 + value7 + value8 + value13 + value14 + value15 +
#       strata(step_id), data = df, method = "efron", iter.max = iter_max)
#   model_aic = AIC(clogit_fit)
#   return(model_aic)
# }
#
# FitClogitSigma_9_3 <- function(sigmas, ua_data){
#   df <- data.frame(case = ua_data$case,
#     step_id = ua_data$step_id,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
#     value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
#     value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])],
#     value4 = ua_data[, paste0(sigma_variables[4], sigmas[4])],
#     value5 = ua_data[, paste0(sigma_variables[5], sigmas[5])],
#     value6 = ua_data[, paste0(sigma_variables[6], sigmas[6])],
#     value7 = ua_data[, paste0(sigma_variables[7], sigmas[7])],
#     value8 = ua_data[, paste0(sigma_variables[8], sigmas[8])],
#     value9 = ua_data[, paste0(sigma_variables[9], sigmas[9])],
#     value13 = ua_data[, "developed_dist0"],
#     value14 = ua_data[, "hydro_dist0"],
#     value15 = ua_data[, "turbine_dist0"])
#   clogit_fit <- clogit(case ~ value1 + value2 + value3 + value4 + value5 +
#       value6 + value7 + value8 + value9 + value13 + value14 + value15 +
#       strata(step_id), data = df, method = "efron", iter.max = iter_max)
#   model_aic = AIC(clogit_fit)
#   return(model_aic)
# }
#
# FitClogitSigma_10_3 <- function(sigmas, ua_data){
#   df <- data.frame(case = ua_data$case,
#     step_id = ua_data$step_id,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
#     value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
#     value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])],
#     value4 = ua_data[, paste0(sigma_variables[4], sigmas[4])],
#     value5 = ua_data[, paste0(sigma_variables[5], sigmas[5])],
#     value6 = ua_data[, paste0(sigma_variables[6], sigmas[6])],
#     value7 = ua_data[, paste0(sigma_variables[7], sigmas[7])],
#     value8 = ua_data[, paste0(sigma_variables[8], sigmas[8])],
#     value9 = ua_data[, paste0(sigma_variables[9], sigmas[9])],
#     value10 = ua_data[, paste0(sigma_variables[10], sigmas[10])],
#     value13 = ua_data[, "developed_dist0"],
#     value14 = ua_data[, "hydro_dist0"],
#     value15 = ua_data[, "turbine_dist0"])
#   clogit_fit <- clogit(case ~ value1 + value2 + value3 + value4 + value5 +
#       value6 + value7 + value8 + value9 + value10 + value13 + value14 + value15 +
#       strata(step_id), data = df, method = "efron", iter.max = iter_max)
#   model_aic = AIC(clogit_fit)
#   return(model_aic)
# }
#
# ### ------------------------------------------------------------------------ ###
# ############################### OLD CODE #######################################
# ### ------------------------------------------------------------------------ ###
#
# FitLogisiticSigma_12_3 <- function(ua_data){
#   sigma_n <- 12
#   domains <- sigma_matrix[1:sigma_n,] # domains are min & max for each covariate
#   starting_values <- sigma_starts[1:sigma_n] # starting values for covariates
#   parms <- starting_values
#   opt_fit <- genoud(fn = OptimizeLogisticSigma_12_3, nvars = sigma_n,
#     pop.size = pop_size, starting.values = starting_values,
#     optim.method = "SANN", max.generations = max_generations,
#     hard.generation.limit = FALSE, wait.generations = wait_generations,
#     solution.tolerance = 0.00001, P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 1, boundary.enforcement = 2,
#     ua_data = ua_data, data.type.int = TRUE, Domains = domains)
#   return(opt_fit)
# }
#
# OptimizeLogisticSigma_12_3 <- function(sigmas, ua_data){
#   df <- data.frame(case = ua_data$case,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
#     value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
#     value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])],
#     value4 = ua_data[, paste0(sigma_variables[4], sigmas[4])],
#     value5 = ua_data[, paste0(sigma_variables[5], sigmas[5])],
#     value6 = ua_data[, paste0(sigma_variables[6], sigmas[6])],
#     value7 = ua_data[, paste0(sigma_variables[7], sigmas[7])],
#     value8 = ua_data[, paste0(sigma_variables[8], sigmas[8])],
#     value9 = ua_data[, paste0(sigma_variables[9], sigmas[9])],
#     value10 = ua_data[, paste0(sigma_variables[10], sigmas[10])],
#     value11 = ua_data[, paste0(sigma_variables[11], sigmas[11])],
#     value12 = ua_data[, paste0(sigma_variables[12], sigmas[12])],
#     value13 = ua_data[, "developed_dist0"],
#     value14 = ua_data[, "hydro_dist0"],
#     value15 = ua_data[, "turbine_dist0"])
#   model_logistic <- glm(case ~ value1 + value2 + value3 + value4 + value5 +
#       value6 + value7 + value8 + value9 + value10 + value11 + value12 +
#       value13 + value14 + value15,
#     family = binomial(link = "logit"), data = df)
#   model_aic = AIC(model_logistic)
#   return(model_aic)
# }


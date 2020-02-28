###################### ModelFit_SSF_Optimize ###################################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, plyr, dplyr, future, furrr, optimx, ggplot2,
  ggthemes, glmulti, lubridate, optimx, purrr, reproducible, rgenoud, stringr,
  survival, surveybootstrap, tibble, tictoc, tidyr) #spatialfil
pacman::p_load(baear, gisr, ibmr)
options(stringsAsFactors=FALSE)

#setwd("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm")
source("R/4c_ModelFit_SSF_Optimize_Assets.R")

# Output Directory
mod_fit_dir = "Output/Analysis/SSF/Models"

# Source Data Directory
ua_data_dir <- "Output/Analysis/SSF/UA_Data"

################ OPTIMIZATION PROCEDURE OF KERNEL BANDWIDTH ####################

# Generate clusters (for parallel processing)
plan(multiprocess, workers = 4)

# Pull files
ua_files <- dir(ua_data_dir)

# Sequence through step_type used/available files and fit sigma optimization
for (i in seq_along(ua_files)[c(2:4)]){
  ua_file_i <- ua_files[i]
  step_type <- str_remove_all(ua_file_i, ("ua_steps_|.rds"))
  if(!dir.exists(file.path(mod_fit_dir, step_type))){
    dir.create(file.path(mod_fit_dir, step_type))
  }
  step_type <- str_remove_all(ua_file_i, ("ua_steps_|.rds"))
  print(paste0("Starting: ", step_type, " (", i , " of ", length(ua_files),")"))
  step_type_split <- str_split(step_type, "_") %>% unlist(.)
  start <- step_type_split[1]
  end <- step_type_split[2]
  rm(step_type_split)

  if (end %in% c("cruise", "flight")){
    keep_covars <- c(
      #"developed",
      #"forest",
      #"open_water",
      #"pasture",
      #"shrub_herb",
      "eastness",
      "northness",
      "wind_class",
      "tpi",
      "tri",
      "roughness",
      "developed_dist", "hydro_dist", "turbine_dist"
      )
  }
  if (end %in% c("perch", "roost")){
    keep_covars <- c(
      "developed",
      "forest",
      "open_water",
      "pasture",
      "shrub_herb",
      #"eastness",
      #"northness",
      #"wind_class",
      #"tpi",
      #"tri",
      #"roughness",
      "developed_dist", "hydro_dist", "turbine_dist"
      )
  }
  all_fixed <- c("developed_dist", "hydro_dist", "turbine_dist")

  length(keep_covars)
#  For reference (# covars = # models):
#  6 = 63; 7 = 127; 8 = 255; 9 = 511; 10 = 1023; 11 = 2047; 12 = 4095

  ## Filter ua_data to full set of keep_covars

  ua_steps_i_org <- readRDS(file.path(ua_data_dir, ua_file_i))
  ua_steps_i <- ua_steps_i_org  %>%
    dplyr::select(c("case", "step_id"), starts_with(keep_covars)) %>%
    dplyr::select_if(function(x) !(all(is.na(x)) | all(x == ""))) %>%
    mutate(dummy = 1) %>%
    dplyr::select(dummy, everything())

  rm(start, end, keep_covars)

  colnames_alpha <- str_replace_all(colnames(ua_steps_i), "[0-9]", "")
  colnames_num <- as.numeric(str_replace_all(colnames(ua_steps_i), "[^0-9]", ""))
  covars <- unique(colnames_alpha %>% .[!. %in% c("dummy", "case", "step_id")]) # vector of covariates
  covars_scale <- covars[!covars %in% all_fixed]
  covars_fixed <- covars[covars %in% all_fixed]

  colnames_tbl <- tibble(colnames_alpha, colnames_num) %>%
    mutate(colnames_sigma = ifelse(!is.na(colnames_num), colnames_num/30, NA)) %>%
    mutate_all(~str_replace_na(., "")) %>%
    mutate(colnames_final = paste0(colnames_alpha, colnames_sigma))

  colnames(ua_steps_i) <- colnames_tbl %>% pull(colnames_final)
  rm(colnames_alpha, colnames_num, colnames_tbl)

  ## Need to generate: domains, sigma_starts, sigma_n

  covar_domains <- tibble(covar = character(), covar_min = integer(),
    covar_max = integer(), covar_median = integer())

  for (k in 1:length(covars)){
    covar <- covars[k]
    covar_k <- str_subset(colnames(ua_steps_i), covar)
    covar_k_bandwidths <- as.numeric(str_replace_all(covar_k, "[^0-9]", ""))
    covar_domains[k, "covar"] <- covar
    covar_domains[k, "covar_min"] <- min(covar_k_bandwidths)
    covar_domains[k, "covar_max"] <- max(covar_k_bandwidths)
    covar_domains[k, "covar_median"] <- quantile(covar_k_bandwidths, p = 0.5,
      type = 1)
  }
  rm(covar, covar_k, covar_k_bandwidths, k)

  ## Create list of models and convert to vector
  list_of_models <- lapply(seq_along((covars)), function(n) {
      left_hand_side <- "case"
      right_hand_side <- apply(X = combn(covars, n), MARGIN = 2, paste,
        collapse = " + ")
      paste(left_hand_side, right_hand_side, sep = " ~ ")
  })
  model <- flatten(list_of_models)

  GetCovarsScale <- function(model){
    model_terms <- str_extract_all(model, boundary("word"))
    model_covars_scale <- model_terms %>% unlist(.) %>% .[. %in% covars_scale]
    return(model_covars_scale)
  }
  GetCovarsFixed <- function(model){
    model_terms <- str_extract_all(model, boundary("word"))
    model_covars_scale <- model_terms %>% unlist(.) %>% .[. %in% covars_fixed]
    return(model_covars_scale)
  }
  GetSigmaDomains <- function(covars_scale){
    sigma_domains <- covar_domains %>% filter(covar %in% covars_scale) %>%
      dplyr::select(covar_min, covar_max) %>% as.matrix(.)
    return(sigma_domains)
  }
  GetSigmaStarts <- function(covars_scale){
    sigma_starts <- covar_domains %>% filter(covar %in% covars_scale) %>%
      pull(covar_median)
    return(sigma_starts)
  }
  GetPopSize <- function(covars_scale){
    if(length(covars_scale) <= 1) pop_size = 100
    if(length(covars_scale) == 2) pop_size = 1000
    if(length(covars_scale) == 3) pop_size = 5000
    if(length(covars_scale) >= 4) pop_size = 20000
    return(pop_size)
  }

  OptimizeClogitSigma <- function(covars_scale, covars_fixed, sigma_domains,
    sigma_starts, pop_size, model, mod_num){
    ua_data <- ua_steps_i
    sigma_n <- length(covars_scale)
    domains <- sigma_domains # domains are min & max for each covariate
    starting_values <- sigma_starts # starting values for covariates
    parms <- starting_values
    tic(paste0("Fit model ", mod_num, " (", model, ") "))
    tryCatch({
      if (length(covars_scale) == 0){ # not optimizing sigmas
        covars_fixed_0 <- paste0(covars_fixed, "0")
        preds <- paste(c(covars_fixed_0, "strata(step_id)"), collapse = " + ")
        clogit_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
        opt_fit <- clogit(clogit_model_formula, data = ua_steps_i,
          method = "efron", iter.max = iter_max)
        return(opt_fit)
      } else { # optimizing sigmas
        FitClogitSigma <- function(sigmas, ua_data){
          covars_scale_sigmas <- paste0(covars_scale, sigmas)
          covars_fixed_0 <- paste0(covars_fixed, "0")
          preds <- paste(c(covars_scale_sigmas, covars_fixed_0,
            "strata(step_id)"), collapse = " + ")
          clogit_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
          clogit_fit <- clogit(clogit_model_formula, data = ua_data,
            method = "efron", iter.max = iter_max)
          model_aic = AIC(clogit_fit)
          return(model_aic)
        }
        opt_fit <- genoud(fn = FitClogitSigma, nvars = sigma_n,
          pop.size = pop_size, starting.values = starting_values,
          optim.method = "SANN", max.generations = max_generations,
          hard.generation.limit = FALSE, wait.generations = wait_generations,
          solution.tolerance = 0.0001, P5 = 0, P6 = 0, P7 = 0, P8 = 0,
          BFGSburnin = burnin_generations, print.level = 0,
          boundary.enforcement = 2, ua_data = ua_data, data.type.int = TRUE,
          Domains = domains)
        return(opt_fit)
      }},
      error = function(cond) {message(cond); return(NA)},
      warning = function(cond) {message(cond); return(NULL)},
      finally = {
        #print(paste0("Finished ", now()))
        toc()
      }
    )
  }

  tbl_models <- tibble(step_type = step_type, model = model) %>%
    mutate(mod_num = str_pad(1:n(), 4, "left", "0")) %>%
    mutate(covars_scale = map(model, GetCovarsScale)) %>%
    mutate(covars_fixed = map(model, GetCovarsFixed)) %>%
    mutate(sigma_domains = map(covars_scale, GetSigmaDomains)) %>%
    mutate(sigma_starts = map(covars_scale, GetSigmaStarts)) %>%
    mutate(pop_size = map(covars_scale, GetPopSize))

  ## OPTIMIZATION PROCEDURE
  print(paste0("Starting ", step_type, " : ", i, now()))
  tbl_models_fitted <- tbl_models %>%
    slice(1:20) %>%
    mutate(opt_fit = future_pmap(.l = list(covars_scale, covars_fixed,
      sigma_domains, sigma_starts, pop_size, model, mod_num),
      .f = OptimizeClogitSigma, .progress = TRUE))
  print(paste0("Finished ", step_type, " : ", i, now()))

  # Save output
  saveRDS(tbl_models_fitted, file.path(mod_fit_dir, step_type, paste0("models_fit_",
    step_type, "_", GetDateTime(), ".rds")))
}

future:::ClusterRegistry("stop")




# -------------------------- START Kathy's Original Script ---------------------
# Annual Grassland
#glm approach
fit1<-glm(status~-1+diff100,data=cats.diff,family='binomial')
summary(fit1)

#survival clogit approach -- equivalent
fit2<-clogit(status~habitat100+strata(Number_),data=cats)
summary(fit2)

#survival coxph approach -- equivalent
fit3<-coxph(Surv(dummy,Y)~habitat100+strata(Number_),data=cats)
summary(fit3)

#AIC equivalent
AIC(fit1,fit2,fit3)
# --------------------------- END Kathy's Original Script ----------------------


### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###
# TESTOptimizeClogitSigma <- function(covars_scale, covars_fixed, sigma_domains,
#   sigma_starts, pop_size){
#   ua_data <- ua_steps_i
#   sigma_n <- length(covars_scale)
#   domains <- sigma_domains # domains are min & max for each covariate
#   starting_values <- sigma_starts # starting values for covariates
#   parms <- starting_values
#   FitClogitSigma <- function(sigmas, ua_data){
#     covars_scale_sigmas <- paste0(covars_scale, sigmas)
#     covars_fixed_0 <- paste0(covars_fixed, "0")
#     preds <- paste(c(covars_scale_sigmas, covars_fixed_0,
#       "strata(step_id)"), collapse = " + ")
#     clogit_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
#     clogit_fit <- clogit(clogit_model_formula, data = ua_data,
#       method = "efron", iter.max = iter_max)
#     model_aic = AIC(clogit_fit)
#     return(model_aic)
#   }
#   opt_fit <- genoud(fn = FitClogitSigma, nvars = sigma_n,
#     pop.size = pop_size, starting.values = starting_values,
#     optim.method = "SANN", max.generations = max_generations,
#     hard.generation.limit = FALSE, wait.generations = wait_generations,
#     solution.tolerance = 0.0001, P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#     BFGSburnin = burnin_generations, print.level = 0,
#     boundary.enforcement = 2, ua_data = ua_data, data.type.int = TRUE,
#     Domains = domains)
#   return(opt_fit)
# }
# df <- data.frame(case = ua_data$case,
#     value1 = ua_data[, paste0(sigma_variables[1], sigmas[1])],
#     value2 = ua_data[, paste0(sigma_variables[2], sigmas[2])],
#     value3 = ua_data[, paste0(sigma_variables[3], sigmas[3])],
#     value4 = ua_data[, paste0(sigma_variables[4], sigmas[4])],
#     value5 = ua_data[, paste0(sigma_variables[5], sigmas[5])],
#     value13 = ua_data[, "developed_dist0"],
#     value14 = ua_data[, "hydro_dist0"],
#     value15 = ua_data[, "turbine_dist0"])
# model_logistic <- glm(case ~ value1 + value2 + value3 + value4 + value5 +
#     value6 + value7 + value13 + value14 + value15,
#   family = binomial(link = "logit"), data = df)
#
# step(model_logistic)
# model_aic = AIC(model_logistic)
# AIC(model_logistic)
#
# # Check for multicollinearity
# # VIF values > 5-10 indicates a problematic collinearity (James et al. 2014).
# vif(model_logistic)
#
# fmla_test <- as.formula(paste("Surv(dummy, case) ~ ",
#   paste(variable_sigmas, collapse = "+"), "+ strata(step_id)"))
# fmla_test
#
# fit1 <- coxph(fmla_test,
#   data = ua_steps_i_sigmas, method = "approximate")
# summary(fit1)
#
# fit2 <- coxph_redefined(fmla_test, data = ua_steps_i_sigmas)
# summary(fit2)
#

# warnings()
#
#   for (j in seq_along(vector_of_models)){
#     # determine sigma_variables
#     model_j <- vector_of_models[j]
#     mod_num <- str_pad(j, 4, "left", "0")
#     model_terms <- str_extract_all(model_j, boundary("word")) %>% unlist(.)
#     covars_scale_j <- model_terms %>% .[. %in% covars_scale]
#     covars_fixed_j <- model_terms %>% .[. %in% covars_fixed]
#
#     # Find the proper SIGMA MATRIX, SIGMA STARTS, DOMAINS
#     sigma_domains <- covar_domains %>% filter(covar %in% covars_scale_j) %>%
#       dplyr::select(covar_min, covar_max) %>% as.matrix(.)
#     sigma_starts <- covar_domains %>% filter(covar %in% covars_scale_j) %>%
#       pull(covar_median)
#     if(exists("pop_size")) rm(pop_size)
#     if(length(covars_scale_j) <= 1) pop_size = 100
#     if(length(covars_scale_j) == 2) pop_size = 1000
#     if(length(covars_scale_j) == 3) pop_size = 5000
#     if(length(covars_scale_j) >= 4) pop_size = 20000
#
#     OptimizeClogitSigma <- function(ua_data){
#       sigma_n <- length(covars_scale_j)
#       domains <- sigma_domains # domains are min & max for each covariate
#       starting_values <- sigma_starts # starting values for covariates
#       parms <- starting_values
#       opt_fit <- genoud(fn = FitClogitSigma, nvars = sigma_n,
#         pop.size = pop_size, starting.values = starting_values,
#         optim.method = "SANN", max.generations = max_generations,
#         hard.generation.limit = FALSE, wait.generations = wait_generations,
#         solution.tolerance = 0.0001, P5 = 0, P6 = 0, P7 = 0, P8 = 0,
#         BFGSburnin = burnin_generations, print.level = 0,
#         boundary.enforcement = 2, ua_data = ua_data, data.type.int = TRUE,
#         Domains = domains)
#       return(opt_fit)
#     }
#
#
#     FitClogitSigma <- function(sigmas, ua_data){
#       covars_scale_j_sigmas <- paste0(covars_scale_j, sigmas)
#       covars_fixed_j_0 <- paste0(covars_fixed_j, "0")
#       preds <- paste(c(covars_scale_j_sigmas, covars_fixed_j_0,
#         "strata(step_id)"), collapse = " + ")
#       clogit_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
#       clogit_fit <- clogit(clogit_model_formula, data = ua_data,
#         method = "efron", iter.max = iter_max)
#       model_aic = AIC(clogit_fit)
#       return(model_aic)
#     }
#
#     # Fit Sigma Combo Models ---------------------------------------------------
#     tic(paste0("Fit model ", mod_num))
#     tryCatch({
#       print(paste0("Starting ", step_type, " : ", i, " - ", mod_num, " (",
#         model_j, ") ", now()))
#       if (length(covars_scale_j) == 0){ # not optimizing sigmas
#         covars_fixed_j_0 <- paste0(covars_fixed_j, "0")
#         preds <- paste(c(covars_fixed_j_0, "strata(step_id)"), collapse = " + ")
#         clogit_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
#         opt_fit_j <- clogit(clogit_model_formula, data = ua_steps_i,
#           method = "efron", iter.max = iter_max)
#         df_fit_sum_j <- tibble(step_type = step_type) %>%
#           mutate(reps = 1) %>%
#           mutate(rep_n = 1) %>%
#           mutate(fit_aic = aic(opt_fit_j)) %>%
#           mutate(covars_scale = list(NA_character_)) %>%
#           mutate(covars_fixed = list(covars_fixed_j)) %>%
#           mutate(mod_formula = model_j) %>%
#           mutate(opt_fit = list(opt_fit_j))
#         saveRDS(df_fit_sum_j, file.path(mod_fit_dir, step_type,
#           paste0("df_fit_sum_", step_type, "_mod_", mod_num, "_",
#             GetDateTime(), ".rds")))
#         rm(covars_fixed_j_0, preds, clogit_model_formula, opt_fit_j,
#           df_fit_sum_j)
#       } else { # optimizing sigmas
#         reps = 1 #8     # number of optimizations run (done in parallel)
#         ua_steps_j_lst <- tibble(
#           step_type = step_type,
#           model_covars = list(unique(c(covars_scale_j, covars_fixed_j))),
#           ua_data = list(ua_steps_i),
#           count = reps) %>%
#           uncount(count)
#         tbl_opt_fit_j <- ua_steps_j_lst %>%
#           mutate(opt_fit = map(ua_data, OptimizeClogitSigma))
#           # mutate(opt_fit = future_map(ua_data, OptimizeClogitSigma,
#           #   .progress =T))
#         df_fit_sum_j <- tbl_opt_fit_j %>%
#           dplyr::select(step_type, opt_fit) %>%
#           mutate(reps = reps) %>%
#           mutate(rep_n = 1:n()) %>%
#           mutate(fit_aic = map_dbl(opt_fit, pluck, "value")) %>%
#           mutate(covars_scale = list(covars_scale_j)) %>%
#           mutate(covars_fixed = list(ifelse(length(covars_fixed_j) > 0,
#             covars_fixed_j, NA_character_))) %>%
#           mutate(pars = list(pluck(opt_fit, 1, "par"))) %>%
#           mutate(mod_formula = model_j) %>%
#           select(step_type, reps, rep_n, fit_aic, covars_scale, covars_fixed,
#             pars, opt_fit, mod_formula) %>%
#           mutate(generations = map_dbl(opt_fit, pluck, "generations")) %>%
#           mutate(peak_generation = map_dbl(opt_fit, pluck, "peakgeneration"))
#         saveRDS(df_fit_sum_j, file.path(mod_fit_dir, step_type,
#           paste0("df_fit_sum_", step_type,"_mod_", mod_num, "_", GetDateTime(),
#             ".rds")))
#         rm(reps, ua_steps_j_lst, tbl_opt_fit_j, df_fit_sum_j)
#       }},
#       error = function(cond) {
#         message(cond)
#         return(NA)
#       },
#       warning=function(cond) {
#         message(cond)
#         if(exists("df_fit_sum_j")){
#           saveRDS(df_fit_sum_j, file.path(mod_fit_dir, step_type,
#             paste0("df_fit_sum_", step_type, "_mod_", mod_num, "_warn_",
#               GetDateTime(), ".rds")))
#         }
#         return(NULL)
#       },
#       finally={
#         if(exists("model_j")) rm(model_j)
#         if(exists("mod_num")) rm(mod_num)
#         if(exists("covars_scale_j")) rm(covars_scale_j)
#         if(exists("covars_fixed_j")) rm(covars_fixed_j)
#         if(exists("sigma_domains")) rm(sigma_domains)
#         if(exists("sigma_starts")) rm(sigma_starts)
#         if(exists("df_fit_sum_j")) rm(df_fit_sum_j)
#     })
#     toc()
#   }
#   print(paste0("Finished: ", step_type, " (", i , " of ", length(ua_files), ")",
#     " at: ", now()))
#   if(exists("step_type")) rm(step_type)
#   if(exists("covars")) rm(covars)
#   if(exists("covars_fixed")) rm(covars_fixed)
#   if(exists("covars_scale")) rm(covars_scale)
#   if(exists("vector_of_models")) rm(vector_of_models)
# }

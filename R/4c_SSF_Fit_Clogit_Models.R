###################### ModelFit_SSF_Optimize ###################################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, plyr, dplyr, future, furrr, optimx, ggplot2,
  lubridate, optimx, purrr, rgenoud, stringr, survival, tibble, tictoc,
  tidyr)
pacman::p_load(baear, gisr, ibmr)
testing <- FALSE

# Directories
ua_data_dir <- "Output/Analysis/SSF/UA_Data"
mod_fit_dir <- "Output/Analysis/SSF/Models/model_fits"

################ OPTIMIZATION PROCEDURE OF KERNEL BANDWIDTH ####################
# Optimization parameters
max_generations = 25
wait_generations = 5
burnin_generations = 5
iter_max = 50

# Generate clusters (for parallel processing)
plan(multiprocess)

# Pull files
#ua_files <- dir(ua_data_dir)[c(3,6,10,11,14,15,17)] # all . -> perch, roost
#ua_files <- dir(ua_data_dir)[c(1,2,4,5,8,9,12,13,16)] # all . -> cruise, flight
ua_files <- dir(ua_data_dir)[c(3,6)]


# WARNING: the ua_perch_perch.rds had to get split into two files to meet the
# GitHub size limits - it needs to be merged together for this process to work

# Sequence through step_type used/available files and fit sigma optimization
for (i in seq_along(ua_files)){
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

  # All Available Covars -----------------
  # "(road)+[0-9]", "(developed)+[0-9]", "forest", "open_water", "pasture",
  # "shrub_herb", "eastness", "northness", "wind_class", "tpi",
  # "developed_dist", "hydro_dist", "turbine_dist", "road_dist")

  if (end %in% c("cruise", "flight")){
    keep_covars <- c(
      "(road)+[0-9]", "open_water",
      "eastness", "northness",
      "turbine_dist")
  }
  if (end %in% c("perch", "roost")){
    keep_covars <- c(
      "open_water", "(road)+[0-9]", "tri", "eastness", "wetland", "hydro_dist")
  }

  fixed_covars <- c("developed_dist", "hydro_dist", "turbine_dist",
    "road_dist")

  length(keep_covars) #  For reference (# covars = # models):
  #  6 = 63; 7 = 127; 8 = 255; 9 = 511; 10 = 1023; 11 = 2047; 12 = 4095

  ## Filter ua_data to full set of keep_covars
  ua_steps_i_org <- readRDS(file.path(ua_data_dir, ua_file_i))
  ua_steps_i_all <- ua_steps_i_org  %>%
    dplyr::select(c("case", "step_id"), matches(keep_covars)) %>%
    dplyr::select_if(function(x) !(all(is.na(x)) | all(x == ""))) %>%
    mutate(dummy = 1) %>%
    dplyr::select(dummy, everything())

  # Find rows with missing data
  ua_steps_na <- ua_steps_i_all %>%
    filter_all(any_vars(is.na(.))) %>%
    select(case, step_id)

  # Remove step_id pairs where any data is missing
  ua_steps_i <- ua_steps_i_all %>% anti_join(., ua_steps_na,
    by = c('case', 'step_id'))

  rm(start, end, keep_covars, ua_steps_i_org, ua_steps_na)

  colnames_alpha <- str_replace_all(colnames(ua_steps_i), "[0-9]", "")
  colnames_num <- as.numeric(str_replace_all(colnames(ua_steps_i), "[^0-9]",""))
  covars <- unique(colnames_alpha %>% .[!. %in% c("dummy", "case", "step_id")])
  length(covars)
  # vector of covariates
  covars_scale <- covars[!covars %in% fixed_covars]
  covars_fixed <- covars[covars %in% fixed_covars]

  colnames_tbl <- tibble(colnames_alpha, colnames_num) %>%
    mutate(colnames_sigma = ifelse(!is.na(colnames_num), colnames_num/30, NA)) %>%
    mutate_all(~str_replace_na(., "")) %>%
    mutate(colnames_final = paste0(colnames_alpha, colnames_sigma))

  colnames(ua_steps_i) <- colnames_tbl %>% pull(colnames_final)
  rm(colnames_alpha, colnames_num, colnames_tbl)

  ## Generate: domains, sigma_starts, sigma_n

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
  if(testing) model <- model[1:20]

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
    if(length(covars_scale) == 2) pop_size = 2000
    if(length(covars_scale) == 3) pop_size = 20000
    if(length(covars_scale) >= 4) pop_size = 50000
    return(pop_size)
  }
  ExtractAICc <- function(x) {
    if(!all(is.na(x))){
      if(any(class(x) %in% 'clogit')){
        aicc <- AICc(x)
      } else if (any(names(x) %in% "value")) {
        aicc <- pluck(x, "value")
      } else {
        aicc <- NA
      }
    } else {
      aicc <- NA
    }
    return(aicc)
  }
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
        covars_scale_sigmas <- paste0(covars_scale, sigmas)
        covars_fixed_0 <- paste0(covars_fixed, "0")
        preds <- paste(c(covars_scale_sigmas, covars_fixed_0,
          "strata(step_id)"), collapse = " + ")
        clogit_model_formula <- as.formula(paste('case', preds,sep = " ~ "))
        clogit_fit <- clogit(clogit_model_formula, data = ua_data,
          method = "efron", iter.max = iter_max)
        model_aicc = AICc(clogit_fit)
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

  # divide models into groups for analysis (so that if threads fail they
  # have a chance to start again)
  model_grp <- as.numeric(cut_number(1:length(model),
    n = ceiling(length(model)/100)))

  tbl_models <- tibble(step_type, model = model, model_grp = model_grp) %>%
    mutate(model_num = str_pad(1:n(), 4, "left", "0")) %>%
    mutate(covars_scale = map(model, GetCovarsScale)) %>%
    mutate(covars_fixed = map(model, GetCovarsFixed)) %>%
    mutate(sigma_domains = map(covars_scale, GetSigmaDomains)) %>%
    mutate(sigma_starts = map(covars_scale, GetSigmaStarts)) %>%
    mutate(pop_size = map(covars_scale, GetPopSize))

  ## OPTIMIZATION PROCEDURE

  print(paste0("Starting ", step_type, " : ", now()))
  tbl_models_fitted_list <- list()
  for (i in unique(model_grp)) {
    print(paste0("Model group: ", i, " of ", length(unique(model_grp))))
    tbl_models_fitted_i <- tbl_models %>%
      filter(model_grp == i)  %>%
      mutate(opt_fit = future_pmap(.l = list(covars_scale, covars_fixed,
        sigma_domains, sigma_starts, pop_size, model, model_num),
        .f = possibly(OptimizeClogitSigma, otherwise = NA_real_),
        .progress = TRUE, .options = future_options(seed = TRUE))) %>%
      mutate(fit_aicc = map_dbl(opt_fit, ExtractAICc))
    tbl_models_fitted_list[[i]] <- tbl_models_fitted_i
  }
  tbl_models_fitted <- tbl_models_fitted_list %>%
    reduce(bind_rows)
  print(paste0("Finished ", step_type, " at ", now()))

# EXTRACT PARAMETERS -----------------------------------------------------------

  PastePreds <- function(x, y){
    xy <- c(x[!is.na(x)], y[!is.na(y)])
    xy <- paste(xy[!is.na(xy)], collapse = " + ")
    out <- if_else(str_length(xy) > 0, xy, NA_character_)
    return(out)
  }

  PasteFixedSigmas <- function(x, y){
    xy <- paste0(x[!is.na(x)], y[!is.na(y)], collapse = " + ")
    out <- if_else(str_length(xy) > 0, xy, NA_character_)
    return(out)
  }

  RenameUAStepsToMeters <- function(ua_steps){
    colnames_alpha <- str_replace_all(colnames(ua_steps), "[0-9]", "")
    colnames_num <- as.numeric(str_replace_all(colnames(ua_steps),"[^0-9]", ""))
    covars <- unique(colnames_alpha %>% .[!. %in% c("dummy", "case","step_id")])
    colnames_tbl <- tibble(colnames_alpha, colnames_num) %>%
      mutate(colnames_sigma = ifelse(!is.na(colnames_num), colnames_num/30,
        NA))%>%
      mutate_all(~str_replace_na(., "")) %>%
      mutate(colnames_final = paste0(colnames_alpha, colnames_sigma))
    colnames(ua_steps) <- colnames_tbl %>% pull(colnames_final)
    return(ua_steps)
  }

  FitClogit <- function(clogit_preds, ua_data){
    clogit_model_formula = as.formula(clogit_preds)
    clogit_fit <- clogit(clogit_model_formula, data = ua_data, method = "efron",
      iter.max = iter_max)
    return(clogit_fit)
  }

  ExtractClogitCoefs <- function(clogit_fit){
    clogit_fit_i <- clogit_fit %>% pluck(coef)
    return(clogit_fit_i)
  }

  ExtractClogitFitTerms <- function(clogit_fit){
    terms_i <- clogit_fit %>% pluck(terms, attr_getter("term.labels"))
    return(terms_i)
  }

  ExtractOptFitFormula <- function(opt_fit, covars_scale, pars, covars_fixed){
    if(all(is.na(opt_fit))){
      opt_fit_formula <- NA
    } else {
      covars_scale_sigmas <- paste0(covars_scale, pars, collapse = " + ")
      covars_scale_sigmas <- ifelse(covars_scale_sigmas == "NA", NA,
        covars_scale_sigmas)
      covars_scale_sigmas <- ifelse(covars_scale_sigmas == "", NA,
        covars_scale_sigmas)
      fixed_sigma <- ifelse(!is.na(covars_fixed), 0, NA)
      covars_fixed_sigmas <- PasteFixedSigmas(covars_fixed, fixed_sigma)
      covars_fixed_sigmas <- ifelse(covars_fixed_sigmas == "NA", NA,
        covars_fixed_sigmas)
      covars_fixed_sigmas <- ifelse(covars_fixed_sigmas == "0", NA,
        covars_fixed_sigmas)
      preds <- PastePreds(covars_scale_sigmas, covars_fixed_sigmas)
      opt_fit_formula <- paste0("case ~ ", preds, " + strata(step_id)")
    }
    return(opt_fit_formula)
  }

  tbl_models_fitted_pars <- tbl_models_fitted %>% #slice(7) %>%
    mutate(model_chr = map_chr(model, as.character)) %>%
    arrange(fit_aicc) %>%
    mutate(delta_aicc = fit_aicc - first(fit_aicc)) %>%
    mutate(pars = map2(opt_fit, "par", pluck)) %>%
    mutate(peak_generation = map2(opt_fit, "peakgeneration", pluck)) %>%
    mutate(clogit_preds = pmap_chr(.l = list(opt_fit, covars_scale, pars,
      covars_fixed), .f = ExtractOptFitFormula)) %>%
    mutate(clogit_fit = list(NA)) %>%
    mutate(clogit_fit_null = list(NA)) %>%
    mutate(fit_aicc_refit = NA,
      fit_coefs = list(NA),
      fit_terms = list(NA),
      concordance_value = NA,
      concordance_var = NA,
      concordance_se = NA,
      fit_coefs_signif = NA)

  for (i in seq_len(nrow(tbl_models_fitted_pars))){
    clogit_preds_i <- tbl_models_fitted_pars %>% slice(i) %>%
      pull("clogit_preds")
    if(!is.na(clogit_preds_i)){
      clogit_model_formula <- as.formula(clogit_preds_i)
      clogit_fit <- clogit(clogit_model_formula, data = ua_steps_i,
        method = "efron", iter.max = iter_max)
      clogit_fit_null <- clogit(as.formula("case ~ 0 + strata(step_id)"),
        data = ua_steps_i, method = "efron", iter.max = iter_max)
      fit_aicc_refit <- AICc(clogit_fit)
      fit_coefs <- clogit_fit %>% pluck(coef)
      fit_terms <- clogit_fit %>% pluck(terms, attr_getter("term.labels"))
      concordance_list <- concordance(clogit_fit)
      concordance_value <- concordance_list %>% pluck("concordance")
      concordance_var <- concordance_list %>% pluck("var")
      concordance_se <- sqrt(concordance_var)
      fit_coefs_signif <- signif(fit_coefs, digits = 4)
      tbl_models_fitted_pars$clogit_fit[i] <- list(clogit_fit)
      tbl_models_fitted_pars$clogit_fit_null[i] <- list(clogit_fit_null)
      tbl_models_fitted_pars$fit_aicc_refit[i] <- fit_aicc_refit
      tbl_models_fitted_pars$fit_coefs[i] <- list(fit_coefs)
      tbl_models_fitted_pars$fit_terms[i] <- list(fit_terms)
      tbl_models_fitted_pars$concordance_value[i] <- concordance_value
      tbl_models_fitted_pars$concordance_var[i] <- concordance_var
      tbl_models_fitted_pars$concordance_se[i] <- concordance_se
      tbl_models_fitted_pars$fit_coefs_signif[i] <- list(fit_coefs_signif)
    }
  }
  tbl_models_fitted_final <- tbl_models_fitted_pars %>%
    dplyr::select(step_type, model_num, fit_aicc, delta_aicc, model_chr,
      clogit_preds, concordance_value, concordance_var, concordance_se,
      fit_terms, fit_coefs, fit_coefs_signif)

# SAVE FIlE -----------------------------------------------------------

  # save output
  saveRDS(tbl_models_fitted_final, file.path(mod_fit_dir, step_type,
    paste0("ssf_fit_", step_type, "_", GetDateTime(), ".rds")))
}

future:::ClusterRegistry("stop")

### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###

# -------------------------- START Kathy's Original Script -
# # Annual Grassland
# #glm approach
# fit1<-glm(status~-1+diff100,data=cats.diff,family='binomial')
# summary(fit1)
#
# #survival clogit approach -- equivalent
# fit2<-clogit(status~habitat100+strata(Number_),data=cats)
# summary(fit2)
#
# #survival coxph approach -- equivalent
# fit3<-coxph(Surv(dummy,Y)~habitat100+strata(Number_),data=cats)
# summary(fit3)
#
# #AIC equivalent
# AIC(fit1,fit2,fit3)
# # --------------------------- END Kathy's Original Script ---


#     # mutate(covars_scale_sigmas = map2_chr(covars_scale, pars, paste0,
#     #   collapse = " + ")) %>%
#     # mutate(covars_scale_sigmas = ifelse(covars_scale_sigmas == "NA", NA,
#     #   covars_scale_sigmas)) %>%
#     # mutate(covars_scale_sigmas = ifelse(covars_scale_sigmas == "", NA,
#     #   covars_scale_sigmas)) %>%
#     # mutate(fixed_sigma = ifelse(!is.na(covars_fixed), 0, NA)) %>%
#     # mutate(covars_fixed_sigmas = map2_chr(covars_fixed, fixed_sigma,
#     #   PasteFixedSigmas)) %>%
#     # mutate(covars_fixed_sigmas = ifelse(covars_fixed_sigmas == "NA", NA,
#     #   covars_fixed_sigmas)) %>%
#     # mutate(covars_fixed_sigmas = ifelse(covars_fixed_sigmas == "0", NA,
#     #   covars_fixed_sigmas)) %>%
#     # mutate(preds = map2_chr(.x = covars_scale_sigmas,
#        .y = covars_fixed_sigmas, .f = PastePreds))  %>%
#     # mutate(clogit_preds = paste0("case ~ ", preds, " + strata(step_id)"))%>%
#     mutate(clogit_preds_null = paste0("case ~ 0 + strata(step_id)")) #%>%
#
# #    left_join(., ua_steps_nested, by = "step_type") %>%
# #    rename(ua_steps = data) %>%
#
#     mutate(clogit_fit = map2(.x = clogit_preds, .y = ua_steps,
#       .f = FitClogit)) %>%
#     mutate(clogit_fit_null = map2(.x = clogit_preds_null, .y = ua_steps,
#       .f = FitClogit)) %>%
#     dplyr::select(-ua_steps)

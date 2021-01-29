###################### ModelFit_SSF_Optimize ###################################

###################### TRUNCATE HYDRO_DIST #####################################
########################## ADDING QUADRATIC ####################################

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

if(!testing) plan(multisession)

# Pull files
#ua_files <- dir(ua_data_dir)[c(3,6,10,11,14,15,17)] # all . -> perch, roost
#ua_files <- dir(ua_data_dir)[c(1,2,4,5,8,9,12,13,16)] # all . -> cruise, flight
ua_files <- dir(ua_data_dir)[c(5)] #1 , 2, 4, 5)]

# WARNING: the ua_perch_perch.rds had to get split into two files to meet the
# GitHub size limits - it needs to be merged together for this process to work

# Sequence through step_type used/available files and fit sigma optimization
for (i in seq_along(ua_files)){
  if(testing) i <- 1
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

  if (end %in% c("cruise")){
    keep_covars <- c(
      "(developed)+[0-9]",
      "forest",
      "open_water",
      "roughness",
      "hydro_dist",
      "turbine_dist")
  }

  if (end %in% c("flight")){
    keep_covars <- c(
      "forest",
      "open_water",
      "eastness",
      "roughness",
      "tri",
      "hydro_dist",
      "turbine_dist")
  }

  if (end %in% c("perch", "roost")){
    keep_covars <- c(
      "open_water", "(road)+[0-9]", "tri", "wetland", "hydro_dist")
  }
  if (end %in% c("roost")){
    keep_covars <- c(
      "open_water", "(road)+[0-9]", "tri", "eastness", "wetland", "hydro_dist")
  }

  poly_covars <- c("(developed)+[0-9]", "forest", "open_water",
    "(road)+[0-9]",  "shrub_herb", "roughness", "tpi", "tri")

  fixed_covars <- c("developed_dist", "hydro_dist", "turbine_dist",
    "road_dist")

  length(keep_covars) #  For reference (# covars = # models):
  #  6 = 63; 7 = 127; 8 = 255; 9 = 511; 10 = 1023; 11 = 2047; 12 = 4095

  ## Filter ua_data to full set of keep_covars
  ua_steps_i_org <- readRDS(file.path(ua_data_dir, ua_file_i))

  # Truncate turbine_dist0
  ua_steps_i_trunc <- ua_steps_i_org %>%
    mutate(turbine_dist0 = if_else(turbine_dist0 > 25000, 25000, turbine_dist0))
  hist(ua_steps_i_org$turbine_dist0)
  hist(ua_steps_i_trunc$turbine_dist0)
  sum(ua_steps_i_org$turbine_dist0 > 25000)
  sum(ua_steps_i_trunc$turbine_dist0 == 25000)

  ua_steps_i_all <- ua_steps_i_trunc  %>%
    dplyr::select(c("case", "step_id"), matches(keep_covars)) %>%
    dplyr::select_if(function(x) !(all(is.na(x)) | all(x == ""))) %>%
    mutate(dummy = 1) %>%
    dplyr::select(dummy, everything())

  # Find rows with missing data
  ua_steps_na <- ua_steps_i_all %>%
    filter_all(any_vars(is.na(.))) %>%
    dplyr::select(case, step_id)

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
  models <- flatten(list_of_models)
  if(testing) models <- models[1:20]

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

        # NEW
        poly_vec <- which(covars_scale %in% poly_covars) # for polynomial covars
        mono_vec <- which(!covars_scale %in% poly_covars) # for monotopic covars
        covars_poly_sigmas <- paste0("poly(", covars_scale[poly_vec], sigmas[poly_vec], ", 2)")
        covars_mono_sigmas <- paste0(covars_scale[mono_vec], sigmas[mono_vec])
        covars_sigma_paste <- paste(c(covars_poly_sigmas, covars_mono_sigmas))
        covars_sigma <- covars_sigma_paste[order(c(poly_vec, mono_vec))]

        covars_fixed_0 <- paste0(covars_fixed, "0")
        preds <- paste(c(covars_sigma, covars_fixed_0, "strata(step_id)"),
          collapse = " + ")

        clogit_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
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
  model_grp <- as.numeric(cut_number(1:length(models),
    n = ceiling(length(models)/100)))

  tbl_models <- tibble(step_type, model = models, model_grp = model_grp) %>%
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
}

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

################## Getting the Quadratic Components Incorporated ###############
library(tidyverse)

# Functions
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

################## Very Simple Model with Quadratic Term #######################

# Terms and data table
intercept <- -5
beta1 <- 35
beta1a <- -40
data_tbl <- tibble(
  var1 = seq(0, 1, by = .01),
  result = intercept + (beta1*var1) + (beta1a*var1^2),
  prob = plogis(result),
  response = rbinom(length(prob), 1, prob))

# Plot result, prob, and response
ggplot(data_tbl) +
  geom_point(aes(x = var1, y = result))

ggplot(data = data_tbl, aes(x = var1, y = prob)) +
  geom_point(color = "red") +
  binomial_smooth(formula = y ~ x + I(x^2))


# Fit model with and without quadratic
formulas <- list(response ~ var1,               # no
                 response ~ var1 + I(var1^2))   # with quadratic

model_fits <-  purrr::map_df(formulas,
  ~{mod <- glm(.x, data = data_tbl, family="binomial")
    tibble(formula = format(.x),
      AIC = round(AIC(mod),2),
      BIC = round(BIC(mod),2),
    R2adj = round(DescTools::PseudoR2(mod,which=c("McFaddenAdj")),4))
    }
  ) %>%
  arrange(desc(AIC))

model_fits




response <- var1 + var2              # both vars linear
response <- var1 + var2 + I(var2^2)  # add quad term for var2
response <- var1 + I(var2^2)        # only quad term for var2
response <- var1 + var2 + var3       # add var3, which is var2^2
response <- var1 + var3              # only var1 and var3







predictors <- seq(-.25, 1.25, by = .01)
intercept <- -5
beta1 <- 10
beta2 <- 1
predictors_logit <- intercept + beta1*(predictors) + beta2*(predictors^2)
df <- data.frame(predictors, probs = plogis(predictors_logit))
(y_mid_int <- (-(1*intercept/beta1)))
rect_df <- data.frame(xmin = c(-.25, 1), ymin=c(0,0), xmax= c(0,1.25),
  ymax = c(1,1))


# Simple logistic function with a quadratic term
set.seed(1)
intercept <- -5
pred1 <- seq(-.25, 1.25, by = .01)
pred2 <- seq(-.25, 1.25, by = .01)
beta1 <- 10
beta1sq <- -.1

df01 <- tibble(
  pred1 = pred1,
  pred1sq = pred1^2,
  pred2 = pred2,
  response = intercept + beta1*pred1 + beta1sq*pred1sq, #+ beta1sq*pred1sq,
  probs = plogis(response))

df01$response
df01$probs

ggplot(df01, aes(x = pred1, y = probs)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
    se = FALSE)

#  coord_cartesian(ylim = c(0,1))

ggplot(df,aes(x=var2,y=response)) +
  geom_point() +
  geom_smooth(method="loess")+
  coord_cartesian(ylim = c(0,1))

ggplot(df,aes(x=var1+var3,y=response)) +
  geom_point() +
  geom_smooth(method="loess")+
  coord_cartesian(ylim = c(0,1))

formulas <- list(response ~ var1 + var2,              # both vars linear
                 response ~ var1 + var2 + I(var2^2),  # add quad term for var2
                 response ~ var1 + I(var2^2),         # only quad term for var2
                 response ~ var1 + var2 + var3,       # add var3, which is var2^2
                 response ~ var1 + var3)              # only var1 and var3

# build a df of some model selection criteria:
selection <-  purrr::map_df(formulas,
  ~{mod <- glm(.x, data = df, family="binomial")
    data.frame(formula = format(.x),
      AIC = round(AIC(mod),2),
      BIC = round(BIC(mod),2),
    R2adj = round(DescTools::PseudoR2(mod,which=c("McFaddenAdj")),4))}) %>%
  arrange(desc(AIC))





x <- seq(-100, 100, by = 1)
x2 <- x^2
y <- -0.000462*x2 + 0.0265*x + 0

df <- bind_cols(y = y, x = x, x2 = x2)

ggplot(df) +
  geom_point(aes(x = x, y = y))+
  geom_line(aes(x = x, y = y))


covars_scale_sigmas <- c("x", "x2") #paste0(covars_scale, sigmas)
covars_fixed_0 <- NULL    #paste0(covars_fixed, "0")
preds <- paste(c("-1", covars_scale_sigmas), #covars_fixed_0
  collapse = " + ")
preds

glm_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
glm_fit <- glm(glm_model_formula, data = ua_data, family = 'binomial')


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




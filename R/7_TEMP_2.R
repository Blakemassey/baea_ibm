###################### SSF_Fit_Clogit_Models ###################################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, arrangements, plyr, dplyr, future, furrr, optimx,
  ggplot2, lubridate, optimx, purrr, rgenoud, stringr, survival, tibble, tictoc,
  tidyr, reproducible)
pacman::p_load(baear, gisr, ibmr)
testing <- FALSE

# Directories
ua_data_dir <- "Output/Analysis/SSF/UA_Data"
mod_fit_dir <- "Output/Analysis/SSF/Models/model_fits"

# Generate clusters (for parallel processing)
if(!testing) plan(multisession)

AddScaleColumn <- function(covar_matrix, pars){
  if(is.null(pars)) pars <- NA
  covar_matrix_out <- covar_matrix %>%
    mutate(scale_fitted = ifelse(scale, pars, 0))
  return(covar_matrix_out)
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
ExtractModelFormula <- function(covar_matrix){
  covars_sigmas <- if_else(covar_matrix$poly2, paste0("poly(",
    covar_matrix$covar, covar_matrix$scale_fitted, ", 2)"),
    paste0(covar_matrix$covar, covar_matrix$scale_fitted))
  model_formula <- paste0("case ~ ", paste0(covars_sigmas, collapse = " + "),
    " + strata(step_id)")
  return(model_formula)
}
ExtractCovarFitted <- function(clogit_fit){
  coef <- clogit_fit %>% pluck("coefficients") %>% as.numeric()
  covar <- clogit_fit %>% pluck("coefficients", names)
  tbl_out <- tibble(covar = covar, coef = coef) %>%
    mutate(coef_signif = signif(coef, digits = 4)) %>%
    mutate(covar_clean = str_replace_all(covar, "poly\\(", "") %>%
        str_replace_all(., ", 2\\)1", "") %>%
        str_replace_all(., ", 2\\)2", "^2")) %>%
    dplyr::select(covar_clean, coef_signif, covar, coef)
}
ExtractModelFull <- function(covar_fitted){
  covars <- covar_fitted %>% pull(covar_clean)
  coefs <- covar_fitted %>% pull(coef_signif)
  model_full <- paste0(coefs, "*", covars, collapse = " + ")
  return(model_full)
}
GetCovarMatrix <- function(covar_index){
  covar_matrix_ij <- covar_matrix_i %>% slice(unlist(covar_index))
  return(covar_matrix_ij)
}
GetPopSize <- function(covars_scale){
  if(testing){
    if(nrow(covars_scale) <= 1) pop_size = 10
    if(nrow(covars_scale) == 2) pop_size = 200
    if(nrow(covars_scale) == 3) pop_size = 2000
    if(nrow(covars_scale) >= 4) pop_size = 5000
  } else {
    if(nrow(covars_scale) <= 1) pop_size = 100
    if(nrow(covars_scale) == 2) pop_size = 2000
    if(nrow(covars_scale) == 3) pop_size = 20000
    if(nrow(covars_scale) >= 4) pop_size = 50000
  }
  return(pop_size)
}
PrepUAStepDataForOptimization <- function(ua_data, keep_covars){
  # Replaces the 'dist' column names for easier selection
  colnames(ua_data) <- colnames(ua_data) %>%
    str_replace_all("developed_dist0", "dist_developed0") %>%
    str_replace_all("hydro_dist0", "dist_hydro0") %>%
    str_replace_all("road_dist0", "dist_road0") %>%
    str_replace_all("turbine_dist0", "dist_turbine0")
  # Limits the dist_turbine to 20km
  ua_data <- ua_data %>%
    mutate(dist_turbine0 = if_else(dist_turbine0 < 20000, dist_turbine0, 20000))
  # Replaces the covar distances with sigma values
  colnames_alpha <- str_replace_all(colnames(ua_data), "[0-9]", "")
  colnames_num <- as.numeric(str_replace_all(colnames(ua_data), "[^0-9]",""))
  colnames_tbl <- tibble(colnames_alpha, colnames_num) %>%
    mutate(colnames_sigma = ifelse(!is.na(colnames_num), colnames_num/30,
      NA)) %>%
    mutate_all(~str_replace_na(., "")) %>%
    mutate(colnames_final = paste0(colnames_alpha, colnames_sigma))
  colnames(ua_data) <- colnames_tbl %>% pull(colnames_final)
  # Subset data to only the ones in the 'keep_covars'
  ua_data <- ua_data  %>%
    dplyr::select(c("case", "step_id"), matches(keep_covars)) %>%
    dplyr::select_if(function(x) !(all(is.na(x)) | all(x == ""))) %>%
    mutate(dummy = 1) %>%
    dplyr::select(dummy, everything())
  # Find rows with missing data
  ua_data_na <- ua_data %>%
    filter_all(any_vars(is.na(.))) %>%
    dplyr::select(case, step_id)
  # Remove step_id pairs where any data is missing
  ua_data_final <- ua_data %>% anti_join(., ua_data_na,
    by = c('case', 'step_id'))
  return(ua_data_final)
}
RunClogitFit <- function(model_formula){
  clogit_fit <- clogit(as.formula(model_formula), data = ua_steps_i,
    method = "efron", iter.max = iter_max)
  return(clogit_fit)
}

################ OPTIMIZATION PROCEDURE OF KERNEL BANDWIDTH ####################

# Optimization parameters
max_generations = 25
wait_generations = 5
burnin_generations = 5
iter_max = 50

# Covariates matrix
covar_matrix <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "developed",      FALSE, TRUE, 1, 100, 50, FALSE,
  "forest",         FALSE, TRUE, 1, 100, 50, FALSE,
  "open_water",     FALSE, TRUE, 1, 100, 50, FALSE,
  "pasture",        FALSE, TRUE, 1, 100, 50, FALSE,
  "road",           FALSE, TRUE, 1, 100, 50, FALSE,
  "shrub_herb",     FALSE, TRUE, 1, 100, 50, FALSE,
  "wetland",        FALSE, TRUE, 1, 100, 50, FALSE,
  "eastness",       FALSE, TRUE, 1, 100, 50, FALSE,
  "northness",      FALSE, TRUE, 1, 100, 50, FALSE,
  "wind_class",     FALSE, TRUE, 1, 100, 50, FALSE,
  "roughness",      FALSE, TRUE, 1,  50, 25,  TRUE,
  "tpi",            FALSE, TRUE, 1,  50, 25,  TRUE,
  "tri",            FALSE, TRUE, 1,  50, 25,  TRUE,
  "dist_developed", TRUE, FALSE, NA, NA, NA, FALSE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE,
  "dist_road",      TRUE, FALSE, NA, NA, NA, FALSE,
  "dist_turbine",   TRUE, FALSE, NA, NA, NA, FALSE
)

covar_matrix_simple_flight_01 <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "road",           FALSE, TRUE, 1, 100, 50, FALSE,
  "eastness",       FALSE, TRUE, 1, 100, 50, FALSE,
  "northness",      FALSE, TRUE, 1, 100, 50, FALSE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE,
  "dist_turbine",   TRUE, FALSE, NA, NA, NA, FALSE
)

covar_matrix_simple_perch_01 <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "open_water",     FALSE, TRUE, 1, 100, 50, FALSE,
#  "roughness",      FALSE, TRUE, 1,  50, 25,  TRUE,
  "tpi",            FALSE, TRUE, 1,  50, 25,  TRUE,
#  "tri",            FALSE, TRUE, 1,  50, 25,  TRUE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE,
)

# Pull files
ua_files <- dir(ua_data_dir)[c(2,5,13)]
ua_files <- dir(ua_data_dir)[c(3,6,10,14)]

# Sequence through step_type used/available files and fit sigma optimization
for (i in seq_along(ua_files)){
  ua_file_i <- ua_files[i]
  step_type <- str_remove_all(ua_file_i, ("ua_steps_|.rds"))
  print(paste0("Starting: ", step_type, " (", i , " of ", length(ua_files),")"))
  if(!dir.exists(file.path(mod_fit_dir, step_type))){
    dir.create(file.path(mod_fit_dir, step_type))
  }
  models_in_dir <- list.files(file.path(mod_fit_dir, step_type),
    pattern = "\\.rds$")
  print(paste0("Models in directory : ", models_in_dir))
  for (i in models_in_dir){
    file.move(file.path(mod_fit_dir, step_type, i), file.path(mod_fit_dir,
      step_type, "Archive"))
  }
  start <- str_split(step_type, "_") %>% unlist(.) %>% pluck(1)
  end <- str_split(step_type, "_") %>% unlist(.) %>% pluck(2)

  if (end %in% c("flight")){
    covar_matrix_i <- covar_matrix_simple_flight_01
  }
  if (end %in% c("perch")){
    covar_matrix_i <- covar_matrix_simple_perch_01
  }

  #covar_matrix_i <- covar_matrix %>% #dplyr::filter(covar %in% keep_covars)

  covar_matrix_i_combos <- do.call(c, lapply(seq_len(nrow(covar_matrix_i)),
    function(y) {arrangements::combinations(nrow(covar_matrix_i), y,
      layout = "l")}))

  ## Filter ua_data to full set of keep_covars
  ua_steps_i_org <- readRDS(file.path(ua_data_dir, ua_file_i))
  ua_steps_i <- PrepUAStepDataForOptimization(ua_steps_i_org, covar_matrix_i$covar)

  #covars_scale, covars_fixed, sigma_domains, sigma_starts, pop_size,
  OptimizeClogitSigma <- function(covars_matrix, mod_num){

    #covars_matrix <- tbl_models$covars_matrix[100] %>% pluck(1)
    covars_scale <- covars_matrix %>% filter(scale)
    covars_fixed <- covars_matrix %>% filter(fixed)
    pop_size <- GetPopSize(covars_scale)
    if (!any(covars_matrix$scale)){ # not optimizing sigmas
      covars_fixed_0 <- covars_fixed %>% pull(covar) %>% paste0(., "0")
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

      domains <- covars_scale %>% dplyr::select(scale_min, scale_max) %>%
        as.matrix(.)
      domains_num <- mapply(as.numeric, domains) # Fixes a FRUSTRATING bug!
      dim(domains_num) <- dim(domains) # rgenoud NOW REQUIRES DOMAINS BE NUMERIC!
      sigma_n <- nrow(covars_scale)
      starting_values <- covars_scale %>% pull(scale_start) # starting values
      parms <- starting_values  #sigmas <- starting_values

      FitClogitSigma <- function(sigmas, ua_data){
        covars_sigma <- covars_scale %>%
          mutate(scale = sigmas) %>%
          mutate(covars_sigma = if_else(poly2,
            paste0("poly(", covar, scale, ", 2)"),
            paste0(covar,scale))) %>%
          pull(covars_sigma)
        covars_fixed_0 <- paste0(covars_fixed %>% pull(covar), "0")
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
          boundary.enforcement = 2, ua_data = ua_steps_i, data.type.int = TRUE,
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

  ## OPTIMIZATION PROCEDURE
  # Divide models into groups for analysis (so if a thread fails, it has a
  # chance to start again)
  model_grp <- as.numeric(cut_number(1:length(covar_matrix_i_combos),
    n = ceiling(length(covar_matrix_i_combos)/100)))

  tbl_models <- tibble(step_type, covar_index = covar_matrix_i_combos,
    model_grp = model_grp) %>%
    mutate(model_num = str_pad(1:nrow(.), 4, "left", "0")) %>%
    mutate(covar_matrix = map(covar_index, GetCovarMatrix)) %>%
    dplyr::select(-covar_index)

  print(paste0("Starting ", step_type, " : ", now()))
  print(paste0("Assessing ", nrow(tbl_models), " models"))

  tbl_models_fitted_list <- list()
  for (i in unique(model_grp)) {
    print(paste0("Model group: ", i, " of ", length(unique(model_grp))))
    tbl_models_fitted_i <- tbl_models %>%
      filter(model_grp == i)  %>%
      mutate(opt_fit = future_pmap(.l = list(covar_matrix, model_num),
        .f = possibly(OptimizeClogitSigma, otherwise = NA_real_),
        .progress = TRUE, .options = furrr_options(seed = TRUE))) %>%
      mutate(fit_aicc = map_dbl(opt_fit, ExtractAICc))
    tbl_models_fitted_list[[i]] <- tbl_models_fitted_i
  }
  tbl_models_fitted <- tbl_models_fitted_list %>%
    reduce(bind_rows)
  writeLines("")
  writeLines(paste0("Finished ", step_type, " at ", now()))

  tbl_models_final <- tbl_models_fitted %>%
      arrange(fit_aicc) %>%
      mutate(delta_aicc = fit_aicc - first(fit_aicc)) %>%
      filter(!is.na(fit_aicc)) %>%
      mutate(peak_gen = map2(opt_fit, "peakgeneration", pluck)) %>%
      mutate(opt_scales = map2(opt_fit, "par", pluck)) %>%
      mutate(covar_matrix = map2(covar_matrix, opt_scales, AddScaleColumn)) %>%
      mutate(model_formula = map_chr(covar_matrix, ExtractModelFormula)) %>%
      mutate(clogit_fit = map(model_formula, RunClogitFit)) %>%
      mutate(covar_fitted = map(clogit_fit, ExtractCovarFitted)) %>%
      mutate(refit_aicc = map_dbl(clogit_fit, AICc)) %>%
      mutate(concordance_value = pmap_dbl(list(clogit_fit, "concordance",
        "concordance"), pluck)) %>%
      mutate(fit_covars_clean = map(covar_fitted, pull, "covar_clean")) %>%
      mutate(fit_coefs_signif = map(covar_fitted, pull, "coef_signif")) %>%
      mutate(model_full = map(covar_fitted, ExtractModelFull)) %>%
      dplyr::select(step_type, covar_matrix, fit_aicc, delta_aicc, refit_aicc,
        peak_gen, concordance_value, covar_fitted, fit_covars_clean,
        fit_coefs_signif, model_full)

  # SAVE FIlE -----------------------------------------------------------

  # save output
  saveRDS(tbl_models_final, file.path(mod_fit_dir, step_type,
    paste0("ssf_fit_", step_type, "_", GetDateTime(), ".rds")))

}

future:::ClusterRegistry("stop")

### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###

# Check for consistency between optimization fit and refit
# which(round(model_fits_compiled_refit %>% pull(fit_aicc), 5) !=
#   round(model_fits_compiled_refit %>% pull(fit_aicc_refit), 5))
# Should be TRUE

# concordance_list <- concordance(clogit_fit)
# concordance_value <- concordance_list %>% pluck("concordance")
# concordance_var <- concordance_list %>% pluck("var")
# concordance_se <- sqrt(concordance_var)
# paste0(b, "*", a, collapse = " + ")
# test_cleaned <- butcher::butcher(test) %>% butcher::axe_data(.)
# obj_size(tbl_models_fitted_pars)
# obj_size(test_cleaned)

# ExtractClogitCoefs <- function(clogit_fit){
#   clogit_fit_i <- clogit_fit %>% pluck(coef)
#   return(clogit_fit_i)
# }


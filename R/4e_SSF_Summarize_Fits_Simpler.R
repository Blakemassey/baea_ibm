###################### ModelFit_SSF_Summarize ##################################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, plyr, dplyr, future, furrr, optimx, ggplot2,
  ggthemes, glmulti, lubridate, optimx, purrr, reproducible, rgenoud, stringr,
  summarytools, survival, surveybootstrap, tibble, tictoc, tidyr)
library(baear, gisr)

# Output Directory
mod_fit_dir = "Output/Analysis/SSF/Models"

# Source Data Directory
ua_data_dir <- "Output/Analysis/SSF/UA_Data"

# Find all of the step_type folders in mod_fit_dir
step_types <- list.dirs(file.path(mod_fit_dir, "simpler_fits"),
  full.names = FALSE, recursive = FALSE)

############################### FUNCTIONS #####################################

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

FitClogit <- function(clogit_preds, ua_data){
  clogit_model_formula = as.formula(clogit_preds)
  clogit_fit <- clogit(clogit_model_formula, data = ua_data, method = "efron",
    iter.max = iter_max)
  return(clogit_fit)
}

RenameUAStepsToMeters <- function(ua_steps){
  colnames_alpha <- str_replace_all(colnames(ua_steps), "[0-9]", "")
  colnames_num <- as.numeric(str_replace_all(colnames(ua_steps), "[^0-9]", ""))
  covars <- unique(colnames_alpha %>% .[!. %in% c("dummy", "case", "step_id")])
  colnames_tbl <- tibble(colnames_alpha, colnames_num) %>%
    mutate(colnames_sigma = ifelse(!is.na(colnames_num), colnames_num/30,NA))%>%
    mutate_all(~str_replace_na(., "")) %>%
    mutate(colnames_final = paste0(colnames_alpha, colnames_sigma))
  colnames(ua_steps) <- colnames_tbl %>% pull(colnames_final)
  return(ua_steps)
}

################### COMPILE MODELS FOR EACH STEP TYPE ##########################

# Compile all models for each step_type into a compiled_fit file
for (i in seq_along(step_types)) {
  step_type_i <- step_types[i]
  print(paste0(step_type_i, " (", i, " of ", length(step_types), ")"))
  ssf_fits_step_type_i <- list.files(path = file.path(mod_fit_dir,
      "simpler_fits", step_type_i), pattern = paste0("ssf_fit_", step_type_i,
      "*")) %>%
    map(~ readRDS(file.path(mod_fit_dir, "simpler_fits", step_type_i, .))) %>%
    reduce(bind_rows) %>%
    as_tibble(.)
  print(paste0("Models evaluated: n = ", nrow(ssf_fits_step_type_i)))
  compiled_ssf_fits_step_type_i <- ssf_fits_step_type_i  %>%
    mutate(model_chr = map_chr(model, as.character)) %>%
    group_by(model_chr) %>% # Precaution in case duplicate models exist in data
    slice(which.min(fit_aicc)) %>%
    ungroup() %>%
    arrange(fit_aicc) %>%
    mutate(pars = map2(opt_fit, "par", pluck)) %>%
    mutate(peak_generation = map2(opt_fit, "peakgeneration", pluck)) %>%
    mutate(covars_scale_sigmas = map2_chr(covars_scale, pars, paste0,
      collapse = " + ")) %>%
    mutate(covars_scale_sigmas = ifelse(covars_scale_sigmas == "NA", NA,
      covars_scale_sigmas)) %>%
    mutate(covars_scale_sigmas = ifelse(covars_scale_sigmas == "", NA,
      covars_scale_sigmas)) %>%
    mutate(fixed_sigma = ifelse(!is.na(covars_fixed), 0, NA)) %>%
    mutate(covars_fixed_sigmas = map2_chr(covars_fixed, fixed_sigma,
      PasteFixedSigmas)) %>%
    mutate(covars_fixed_sigmas = ifelse(covars_fixed_sigmas == "NA", NA,
      covars_fixed_sigmas)) %>%
    mutate(covars_fixed_sigmas = ifelse(covars_fixed_sigmas == "0", NA,
      covars_fixed_sigmas)) %>%
    mutate(preds = map2_chr(.x = covars_scale_sigmas, .y = covars_fixed_sigmas,
      .f = PastePreds))# %>%
  #dplyr::select(step_type, model_num, opt_fit, fit_aicc, preds)
  print(paste0("Models with fits: n = ", nrow(compiled_ssf_fits_step_type_i)))
  # Save files (not needed if all of this script is working and run)
  saveRDS(compiled_ssf_fits_step_type_i, file.path(mod_fit_dir,
    "compiled_simpler_fits", paste0("compiled_ssf_fits_", step_type_i, ".rds")))
}
rm(step_type_i, ssf_fits_step_type_i, compiled_ssf_fits_step_type_i)

################ COMPILE BEST FIT MODELS FOR EACH STEP TYPE  ###################

# Find the best fit models for each step_type
simpler_models <- list.files(path = file.path(mod_fit_dir,
    "compiled_simpler_fits"), pattern = "^compiled_ssf_fits_*")  %>%
  map(~ readRDS(file.path(mod_fit_dir, "compiled_simpler_fits", .))) %>%
  reduce(bind_rows) %>%
  group_by(step_type) %>%
  arrange(fit_aicc) %>%
  mutate(delta_aicc = fit_aicc - first(fit_aicc)) %>%
  #slice(1:10) %>%
  ungroup(.) %>%
  arrange(step_type) %>%
  dplyr::select(step_type, model_num, opt_fit, fit_aicc, delta_aicc, preds)

# Match ua_step data with the best fit models
ua_steps_sigma <- list.files(path = ua_data_dir,
  pattern = paste0("ua_steps_*")) %>%
  map(~ readRDS(file.path(ua_data_dir, .))) %>%
  reduce(bind_rows) %>%
  mutate(step_type = str_replace_all(behavior_behavior, " -> ", "_")) %>%
  mutate(step_type = str_to_lower(step_type))

ua_steps <- RenameUAStepsToMeters(ua_steps_sigma)

ua_steps_nested <- ua_steps %>%
  as_tibble(.) %>%
  group_by(step_type) %>%
  nest(.) %>%
  ungroup(.)

simpler_models_data <- simpler_models %>%
  dplyr::select(step_type, fit_aicc, delta_aicc, preds) %>%
  left_join(., ua_steps_nested, by = "step_type") %>%
  rename(ua_steps = data)
rm(simpler_models, ua_steps_sigma, ua_steps, ua_steps_nested)

# Re-checking model fits -------------------------------------------------------
# Directly recalculate the fit to ensure that the optimization and
# summarization worked as expected. Optimization result fit aic and
# recalculated fit aic should match.

# Use the best-aic predictors and original data to refit clogit model
iter_max <- 200
simpler_models_refit <- simpler_models_data %>%
  mutate(clogit_preds = paste0("case ~ ", preds, " + strata(step_id)")) %>%
  mutate(clogit_preds_null = paste0("case ~ 0 + strata(step_id)")) %>%
  dplyr::select(step_type, fit_aicc, delta_aicc, clogit_preds,
    clogit_preds_null, ua_steps, preds) %>%
  mutate(clogit_fit = map2(.x = clogit_preds, .y = ua_steps,
    .f = FitClogit)) %>%
  mutate(clogit_fit_null = map2(.x = clogit_preds_null, .y = ua_steps,
     .f = FitClogit)) %>%
  mutate(fit_aicc_refit = map_dbl(clogit_fit, AICc)) %>%
  dplyr::select(-ua_steps)
  # Got a Warning about nest_roost didn't converge, but it appears to be refit.

glimpse(simpler_models_refit)

# Check for consistency between optimization fit and refit
identical(round(simpler_models_refit %>% pull(fit_aicc), 5),
  round(simpler_models_refit %>% pull(fit_aicc_refit), 5)) # Should be TRUE

######################## EXTRACT COEFFICIENTS ##################################

ExtractClogitCoefs <- function(clogit_fit){
  clogit_fit_i <- clogit_fit %>% pluck(coef)
  return(clogit_fit_i)
}

ExtractClogitFitTerms <- function(clogit_fit){
  terms_i <- clogit_fit %>% pluck(terms, attr_getter("term.labels"))
  return(terms_i)
}

simpler_models_coefs <- simpler_models_refit %>%
  mutate(clogit_fit_coefs = map(.x = clogit_fit, .f = ExtractClogitCoefs)) %>%
  mutate(clogit_fit_terms = map(.x = clogit_fit, .f = ExtractClogitFitTerms))%>%
  dplyr::select(step_type, fit_aicc, delta_aicc, clogit_fit, clogit_fit_null,
    clogit_fit_coefs, clogit_fit_terms, preds)

################### CALCULATE AND EXTRACT CONCORDANCE ##########################

simpler_models_concord <- simpler_models_coefs %>%
  mutate(concordance_list = map(.x = clogit_fit, .f = concordance)) %>%
  mutate(concordance_value = map2_dbl(concordance_list, "concordance",
    pluck)) %>%
  mutate(concordance_var = map2_dbl(concordance_list, "var", pluck)) %>%
  mutate(concordance_se = sqrt(concordance_var))

# Check concordance stats
simpler_models_concord %>%
  dplyr::select(step_type, concordance_value, concordance_var,
    concordance_se) %>%
  group_by(., step_type) %>%
  skimr::skim()

################## CALCULATE DEVIANCE EXPLAINED ################################

simpler_models_deviance <- simpler_models_concord %>%
  mutate(deviance_values = map(.x = clogit_fit, .f = residuals,
    type = "deviance")) %>%
  mutate(deviance_squared = map(deviance_values, ~.^2)) %>%
  mutate(deviance_sum_of_squares = map_dbl(deviance_squared, sum)) %>%
  mutate(deviance_values_null = map(.x = clogit_fit_null, .f = residuals,
    type = "deviance")) %>%
  mutate(deviance_squared_null = map(deviance_values_null, ~.^2)) %>%
  mutate(deviance_sum_of_squares_null = map_dbl(deviance_squared_null, sum)) %>%
  mutate(deviance_explained = 1 -
      (deviance_sum_of_squares/deviance_sum_of_squares_null)) #%>%

# Check deviance stats
simpler_models_deviance %>%
  dplyr::select(step_type, deviance_values, deviance_squared,
    deviance_squared_null, deviance_sum_of_squares,
    deviance_sum_of_squares_null, deviance_explained) %>%
  group_by(., step_type) %>%
  skimr::skim()

##################### SAVE SIMPLER MODELS SUM FILE #############################

# Save file with models for each step-type
simpler_models_sum <- simpler_models_deviance %>%
  dplyr::select(step_type, fit_aicc, delta_aicc, preds,
    concordance_value, concordance_var, concordance_se,
    deviance_values, deviance_squared, deviance_squared_null,
    deviance_sum_of_squares, deviance_sum_of_squares_null, deviance_explained)
saveRDS(simpler_models_sum, file.path(mod_fit_dir, "simpler_models_sum",
  "simpler_models_sum.rds"))
rm(simpler_models_sum)

############## FIND AND SAVE BEST FIT MODEL FOR EACH STEP TYPE #################

# Find the best fit models for each step_type
simpler_models_sum <- simpler_models_deviance %>%
  group_by(step_type) %>%
  arrange(fit_aicc) %>%
#  slice(which.min(fit_aicc)) %>%
  ungroup(.) %>%
  arrange(step_type) %>%
  dplyr::select(step_type, fit_aicc, preds,
    concordance_value, concordance_var, concordance_se,
    deviance_sum_of_squares, deviance_sum_of_squares_null, deviance_explained)

# Save each 'best_ssf_fits_i'
for (i in unique(simpler_models_sum$step_type)){
  ssf_fit_i <- simpler_models_sum %>%
    filter(step_type == i)
  saveRDS(ssf_fit_i, file.path(mod_fit_dir, "best_simpler_fits",
    paste0("best_ssf_simpler_fit_", i, ".rds")))
}

############## COMPILE AND SAVE BEST_SSF_FITS FILE #############################

best_fit_all <- list.files(path = file.path(mod_fit_dir, "best_simpler_fits"),
    pattern = paste0("^best_ssf_simpler_fit_[^all]"))  %>%
  map(~ readRDS(file.path(mod_fit_dir, "best_simpler_fits", .))) %>%
  reduce(bind_rows)

saveRDS(best_fit_all, file.path(mod_fit_dir, "best_simpler_fits",
  "best_ssf_simpler_fit_all.rds")) # File is >100Mb, so it is included in .gitignore

### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###



# # This gets the loglik comparisons, but I don't think that's what I want.
# fit <- coxph(Surv(futime, fustat) ~ resid.ds *rx + ecog.ps, data = ovarian)
# anova(fit)
# fit_null <- coxph(Surv(futime, fustat) ~ 0, data=ovarian)
# anova(fit_null, fit)


# ## COMPILE ALL BEST NO-MAX-SIGMA FIT MODELS
#
# # Determine the best fit models that do not have a "maximum sigma" variable,
# # specifically 100 or 50 sigma values (depending on class).
# best_fit_models_no_max_variables <- list.files(path = file.path(mod_fit_dir),
#     pattern = paste0("^compiled_ssf_fits_*"))  %>%
#   map(~ readRDS(file.path(mod_fit_dir, .))) %>%
#   reduce(bind_rows) %>%
#   group_by(step_type) %>%
#   arrange(fit_aicc) %>%
#   mutate(mod_rank = 1:n()) %>%
#   filter(!str_detect(preds, paste0("developed100|forest100|open_water100|",
#     "pasture100|shrub_herb100|eastness100|northness100|wind_class100"))) %>%
#   filter(!str_detect(preds, "tpi50|tri50|roughness50")) %>%
#   slice(which.min(fit_aicc)) %>%
#   ungroup() %>%
#   arrange(step_type) %>%
#   mutate(ua_steps = list(NA))

# # Find all of the step_type mods_sum in fit_file_dir
# all_fit_sums <- list.files(path = file.path(mod_fit_dir),
#     pattern = paste0("^compiled_ssf_fits_*"))  %>%
#   map(~ readRDS(file.path(mod_fit_dir, .))) %>%
#   reduce(bind_rows) %>%
#   arrange(step_type, fit_aicc)
#
# saveRDS(all_fit_sums, file.path(mod_fit_dir, "compiled_ssf_fits_all.rds"))


# for (i in seq_along(step_types)) {
#   model_num_max <- fit_sum_step_type_i %>% pull(model_num) %>% max(.)
#   fit_sum_step_type_missing <- setdiff(1:model_num_max,
#     fit_sum_step_type_i %>% pull(model_num) %>% as.numeric(.) %>% sort(.))
#   d_as <- diff(fit_sum_step_type_missing)
#   which(diff(fit_sum_step_type_missing) != 1)
# }
#
# # Make all step_type best fit model - generate all subsets
# for (i in 1:nrow(best_fit_models_data)){
#   covars <- str_split(best_fit_models_data %>% slice(i) %>% pull(preds),
#     " \\+ ", simplify = TRUE)
#   list_of_models <- lapply(seq_along((covars)), function(n) {
#       left_hand_side <- "case"
#       right_hand_side <- apply(X = combn(covars, n), MARGIN = 2, paste,
#         collapse = " + ")
#       paste(left_hand_side, right_hand_side, sep = " ~ ")
#   })
#   model <- flatten_chr(list_of_models)
#   best_fit_models_data_i <- best_fit_models_data %>% slice(i) %>%
#     mutate(count = length(model)) %>%
#     uncount(count) %>%
#     mutate(preds = model)
# }

# # Find all of the step_type mods_sum in fit_file_dir
# all_fit_sums <- list.files(path = file.path(mod_fit_dir),
#     pattern = paste0("^fit_sum_*"))  %>%
#   map(~ readRDS(file.path(mod_fit_dir, .))) %>%
#   reduce(bind_rows) %>%
#   arrange(step_type, fit_aic)
# saveRDS(all_fit_sums, file.path(mod_fit_dir, "all_fit_sums.rds"))
#
# all_fit_sums_simple <- all_fit_sums %>%
#   select(step_type, preds)
# writexl::write_xlsx(all_fit_sums_simple, file.path(mod_fit_dir,
#   "all_fit_sums_simple.xlsx"))
#
# all_fit_sums_1variable <- all_fit_sums %>%
#   mutate(covars_fixed_chr = map_chr(covars_fixed, paste, collapse = " + ")) %>%
#   filter(covars_fixed_chr == "") %>%
#   mutate(preds = str_replace_all(preds, " \\+ 0", "")) %>%
#   filter(pop_size == 100) %>%
#   filter(!preds %in% c("developed100", "forest100", "open_water100",
#     "pasture100", "shrub_herb100", "eastness100", "northness100",
#     "wind_class100")) %>%
#   filter(!preds %in% c("tpi50","tri50","roughness50")) %>%
#   select(step_type, model_num, fit_aic, preds) %>%
#   arrange(step_type, fit_aic)

# best_fit_models_simple <- best_fit_models %>%
#   select(step_type, mod_rank, preds)

#   list_of_models <- lapply(seq_along((covars)), function(n) {
#       left_hand_side <- "case"
#       right_hand_side <- apply(X = combn(covars, n), MARGIN = 2, paste,
#         collapse = " + ")
#       paste(left_hand_side, right_hand_side, sep = " ~ ")
#   })
#   model <- flatten(list_of_models)
#
#
#
# # For each step_type best fit model, run an all subsets regression
#
# # Method to redefine a fit function comes from the 'multiglm' package PDF
# coxph_redefined = function(formula, data, always = "", ...) {
#   coxph(as.formula(paste(deparse(formula), always)), data = data,
#     method = "efron", ...)
# }
#
# # For each of best_fit_models rows, run all-subset regression w/ ua_steps & preds
# # BUT this doesn't really work for my analysis b/c the subset models that I
# # evaluated allowed the variable-sigma covariates to change, so they would not
# # truly be subsets
# for (i in 1:nrow(best_fit_models_data)){
#   i <- 1
#   ua_steps_i <- best_fit_models_data %>% select("ua_steps") %>% pluck(1, i) %>%
#     mutate(dummy = 1)
#   preds <- best_fit_models %>% slice(i) %>% pull(preds)
#   print(preds)
#   fmla_full <- as.formula(paste("Surv(dummy, case) ~ ", preds))
#   glmulti_coxph <-
#     glmulti(fmla_full,
#             data = ua_steps_i,
#             level = 1,               # No interaction considered
#             method = "h",            # Exhaustive approach
#             crit = "aic",            # AIC as criteria
#             confsetsize = 5,         # Keep 5 best models
#             plotty = F, report = F,  # No plot or interim reports
#             always = "+ strata(step_id)",
#             fitfunction = coxph_redefined)   # coxph function
#
#   ## Show 5 best models (Use @ instead of $ for an S4 object)
#   glmulti_coxph@formulas
#   print(glmulti_coxph@formulas[[1]])
# }
#   glmulti_coxph@formulas[[2]]
#
#   ## Show result for the best model
#   summary(glmulti_coxph@objects[[1]])
#
#   coef(glmulti_coxph@objects[[1]])
#   coef(glmulti_coxph@objects[[2]])
#   coef(glmulti_coxph@objects[[3]])
#   coef(glmulti_coxph@objects[[4]])
#   coef(glmulti_coxph@objects[[5]])
#
#   test <- glmulti_coxph@objects[[1]]
#   summary(glmulti_coxph)
#
#   ## Can get model weights:
#   summary(glmulti_coxph)$modelweights
#


# # glm approach
# fit1 <- glm(case ~ 1 + developed100 + forest100 + open_water68 + pasture3 +
#   shrub_herb29 + developed_dist0 + hydro_dist0 + turbine_dist0,
#   family = 'binomial', data = ua_steps_i)
# summary(fit1)
#
# # survival clogit approach -- equivalent
# fit2 <- clogit(case ~ developed100 + forest100 + open_water68 + pasture3 +
#   shrub_herb29 + developed_dist0 + hydro_dist0 + turbine_dist0 + strata(step_id),
#   data = ua_steps_i)
# summary(fit2)
#
# # survival coxph approach -- equivalent
# fit3 <- coxph(Surv(dummy, case) ~ developed100 + forest100 + open_water68 +
#   pasture3 + shrub_herb29 + developed_dist0 + hydro_dist0 + turbine_dist0 +
#   strata(step_id), data = ua_steps_i)
# summary(fit3)
#
# #AIC equivalent
# AIC(fit1,fit2,fit3)



# getmode <- function(v) {
#    unique_v <- unique(v)
#    unique_v[which.max(tabulate(match(v, unique_v)))]
# }
#
# opt_sigmas <- df_opt_results %>%
#   filter(AIC == min(AIC)) %>%
#   summarize(
#     var1 = getmode(get(sigma_variables[1])),
#     var2 = getmode(get(sigma_variables[2])),
#     var3 = getmode(get(sigma_variables[3])),
#     var4 = getmode(get(sigma_variables[4])),
#     var5 = getmode(get(sigma_variables[5]))) %>%
#   slice(1) %>%
#   unlist(., use.names=FALSE)


# clogit_preds <- best_fit_sums3 %>% slice(1) %>% pull(clogit_preds)
# ua_data <- best_fit_sums3 %>% slice(1) %>% select(ua_steps, step_type) %>%
#   rename(step_type2 = step_type) %>%
#   unnest(cols = c(ua_steps)) %>%
#   select(step_type, step_type2, everything(.))
#
# tail(colnames(ua_data))

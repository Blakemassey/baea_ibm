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
step_types <- list.dirs(file.path(mod_fit_dir, "model_fits"),
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

RenameUAStepsToSigma <- function(ua_steps, meters = 30){
  colnames_alpha <- str_replace_all(colnames(ua_steps), "[0-9]", "")
  colnames_num <- as.numeric(str_replace_all(colnames(ua_steps), "[^0-9]", ""))
  covars <- unique(colnames_alpha %>% .[!. %in% c("dummy", "case", "step_id")])
  colnames_tbl <- tibble(colnames_alpha, colnames_num) %>%
    mutate(colnames_sigma = ifelse(!is.na(colnames_num),
      colnames_num/meters, NA))%>%
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
      "model_fits", step_type_i), pattern = paste0("ssf_fit_",
        step_type_i, "*")) %>%
    map(~ readRDS(file.path(mod_fit_dir, "model_fits", step_type_i, .))) %>%
    reduce(bind_rows) %>%
    as_tibble(.)
  print(paste0("Models evaluated: n = ", nrow(ssf_fits_step_type_i)))
  compiled_ssf_fits_step_type_i <- ssf_fits_step_type_i  %>%
    group_by(model_chr) %>% # Precaution in case duplicate models exist in data
    slice(which.min(fit_aicc)) %>%
    ungroup() %>%
    arrange(fit_aicc) %>%
    mutate(preds = str_replace_all(clogit_preds, "case ~ ", "")) %>%
    mutate(preds = str_replace_all(preds, " \\+ strata\\(step_id\\)", "")) %>%
    filter(!is.na(fit_aicc))
  print(paste0("Models with fits: n = ", nrow(compiled_ssf_fits_step_type_i)))
  saveRDS(compiled_ssf_fits_step_type_i, file.path(mod_fit_dir,
    "model_fits_compiled",
    paste0("ssf_fits_compiled_", step_type_i, ".rds")))
}
rm(step_type_i, ssf_fits_step_type_i, compiled_ssf_fits_step_type_i)

################ COMPILE BEST FIT MODELS FOR EACH STEP TYPE  ###################

# Find the best fit models for each step_type
model_fits_compiled <- list.files(path = file.path(mod_fit_dir,
    "model_fits_compiled"), pattern = "^ssf_fits_compiled_*")  %>%
  map(~ readRDS(file.path(mod_fit_dir, "model_fits_compiled", .))) %>%
  reduce(bind_rows) %>%
  group_by(step_type) %>%
  arrange(fit_aicc) %>%
  mutate(delta_aicc = fit_aicc - first(fit_aicc)) %>%
  slice(1:10) %>%
  ungroup(.) %>%
  arrange(step_type)

# Match ua_step data with the best fit models
ua_steps_sigma <- list.files(path = ua_data_dir,
  pattern = paste0("ua_steps_*")) %>%
  map(~ readRDS(file.path(ua_data_dir, .))) %>%
  reduce(bind_rows) %>%
  mutate(step_type = str_replace_all(behavior_behavior, " -> ", "_")) %>%
  mutate(step_type = str_to_lower(step_type))

ua_steps <- RenameUAStepsToSigma(ua_steps_sigma)

ua_steps_nested <- ua_steps %>%
  as_tibble(.) %>%
  group_by(step_type) %>%
  nest(.) %>%
  ungroup(.)

model_fits_compiled_data <- model_fits_compiled %>%
  #dplyr::select(step_type, fit_aicc, delta_aicc, preds) %>%
  left_join(., ua_steps_nested, by = "step_type") %>%
  dplyr::rename(ua_steps = data)
rm(model_fits_compiled, ua_steps_sigma, ua_steps, ua_steps_nested)

# Re-checking model fits -------------------------------------------------------
# Directly recalculate the fit to ensure that the optimization and
# summarization worked as expected. Optimization result fit aic and
# recalculated fit aic should match.

# Use the best-aic predictors and original data to refit clogit model
iter_max <- 200
model_fits_compiled_refit <- model_fits_compiled_data %>%
  mutate(clogit_preds = paste0("case ~ ", preds, " + strata(step_id)")) %>%
  mutate(clogit_preds_null = paste0("case ~ 0 + strata(step_id)")) %>%
  #dplyr::select(step_type, fit_aicc, delta_aicc, clogit_preds,
  #  clogit_preds_null, ua_steps, preds) %>%
  mutate(clogit_fit = map2(.x = clogit_preds, .y = ua_steps,
    .f = FitClogit)) %>%
  mutate(clogit_fit_null = map2(.x = clogit_preds_null, .y = ua_steps,
     .f = FitClogit)) %>%
  mutate(fit_aicc_refit = map_dbl(clogit_fit, AICc)) %>%
  dplyr::select(-c(ua_steps, clogit_fit_null))
  # Got a Warning about nest_roost didn't converge, but it appears to be refit.

glimpse(model_fits_compiled_refit)
rm(model_fits_compiled_data)

# Check for consistency between optimization fit and refit
which(round(model_fits_compiled_refit %>% pull(fit_aicc), 5) !=
  round(model_fits_compiled_refit %>% pull(fit_aicc_refit), 5))
  # Should be TRUE

saveRDS(model_fits_compiled_refit %>% dplyr::select(-c(clogit_fit)),
  file.path("Output/Analysis/SSF/Models", "model_fits_compiled_refit",
  "model_fits_compiled_refit.rds"))

########### FIND, REFIT, AND SAVE BEST FIT MODEL FOR EACH STEP TYPE ############

# Find the best fit models for each step_type
model_fits_compiled_refit_best <- model_fits_compiled_refit %>%
  group_by(step_type) %>%
  arrange(fit_aicc) %>%
  slice(which.min(fit_aicc)) %>%
  ungroup(.) %>%
  arrange(step_type)

saveRDS(model_fits_compiled_refit_best, file.path(mod_fit_dir,
  "model_fits_compiled_refit_best",
  "model_fits_compiled_refit_best.rds"))

### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###

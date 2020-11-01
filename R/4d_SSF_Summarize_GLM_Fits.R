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
ua_data_dir <- "Output/Analysis/SSF/UA_Data_Diff"

# Find all of the step_type folders in mod_fit_dir
step_types <- list.dirs(file.path(mod_fit_dir, "glm_model_fits"),
  full.names = FALSE, recursive = FALSE)

################### COMPILE MODELS FOR EACH STEP TYPE ##########################

# Compile all models for each step_type into a compiled_fit file
for (i in seq_along(step_types)) {
  step_type_i <- step_types[i]
  print(paste0(step_type_i, " (", i, " of ", length(step_types), ")"))
  ssf_fits_step_type_i <- list.files(path = file.path(mod_fit_dir,
      "glm_model_fits", step_type_i), pattern = paste0("ssf_fit_",
        step_type_i, "*")) %>%
    map(~ readRDS(file.path(mod_fit_dir, "glm_model_fits", step_type_i, .))) %>%
    reduce(bind_rows) %>%
    as_tibble(.)
  print(paste0("Models evaluated: n = ", nrow(ssf_fits_step_type_i)))
  compiled_ssf_fits_step_type_i <- ssf_fits_step_type_i  %>%
    group_by(model_chr) %>% # Precaution in case duplicate models exist in data
    slice(which.min(fit_aicc)) %>%
    ungroup() %>%
    arrange(fit_aicc) %>%
    mutate(preds = str_replace_all(glm_preds, "case \\~ \\-1 \\+ ", "")) %>%
    filter(!is.na(fit_aicc))
  print(paste0("Models with fits: n = ", nrow(compiled_ssf_fits_step_type_i)))
  saveRDS(compiled_ssf_fits_step_type_i, file.path(mod_fit_dir,
    "glm_model_fits_compiled",
    paste0("ssf_fits_compiled_", step_type_i, ".rds")))
}
rm(step_type_i, ssf_fits_step_type_i, compiled_ssf_fits_step_type_i)

################ COMPILE BEST FIT MODELS FOR EACH STEP TYPE  ###################

# Find the best fit models for each step_type
models_compiled <- list.files(path = file.path(mod_fit_dir,
    "glm_model_fits_compiled"), pattern = "^ssf_fits_compiled_*")  %>%
  map(~ readRDS(file.path(mod_fit_dir, "glm_model_fits_compiled", .))) %>%
  reduce(bind_rows) %>%
  group_by(step_type) %>%
  arrange(fit_aicc) %>%
  mutate(delta_aicc = fit_aicc - first(fit_aicc)) %>%
  #slice(1:10) %>%
  ungroup(.) %>%
  arrange(step_type)

############## FIND AND SAVE BEST FIT MODEL FOR EACH STEP TYPE #################

# Find the best fit models for each step_type
models_compiled_best <- models_compiled %>%
  group_by(step_type) %>%
  arrange(fit_aicc) %>%
  slice(which.min(fit_aicc)) %>%
  ungroup(.) %>%
  arrange(step_type)

saveRDS(models_compiled_best, file.path(mod_fit_dir,
  "glm_model_fits_compiled_best", "glm_models_compiled_best.rds"))

### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###

###################### SSF_Summarize_ClogitFits ################################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, tidyverse, ggthemes, glmulti, lubridate, optimx,
  purrr, reproducible, summarytools, survival, surveybootstrap)
library(baear, gisr)

# Output Directory
mod_fit_dir = "Output/Analysis/SSF/Models"

# Find all of the step_type folders in mod_fit_dir
step_types <- list.dirs(file.path(mod_fit_dir, "model_fits"),
  full.names = FALSE, recursive = FALSE)[c(3,6,10,14)]#[c(2,5,13)]

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
    #group_by(covar_matrix) %>% # Precaution in case duplicate models exist in data
    #slice(which.min(fit_aicc)) %>%
    #ungroup() %>%
    arrange(fit_aicc) %>%
    filter(!is.na(fit_aicc))
  print(paste0("Models with fits: n = ", nrow(compiled_ssf_fits_step_type_i)))
  saveRDS(compiled_ssf_fits_step_type_i, file.path(mod_fit_dir,
    "model_fits_compiled",
    paste0("ssf_fits_compiled_", step_type_i, ".rds")))
}
rm(step_type_i, ssf_fits_step_type_i, compiled_ssf_fits_step_type_i)

####### COMPILE TOP 10 AND BEST FIT MODELS FOR EACH STEP TYPE  #################

# Find the top 10 fit models for each step_type
model_fits_compiled_top_10 <- list.files(path = file.path(mod_fit_dir,
    "model_fits_compiled"), pattern = "^ssf_fits_compiled_*")  %>%
  map(~ readRDS(file.path(mod_fit_dir, "model_fits_compiled", .))) %>%
  reduce(bind_rows) %>%
  group_by(step_type) %>%
  arrange(fit_aicc) %>%
  mutate(delta_aicc = fit_aicc - first(fit_aicc)) %>%
  slice(1:10) %>%
  ungroup(.) %>%
  arrange(step_type)

saveRDS(model_fits_compiled_top_10, file.path(mod_fit_dir,
  "model_fits_compiled_best", "model_fits_compiled_top_10.rds"))

# Find the top models for each step_type
model_fits_compiled_best <- model_fits_compiled_top_10 %>%
  group_by(step_type) %>%
  arrange(fit_aicc) %>%
  slice(which.min(fit_aicc)) %>%
  ungroup(.) %>%
  arrange(step_type)

model_fits_compiled_best %>% pluck("model_full")

saveRDS(model_fits_compiled_best, file.path(mod_fit_dir,
  "model_fits_compiled_best", "model_fits_compiled_best.rds"))

### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###

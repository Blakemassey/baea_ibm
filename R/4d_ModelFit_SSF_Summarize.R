###################### ModelFit_SSF_Summarize ##################################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, plyr, dplyr, future, furrr, optimx, ggplot2,
  ggthemes, glmulti, lubridate, optimx, purrr, reproducible, rgenoud, stringr,
  survival, surveybootstrap, tibble, tictoc, tidyr) #spatialfil
pacman::p_load(baear, gisr, ibmr)
options(stringsAsFactors=FALSE)
setwd("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm")

source("R/4c_ModelFit_SSF_Optimize_Assets.R")

# Output Directory
mod_fit_dir = "Output/Analysis/SSF/Models"

# Source Data Directory
ua_data_dir <- "Output/Analysis/SSF/UA_Data"

# Find all of the step_type folders in fit_file_dir
step_types <- list.dirs(mod_fit_dir, full.names = FALSE, recursive = FALSE) %>%
  .[!. %in% c("Archive")]

# Compile all models for each step_type into a mods_sum file
for (i in seq_along(step_types)) {
  step_type_i <- step_types[i]
  fit_sum_step_type_i <- list.files(path = file.path(mod_fit_dir, step_type_i),
      pattern = paste0("df_opt_sum_", step_type_i, "*")) %>%
    #  pattern = paste0("df_fit_sum_", step_type_i, "*")) %>%
    map(~ readRDS(file.path(mod_fit_dir, step_type_i, .))) %>%
    reduce(bind_rows) %>%
    as_tibble(.) %>%
    arrange(fit_aic)%>%
    group_by(mod_formula) %>%
    slice(which.min(fit_aic)) %>%
    ungroup() %>%
    arrange(fit_aic) %>%
    mutate(covars_scale_sigmas = map2_chr(covars_scale, pars, paste0,
      collapse = " + ")) %>%
    mutate(covars_scale_sigmas = ifelse(covars_scale_sigmas == "NA", NA,
      covars_scale_sigmas)) %>%
    mutate(fixed_sigma = ifelse(!is.na(covars_fixed), 0, NA)) %>%
    mutate(covars_fixed_sigmas = map2_chr(covars_fixed, fixed_sigma, paste3)) %>%
    mutate(covars_fixed_sigmas = ifelse(covars_fixed_sigmas == "NA", NA,
      covars_fixed_sigmas)) %>%
    mutate(preds = map2_chr(.x = covars_scale_sigmas, .y = covars_fixed_sigmas,
      .f = paste2)) %>%
    select(step_type:peak_generation, preds)
  saveRDS(fit_sum_step_type_i, file.path(mod_fit_dir, paste0("fit_sum_",
    step_type_i, ".rds")))
}

# Find all of the step_type mods_sum in fit_file_dir
all_fit_sums <- list.files(path = file.path(mod_fit_dir),
    pattern = paste0("fit_sum_*"))  %>%
  map(~ readRDS(file.path(mod_fit_dir, .))) %>%
  reduce(bind_rows) %>%
  arrange(step_type, fit_aic)
saveRDS(all_fit_sums, file.path(mod_fit_dir, "all_fit_sums.rds"))

# Find all of the step_type mods_sum in fit_file_dir
best_fit_sums <- list.files(path = file.path(mod_fit_dir),
    pattern = paste0("fit_sum_*"))  %>%
  map(~ readRDS(file.path(mod_fit_dir, .))) %>%
  reduce(bind_rows) %>%
  group_by(step_type) %>%
  slice(which.min(fit_aic)) %>%
  ungroup() %>%
  arrange(step_type) %>%
  mutate(ua_steps = list(NA))
saveRDS(best_fit_sums, file.path(mod_fit_dir, "best_fit_sums.rds"))

all_fit_sums <- readRDS("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Output/Analysis/SSF/Models/all_fit_sums.rds")
best_fit_sums <- readRDS("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Output/Analysis/SSF/Models/best_fit_sums.rds")

ua_steps <- list.files(path = ua_data_dir,
  pattern = paste0("ua_steps_*")) %>%
  map(~ readRDS(file.path(ua_data_dir, .))) %>%
  reduce(bind_rows) %>%
  mutate(step_type = str_replace_all(behavior_behavior, " -> ", "_")) %>%
  mutate(step_type = str_to_lower(step_type))

colnames_alpha <- str_replace_all(colnames(ua_steps), "[0-9]", "")
colnames_num <- as.numeric(str_replace_all(colnames(ua_steps), "[^0-9]", ""))
covars <- unique(colnames_alpha %>% .[!. %in% c("dummy", "case", "step_id")])
colnames_tbl <- tibble(colnames_alpha, colnames_num) %>%
  mutate(colnames_sigma = ifelse(!is.na(colnames_num), colnames_num/30, NA)) %>%
  mutate_all(~str_replace_na(., "")) %>%
  mutate(colnames_final = paste0(colnames_alpha, colnames_sigma))
colnames(ua_steps) <- colnames_tbl %>% pull(colnames_final)
rm(colnames_alpha, colnames_num, colnames_tbl)

ua_steps_nested <- ua_steps %>%
  as_tibble(.) %>%
  group_by(step_type) %>%
  nest()

best_fit_sums_data <- best_fit_sums %>%
  left_join(., ua_steps_nested, by ="step_type") %>%
  rename(ua_steps = data)

# Re-checking model fits -------------------------------------------------------
# Directly recalculate the fit to ensure that the optimization and
# summarization worked as expected. Optimization result fit aic and
# recalculated fit aic should match.

# Function to be used in the next step using map()
FitClogit <- function(clogit_preds, ua_data){
  clogit_model_formula = as.formula(clogit_preds)
  clogit_fit <- clogit(clogit_model_formula, data = ua_data, method = "efron",
    iter.max = iter_max)
  return(clogit_fit)
}

# Use the best-aic predictors and original data to refit clogit model
best_fit_sums_refit <- best_fit_sums_data %>%
  mutate(clogit_preds = paste('case ~ ', preds, " + strata(step_id)")) %>%
  select(step_type, fit_aic, clogit_preds, ua_steps) %>%
  mutate(clogit_fit2 = map2(.x = clogit_preds, .y = ua_steps,
    .f = FitClogit)) %>%
  mutate(fit_aic2 = map_dbl(clogit_fit2, AIC))

# For each step_type best fit model, run an all subsets regression

# Method to redefine a fit function comes from the 'multiglm' package PDF
coxph_redefined = function(formula, data, always = "", ...) {
  coxph(as.formula(paste(deparse(formula), always)), data = data,
    method = "efron", ...)
}

# For each of best_fit_sums rows, run all-subset regression w/ ua_steps & preds
for (i in 1:nrow(best_fit_sums_data)){
  #i <- 3
  ua_steps_i <- best_fit_sums_data %>% select("ua_steps") %>% pluck(1, i) %>%
    mutate(dummy = 1)
  preds <- best_fit_sums %>% slice(i) %>% pull(preds)
  print(preds)
  fmla_full <- as.formula(paste("Surv(dummy, case) ~ ", preds))
  glmulti_coxph <-
    glmulti(fmla_full,
            data = ua_steps_i,
            level = 1,               # No interaction considered
            method = "h",            # Exhaustive approach
            crit = "aic",            # AIC as criteria
            confsetsize = 5,         # Keep 5 best models
            plotty = F, report = F,  # No plot or interim reports
            always = "+ strata(step_id)",
            fitfunction = coxph_redefined)   # coxph function

  ## Show 5 best models (Use @ instead of $ for an S4 object)
  glmulti_coxph@formulas
  print(glmulti_coxph@formulas[[1]])

}
  glmulti_coxph@formulas[[2]]

  ## Show result for the best model
  summary(glmulti_coxph@objects[[1]])

  coef(glmulti_coxph@objects[[1]])
  coef(glmulti_coxph@objects[[2]])
  coef(glmulti_coxph@objects[[3]])
  coef(glmulti_coxph@objects[[4]])
  coef(glmulti_coxph@objects[[5]])

  test <- glmulti_coxph@objects[[1]]
  summary(glmulti_coxph)

  ## Can get model weights:
  summary(glmulti_coxph)$modelweights

}

# glm approach
fit1 <- glm(case ~ 1 + developed100 + forest100 + open_water68 + pasture3 +
  shrub_herb29 + developed_dist0 + hydro_dist0 + turbine_dist0,
  family = 'binomial', data = ua_steps_i)
summary(fit1)

# survival clogit approach -- equivalent
fit2 <- clogit(case ~ developed100 + forest100 + open_water68 + pasture3 +
  shrub_herb29 + developed_dist0 + hydro_dist0 + turbine_dist0 + strata(step_id),
  data = ua_steps_i)
summary(fit2)

# survival coxph approach -- equivalent
fit3 <- coxph(Surv(dummy, case) ~ developed100 + forest100 + open_water68 +
  pasture3 + shrub_herb29 + developed_dist0 + hydro_dist0 + turbine_dist0 +
  strata(step_id), data = ua_steps_i)
summary(fit3)

#AIC equivalent
AIC(fit1,fit2,fit3)

### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###


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

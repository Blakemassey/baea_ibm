###################### ModelFit_SSF_Optimize ###################################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, plyr, dplyr, future, furrr, optimx, ggplot2,
  ggthemes, glmulti, lubridate, optimx, purrr, reproducible, rgenoud, stringr,
  survival, surveybootstrap, tibble, tictoc, tidyr) #spatialfil
pacman::p_load(baear, gisr, ibmr)
options(stringsAsFactors=FALSE)

#setwd("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm")

fit_file_dir = "Output/Analysis/Movements/SSF/Models"
ua_data_dir <- "Output/Analysis/Movements/SSF/UA_Data"
source("R/4c_ModelFit_SSF_Optimize_Assets.R")

################ OPTIMIZATION PROCEDURE OF KERNEL BANDWIDTH ####################

# Generate clusters (for parallel processing)
plan(multiprocess)

# Pull files
ua_files <- dir(ua_data_dir)

# Sequence through step_type used/available files and fit sigma optimization
for (i in seq_along(ua_files)[1]){
  step_type <- str_remove_all(ua_files[i], ("ua_steps_|.rds"))
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

  ## Filter ua_data to full set of keep_covars

  ua_steps_i_org <- readRDS(file.path(ua_data_dir, ua_files[i]))
  ua_steps_i <- ua_steps_i_org  %>%
    dplyr::select(c("case", "step_id"), starts_with(keep_covars)) %>%
    dplyr::select_if(function(x) !(all(is.na(x)) | all(x == ""))) %>%
    mutate(dummy = 1) %>%
    dplyr::select(dummy, everything())

  rm(start, end, keep_covars)

  colnames_alpha <- str_replace_all(colnames(ua_steps_i), "[0-9]", "")
  colnames_num <- as.numeric(str_replace_all(colnames(ua_steps_i), "[^0-9]", ""))
  covars <- unique(colnames_alpha %>% .[!. %in% c("dummy", "case", "step_id")]) # vector of covariates
  covars_sigma <- covars[!covars %in% all_fixed]
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

  for (i in 1:length(covars)){
    covar <- covars[i]
    covar_i <- str_subset(colnames(ua_steps_i), covar)
    covar_i_bandwidths <- as.numeric(str_replace_all(covar_i, "[^0-9]", ""))
    covar_domains[i, "covar"] <- covar
    covar_domains[i, "covar_min"] <- min(covar_i_bandwidths)
    covar_domains[i, "covar_max"] <- max(covar_i_bandwidths)
    covar_domains[i, "covar_median"] <- quantile(covar_i_bandwidths, p = 0.5,
      type = 1)
  }
  rm(covar, covar_i, covar_i_bandwidths, i)

  ## Create list of models
  list_of_models <- lapply(seq_along((covars)), function(n) {
      left_hand_side  <- "case"
      right_hand_side <- apply(X = combn(covars, n), MARGIN = 2, paste,
        collapse = " + ")
      paste(left_hand_side, right_hand_side, sep = "  ~  ")
  })
  vector_of_models <- unlist(list_of_models)
  covars_sigma_regex <- paste0("(", paste(covars_sigma, collapse = "|"), ")")
  covars_fixed_regex <- paste0("(", paste(covars_fixed, collapse = "|"), ")")

  #j <- 46  #2/2
  #j <- 21  #0/2
  #j <- 2   #1/0

  for (j in seq_along(vector_of_models)[131:133]){
    # determine sigma_variables
    model_j <- vector_of_models[j]
    mod_num <- str_pad(j, 4, "left", "0")
    # print(paste0("Model(", mod_num , "): ", model_j))
    covars_sigma_j <- str_extract_all(model_j, covars_sigma_regex) %>% unlist(.)
    covars_fixed_j <- str_extract_all(model_j, covars_fixed_regex) %>% unlist(.)

    # Find the proper SIGMA MATRIX, SIGMA STARTS, DOMAINS
    sigma_domains <- covar_domains %>% filter(covar %in% covars_sigma_j) %>%
      dplyr::select(covar_min, covar_max) %>% as.matrix(.)
    sigma_starts <- covar_domains %>% filter(covar %in% covars_sigma_j) %>%
      pull(covar_median)

    if(exists("pop_size")) rm(pop_size)
    if (length(covars_sigma_j) <= 1) pop_size = 100
    if (length(covars_sigma_j) == 2) pop_size = 500
    if (length(covars_sigma_j) == 3) pop_size = 5000
    if (length(covars_sigma_j) >= 4) pop_size = 20000

    OptimizeClogitSigma <- function(ua_data){
      sigma_n <- length(covars_sigma_j)
      domains <- sigma_domains # domains are min & max for each covariate
      starting_values <- sigma_starts # starting values for covariates
      parms <- starting_values
      opt_fit <- genoud(fn = FitClogitSigma, nvars = sigma_n,
        pop.size = pop_size, starting.values = starting_values,
        optim.method = "SANN", max.generations = max_generations,
        hard.generation.limit = FALSE, wait.generations = wait_generations,
        solution.tolerance = 0.0001, P5 = 0, P6 = 0, P7 = 0, P8 = 0,
        BFGSburnin = burnin_generations, print.level = 0,
        boundary.enforcement =2, ua_data = ua_data, data.type.int = TRUE,
        Domains = domains)
      return(opt_fit)
    }

    FitClogitSigma <- function(sigmas, ua_data){
      covars_sigma_j_sigmas <- paste0(covars_sigma_j, sigmas)
      covars_fixed_j_0 <- paste0(covars_fixed_j, "0")
      preds <- paste(c(covars_sigma_j_sigmas, covars_fixed_j_0,
        "strata(step_id)"), collapse = " + ")
      clogit_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
      clogit_fit <- clogit(clogit_model_formula, data = ua_data,
        method = "efron", iter.max = iter_max)
      model_aic = AIC(clogit_fit)
      return(model_aic)
    }

    # Fit Sigma Combo Models ---------------------------------------------------

    tic(paste0("Fit Model - ", mod_num))
    tryCatch({
      print(paste0("Starting at: ", now()))
      if (length(covars_sigma_j) == 0){ # not optimizing sigmas
        covars_fixed_j_0 <- paste0(covars_fixed_j, "0")
        preds <- paste(c(covars_fixed_j_0, "strata(step_id)"), collapse = " + ")
        clogit_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
        opt_fit_j <- clogit(clogit_model_formula, data = ua_steps_i,
          method = "efron", iter.max = iter_max)
        df_opt_sum_j <- tibble(step_type = step_type) %>%
          mutate(reps = 1) %>%
          mutate(rep_n = 1) %>%
          mutate(fit_aic = aic(opt_fit_j)) %>%
          mutate(covars_sigma = list(NA_character_)) %>%
          mutate(covars_fixed = list(covars_fixed_j)) %>%
          mutate(mod_formula = model_j) %>%
          mutate(opt_fit = list(opt_fit_j))
        saveRDS(df_opt_sum_j, file.path(fit_file_dir, paste0("df_opt_sum_",
          step_type, "_mod_", mod_num, "_", GetDateTime(), ".rds")))
        rm(covars_fixed_j_0, preds, clogit_model_formula, opt_fit_j,
          df_opt_sum_j)
      } else { # optimizing sigmas
        reps = 8 # number of times the optimization is run (done in parallel)
        ua_steps_j_lst <- tibble(
          step_type = step_type,
          model_covars = list(unique(c(covars_sigma_j, covars_fixed_j))),
          ua_data = list(ua_steps_i),
          count = reps) %>%
          uncount(count)
        tbl_opt_fit_j <- ua_steps_j_lst %>%
          mutate(opt_fit = future_map(ua_data, OptimizeClogitSigma, .progress =T))

        df_opt_sum_j <- tbl_opt_fit_j %>%
          dplyr::select(step_type, opt_fit) %>%
          mutate(reps = reps) %>%
          mutate(rep_n = 1:n()) %>%
          mutate(fit_aic = map_dbl(opt_fit, pluck, "value")) %>%
          mutate(covars_sigma = list(covars_sigma_j)) %>%
          mutate(covars_fixed = list(ifelse(length(covars_fixed_j) > 0,
            covars_fixed_j, NA_character_))) %>%
          mutate(pars = list(pluck(opt_fit, 1, "par"))) %>%
          mutate(mod_formula = model_j) %>%
          select(step_type, reps, rep_n, fit_aic, covars_sigma, covars_fixed,
            pars, opt_fit, mod_formula) %>%
          mutate(generations = map_dbl(opt_fit, pluck, "generations")) %>%
          mutate(peak_generation = map_dbl(opt_fit, pluck, "peakgeneration"))
        saveRDS(df_opt_sum_j, file.path(fit_file_dir, paste0("df_opt_sum_",
          step_type,"_mod_", mod_num, "_", GetDateTime(), ".rds")))
        rm(reps, ua_steps_j_lst, tbl_opt_fit_j, df_opt_sum_j)
      }},
      error = function(cond) {
        message(cond)
        return(NA)
      },
      warning=function(cond) {
        message(cond)
        if(exists("df_opt_sum_j")){
          saveRDS(df_opt_sum_j, file.path(fit_file_dir, paste0("df_opt_sum_",
            step_type, "_mod_", mod_num, "_warn_", GetDateTime(), ".rds")))
        }
        return(NULL)
      },
      finally={
        if(exists("model_j")) rm(model_j)
        if(exists("mod_num")) rm(mod_num)
        if(exists("covars_sigma_j")) rm(covars_sigma_j)
        if(exists("covars_fixed_j")) rm(covars_fixed_j)
        if(exists("sigma_domains")) rm(sigma_domains)
        if(exists("sigma_starts")) rm(sigma_starts)
        if(exists("df_opt_fit_j")) rm(df_opt_fit_j)
        if(exists("df_opt_sum")) rm(df_opt_sum)
    })
    toc()
    print(paste0("Ended at: ", GetDateTime()))
  }
  #if(exists("step_type")) rm(step_type)
  if(exists("covars")) rm(covars)
  if(exists("covars_fixed")) rm(covars_fixed)
  if(exists("covars_fixed_regex")) rm(covars_fixed_regex)
  if(exists("covars_sigma")) rm(covars_sigma)
  if(exists("covars_sigma_regex")) rm(covars_sigma_regex)
  if(exists("vector_of_models")) rm(vector_of_models)
}

df_opt_fits <- list.files(path = fit_file_dir, pattern = paste0("df_opt_sum_",
  step_type, "*")) %>%
  map(~ readRDS(file.path(fit_file_dir, .))) %>%
  reduce(bind_rows) %>%
  as_tibble(.) %>%
  arrange(fit_aic)%>%
  group_by(mod_formula) %>%
  slice(which.min(fit_aic)) %>%
  ungroup() %>%
  arrange(fit_aic) %>%
  mutate(covars_sigma_sigmas = map2_chr(covars_sigma, pars, paste0,
    collapse = " + ")) %>%
  mutate(covars_sigma_sigmas = ifelse(covars_sigma_sigmas == "NA", NA,
    covars_sigma_sigmas)) %>%
  mutate(fixed_sigma = ifelse(!is.na(covars_fixed), 0, NA)) %>%
  mutate(covars_fixed_sigmas = map2_chr(covars_fixed, fixed_sigma, paste3)) %>%
  mutate(covars_fixed_sigmas = ifelse(covars_fixed_sigmas == "NA", NA,
    covars_fixed_sigmas)) %>%
  mutate(preds = map2_chr(.x = covars_sigma_sigmas, .y = covars_fixed_sigmas,
    .f = paste2)) %>%
  select(step_type:peak_generation, preds)
View(df_opt_fits_sum)


future:::ClusterRegistry("stop")

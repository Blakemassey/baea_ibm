#!/usr/bin/env Rscript

#############################################################################
# ssf_optimize.R
# Optimizes step-selection function sigma values (scale)
#
# Authors: Blake Massey (USFWS)
#   email: blake_massey@fws.gov
# Created: 2020-03-25
#############################################################################

# ----------------------------------------------------------------------------
# Load command line arguments
#   ua_file_i = input data (e.g., ua_steps_cruise_cruise.rds )
#   range_start = input data (e.g., 1 )
#   range_end = input data (e.g., 200 )
# ----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 2) {
  ua_file_i <- args[1]
  range_group_i <- args[2]
} else {
  stop("Incorrect number of command line arguments.")
}

# check input file
if (!file.exists(ua_file_i)) {
  stop("The input file ", ua_file_i, " does not exist.")
}

# ----------------------------------------------------------------------------
# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, plyr, dplyr, ggplot2, lubridate,
    purrr, rgenoud, stringr, survival, tibble, tictoc, tidyr)
suppressMessages(suppressWarnings(require(pbdMPI)))
Sys.setenv(TZ = "America/New_York")
options(stringsAsFactors = FALSE)

# Set range_start and range_end

range_file <- "/home/bmassey/Assets/range_groups.rds"
tbl_range <- readRDS(range_file)

range_start <- tbl_range %>% filter(range_group == range_group_i) %>%
  pull(range_start)
range_end <- tbl_range %>% filter(range_group == range_group_i) %>%
  pull(range_end)

# check the range values
stopifnot(range_start > 0)
stopifnot(range_end > 0)
stopifnot(range_start <= range_end)

# initialize MPI
init()
rank <- comm.rank()

# ----------------------------------------------------------------------------
# Functions
# ----------------------------------------------------------------------------

GetDateTime <- function(){
  date_time <- paste0(date(now()), "_",
    str_pad(hour(now()), 2, "left", "0"),
    str_pad(minute(now()), 2, "left", "0"))
  return(date_time)
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
  if(length(covars_scale) == 4) pop_size = 20000
  if(length(covars_scale) == 5) pop_size = 50000
  if(length(covars_scale) >= 6) pop_size = 75000
  return(pop_size)
}

OptimizeClogitSigma <- function(covars_scale, covars_fixed, sigma_domains,
  sigma_starts, pop_size, model, mod_num){
  ua_data <- ua_steps_i
  sigma_n <- length(covars_scale)
  domains <- sigma_domains # domains are min & max for each covariate
  domains2 <- mapply(as.numeric, domains) # Fixes a FRUSTRATING bug!
  dim(domains2) <- dim(domains) # rgenoud NOW REQUIRES DOMAINS BE NUMERIC!
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
        Domains = domains2)
      return(opt_fit)
    }, error = function(cond) {
      message(cond)
      # opt_fit <- NA
      return(opt_fit)
    }, warning = function(cond) {
      # message(cond)
      opt_fit <- NA
      return(opt_fit)
    }, finally={})
  }
  toc()
  return(opt_fit)
}

FitSSFModel <- function(model_i){
  model_num <- names(models[which(unlist(models) == model_i)])
  print(paste0("Starting: ", model_num, " at ", GetDateTime()))
  covars_scale <- GetCovarsScale(model_i)
  covars_fixed <- GetCovarsFixed(model_i)
  sigma_domains <- GetSigmaDomains(covars_scale)
  sigma_starts <- GetSigmaStarts(covars_scale)
  pop_size <- GetPopSize(covars_scale)
  tic(paste0("Fit model: ", model_num, " (", model_i, ") "))
  opt_fit <- OptimizeClogitSigma(covars_scale, covars_fixed,
      sigma_domains, sigma_starts, pop_size, model_i, model_num)
  tbl_model <- tibble(
    step_type = step_type,
    model = model_i,
    model_num = model_num,
    covars_scale = list(covars_scale),
    covars_fixed = list(covars_fixed),
    sigma_domains = list(sigma_domains),
    sigma_starts = list(sigma_starts),
    pop_size = pop_size,
    opt_fit = list(opt_fit),
    fit_aicc = ExtractAICc(opt_fit %>% pluck(1))
  )
  toc()
  gc()
  print(paste0("Finished: ", model_num, " at ", GetDateTime()))
  return(tbl_model)
}

# ----------------------------------------------------------------------------
# Process
# ----------------------------------------------------------------------------

# Optimization parameters
max_generations = 25
wait_generations = 5
burnin_generations = 5
iter_max = 50

# Set up data
ua_steps_i_org <- readRDS(ua_file_i)
step_type <- ua_steps_i_org %>% slice(1) %>% pull(behavior_behavior) %>%
  str_replace(., " -> ", "_") %>% str_to_lower(.)

# Starting message
print(paste0("Starting batch script for ", step_type, " at: ", GetDateTime()))
step_type_split <- str_split(step_type, "_") %>% unlist(.)
start <- step_type_split[1]
end <- step_type_split[2]
rm(step_type_split)

# Get the right model covariates based on the step-type's end behavior
if (end %in% c("cruise", "flight")){
  keep_covars <- c("developed", "forest","open_water",
      "eastness", "northness", "wind_class", "tpi", "tri", "roughness",
      "developed_dist", "hydro_dist", "turbine_dist")
}
if (end %in% c("perch", "roost")){
  keep_covars <- c(
    "developed", "forest", "open_water", "pasture", "shrub_herb",
    "eastness", "northness", "wind_class", "tpi",
    "developed_dist", "hydro_dist", "turbine_dist")
}
all_fixed <- c("developed_dist", "hydro_dist", "turbine_dist")

# Filter ua_data to full set of keep_covars
ua_steps_i <- ua_steps_i_org  %>%
  dplyr::select(c("case", "step_id"), starts_with(keep_covars)) %>%
  dplyr::select_if(function(x) !(all(is.na(x)) | all(x == ""))) %>%
  mutate(dummy = 1) %>%
  dplyr::select(dummy, everything())
rm(start, end, keep_covars)

# Adjust column names to correlate to sigma (originial names are in meters)
colnames_alpha <- str_replace_all(colnames(ua_steps_i), "[0-9]", "")
colnames_num <- as.numeric(str_replace_all(colnames(ua_steps_i), "[^0-9]",""))
covars <- unique(colnames_alpha %>% .[!. %in% c("dummy", "case", "step_id")])
covars_scale <- covars[!covars %in% all_fixed]
covars_fixed <- covars[covars %in% all_fixed]
colnames_tbl <- tibble(colnames_alpha, colnames_num) %>%
  mutate(colnames_sigma = ifelse(!is.na(colnames_num), colnames_num/30, NA)) %>%
  mutate_all(~str_replace_na(., "")) %>%
  mutate(colnames_final = paste0(colnames_alpha, colnames_sigma))
colnames(ua_steps_i) <- colnames_tbl %>% pull(colnames_final)
rm(colnames_alpha, colnames_num, colnames_tbl)

# Generate the following: domains, sigma_starts, sigma_n
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

# Create list of models and convert to vector
# These steps are so the same the models are created, named, then subsetted.
# The sampling is done so more complex models are distributed across all ranks

list_of_models <- lapply(seq_along((covars)), function(n) {
  preds <- apply(X = combn(covars, n), MARGIN = 2, paste, collapse = " + ")
  paste("case", preds, sep = " ~ ") }) # 12 covars = 4095 models
models_flat <- flatten(list_of_models)
models_all <- set_names(models_flat, nm = str_pad(seq_along(models_flat), 4,
  "left", "0"))
models <- models_all[range_start:range_end]
gc()

# each core computes step-selection-function optimization on each chunk
result_parts <- pbdLapply(models, FitSSFModel, pbd.mode = "spmd")

# gather results
results_all <- pbdMPI::gather(result_parts, rank.dest = 0, unlist=FALSE)

Sys.sleep(10) # Problem with the code failing at: "finalize()etDateTime"

# shut down MPI
if (rank == 0) {
  tbl_models_fitted <- results_all %>% purrr::reduce(bind_rows) %>%
    purrr::reduce(bind_rows)
  saveRDS(tbl_models_fitted, paste0("/home/bmassey/Results/ssf_fit_",
    step_type, "_", str_pad(range_group_i, 3, "left", "0"), "_", GetDateTime(),
    ".rds"))
}

Sys.sleep(10) # Problem with the code failing at: "finalize()etDateTim

# Finalize
finalize()

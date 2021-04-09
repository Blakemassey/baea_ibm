###################### SSF_Model_Fit_Testing ###################################

########################### LOAD PACKAGES AND DATA  ############################
# Load packages, scripts, and input parameters
pacman::p_load(AICcmodavg, arrangements, plyr, dplyr, future, furrr, optimx,
  ggplot2, lubridate, optimx, purrr, raster, rgenoud, reproducible, sf, stars,
  stringr, survival, tibble, tictoc, tidyr, tmap, tmaptools, viridis,
  whitebox, xtable)
pacman::p_load(baear, gisr, ibmr)
whitebox::wbt_init() # required for WhiteboxTools to work
suppressMessages(extrafont::loadfonts(device="win"))
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE,
  memfrac = .85)
wbt_version() # check WhiteboxTools version

# Important: set new_fit to TRUE or set previous model_id
new_fit <- TRUE
model_id <- ifelse(new_fit, GetDateTime(), "")

testing <- FALSE
fit_models <- TRUE
archive_older_fits <- TRUE
save_individual_maps <- FALSE

# Set model_id to current date/time for new fits
model_file <- paste0("model_fits_best_", model_id, ".rds")
preds_file <- paste0("preds_tbl_", model_id, ".rds")

writeLines(paste0("Working on model: ", model_id))

# Theme (for LaTeX font)
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))

# Model directories
ua_data_dir <- "Output/Analysis/SSF/UA_Data"
mod_dir <- "Output/Analysis/SSF/Models"
mod_fit_dir <- file.path(mod_dir, "model_fits")
mod_best_dir <- file.path(mod_dir, "model_fits_best")

# Model files
fits_best_file <- file.path(mod_best_dir, model_file)
preds_tbl_file <- file.path(mod_best_dir, preds_file)

# SSF directories
input_dir <- "C:/ArcGIS/Data/R_Input/BAEA"
elev_file <- file.path(input_dir, "elev_30mc.tif")
kernel_dir <- file.path(input_dir, "SSF_Rasters/Processed_Sections/Kernel")
terrain_dir <- file.path(input_dir, "SSF_Rasters/Processed_Sections/Terrain")

# SSF directories
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"
covars_full_dir <- file.path(ssf_raster_dir, "Covars_Full")
covars_crop_dir <- file.path(ssf_raster_dir, "Covars_Crop")
ssf_value_dir <- file.path(ssf_raster_dir, "Step_Types")
ssf_prob_dir <- file.path(ssf_raster_dir, "Step_Types_Prob")
map_temp_dir <- "C:/TEMP/SSF_Maps"

# Maine files
maine_raster_trim_file <- "C:/ArcGIS/Data/BlankRaster/maine_trim.tif"
maine_polygon_file <- "C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp"

# Nests
nests_study_file <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/",
  "baea_ibm/Data/Nests/Nests_rds/nests_study.rds")

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

# Coordinate systems
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

## Set run parameters ----------------------------------------------------------

step_type_name = "sc"

step_types_df <- tribble(
   ~step_type_full_name,  ~step_type, ~step_type_index,
  "air -> cruise",        "ac",  c(1,4),
  "air -> flight",        "af",  c(2,5),
  "stationary -> cruise", "sc",  c(8,12),
  "stationary -> flight", "sf",  c(9,13,16),
  "air -> perch",         "ap",  c(3,6),
  "air -> roost",         "ar",  c(7),
  "stationary -> perch",  "sp",  c(10,14,17),
  "stationary -> roost",  "sr",  c(11,15)
)

step_type_index <- step_types_df %>%
  slice(which(step_types_df$step_type == step_type_name)) %>%
  pluck("step_type_index", 1)

covar_matrix_ac <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "eastness",       FALSE, TRUE, 1, 100, 50, FALSE,
  "roughness",      FALSE, TRUE, 1,  50, 25, FALSE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE,
  "dist_turbine",   TRUE, FALSE, NA, NA, NA, FALSE
)

covar_matrix_af <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "eastness",       FALSE, TRUE, 1, 100, 50, FALSE,
  "roughness",      FALSE, TRUE, 1,  50, 25, FALSE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE,
  "dist_turbine",   TRUE, FALSE, NA, NA, NA, FALSE
)

covar_matrix_sc_STABLE <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "forest",         FALSE, TRUE, 1, 100, 50, TRUE, # Added 2021-03-07
  "open_water",     FALSE, TRUE, 1, 100, 50, TRUE, # Added 2021-03-07
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE,
  "dist_turbine",   TRUE, FALSE, NA, NA, NA, FALSE
)

covar_matrix_sc <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "forest",         FALSE, TRUE, 1, 100, 50, TRUE, # KEEP
  "open_water",     FALSE, TRUE, 1, 100, 50, TRUE, # KEEP
  "wetland",        FALSE, TRUE, 1, 100, 50, FALSE, # GOOD FOR N->C
  "eastness",       FALSE, TRUE, 1, 100, 50, FALSE, # May work
  "northness",      FALSE, TRUE, 1, 100, 50, FALSE, # May work
  "wind_class",     FALSE, TRUE, 1, 100, 50, TRUE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE, # KEEP
  "dist_turbine",   TRUE, FALSE, NA, NA, NA, FALSE  # KEEP
)

covar_matrix_sf <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "open_water",     FALSE, TRUE, 1, 100, 50, FALSE,
  "road",           FALSE, TRUE, 1, 100, 50, FALSE,
  "shrub_herb",     FALSE, TRUE, 1, 100, 50, FALSE,
  "wetland",        FALSE, TRUE, 1, 100, 50, FALSE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE,
  "dist_turbine",   TRUE, FALSE, NA, NA, NA, FALSE
)

covar_matrix_ap <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "forest",         FALSE, TRUE, 1, 100, 50, TRUE,
  "open_water",     FALSE, TRUE, 1, 100, 50, FALSE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE
)

covar_matrix_sp <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "developed",      FALSE, TRUE, 1, 100, 50, FALSE,
  "open_water",     FALSE, TRUE, 1, 100, 50, FALSE,
  "road",           FALSE, TRUE, 1, 100, 50, FALSE,
  "wetland",        FALSE, TRUE, 1, 100, 50, FALSE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE
)

covar_matrix_ar <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "wetland",        FALSE, TRUE, 1, 100, 50, FALSE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE
)

covar_matrix_sr <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  "northness",      FALSE, TRUE, 1, 100, 50, FALSE,
  "open_water",     FALSE, TRUE, 1, 100, 50, TRUE,
  "tri",            FALSE, TRUE, 1,  50, 25, FALSE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE
)

# Covariates matrix
covar_matrix_all <- tribble(
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


## Start Workflow --------------------------------------------------------------

covar_matrix_i <- get(paste0("covar_matrix_", step_type_name))

writeLines(paste0("Step-types: ",
  paste0(str_replace_all(dir(ua_data_dir)[step_type_index],
  "ua_steps_|.rds", ""), collapse = ", ")))
writeLines(paste0("Covariates: ", paste0(covar_matrix_i$covar,
  ifelse(covar_matrix_i$poly2, "^2", ""), collapse =  ", ")))

# Functions
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

########################### OPTIMIZATION PROCEDURE  ############################

# Optimization parameters
max_generations <- ifelse(testing, 25, 25)
wait_generations = 5
burnin_generations = 5
iter_max = 50

# Pull files
ua_files <- dir(ua_data_dir)[step_type_index]

# Sequence through step_type used/available files and fit sigma optimization

if(fit_models){
  # Generate clusters (for parallel processing)
  if(!testing) plan(multisession)

  for (i in seq_along(ua_files)){
    ua_file_i <- ua_files[i]
    step_type <- str_remove_all(ua_file_i, ("ua_steps_|.rds"))
    writeLines(paste0("Starting: ", step_type, " (", i, " of ",
      length(ua_files), ")"))
    if(!dir.exists(file.path(mod_fit_dir, step_type))){
      dir.create(file.path(mod_fit_dir, step_type))
    }
    if(archive_older_fits){
      files_in_dir <- list.files(file.path(mod_fit_dir, step_type),
        pattern = "\\.rds$")
      writeLines(paste0("Models already in directory: ", files_in_dir))
      for (k in seq_len(length(files_in_dir))){
        file_k <- files_in_dir[k]
        file.move(file.path(mod_fit_dir, step_type, file_k),
          file.path(mod_fit_dir, step_type, "Archive"), overwrite = TRUE)
        Sys.sleep(2)
      }
    }
    start <- str_split(step_type, "_") %>% unlist(.) %>% pluck(1)
    end <- str_split(step_type, "_") %>% unlist(.) %>% pluck(2)

    covar_matrix_i_combos <- do.call(c, lapply(seq_len(nrow(covar_matrix_i)),
      function(y) {arrangements::combinations(nrow(covar_matrix_i), y,
        layout = "l")}))

    ## Filter ua_data to full set of keep_covars
    ua_steps_i_org <- readRDS(file.path(ua_data_dir, ua_file_i))
    ua_steps_i <- PrepUAStepDataForOptimization(ua_steps_i_org,
      covar_matrix_i$covar)
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
        dim(domains_num) <- dim(domains) # rgenoud REQUIRES DOMAINS BE NUMERIC!
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

    writeLines(paste0("Starting ", step_type, " : ", now()))
    writeLines(paste0("Assessing ", nrow(tbl_models), " models"))

    tbl_models_fitted_list <- list()
    for (j in unique(model_grp)) {
      writeLines(paste0("\nModel group: ", j, " of ",length(unique(model_grp))))
      tbl_models_fitted_j <- tbl_models %>%
        filter(model_grp == j)  %>%
        mutate(opt_fit = future_pmap(.l = list(covar_matrix, model_num),
          .f = possibly(OptimizeClogitSigma, otherwise = NA_real_),
          .progress = TRUE, .options = furrr_options(seed = TRUE))) %>%
        mutate(fit_aicc = map_dbl(opt_fit, ExtractAICc))
      tbl_models_fitted_list[[j]] <- tbl_models_fitted_j
    }
    tbl_models_fitted <- tbl_models_fitted_list %>%
      reduce(bind_rows)

    models_all <- tbl_models_fitted %>%
      arrange(fit_aicc) %>%
      mutate(delta_aicc = fit_aicc - first(fit_aicc)) %>%
      filter(!is.na(fit_aicc)) %>%
      mutate(peak_gen = map2(opt_fit, "peakgeneration", pluck)) %>%
      mutate(opt_scales = map2(opt_fit, "par", pluck)) %>%
      mutate(covar_matrix = map2(covar_matrix, opt_scales, AddScaleColumn)) %>%
      mutate(model_formula = map_chr(covar_matrix, ExtractModelFormula)) %>%
      mutate(clogit_fit = map(model_formula, RunClogitFit)) %>%
      mutate(clogit_xtable = map(clogit_fit, xtable)) %>%
      mutate(clogit_fit_tbl = map(clogit_xtable, as_tibble)) %>%
      mutate(covar_fitted = map(clogit_fit, ExtractCovarFitted)) %>%
      mutate(refit_aicc = map_dbl(clogit_fit, AICc)) %>%
      mutate(concordance_value = pmap_dbl(list(clogit_fit, "concordance",
        "concordance"), pluck)) %>%
      mutate(fit_covars_clean = map(covar_fitted, pull, "covar_clean")) %>%
      mutate(fit_coefs_signif = map(covar_fitted, pull, "coef_signif")) %>%
      mutate(model_full = map(covar_fitted, ExtractModelFull)) %>%
      dplyr::select(step_type, covar_matrix, clogit_fit_tbl, fit_aicc,
        delta_aicc, refit_aicc, peak_gen, concordance_value, covar_fitted,
        fit_covars_clean, fit_coefs_signif, model_full)

    # Find the top 10 and best fit models for each step_type
    models_top_10 <- models_all %>%
      dplyr::select(-step_type) %>% # Not needed b/c unnest() keeps step_type
      arrange(fit_aicc) %>%
      slice(1:10)

    models_best <- models_all %>%
      dplyr::select(-step_type) %>% # Not needed b/c unnest() keeps step_type
      arrange(fit_aicc) %>%
      slice(1)

    writeLines(paste0("\nFinished ", step_type, " at ", now()))
    writeLines(paste("Best model:", models_best %>% pluck("model_full"), "\n"))

    tbl_fit_final <- models_all %>%
      group_by(step_type) %>%
      nest() %>%
      dplyr::rename(models_all = data) %>%
      mutate(models_n = length(covar_matrix_i_combos)) %>%
      mutate(covars_all = list(covar_matrix_i)) %>%
      mutate(models_top_10 = list(models_top_10)) %>%
      mutate(models_best = list(models_best)) %>%
      dplyr::select(step_type, models_n, covars_all, models_all, models_top_10,
        models_best)
    # Save output
    saveRDS(tbl_fit_final, file.path(mod_fit_dir, step_type,
      paste0("ssf_fit_", step_type, "_", model_id, ".rds")))
    rm(models_all, models_best, models_top_10, tbl_fit_final, tbl_models,
      tbl_models_fitted, tbl_models_fitted_j, tbl_models_fitted_list,
      ua_steps_i, ua_steps_i_org, j, model_grp, step_type, ua_file_i)
    gc()
  }
  future:::ClusterRegistry("stop")
  rm(covar_matrix_all, covar_matrix_i)
  rm(burnin_generations, files_in_dir, i, iter_max, max_generations,
    ua_files, wait_generations)
}
gc()

############################## COMPILE BEST MODELS #############################

# Find all of the step_type folders in mod_fit_dir
step_types <- list.dirs(file.path(mod_fit_dir), full.names = FALSE,
  recursive = FALSE)[step_type_index]

# Compile best models for each step_type into a model_best_fit file
for (i in seq_len(length(step_types))){
  step_type_i <- step_types[i]
  writeLines(paste0(step_type_i, " (", i, " of ", length(step_types), ")"))
  ssf_fits_best_step_type_i <- list.files(path = file.path(mod_fit_dir,
    step_type_i), pattern = paste0("ssf_fit_",
        step_type_i, "*")) %>%
    map(~ readRDS(file.path(mod_fit_dir, step_type_i, .))) %>%
    reduce(bind_rows) %>%
    as_tibble(.) %>%
    dplyr::select(step_type, covars_all, models_best) %>%
    unnest(models_best) %>%
    arrange(fit_aicc) %>%
    slice(which.min(fit_aicc))
  if(i == 1){
    ssf_fits_best <- ssf_fits_best_step_type_i
  } else {
    ssf_fits_best <- bind_rows(ssf_fits_best, ssf_fits_best_step_type_i)
  }
}
rm(step_type_i, ssf_fits_best_step_type_i)

saveRDS(ssf_fits_best, fits_best_file)

######################### CREATE SSF LAYERS ####################################

# Load libraries, scripts, and input parameters
## Get SSF FITS ----------------------------------------------------------------

ssf_fits_best_org <- readRDS(fits_best_file) #%>% slice(c(step_type_index))
ssf_fits_best_org %>% dplyr::select(step_type, fit_covars_clean, model_full)
ssf_fits_best <- ssf_fits_best_org

ssf_fits_best %>% pluck("model_full") %>% unlist()

# Determine all the raster_sigma layers
preds_all <- unlist(ssf_fits_best$fit_covars_clean) %>% str_remove_all("\\^2")
preds_unique <- unique(preds_all)
preds_rasters <- unique(str_replace_all(preds_unique, "[0-9]", ""))

# Raster classes (used in next step)
raster_classes <- c(developed = "kernel_class",
  forest = "kernel_class",
  open_water = "kernel_class",
  pasture = "kernel_class",
  shrub_herb = "kernel_class",
  wetland = "kernel_class",
  road = "kernel_class",
  eastness = "kernel_class",
  northness = "kernel_class",
  wind_class = "kernel_class",
  developed_dist = "extract_class",
  hydro_dist = "extract_class",
  turbine_dist = "extract_class",
  road_dist = "extract_class",
  tpi = "terrain_class",
  tri = "terrain_class",
  roughness = "terrain_class")

preds_tbl <- tibble(preds_unique) %>%
  mutate(sigma = as.integer(str_extract(preds_unique, "[0-9]{1,3}"))) %>%
  mutate(covar = str_replace_all(preds_unique, "[0-9]", "")) %>%
  dplyr::select(covar, sigma) %>%
  mutate(covar_sigma = paste0(covar, sigma)) %>%
  mutate(raster_class = recode(covar, !!!raster_classes)) %>%
  mutate(raster_layer = vector(mode = "list", length = nrow(.))) %>%
  arrange(covar, sigma)

saveRDS(preds_tbl, preds_tbl_file)

################ CREATE COVARIATE RASTERS FOR MAINE ############################

preds_tbl <- readRDS(preds_tbl_file)
elev_org <- raster(elev_file) # all other layers' extent are set to this layer

x_min <- xmin(elev_org)
x_max <- xmax(elev_org)
y_min <- ymin(elev_org)
y_max <- ymax(elev_org)
y_mid <- (ymax(elev_org) + ymin(elev_org))/2
y_half <- (ymax(elev_org) - ymin(elev_org))*(1/2)

ext_top <- extent(x_min, x_max, y_min + y_half, y_max)
ext_mid <- extent(x_min, x_max, y_mid - 100000, y_mid + 100000)
ext_mid_crop <- extent(x_min, x_max, y_mid - 50000, y_mid + 50000)
ext_bot <- extent(x_min, x_max, y_min, y_max - y_half)

removeTmpFiles(h=0)

# Start of Kernel Rasters ------------------------------------------------------
preds_kernel <- preds_tbl %>%
  filter(raster_class == "kernel_class")

for (i in 1:nrow(preds_kernel)){
  preds_kernel_i <- preds_kernel %>% slice(i)
  sigma <- preds_kernel_i %>% pull(sigma)
  covar <- preds_kernel_i %>% pull(covar)
  writeLines(paste0("Checking: ", covar, sigma))
  out_crop_file <- file.path(covars_crop_dir, paste0(covar, sigma, ".tif"))
  if(!file.exists(out_crop_file)){
    raster_file <- file.path(input_dir, paste0(covar, "_30mc.tif"))
    writeLines(paste0("Needed: ", covar, sigma))
  }
}

for (i in seq_len(nrow(preds_kernel))){
  preds_kernel_i <- preds_kernel %>% slice(i)
  sigma <- preds_kernel_i %>% pull(sigma)
  covar <- preds_kernel_i %>% pull(covar)
  writeLines(paste0("Checking: ", covar, sigma))
  out_full_file <- file.path(covars_full_dir, paste0(covar, sigma, ".tif"))
  out_crop_file <- file.path(covars_crop_dir, paste0(covar, sigma, ".tif"))
  if(!file.exists(out_crop_file)){
    raster_file <- file.path(input_dir, paste0(covar, "_30mc.tif"))
    writeLines(paste0("Starting: ", covar, sigma, " (", i, " of ",
      nrow(preds_kernel), ") at: ", lubridate::now()))
    if(sigma == 0){
      out_raster <- raster(raster_file)
      # Write out raster
      writeRaster(out_raster, out_full_file, format = "GTiff", overwrite = TRUE)
    } else {
      top_file <- file.path(kernel_dir, paste0(covar, sigma, "_top.grd"))
      mid_file <- file.path(kernel_dir, paste0(covar, sigma, "_mid.grd"))
      bot_file <- file.path(kernel_dir, paste0(covar, sigma, "_bot.grd"))
      # Covar TOP
      writeLines("top")
      covar_top <- crop(raster(raster_file), ext_top, snap = "near")
      covar_top_smooth <- raster::raster(covar_top) # creates blank raster
      values(covar_top_smooth) <-
        smoothie::gauss2dsmooth(raster::as.matrix(covar_top),
        lambda = sigma, nx = DescTools::RoundTo(nrow(covar_top), 2),
        ny = DescTools::RoundTo(ncol(covar_top), 2))
      writeRaster(covar_top_smooth, top_file, format = "raster",
        overwrite = TRUE)
      rm(covar_top, covar_top_smooth)
      gc()
      # Covar MID
      writeLines("mid")
      covar_mid <- crop(raster(raster_file), ext_mid, snap = "near")
      covar_mid_smooth <- raster::raster(covar_mid) # creates blank raster
      values(covar_mid_smooth) <-
        smoothie::gauss2dsmooth(raster::as.matrix(covar_mid),
        lambda = sigma, nx = DescTools::RoundTo(nrow(covar_mid), 2),
        ny = DescTools::RoundTo(ncol(covar_mid), 2))
      covar_mid_smooth <- crop(covar_mid_smooth, ext_mid_crop)
      writeRaster(covar_mid_smooth, mid_file, format = "raster",
        overwrite = TRUE)
      rm(covar_mid, covar_mid_smooth)
      gc()
      # Covar BOTTOM
      writeLines("bottom")
      covar_bot <- crop(raster(raster_file), ext_bot, snap = "near")
      covar_bot_smooth <- raster::raster(covar_bot) # creates blank raster
      values(covar_bot_smooth) <-
        smoothie::gauss2dsmooth(raster::as.matrix(covar_bot),
        lambda = sigma, nx = DescTools::RoundTo(nrow(covar_bot), 2),
        ny = DescTools::RoundTo(ncol(covar_bot), 2))
      writeRaster(covar_bot_smooth, bot_file, format = "raster",
        overwrite = TRUE)
      rm(covar_bot, covar_bot_smooth)
      gc()
      # Merge three rasters
      top_raster <- raster(top_file)
      mid_raster <- raster(mid_file)
      bot_raster <- raster(bot_file)
      out_raster <- raster::merge(mid_raster, top_raster, bot_raster)
      # Write out merged raster
      writeRaster(out_raster, out_full_file, format = "GTiff", overwrite = TRUE)
      rm(top_file, mid_file, bot_file, top_raster, mid_raster, bot_raster)
    }
  }
}

# Start of Terrain Rasters -----------------------------------------------------

elev_org <- raster(elev_file) # all other layers' extent are set to this layer

x_min <- xmin(elev_org)
x_max <- xmax(elev_org)
y_min <- ymin(elev_org)
y_max <- ymax(elev_org)
y_mid <- (ymax(elev_org) + ymin(elev_org))/2
y_half <- (ymax(elev_org) - ymin(elev_org))*(1/2)

ext_top <- extent(x_min, x_max, y_min + y_half, y_max)
ext_mid <- extent(x_min, x_max, y_mid - 100000, y_mid + 100000)
ext_mid_crop <- extent(x_min, x_max, y_mid - 50000, y_mid + 50000)
ext_bot <- extent(x_min, x_max, y_min, y_max - y_half)

removeTmpFiles(h=0)

CalculateTerrainMetricWithSigma <- function(sigma, metric, ext, elev =elev_org){
  if(sigma == 0) sigma <- 1
  size <- (sigma*2) + 1
  x <- crop(elev, ext)
  weight_matrix <- matrix(1, nrow = size, ncol = size)
  center <- ceiling(0.5 * length(weight_matrix))
  window <- length(weight_matrix)-1
  if (metric == "tri"){
    tri <- focal(x, w = weight_matrix,
      fun = function(x, ...) sum(abs(x[-center] - x[center]))/window,
      pad = TRUE, padValue = NA)
      out_matrix <- tri
  } else if (metric == "tpi"){
    tpi <- focal(x, w = weight_matrix,
      fun = function(x, ...) x[center] - mean(x[-center]),
      pad = TRUE, padValue = NA)
    out_matrix <- tpi
  } else if (metric == "roughness"){
    rough <- focal(x, w = weight_matrix,
      fun = function(x, ...) max(x) - min(x),
      pad = TRUE, padValue = NA, na.rm = TRUE)
    out_matrix <- rough
  } else {
    stop("'metric' must equal 'tpi', 'tri', or 'roughness'", call. = FALSE)
  }
  return(out_matrix)
}

preds_terrain <- preds_tbl %>%
  filter(raster_class == "terrain_class")

for (i in seq_len(nrow(preds_terrain))){
  preds_terrain_i <- preds_terrain %>% slice(i)
  sigma <- preds_terrain_i %>% pull(sigma)
  covar <- preds_terrain_i %>% pull(covar)
  writeLines(paste0("Checking: ", covar, sigma))
  out_file <- file.path(covars_crop_dir, paste0(covar, sigma, ".tif"))

  if(!file.exists(out_file)){
    writeLines(paste0("Needed: ", covar, sigma))
  }
}

for (i in seq_len(nrow(preds_terrain))){
  preds_terrain_i <- preds_terrain %>% slice(i)
  sigma <- preds_terrain_i %>% pull(sigma)
  covar <- preds_terrain_i %>% pull(covar)
  writeLines(paste0("Checking: ", covar, sigma))
  out_crop_file <- file.path(covars_crop_dir, paste0(covar, sigma, ".tif"))
  out_full_file <- file.path(covars_full_dir, paste0(covar, sigma, ".tif"))

  if(!file.exists(out_crop_file)){
    writeLines(paste0("Starting: ", covar, sigma, " (", i, " of ",
      nrow(preds_terrain), ") at: ", lubridate::now()))

    top_file <- file.path(terrain_dir, paste0(covar, sigma, "_top.grd"))
    mid_file <- file.path(terrain_dir, paste0(covar, sigma, "_mid.grd"))
    bot_file <- file.path(terrain_dir, paste0(covar, sigma, "_bot.grd"))

    # Covar TOP
    writeLines(paste0("Starting top at: ", lubridate::now()))
    covar_i_top <- CalculateTerrainMetricWithSigma(sigma, covar, ext_top,
      elev_org)
    writeRaster(covar_i_top, top_file, format = "raster", overwrite = TRUE)
    rm(covar_i_top)
    gc()

    # Covar MID
    writeLines(paste0("Starting mid at: ", lubridate::now()))
    covar_i_mid <- CalculateTerrainMetricWithSigma(sigma, covar, ext_mid,
      elev_org)
    covar_i_mid_crop <- crop(covar_i_mid, ext_mid_crop)
    writeRaster(covar_i_mid_crop, mid_file, format = "raster", overwrite = TRUE)
    rm(covar_i_mid)
    gc()

    # Covar BOT
    writeLines(paste0("Starting bot at: ", lubridate::now()))
    covar_i_bot <- CalculateTerrainMetricWithSigma(sigma, covar, ext_bot,
      elev_org)
    writeRaster(covar_i_bot, bot_file, format = "raster", overwrite = TRUE)
    rm(covar_i_bot)
    gc()

     # Merge together rasters
    top_raster <- raster(top_file)
    mid_raster <- raster(mid_file)
    bot_raster <- raster(bot_file)
    out_raster <- raster::merge(mid_raster, top_raster, bot_raster)

    # Write out merged raster
    writeRaster(out_raster, out_full_file, format = "GTiff", overwrite = TRUE)

    rm(top_raster, mid_raster, bot_raster, top_file, mid_file, bot_file)

  }
}

################### MASK COVARIATE RASTERS TO MAINE ONLY #######################

preds_tbl <- readRDS(preds_tbl_file)
maine_raster_trim <- raster(maine_raster_trim_file)

for (i in seq_len(nrow(preds_tbl))){
  preds_tbl_i <- preds_tbl %>% slice(i)
  covar_sigma <- preds_tbl_i %>% pull(covar_sigma)
  writeLines(paste0("Starting: ", covar_sigma, " (", i, " of ", nrow(preds_tbl),
    ") at: ", lubridate::now()))

  covar_file <- file.path(covars_full_dir, paste0(covar_sigma, ".tif"))
  out_file <- file.path(covars_crop_dir, paste0(covar_sigma, ".tif"))

  if(!file.exists(out_file)){
    writeLines(paste0("Writing: ", covar_sigma, " at: ", lubridate::now()))
    covar_raster <- raster(covar_file)
    covar_raster_crop <- crop(covar_raster, maine_raster_trim)
    covar_raster_mask <- mask(covar_raster_crop, maine_raster_trim)
    writeRaster(covar_raster_mask, out_file, format = "GTiff", overwrite = TRUE)
  }
}

################### GENERATE SSF_LAYERS FOR MAINE ##############################

ssf_fits_best <- readRDS(fits_best_file) #%>% slice(step_type_index)
ssf_fits_best %>% pluck("model_full") %>% unlist()

maine_raster_trim <- raster(maine_raster_trim_file)

# Original
# Generate layer for each ssf based on original fits
for (i in 1:nrow(ssf_fits_best)){
  step_type_i <- ssf_fits_best %>% slice(i) %>% pull(step_type)
  writeLines(paste0("Creating SSF Layer for: ", step_type_i))

  covars_i <- ssf_fits_best %>% slice(i) %>% pluck("covar_fitted", 1) %>%
    pull("covar_clean") %>% str_remove_all("\\^2") %>% unique()

  # Create Raster_Brick
  covars_list <- vector(mode = "list", length = length(covars_i))
  for (j in seq_along(covars_i)){
    covars_i_j <- covars_i[j]
    writeLines(paste0("covariates: ", covars_i_j))
    raster_file <- file.path(covars_crop_dir, paste0(covars_i_j, ".tif"))
    covars_list[[j]] <- raster(raster_file)
    writeLines(paste0(extent(covars_list[[j]])))
  }
  covars_brick <- raster::brick(covars_list)
  rm(covars_list)

  # Generate formula

  covars_clean_i <- ssf_fits_best %>% slice(i) %>% pluck("covar_fitted", 1) %>%
    pull("covar_clean")

  covars_i <- ifelse(str_detect(covars_clean_i, "\\^2"),
    paste0("covars_brick[['", str_remove_all(covars_clean_i, "\\^2"), "']]^2"),
    paste0("covars_brick[['", covars_clean_i, "']]"))

  coefs_i <- ssf_fits_best %>% slice(i) %>% pluck("covar_fitted", 1) %>%
    pull("coef_signif")

  # Generate formulas
  ssf_formula <- paste0("(", paste0(paste0(coefs_i, "*",covars_i),
    collapse = ") + ("), ")")
  writeLines(ssf_formula)

  # Create value raster, then crop and mask
  ssf_value_raster <- eval(parse(text = ssf_formula))
  #plot(ssf_value_raster, main = step_type_i)

  # Calculate probability
  ssf_prob_raster <- raster::calc(ssf_value_raster, fun = boot::inv.logit)
  #plot(ssf_prob_raster, main = step_type_i)
  #hist(ssf_prob_raster)

  # Write Rasters to output dir
  step_type_i_numeric <- step_type_i %>% str_replace_all(c("cruise" = "1",
    "flight" = "2", "nest" = "3", "perch" = "4", "roost" = "5"))
  writeRaster(ssf_value_raster, file.path(ssf_raster_dir, "Step_Types",
    step_type_i_numeric), format = "GTiff", overwrite = TRUE)
  writeRaster(ssf_prob_raster, file.path(ssf_prob_dir, step_type_i_numeric),
    format = "GTiff", overwrite = TRUE)
  rm(coefs_i, covars_brick, raster_file, ssf_formula, ssf_value_raster,
    ssf_prob_raster, step_type_i_numeric)
  gc()
}


######################### GENERATE MAPS FOR MAINE ##############################

#### ------------------------- Maine By Step_Type Maps -------------------------

ssf_fits_best <- readRDS(fits_best_file)

# Maine Outline
maine <- read_sf(maine_polygon_file) %>% st_transform(., crs = 4326) %>%
  mutate(state = "Maine") %>% dplyr::select(state)

# Nests
nests_study <- st_as_sf(x = readRDS(nests_study_file), coords = c("long","lat"),
  crs = "+proj=longlat +datum=WGS84") %>% filter(!name %in% c("Davis", "Upper"))

# For Individual Maps
if(save_individual_maps){
  for (i in seq_len(nrow(ssf_fits_best))){
    writeLines(i)
    step_type_i_numeric <- ssf_fits_best %>% slice(i) %>% pull(step_type) %>%
      str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
        "perch" = "4", "roost" = "5"))
    ssf_prob_file <- list.files(ssf_prob_dir, pattern =
      paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
    ssf_prob_i <- read_stars(ssf_prob_file)
    if (i ==  1){
      # Use "Tmap_baselayers.R" script to get other baselayers
      maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1.15,
        width = 1.35))
      maine_om = read_osm(maine_bb_sf, zoom = 7, minNumTiles = 9, type =
        om_nat_geo)
    }
    step_type_i_text <- step_type_i_numeric %>%
      str_replace_all("1", "Cruise") %>%
      str_replace_all("2", "Flight") %>%
      str_replace_all("3", "Nest") %>%
      str_replace_all("4", "Perch") %>%
      str_replace_all("5", "Roost")
    step_type_i_arrow <- step_type_i_text %>%
      str_replace_all("_", "$\\\\rightarrow$ ") %>%
      latex2exp::TeX(.)
    ssf_prob_i_map <-
      tm_shape(maine_om) +
        tm_rgb() +
      tm_shape(ssf_prob_i, raster.downsample = FALSE) +
        tm_raster(palette = viridis(20, direction = 1),
          alpha = .8,
          legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_scale_bar(breaks = c(0, 50, 100), text.size = .75,
        position = c(.75, .01)) +
      tm_compass(type = "4star",  show.labels = 1, size = 3,
        position = c(.85, .75)) +
      tm_shape(maine) + tm_borders(col = "black") +  # ME outline overlays grid
      tm_shape(nests_study %>% filter(nest_site != "446R01")) +
      tm_symbols(shape = 8, col = "black", size = .4) +
      tm_layout(#asp = .75,
        title.bg.color = "white",
        title.position = c("left", "top"),
        title.fontfamily = "Latin Modern Roman",
        title = step_type_i_arrow,
        #title.size = ,
        title.snap.to.legend = FALSE,
        legend.position = c(.80,.10),
        legend.outside = FALSE,
        legend.bg.color = "white",
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman",
        frame = FALSE) #+ tm_credits(step_type_arrow, position=c("right","top"))
    tmap_save(tm = ssf_prob_i_map, filename = file.path(map_temp_dir,
      paste0("SSF_Probability_Map_", step_type_i_text, ".svg")), unit = "in",
      dpi = 300, height = 8, width = 6)
  }
}

#### -------------------- All Maine Step_Type Maps Combined --------------------

# List for output
ssf_tmap_list <- vector(mode = "list", length = 20)

# For Individual Maps
for (i in seq_len(nrow(ssf_fits_best))){
  #writeLines(i)
  step_type_i_numeric <- ssf_fits_best %>% slice(i) %>% pull(step_type) %>%
    str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
      "perch" = "4", "roost" = "5"))
  ssf_prob_file <- list.files(ssf_prob_dir, pattern =paste0(step_type_i_numeric,
    "\\.tif$"), full.names = TRUE)
  ssf_prob_i <- read_stars(ssf_prob_file)
  step_type_i_text <- step_type_i_numeric %>%
    str_replace_all("1", "Cruise") %>%
    str_replace_all("2", "Flight") %>%
    str_replace_all("3", "Nest") %>%
    str_replace_all("4", "Perch") %>%
    str_replace_all("5", "Roost")
  step_type_i_arrow <- step_type_i_text %>%
    str_replace_all("_", "$\\\\rightarrow$ ") %>%
    latex2exp::TeX(.)
  ssf_prob_i_map <-
    tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1),
        legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_layout(asp = .75,
        title.position = c("LEFT", "TOP"),
        title.fontfamily = "Latin Modern Roman",
        title = step_type_i_arrow,
        title.size = .5,
        title.snap.to.legend =  FALSE,
        legend.position = c("RIGHT", "BOTTOM"),
        legend.height = .3,
        legend.title.size = .65,
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman",
        frame = FALSE)
  ssf_prob_i_map
  tmap_position <- switch(step_type_i_numeric,
    "1_1" = 1,  "1_2" = 2,  "1_4" = 3,
    "2_1" = 5,  "2_2" = 6,  "2_4" = 7,  "2_5" = 8,
    "3_1" = 9,  "3_2" = 10, "3_4" = 11, "3_5" = 12,
    "4_1" = 13, "4_2" = 14, "4_4" = 15, "4_5" = 16,
                "5_2" = 18, "5_4" = 19)
  writeLines(as.character(tmap_position))
  ssf_tmap_list[[tmap_position]] <- ssf_prob_i_map
}

tmap_blank <-
  tm_shape(ssf_prob_i, raster.downsample = TRUE) +
  tm_raster(palette = "white", style = "cont") +
  tm_layout(asp = .75, legend.show = FALSE, frame = FALSE)

for (i in seq_len(length(ssf_tmap_list))){
  if(is.null(ssf_tmap_list[[i]])) ssf_tmap_list[[i]] <- tmap_blank
}

# TEST arrangement and position of main.title, legend, etc.
test_map_arrangement <- FALSE
if (test_map_arrangement){
  ssf_tmap_arrange_test <- tmap_arrange(
    tmap_blank, tmap_blank, tmap_blank, tmap_blank,
    tmap_blank, tmap_blank, tmap_blank, tmap_blank,
    tmap_blank, tmap_blank, ssf_tmap_list[[1]], tmap_blank,
    tmap_blank, tmap_blank, tmap_blank, tmap_blank,
    tmap_blank, tmap_blank, tmap_blank, tmap_blank,
    ncol = 4)
  ssf_tmap_arrange_test
  tmap_save(tm = ssf_tmap_arrange_test, filename =  file.path(
    "C:/TEMP/SSF_Maps/", paste0("SSF_Probability_Maps_Overview_",
    model_id, ".svg")), unit = "in", dpi = 300, height = 8, width = 6)
}

# Arrange map of probability surfaces
ssf_tmap_arrange <- tmap_arrange(
  ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]],
  ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
  ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
  ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
  ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
  ssf_tmap_list[[16]], ssf_tmap_list[[17]], ssf_tmap_list[[18]],
  ssf_tmap_list[[19]],ssf_tmap_list[[20]], ncol = 4)

tmap_save(tm = ssf_tmap_arrange, filename =  file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", model_id, ".svg")), unit = "in",
  dpi = 300, height = 8, width = 6)

rsvg::rsvg_pdf(file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", model_id, ".svg")),
  file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", model_id, ".pdf")))

file.remove(file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", model_id, ".svg")))


#### -------------------- All Nest Step_Type Maps Combined --------------------

tmap_mode("plot")

# Nests
nests_study <- st_as_sf(x = readRDS(nests_study_file), coords = c("long","lat"),
  crs = "+proj=longlat +datum=WGS84") %>% filter(!name %in%
    c("Davis", "Upper")) %>% st_transform(wgs84n19)
nests_sim <- nests_study %>% slice(c(2, 4, 7, 13))

# For Individual Maps
for (j in seq_len(nrow(nests_sim))){
  # Get nest
  nest_j <- nests_sim %>% slice(j)
  nest_j_name <- nest_j %>% pull(name)
  # List for output
  ssf_tmap_list <- vector(mode = "list", length = 20)
  for (i in seq_len(nrow(ssf_fits_best))){
    step_type_i_numeric <- ssf_fits_best %>% slice(i) %>% pull(step_type) %>%
      str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
        "perch" = "4", "roost" = "5"))
    ssf_prob_file <- list.files(ssf_prob_dir, pattern =
      paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
    if (i ==  1){
      # Use "Tmap_baselayers.R" script to get other baselayers
      nest_bbox <- st_as_sfc(st_bbox(st_buffer(nest_j, dist = 6000)))
      nest_buffer <- st_buffer(nest_j, dist = 6000)
      nest_bb_sf <- st_as_sfc(bb(nest_buffer, relative = TRUE, height = 1.35,
        width = 1.35))
      Sys.sleep(60)
      nest_om = read_osm(nest_bb_sf, type = om_nat_geo, zoom = 12)
        #type = "osm") #zoom = 12, minNumTiles=9,
      nest_om_bb <- bb_poly(nest_om)
    }
    ssf_prob_i <- raster(ssf_prob_file) #%>% slice(1)
    ssf_prob_i_crop <- crop(ssf_prob_i, nest_buffer)
    ssf_prob_i_mask <- mask(ssf_prob_i_crop, nest_buffer)
    step_type_i_text <- step_type_i_numeric %>%
      str_replace_all("1", "Cruise") %>%
      str_replace_all("2", "Flight") %>%
      str_replace_all("3", "Nest") %>%
      str_replace_all("4", "Perch") %>%
      str_replace_all("5", "Roost")
    writeLines(paste0("Mapping: ", step_type_i_text))
    step_type_i_arrow <- step_type_i_text %>%
      str_replace_all("_", "$\\\\rightarrow$ ") %>%
      latex2exp::TeX(.)
    ssf_prob_i_nest_map <-
      tm_shape(nest_om) +
        tm_rgb() +
      tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1), alpha = .6,
        legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_scale_bar(breaks = c(0, 1, 2), text.size = .25, lwd = .25,
        position = c(.825, .02)) +
      tm_compass(type = "4star", text.size = 0.45, show.labels = 1, size = 1.2,
        position = c(.85, .825), lwd = .25) +
      tm_shape(nests_sim) +
      tm_symbols(shape = 8, col = "red", size = .075) +
      tm_layout(#asp = .75,
        title.bg.color = "white",
        title.position = c("left", "top"),
        title.fontfamily = "Latin Modern Roman",
        title = step_type_i_arrow,
        title.size = .4,
        title.snap.to.legend = FALSE,
        legend.title.size = .275,
        legend.text.size = .25,
        legend.position = c(.025,.05),
        legend.outside = FALSE,
        legend.bg.color = "white",
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman",
        frame = FALSE) #+ tm_credits(step_type_arrow, position=c("right","top"))
    #ssf_prob_i_nest_map
    tmap_position <- switch(step_type_i_numeric,
      "1_1" = 1,  "1_2" = 2,  "1_4" = 3,
      "2_1" = 5,  "2_2" = 6,  "2_4" = 7,  "2_5" = 8,
      "3_1" = 9,  "3_2" = 10, "3_4" = 11, "3_5" = 12,
      "4_1" = 13, "4_2" = 14, "4_4" = 15, "4_5" = 16,
                  "5_2" = 18, "5_4" = 19)
    writeLines(as.character(tmap_position))
    ssf_tmap_list[[tmap_position]] <- ssf_prob_i_nest_map
  }

  tmap_blank <-
    tm_shape(nest_om_bb, is.master = TRUE) +
      tm_fill(col = "white") +
    tm_shape(nest_buffer, is.master = TRUE) +
      tm_polygons(col = "white", border.col = "grey") +
    tm_layout(asp = .75, legend.show = FALSE, frame = FALSE)

  for (i in seq_len(length(ssf_tmap_list))){
    if(is.null(ssf_tmap_list[[i]])) ssf_tmap_list[[i]] <- tmap_blank
  }
  # Arrange map of probability surfaces
  ssf_tmap_nest_arrange <- tmap_arrange(
    ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]],
    ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
    ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
    ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
    ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
    ssf_tmap_list[[16]], ssf_tmap_list[[17]], ssf_tmap_list[[18]],
    ssf_tmap_list[[19]],ssf_tmap_list[[20]], ncol = 4)

  tmap_save(tm = ssf_tmap_nest_arrange, filename =file.path("C:/TEMP/SSF_Maps/",
    paste0("SSF_Probability_Maps_Nest_", model_id, "_", nest_j_name, ".svg")),
    unit = "in", dpi = 300, height = 8, width = 6)

  rsvg::rsvg_pdf(file.path("C:/TEMP/SSF_Maps/",
    paste0("SSF_Probability_Maps_Nest_", model_id, "_", nest_j_name, ".svg")),
    file.path("C:/TEMP/SSF_Maps/",
    paste0("SSF_Probability_Maps_Nest_", model_id, "_", nest_j_name, ".pdf")))

  file.remove(file.path("C:/TEMP/SSF_Maps/",
    paste0("SSF_Probability_Maps_Nest_", model_id, "_", nest_j_name, ".svg")))

}
##################### GENERATE COVARIATE TABLE #################################

for (i in seq_len(nrow(ssf_fits_best))){

  step_type_i_latex <- ssf_fits_best %>%
      slice(i) %>%
      pull(step_type) %>%
      str_replace_all("cruise", "Cruise") %>%
      str_replace_all("flight", "Flight") %>%
      str_replace_all("nest", "Nest") %>%
      str_replace_all("perch", "Perch") %>%
      str_replace_all("roost", "Roost") #%>%
    #  str_replace_all("_", "$\\\\rightarrow$ ") #%>%
    #  latex2exp::TeX(.)

  covars_all_i <- ssf_fits_best %>%
    slice(i) %>%
    pluck("covars_all", 1)
  covars_all_i_poly2 <-  bind_rows(covars_all_i,
      covars_all_i %>%
      dplyr::filter(poly2) %>%
      mutate(covar = paste0(covar, "^2"))) %>%
    dplyr::select(covar) %>%
    arrange(covar)
  covars_all_i_poly2

  covar_matrix_i <- ssf_fits_best %>%
    slice(i) %>%
    pluck("covar_matrix", 1) %>%
    dplyr::select(covar, poly2, scale_fitted) %>%
    mutate(covar_sigma = paste0(covar, scale_fitted))
  covar_matrix_i_poly2 <-  bind_rows(covar_matrix_i,
      covar_matrix_i %>%
      dplyr::filter(poly2) %>%
      mutate(covar = paste0(covar, "^2")) %>%
      mutate(covar_sigma = paste0(covar_sigma, "^2"))) %>%
    dplyr::select(covar, covar_sigma) %>%
    arrange(covar)
  covar_matrix_i_poly2

  covars_all_matrix_i <- left_join(covars_all_i_poly2, covar_matrix_i_poly2)

  covar_fitted_i <- ssf_fits_best %>%
    slice(i) %>%
    pluck("covar_fitted", 1) %>%
    dplyr::rename(covar_sigma = covar_clean) %>%
    dplyr::select(-covar)

  covar_table_i <- left_join(covars_all_matrix_i, covar_fitted_i)

  step_type_col <- c(step_type_i_latex, rep(NA, nrow(covar_table_i)-1))

  covar_table_i <- covar_table_i %>%
    add_column(step_type = step_type_col, .before = 1)

  model_full_i <- covar_table_i %>%
    filter(!is.na(covar_sigma)) %>%
    mutate(coef_i = signif(coef_signif, digits = 3)) %>%
    mutate(model_terms = paste0(coef_i, "*", covar_sigma)) %>%
    pull(model_terms) %>% paste0(., collapse = " + ")

  model_table_i <- tibble(step_type = step_type_i_latex,
    model_full = model_full_i)

  if(i == 1){
    covar_table <- covar_table_i
    model_table <- model_table_i
  } else {
    covar_table <- bind_rows(covar_table, covar_table_i)
    model_table <- bind_rows(model_table, model_table_i)
  }
}
covar_table_final <- covar_table %>%
  dplyr::select(step_type, covar, covar_sigma, coef_signif) %>%
  dplyr::rename("Step Type" = step_type, "Candidates" = covar,
    "Covariates (sigma)" = covar_sigma, "Coefficient" = coef_signif)

model_table_final <- model_table %>%
  dplyr::rename("Step Type" = step_type, Model = model_full)

##################### RUN RMARKDOWN REPORT #####################################

nest_map_files <- list.files(path = "C:/TEMP/SSF_Maps/",  full.names = TRUE,
  pattern = paste0("SSF_Probability_Maps_Nest_", model_id, "*"))

rmarkdown::render(input = "R/RMarkdown/SSF_Map_Table.Rmd",
  params = list(
    covar_table = covar_table_final,
    map_file = file.path("C:/TEMP/SSF_Maps/",
      paste0("SSF_Probability_Maps_Overview_", model_id, ".pdf")),
    model_table = model_table_final,
    nest_map_files = nest_map_files),
  output_dir = "C:/TEMP/SSF_Maps", output_file = paste0("SSF_Fits_", model_id,
    "_", step_type_name, ".pdf"), quiet = TRUE, clean = TRUE)

# Delete unneeded map files
file.remove(file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", model_id, ".pdf")))

for (i in seq_len(length(nest_map_files))){
  file.remove(nest_map_files[i])
}

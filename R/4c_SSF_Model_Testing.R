###################### SSF_Fit_Clogit_Models ###################################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(AICcmodavg, arrangements, plyr, dplyr, future, furrr, optimx,
  ggplot2, lubridate, optimx, purrr, rgenoud, reproducible, sf, stars, stringr,
  survival, tibble, tictoc, tidyr, tmap, tmaptools, viridis, xtable)
pacman::p_load(baear, gisr, ibmr)
suppressMessages(extrafont::loadfonts(device="win"))
testing <- FALSE
save_individual_maps <- FALSE

BoldText <- function(x) {paste('{\\textbf{',x,'}}', sep ='')}
BoldTextCentered <- function(x) {paste('{c}{\\textbf{',x,'}}', sep ='')}

# Theme (for LaTeX font)
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))


# Model directories
ua_data_dir <- "Output/Analysis/SSF/UA_Data"
mod_dir <- "Output/Analysis/SSF/Models"
mod_fit_dir <- file.path(mod_dir, "model_fits")
mod_compiled_dir <- file.path(mod_dir, "model_fits_compiled")
mod_best_dir <- file.path(mod_dir, "model_fits_compiled_best")

# Model files
top10_fits_file <- file.path(mod_best_dir, "model_fits_compiled_top_10.rds")
best_fits_file <- file.path(mod_best_dir, "model_fits_compiled_best.rds")
preds_tbl_file <- file.path(mod_best_dir, "preds_tbl.rds")

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
step_type_dir <- file.path(ssf_raster_dir, "Step_Types_Simpler")
map_temp_dir <- "C:/TEMP/SSF_Maps"

# Maine files
maine_raster_trim_file = "C:/ArcGIS/Data/BlankRaster/maine_trim.tif"
maine_polygon_file <- "C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp"

# Nests
nests_study_file <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/",
  "baea_ibm/Data/Nests/Nests_rds/nests_study.rds")

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

## Set run parameters ----------------------------------------------------------

# Step-Type
#step_type_index <- c(2,5,13) # cruise/flight/perch_flight
step_type_index <- c(3,6,10,14,17) # cruise/flight/nest/perch/roost_perch

covar_matrix_i <- tribble(
  ~covar, ~fixed, ~scale, ~scale_min, ~scale_max, ~scale_start, ~poly2,
  #"developed",      FALSE, TRUE, 1, 100, 50, FALSE,
  "wetland",        FALSE, TRUE, 1, 100, 50, FALSE,
  "open_water",     FALSE, TRUE, 1, 100, 50, FALSE,
  #"roughness",      FALSE, TRUE, 1,  50, 25,  TRUE,
  #"tpi",            FALSE, TRUE, 1,  50, 25,  TRUE,
  "tri",            FALSE, TRUE, 1,  50, 25,  TRUE,
  "dist_hydro",     TRUE, FALSE, NA, NA, NA, FALSE,
  "dist_road",      TRUE, FALSE, NA, NA, NA, FALSE
  #"dist_turbine",   TRUE, FALSE, NA, NA, NA, FALSE
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

print(paste0("Step-types: ",
  paste0(str_replace_all(dir(ua_data_dir)[step_type_index],
  "ua_steps_|.rds", ""), collapse = ", ")))
print(paste0("Covariates: ", paste0(covar_matrix_i$covar,
  ifelse(covar_matrix_i$poly2, "^2", ""), collapse =  ", ")))

# Generate clusters (for parallel processing)
if(!testing) plan(multisession)

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

################ OPTIMIZATION PROCEDURE OF KERNEL BANDWIDTH ####################

# Optimization parameters
max_generations = 25
wait_generations = 5
burnin_generations = 5
iter_max = 50

# Pull files
ua_files <- dir(ua_data_dir)[step_type_index]

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

  covar_matrix_i <- covar_matrix_i
  #covar_matrix_i <- covar_matrix %>% #dplyr::filter(covar %in% keep_covars)

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
      dplyr::select(step_type, covar_matrix, fit_aicc,
        delta_aicc, refit_aicc, peak_gen, concordance_value, covar_fitted,
        fit_covars_clean, fit_coefs_signif, model_full)

  tbl_models_run <- tbl_models_final %>%
    group_by(step_type) %>%
    nest() %>%
    rename(run_data = data) %>%
    mutate(covars_run = list(covar_matrix_i)) %>%
    mutate(models_tested = length(covar_matrix_i_combos))

  # Save output
  saveRDS(tbl_models_run, file.path(mod_fit_dir, step_type,
    paste0("ssf_fit_", step_type, "_", GetDateTime(), ".rds")))

}

future:::ClusterRegistry("stop")

################### COMPILE MODELS FOR EACH STEP TYPE ##########################

# Find all of the step_type folders in mod_fit_dir
step_types <- list.dirs(file.path(mod_fit_dir),
  full.names = FALSE, recursive = FALSE)[step_type_index]

# Compile all models for each step_type into a compiled_fit file
for (i in seq_along(step_types)) {
  step_type_i <- step_types[i]
  print(paste0(step_type_i, " (", i, " of ", length(step_types), ")"))
  ssf_fits_step_type_i <- list.files(path = file.path(mod_fit_dir,
    step_type_i), pattern = paste0("ssf_fit_",
        step_type_i, "*")) %>%
    map(~ readRDS(file.path(mod_fit_dir, step_type_i, .))) %>%
    reduce(bind_rows) %>%
    as_tibble(.) %>%
    unnest(run_data)
  compiled_ssf_fits_step_type_i <- ssf_fits_step_type_i  %>%
    #group_by(covar_matrix) %>% # Precaution in case duplicate models exist in data
    #slice(which.min(fit_aicc)) %>%
    #ungroup() %>%
    arrange(fit_aicc) %>%
    filter(!is.na(fit_aicc))
  print(paste0("Models with fits: n = ", nrow(compiled_ssf_fits_step_type_i)))
  saveRDS(compiled_ssf_fits_step_type_i, file.path(mod_compiled_dir,
    paste0("ssf_fits_compiled_", step_type_i, ".rds")))
}
rm(step_type_i, ssf_fits_step_type_i, compiled_ssf_fits_step_type_i)

####### COMPILE TOP 10 AND BEST FIT MODELS FOR EACH STEP TYPE  #################

# Find the top 10 fit models for each step_type
model_fits_compiled_top_10 <- list.files(path = mod_compiled_dir,
    pattern = "^ssf_fits_compiled_*")  %>%
  map(~ readRDS(file.path(mod_compiled_dir, .))) %>%
  reduce(bind_rows) %>%
  group_by(step_type) %>%
  arrange(fit_aicc) %>%
  mutate(delta_aicc = fit_aicc - first(fit_aicc)) %>%
  slice(1:10) %>%
  ungroup(.) %>%
  arrange(step_type, fit_aicc)

saveRDS(model_fits_compiled_top_10, top10_fits_file)

# Find the top models for each step_type
model_fits_compiled_best <- model_fits_compiled_top_10 %>%
  group_by(step_type) %>%
  arrange(fit_aicc) %>%
  slice(which.min(fit_aicc)) %>%
  ungroup(.) %>%
  arrange(step_type)

model_fits_compiled_best %>% pluck("model_full")

saveRDS(model_fits_compiled_best, best_fits_file)

########## This script converts SSF Models to SSF Input Surfaces #############-#

# Load libraries, scripts, and input parameters
pacman::p_load(raster, whitebox)

whitebox::wbt_init() # required for WhiteboxTools to work
wbt_version() # check WhiteboxTools version
rasterOptions(maxmem = Inf, progress = "text", timer = TRUE,
  memfrac = .85)

######################### CREATE SSF LAYERS ####################################

## Get SSF FITS ----------------------------------------------------------------

best_ssf_fits_org <- readRDS(best_fits_file) %>% slice(c(step_type_index))

best_ssf_fits_org %>% dplyr::select(step_type, fit_covars_clean, model_full)
best_ssf_fits <- best_ssf_fits_org

best_ssf_fits %>% pluck("model_full") %>% unlist()

# Determine all the raster_sigma layers
preds_all <- unlist(best_ssf_fits$fit_covars_clean) %>% str_remove_all("\\^2")
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
  print(paste0("Checking: ", covar, sigma))
  out_file <- file.path(covars_full_dir, paste0(covar, sigma, ".tif"))
  if(!file.exists(out_file)){
    raster_file <- file.path(input_dir, paste0(covar, "_30mc.tif"))
    print(paste0("Needed: ", covar, sigma))
  }
}

for (i in 1:nrow(preds_kernel)){
  preds_kernel_i <- preds_kernel %>% slice(i)
  sigma <- preds_kernel_i %>% pull(sigma)
  covar <- preds_kernel_i %>% pull(covar)
  print(paste0("Checking: ", covar, sigma))
  out_file <- file.path(covars_full_dir, paste0(covar, sigma, ".tif"))
  if(!file.exists(out_file)){
    raster_file <- file.path(input_dir, paste0(covar, "_30mc.tif"))
    print(paste0("Starting: ", covar, sigma, " (", i, " of ",
      nrow(preds_kernel), ") at: ", lubridate::now()))
    if(sigma == 0){
      out_raster <- raster(raster_file)
      # Write out raster
      writeRaster(out_raster, out_file, format = "GTiff", overwrite = TRUE)
    } else {
      top_file <- file.path(kernel_dir, paste0(covar, sigma, "_top.grd"))
      mid_file <- file.path(kernel_dir, paste0(covar, sigma, "_mid.grd"))
      bot_file <- file.path(kernel_dir, paste0(covar, sigma, "_bot.grd"))
      # Covar TOP
      print("top")
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
      print("mid")
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
      print("bottom")
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
      writeRaster(out_raster, out_file, format = "GTiff", overwrite = TRUE)
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
  print(paste0("Checking: ", covar, sigma))
  out_file <- file.path(covars_full_dir, paste0(covar, sigma, ".tif"))

  if(!file.exists(out_file)){
    print(paste0("Needed: ", covar, sigma))
  }
}

for (i in seq_len(nrow(preds_terrain))){
  preds_terrain_i <- preds_terrain %>% slice(i)
  sigma <- preds_terrain_i %>% pull(sigma)
  covar <- preds_terrain_i %>% pull(covar)
  print(paste0("Checking: ", covar, sigma))
  out_file <- file.path(covars_full_dir, paste0(covar, sigma, ".tif"))

  if(!file.exists(out_file)){
    print(paste0("Starting: ", covar, sigma, " (", i, " of ",
      nrow(preds_terrain), ") at: ", lubridate::now()))

    top_file <- file.path(terrain_dir, paste0(covar, sigma, "_top.grd"))
    mid_file <- file.path(terrain_dir, paste0(covar, sigma, "_mid.grd"))
    bot_file <- file.path(terrain_dir, paste0(covar, sigma, "_bot.grd"))

    # Covar TOP
    print(paste0("Starting top at: ", lubridate::now()))
    covar_i_top <- CalculateTerrainMetricWithSigma(sigma, covar, ext_top,
      elev_org)
    writeRaster(covar_i_top, top_file, format = "raster", overwrite = TRUE)
    rm(covar_i_top)
    gc()

    # Covar MID
    print(paste0("Starting mid at: ", lubridate::now()))
    covar_i_mid <- CalculateTerrainMetricWithSigma(sigma, covar, ext_mid,
      elev_org)
    covar_i_mid_crop <- crop(covar_i_mid, ext_mid_crop)
    writeRaster(covar_i_mid_crop, mid_file, format = "raster", overwrite = TRUE)
    rm(covar_i_mid)
    gc()

    # Covar BOT
    print(paste0("Starting bot at: ", lubridate::now()))
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
    writeRaster(out_raster, out_file, format = "GTiff", overwrite = TRUE)

    rm(top_raster, mid_raster, bot_raster, top_file, mid_file, bot_file)

  }
}

################### MASK COVARIATE RASTERS TO MAINE ONLY #######################

preds_tbl <- readRDS(preds_tbl_file)
maine_raster_trim <- raster(maine_raster_trim_file)

for (i in seq_len(nrow(preds_tbl))){
  preds_tbl_i <- preds_tbl %>% slice(i)
  covar_sigma <- preds_tbl_i %>% pull(covar_sigma)
  print(paste0("Starting: ", covar_sigma, " (", i, " of ", nrow(preds_tbl),
    ") at: ", lubridate::now()))

  covar_file <- file.path(covars_full_dir, paste0(covar_sigma, ".tif"))
  out_file <- file.path(covars_crop_dir, paste0(covar_sigma, ".tif"))

  if(!file.exists(out_file)){
    print(paste0("Writing: ", covar_sigma, " at: ", lubridate::now()))
    covar_raster <- raster(covar_file)
    covar_raster_crop <- crop(covar_raster, maine_raster_trim)
    covar_raster_mask <- mask(covar_raster_crop, maine_raster_trim)
    writeRaster(covar_raster_mask, out_file, format = "GTiff", overwrite = TRUE)
  }
}

################### GENERATE SSF_LAYERS FOR MAINE ##############################

best_ssf_fits <- readRDS(best_fits_file) %>% slice(step_type_index)
best_ssf_fits %>% pluck("model_full") %>% unlist()

maine_raster_trim <- raster(maine_raster_trim_file)

# Original
# Generate layer for each ssf based on original fits
for (i in 1:nrow(best_ssf_fits)){
  print(paste0("i:", i))
  step_type_i <- best_ssf_fits %>% slice(i) %>% pull(step_type)

  covars_i <- best_ssf_fits %>% slice(i) %>% pluck("covar_fitted", 1) %>%
    pull("covar_clean") %>% str_remove_all("\\^2") %>% unique()

  # Create Raster_Brick
  covars_list <- vector(mode = "list", length = length(covars_i))
  for (j in seq_along(covars_i)){
    covars_i_j <- covars_i[j]
    print(paste0("covariates: ", covars_i_j))
    raster_file <- file.path(covars_crop_dir, paste0(covars_i_j, ".tif"))
    covars_list[[j]] <- raster(raster_file)
  }
  covars_brick <- raster::brick(covars_list)
  rm(covars_list)

  # Generate formula

  covars_clean_i <- best_ssf_fits %>% slice(i) %>% pluck("covar_fitted", 1) %>%
    pull("covar_clean")

  covars_i <- ifelse(str_detect(covars_clean_i, "\\^2"),
    paste0("covars_brick[['", str_remove_all(covars_clean_i, "\\^2"), "']]^2"),
    paste0("covars_brick[['", covars_clean_i, "']]"))

  coefs_i <- best_ssf_fits %>% slice(i) %>% pluck("covar_fitted", 1) %>%
    pull("coef_signif")

  # Generate formulas
  ssf_formula <- paste0("(", paste0(paste0(coefs_i, "*",covars_i),
    collapse = ") + ("), ")")
  print(ssf_formula)

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

best_ssf_fits <- readRDS(best_fits_file) %>% slice(step_type_index)

# Maine Outline
maine <- read_sf(maine_polygon_file) %>% st_transform(., crs = 4326) %>%
  mutate(state = "Maine") %>% dplyr::select(state)

# Nests
nests_study <- st_as_sf(x = readRDS(nests_study_file), coords = c("long","lat"),
  crs = "+proj=longlat +datum=WGS84") %>% filter(!name %in% c("Davis", "Upper"))

# For Individual Maps
if(save_individual_maps){
  for (i in seq_len(nrow(best_ssf_fits))){
    print(i)
    step_type_i_numeric <- best_ssf_fits %>% slice(i) %>% pull(step_type) %>%
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
for (i in seq_len(nrow(best_ssf_fits))){
  #print(i)
  step_type_i_numeric <- best_ssf_fits %>% slice(i) %>% pull(step_type) %>%
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
  print(tmap_position)
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
ssf_tmap_arrange_test <- tmap_arrange(
  tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, ssf_tmap_list[[1]], tmap_blank,
  tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  ncol = 4)
ssf_tmap_arrange_test
tmap_save(tm = ssf_tmap_arrange_test, filename =  file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", GetDateTime(), ".svg")), unit = "in",
  dpi = 300, height = 8, width = 6)

ssf_tmap_arrange <- tmap_arrange(
  ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]],
  ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
  ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
  ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
  ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
  ssf_tmap_list[[16]], ssf_tmap_list[[17]], ssf_tmap_list[[18]],
  ssf_tmap_list[[19]],ssf_tmap_list[[20]], ncol = 4)

date_time <- GetDateTime()

tmap_save(tm = ssf_tmap_arrange, filename =  file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", date_time, ".svg")), unit = "in",
  dpi = 300, height = 8, width = 6)

rsvg::rsvg_pdf(file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", date_time, ".svg")),
  file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", date_time, ".pdf")))

#### -------------------- Generate Covariate Table -----------------------------

for (i in seq_len(nrow(best_ssf_fits))){

  step_type_i_latex <- best_ssf_fits %>%
      slice(i) %>%
      pull(step_type) %>%
      str_replace_all("cruise", "Cruise") %>%
      str_replace_all("flight", "Flight") %>%
      str_replace_all("nest", "Nest") %>%
      str_replace_all("perch", "Perch") %>%
      str_replace_all("roost", "Roost") #%>%
    #  str_replace_all("_", "$\\\\rightarrow$ ") #%>%
    #  latex2exp::TeX(.)

  covars_run_i <- best_ssf_fits %>%
    slice(i) %>%
    pluck("covars_run", 1)
  covars_run_i_poly2 <-  bind_rows(covars_run_i,
      covars_run_i %>%
      dplyr::filter(poly2) %>%
      mutate(covar = paste0(covar, "^2"))) %>%
    dplyr::select(covar) %>%
    arrange(covar)
  covars_run_i_poly2

  covar_matrix_i <- best_ssf_fits %>%
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

  covar_run_matrix_i <- left_join(covars_run_i_poly2, covar_matrix_i_poly2)

  covar_fitted_i <- best_ssf_fits %>%
    slice(i) %>%
    pluck("covar_fitted", 1) %>%
    rename(covar_sigma = covar_clean) %>%
    dplyr::select(-covar)

  covar_table_i <- left_join(covar_run_matrix_i, covar_fitted_i)

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
  rename("Step Type" = step_type, "Candidates" = covar,
    "Covariates (sigma)" = covar_sigma, "Coefficient" = coef_signif)

model_table_final <- model_table %>%
  rename("Step Type" = step_type, Model = model_full)

rmarkdown::render(input = "R/RMarkdown/SSF_Map_Table.Rmd",
  params = list(
    covar_table = covar_table_final,
    map_file = file.path("C:/TEMP/SSF_Maps/",
      paste0("SSF_Probability_Maps_Overview_", date_time, ".pdf")),
    model_table = model_table_final),
  output_dir = "C:/TEMP/SSF_Maps", output_file = paste0("SSF_Fits_", date_time,
    ".pdf"), quiet = TRUE)

file.remove(file.path("C:/TEMP/SSF_Maps/",
  paste0("SSF_Probability_Maps_Overview_", date_time, ".pdf")))

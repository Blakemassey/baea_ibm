###################### ModelFit_SSF_Analyze_Models #############################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(plyr, dplyr, optimx, ggplot2, ggthemes, lubridate,
  optimx, purrr, reproducible, rgenoud, stringr, summarytools, survival,
  surveybootstrap, tibble, tictoc, tidyr, xtable)
library(baear, gisr)
options(stringsAsFactors=FALSE)

# Calculate the quantiles for the Covariates -----------------------------------
preds_tbl_file = "Output/Analysis/SSF/Models/best_fits/preds_tbl.rds"
quantile_tbl_file = "Output/Analysis/SSF/Models/best_fits/covar_quantiles.rds"
covars_crop_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters/Covars_Crop"

preds_tbl <- readRDS("Output/Analysis/SSF/Models/best_fits/preds_tbl.rds")

probs_char <- c("0.01",
                "0.05", "0.10",
                "0.15", "0.20",
                "0.25", "0.30",
                "0.35", "0.40",
                "0.45", "0.50",
                "0.55", "0.60",
                "0.65", "0.70",
                "0.75", "0.80",
                "0.85", "0.90",
                "0.95", "0.99")
probs_names <- str_replace_all(probs_char, "0\\.", "p_")
probs <- as.numeric(probs_char)

prob_tbl <- probs_names %>% purrr::map_dfc(setNames, object = list(numeric()))
covar_tbl <- tibble(covar = vector(mode = "character", 0))
quantile_tbl <- bind_cols(covar_tbl, prob_tbl)

for (i in seq_len(nrow(preds_tbl))){
  preds_tbl_i <- preds_tbl %>% slice(i)
  covar_sigma <- preds_tbl_i %>% pull(covar_sigma)
  print(paste0("Starting: ", covar_sigma, " (", i, " of ", nrow(preds_tbl),
    ") at: ", lubridate::now()))

  covar_raster_file <- file.path(covars_crop_dir, paste0(covar_sigma, ".tif"))
  covar_raster <- raster(covar_raster_file)
  covar_quantile <- quantile(covar_raster, probs = probs, names = FALSE)
  names(covar_quantile) <- probs_names

  quantile_row_i <- bind_cols(covar = covar_sigma, bind_rows(covar_quantile))
  quantile_tbl <- quantile_tbl %>% add_row(quantile_row_i)
}

# Save quantile file
saveRDS(quantile_tbl, quantile_tbl_file)

rm(covars_crop_dir, covar_quantile, covar_raster_file, covar_raster,
  covar_sigma,  covar_tbl, i, preds_tbl, preds_tbl_file, preds_tbl_i, probs,
  probs_char, probs_names, prob_tbl, quantile_row_i, quantile_tbl,
  quantile_tbl_file)

# Extract model data (terms, coefs, etc.) from best_ssf_fit_models -------------
mod_fit_dir = "Output/Analysis/SSF/Models"
model_data_file = "Output/Analysis/SSF/Models/best_fits/model_data.rds"

best_ssf_fit_all_org <- readRDS(file.path(mod_fit_dir, "best_fits",
  "best_ssf_fit_all.rds"))

best_fit_all <- best_ssf_fit_all_org %>%
  mutate(start_behavior = word(step_type, 1, sep = "_")) %>%
  mutate(end_behavior = word(step_type, 2, sep = "_"))

start_step_types <- best_fit_all %>% pull(start_behavior) %>% unique(.)

#i <- start_step_types[3]; j <- 1  # for testing
for (i in start_step_types){
  best_fit_i <- best_fit_all %>% filter(start_behavior == i)
  fit_list <- vector(mode = "list", length = nrow(best_fit_i))
  for (j in seq_len(nrow(best_fit_i))){
    fit_ij <- best_fit_i %>% slice(j) %>% pull(clogit_fit) %>% pluck(1)
    step_type_ij <- best_fit_i %>% slice(j) %>% pull(step_type)
    fit_ij <- as.data.frame(xtable(fit_ij)) %>%
      rownames_to_column(., var = "term") %>%
      mutate(step_type = NA)
    fit_ij[, "step_type"] <- step_type_ij
    fit_list[[j]] <- fit_ij
  }
  ssf_best_fits_table_i <- fit_list %>%
    reduce(bind_rows) %>%
    dplyr::select(step_type, everything())
  if(i == start_step_types[1]){
    model_data <- ssf_best_fits_table_i
  } else {
    model_data <- ssf_best_fits_xtable %>%
      bind_rows(ssf_best_fits_table_i)
  }
}

saveRDS(model_data, model_data_file)

# Combine the model fit data with the raster quantile data ---------------------
mod_fit_dir = "Output/Analysis/SSF/Models"
model_data_file = "Output/Analysis/SSF/Models/best_fits/model_data.rds"
quantile_tbl_file = "Output/Analysis/SSF/Models/best_fits/covar_quantiles.rds"

best_fit_all <- readRDS(file.path(mod_fit_dir, "best_fits",
  "best_ssf_fit_all.rds"))
model_data <- readRDS(model_data_file)
quantile_tbl <- readRDS(quantile_tbl_file)

fits_quantile_tbl <- model_data %>%
  left_join(., quantile_tbl, c("term" = "covar"))

for (i in seq_len(nrow(best_fit_all))){
  best_fit_i <- best_fit_all %>% slice(i)
  step_type_i <- best_fit_i %>% pull(step_type)
  clogit_i <- best_fit_i %>% pull(clogit_fit) %>% pluck(1)
  fits_quantile_i <- fits_quantile_tbl %>% filter(step_type == step_type_i)
  for (j in seq_len(nrow(fits_quantile_tbl))){
    fits_quantile_ij <- fits_quantile_i %>% slice(j)
    term_ij <- fits_quantile_ij %>% pull(term)
    p_05_ij <- fits_quantile_ij %>% pull(p_05)
    term_tbl_ij <- tibble(value = p_05_ij) %>%
      rename(!!term_ij := value)
    other_terms_ij  <- fits_quantile_i %>% slice(-c(j)) %>%
      dplyr::select(term, p_50) %>%
      rename(value = p_50)
    other_wide_ij <- other_terms_ij %>%
      pivot_wider(names_from = term, values_from = value)
    terms_tbl_ij <- bind_cols(term_tbl_ij, other_wide_ij)

    clogit_i$terms
    # HOW DO I FORMULATE MODEL AND RUN A PREDICTION?
    formula_i <- summary(clogit_i)
    risk <- predict(clogit_i, newdata = terms_tbl_ij, type = "risk")


  }
}


# Demonstrate Inverse Logit and Rescale ----------------------------------------

values = -10:10
inv_logit <- boot::inv.logit(values)
logit_df <- tibble(values, inv_logit)

gg_invlogit <- ggplot(logit_df) +
  geom_line(aes(x = values, y = inv_logit))

gg_invlogit

# Code to rescale
wbt_rescale_value_range(i, output, out_min_val, out_max_val, clip_min = None,
  clip_max = None)

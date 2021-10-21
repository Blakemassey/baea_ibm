#------------------------ SSF Analyze SSF Layers ------------------------------#
# This script is to run model fitting procedures for the SSF using different
# covariates to determine the best fitting model given the covariate candidates
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------

# Load packages
pacman::p_load(plyr, dplyr, optimx, ggplot2, ggthemes, lubridate, optimx, purrr,
  raster, readr, reproducible, rgenoud, rlang, stringr, summarytools, survival,
  surveybootstrap, tibble, tictoc, tidyr, xtable)
pacman::p_load(baear, gisr)

# Model directories
covars_crop_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters/Covars_Crop"
mod_dir <- "Output/Analysis/SSF/Models"
mod_best_dir <- file.path(mod_dir, "model_fits_best")
margin_plots_dir <- "C:/TEMP/Covar_Figures"

# Set to "" for "model_best_fits.rds"
model_id <- NA
#model_id <- "2021-02-11_1814"
nest_index <- c(2, 4, 7, 13)

# SSF model file
model_file <- ifelse(is.na(model_id), "model_fits_best.rds", paste0(
  "model_fits_best_", model_id, ".rds"))
fits_best_file <- file.path(mod_best_dir, model_file)

# Predictors table file
preds_file <- ifelse(is.na(model_id), "preds_tbl.rds", paste0(
  "preds_tbl_", model_id, ".rds"))
preds_tbl_file <- file.path(mod_best_dir, preds_file)

# Quantiles table file
quantile_file <- ifelse(is.na(model_id), "covar_quantiles.rds", paste0(
  "covar_quantiles_", model_id, ".rds"))
quantile_tbl_file <- file.path(mod_best_dir, quantile_file)

# Oher files / directories
predictions_file <- file.path(mod_best_dir, "covar_predictions.rds")
model_data_file <- file.path(mod_best_dir, "model_data.rds")
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"

# Theme (for LaTeX font)
suppressMessages(extrafont::loadfonts(device="win"))
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))
pointSize = 2; textSize = 5; spaceLegend = 1

# Calculate Covariate Percentiles ----------------------------------------------

preds_tbl <- readRDS(preds_tbl_file)
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

# Calculate the covariate quantiles
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
  probs_char, probs_names, prob_tbl, quantile_row_i, quantile_tbl)

# Extract Model Data (Terms, Coefs, etc.) from Best SSF Fit Models -------------

ssf_fits_best <- readRDS(fits_best_file)
quantile_tbl <- readRDS(quantile_tbl_file)

pred_list_all <- vector(mode = "list", length = nrow(ssf_fits_best))
if(FALSE) i <- 3; j <- 1 # for testing
for (i in seq_len(nrow(ssf_fits_best))){
  ssf_fit_i <- ssf_fits_best %>% slice(i)
  step_type_i <- ssf_fit_i %>% pull(step_type)
  print(paste0("step_type = ", step_type_i))
  model_i <- ssf_fit_i %>% pluck("model_full", 1)
  covars_i <- ssf_fit_i %>% pluck("covar_fitted", 1) %>% pull(covar_clean) %>%
    str_remove_all(., "\\^2") %>% unique(.)
  pred_list_i <- vector(mode = "list", length = length(covars_i))
  for (j in seq_len(length(covars_i))){
    covar_ij <- covars_i[j]
    #covarij_enquo <- enquo(covar_ij)
    print(paste0("covar_ij(", i, "-", j, "): ", covar_ij))
    covar_tbl_ij <- quantile_tbl %>%
      filter(covar == covar_ij) %>%
      pivot_longer(starts_with("p_"), names_to = "percentile") %>%
      mutate(percentile = parse_number(percentile)) %>%
      mutate(covar_value = value) %>%
      #rename(!!covar_ij := value) %>%
      dplyr::rename(!!covar_ij := value)

    other_covars_tbl_ij  <- quantile_tbl %>%
      filter(covar %in% covars_i[-j]) %>%
      dplyr::select(covar, p_50) %>%
      dplyr::rename(value = p_50) %>%
      pivot_wider(names_from = covar, values_from = value)
    if(nrow(other_covars_tbl_ij) > 0){
      covars_tbl_ij <- bind_cols(covar_tbl_ij, other_covars_tbl_ij)
    } else {
      covars_tbl_ij <- covar_tbl_ij
    }
    coefs_i <- ssf_fit_i %>% pluck("covar_fitted", 1) %>% pull(covar_clean)
    betas_i <- ssf_fit_i %>% pluck("covar_fitted", 1) %>% pull(coef)
    ssf_formula <- paste0("pred = (", paste(betas_i, coefs_i, sep = "*",
      collapse = ") + ("), ")")
    AddPredictions = function(df, s){
        q = quote(mutate(df, z = s))
        eval(parse(text = sub("z = s", s, deparse(q))))
    }
    preds_tbl_ij <- AddPredictions(covars_tbl_ij, ssf_formula) %>%
      mutate(step_type = step_type_i) #%>%
      #mutate(coef = ) #Not sure where I was going with this
    pred_list_i[[j]] <- preds_tbl_ij
    print("done")
  }
  pred_list_all[[i]] <- pred_list_i %>%
    reduce(bind_rows)
}

predictions_tbl_all <- pred_list_all %>%
  reduce(bind_rows) %>%
  mutate(prob = boot::inv.logit(pred)) %>%
  dplyr::select(step_type, term = covar, percentile, covar_value, pred, prob)

saveRDS(predictions_tbl_all, predictions_file)

# Plots of Marginal Predicted Fits ---------------------------------------------

predictions_tbl_all <- readRDS(predictions_file)

step_types <- unique(predictions_tbl_all$step_type)
#i <- step_types[1]
for (i in step_types) {
  predictions_step_type_i <- predictions_tbl_all %>%
    filter(step_type == i)
  terms_i <- unique(predictions_step_type_i$term)
  i_name <- paste0(i) %>% str_replace_all(., "_", "-") %>%
      str_to_title(.) %>% str_replace_all(., "-", "_")

  for (j in terms_i){
    j_name <- paste0(j) %>% str_replace_all(., "_", "-") %>%
      str_to_title(.) %>%  str_replace_all(., "-", "")
    ij_name <- paste0(i_name, "_", j_name, ".png")
    predictions_ij <- predictions_step_type_i %>%
      filter(term == j)

    # Probability Value (after converted using inverse logit)
    gg_prob_ij <- ggplot(predictions_ij, aes(x = covar_value, y = prob)) +
      geom_line(color = "blue", size = 1.5) +
      #geom_smooth() +
      xlab(j_name) +
      ylab("Probability") +
      ggtitle("") + guides(NA) + theme_latex + theme(legend.position = "none") +
      theme_minimal() +
      theme_latex +
      theme(axis.text = element_text(size = 7)) +
      theme(axis.title = element_text(size = 9)) +
      theme(plot.title = element_text(size = 11)) +
      guides(shape = guide_legend(override.aes = list(size = pointSize)),
        color = guide_legend(override.aes = list(size = pointSize))) +
      theme(legend.title = element_text(size = textSize),
        legend.text  = element_text(size = textSize),
        legend.key.size = unit(spaceLegend, "lines")) +
      theme(panel.grid.major.x = element_blank()) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))) +
      ylim(c(0, 1))
    ggsave(filename = paste0("Prob_", ij_name), plot = gg_prob_ij,
      path = file.path(margin_plots_dir, "Margin_Plots"),
      scale = 1, width = 6, height = 4, units = "in",
      dpi = 300)
    # SAVE FOR DISSERTATION FIGURES
    # ggsave(filename = paste0("Prob_", ij_name), plot = gg_prob_ij,
    #   path = file.path(tex_dir, "Figures/Ch2/Step_Type_Covar_Prob"),
    #   scale = 1, width = 6, height = 4, units = "in",
    #   dpi = 300)
  }
}

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#
#
# # Predicted Value (not converted to inverse logit)
# gg_predict_ij <- ggplot(predictions_ij, aes(x = covar_value, y = pred)) +
#   geom_line(color = "blue", size = 1.5) +
#   #geom_smooth() +
#   xlab(j_name) +
#   ylab("Value") +
#   ggtitle("") + guides(NA) + theme_latex + theme(legend.position = "none") +
#   theme_minimal() +
#   theme_latex +
#   theme(axis.text = element_text(size = 7)) +
#   theme(axis.title = element_text(size = 9)) +
#   theme(plot.title = element_text(size = 11)) +
#   guides(shape = guide_legend(override.aes = list(size = pointSize)),
#     color = guide_legend(override.aes = list(size = pointSize))) +
#   theme(legend.title = element_text(size = textSize),
#     legend.text  = element_text(size = textSize),
#     legend.key.size = unit(spaceLegend, "lines")) +
#   theme(panel.grid.major.x = element_blank()) +
#   scale_x_continuous(expand = expansion(mult = c(0.01, 0.01)))
# ggsave(filename = paste0("Predict_", ij_name, ".png"), plot = gg_predict_ij,
#   path = file.path(tex_dir, "Figures/Ch2/Step_Type_Covar_Predict"),
#   scale = 1, width = 6, height = 4, units = "in",
#   dpi = 300)
#
# # Demonstrate Inverse Logit and Rescale ----------------------------------- #
#
# values = seq(from = -8, to = 8, by = .5) #-10:10
# inv_logit <- boot::inv.logit(values)
# logit_df <- tibble(values, inv_logit)
#
# gg_invlogit <- ggplot(logit_df) +
#   geom_line(aes(x = values, y = inv_logit)) +
#   theme_minimal()
#
# gg_invlogit
#
# # Code to rescale
# wbt_rescale_value_range(i, output, out_min_val, out_max_val, clip_min = None,
#   clip_max = None)

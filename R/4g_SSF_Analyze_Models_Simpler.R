###################### ModelFit_SSF_Analyze_Models #############################

########################### LOAD PACKAGES AND DATA  ############################
# Load libraries, scripts, and input parameters
pacman::p_load(plyr, dplyr, optimx, ggplot2, ggthemes, lubridate, optimx, purrr,
  raster, readr, reproducible, rgenoud, stringr, summarytools, survival,
  surveybootstrap, tibble, tictoc, tidyr, xtable)
library(baear, gisr)

# Calculate the percentiles for the covariates ---------------------------------
model_dir = "Output/Analysis/SSF/Models/best_simpler_fits"
preds_tbl_file = file.path(model_dir, "preds_tbl.rds")
quantile_tbl_file = file.path(model_dir, "covar_quantiles.rds")
covars_crop_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters/Covars_Crop"

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
  probs_char, probs_names, prob_tbl, quantile_row_i, quantile_tbl,
  quantile_tbl_file)

# Extract model data (terms, coefs, etc.) from best_ssf_fit_models -------------
mod_fit_dir = "Output/Analysis/SSF/Models"
model_data_file = "Output/Analysis/SSF/Models/best_simpler_fits/model_data.rds"

best_ssf_fit_all_org <- readRDS(file.path(mod_fit_dir, "best_simpler_fits",
  "best_ssf_simpler_fit_all.rds"))

best_fit_all <- best_ssf_fit_all_org %>%
  mutate(start_behavior = word(step_type, 1, sep = "_")) %>%
  mutate(end_behavior = word(step_type, 2, sep = "_"))

start_step_types <- best_fit_all %>% pull(start_behavior) %>% unique(.)

# Extract model data using a for loop of each starting behaviors
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
    model_data <- model_data %>%
      bind_rows(ssf_best_fits_table_i)
  }
}

saveRDS(model_data, model_data_file)

# Combine the model fit data w/ raster quantile data and predict margins -------
mod_fit_dir = "Output/Analysis/SSF/Models"
model_data_file = "Output/Analysis/SSF/Models/best_simpler_fits/model_data.rds"
quantile_tbl_file = file.path("Output/Analysis/SSF/Models/best_simpler_fits",
  "covar_quantiles.rds")
predictions_file = file.path("Output/Analysis/SSF/Models/best_simpler_fits",
  "covar_predictions.rds")

best_fit_all <- readRDS(file.path(mod_fit_dir, "best_simpler_fits",
  "best_ssf_simpler_fit_all.rds"))
model_data <- readRDS(model_data_file)
quantile_tbl <- readRDS(quantile_tbl_file)

fits_quantile_tbl <- model_data %>%
  left_join(., quantile_tbl, c("term" = "covar")) %>%
  dplyr::select(-c("exp(coef)":"p"))

pred_list_all <- vector(mode = "list", length = nrow(best_fit_all))
i <- j <- 1
for (i in seq_len(nrow(best_fit_all))){
  best_fit_i <- best_fit_all %>% slice(i)
  step_type_i <- best_fit_i %>% pull(step_type)
  print(paste0("step_type = ", step_type_i))
  clogit_i <- best_fit_i %>% pull(clogit_fit) %>% pluck(1)
  fits_quantile_i <- fits_quantile_tbl %>% filter(step_type == step_type_i)
  pred_list_i <- vector(mode = "list", length = nrow(fits_quantile_i))
  for (j in seq_len(nrow(fits_quantile_i))){
    fits_quantile_ij <- fits_quantile_i %>% slice(j)
    term_ij <- fits_quantile_ij %>% pull(term)
    print(paste0("term_ij = ", i,"-", j, " ", term_ij))
    term_tbl_ij <- fits_quantile_ij %>%
      pivot_longer(starts_with("p_"), names_to = "percentile") %>%
      mutate(percentile = parse_number(percentile)) %>%
      mutate(covar_value = value) %>%
      rename(!!term_ij := value)
    other_terms_tbl_ij  <- fits_quantile_i %>% slice(-c(j)) %>%
      dplyr::select(term, p_50) %>%
      rename(value = p_50) %>%
      pivot_wider(names_from = term, values_from = value)
    if(nrow(other_terms_tbl_ij) > 0){
      terms_tbl_ij <- bind_cols(term_tbl_ij, other_terms_tbl_ij)
    } else {
      terms_tbl_ij <- term_tbl_ij
    }
    coefs_i <- clogit_i$coefficients
    ssf_formula <- paste0("pred = (", paste(coefs_i, names(coefs_i), sep = "*",
      collapse = " ) + ("), ")")
    AddPredictions = function(df, s){
        q = quote(mutate(df, z = s))
        eval(parse(text = sub("z = s", s, deparse(q))))
    }
    preds_tbl_ij <- AddPredictions(terms_tbl_ij, ssf_formula)
    pred_list_i[[j]] <- preds_tbl_ij
    print("done")
  }
  pred_list_all[[i]] <- pred_list_i %>%
    reduce(bind_rows)
}

predictions_tbl_all <- pred_list_all %>%
  reduce(bind_rows) %>%
  mutate(prob = boot::inv.logit(pred)) %>%
  dplyr::select(step_type, term, coef, percentile, covar_value, pred, prob)

saveRDS(predictions_tbl_all, predictions_file)

# Plots of Marginal Predicted Fits ---------------------------------------------

# Theme (for LaTeX font)
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"
file_dir <- "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters/Step_Types_Simpler"
suppressMessages(extrafont::loadfonts(device="win"))
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))
pointSize = 2; textSize = 5; spaceLegend = 1

predictions_file = file.path("Output/Analysis/SSF/Models/best_simpler_fits",
  "covar_predictions.rds")
predictions_tbl_all <- readRDS(predictions_file)

step_types <- unique(predictions_tbl_all$step_type)
i <- step_types[1]
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

    # Predicted Value (not converted to inverse logit)
    gg_predict_ij <- ggplot(predictions_ij, aes(x = covar_value, y = pred)) +
      geom_line(color = "blue", size = 1.5) +
      #geom_smooth() +
      xlab(j_name) +
      ylab("Value") +
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
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.01)))
    ggsave(filename = paste0("Predict_", ij_name, ".png"), plot = gg_predict_ij,
      path = file.path(tex_dir, "Figures/Ch2/Step_Type_Covar_Predict"),
      scale = 1, width = 6, height = 4, units = "in",
      dpi = 300)

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
    ggsave(filename = paste0("Prob_", ij_name, ".png"), plot = gg_prob_ij,
      path = file.path(file_dir, "Margin_Plots"),
      scale = 1, width = 6, height = 4, units = "in",
      dpi = 300)
    # SAVE FOR DISSERTATION FIGURES
    # ggsave(filename = paste0("Prob_", ij_name, ".png"), plot = gg_prob_ij,
    #   path = file.path(tex_dir, "Figures/Ch2/Step_Type_Covar_Prob"),
    #   scale = 1, width = 6, height = 4, units = "in",
    #   dpi = 300)
  }
}

# Demonstrate Inverse Logit and Rescale ----------------------------------------

values = seq(from = -8, to = 8, by = .5) #-10:10
inv_logit <- boot::inv.logit(values)
logit_df <- tibble(values, inv_logit)

gg_invlogit <- ggplot(logit_df) +
  geom_line(aes(x = values, y = inv_logit)) +
  theme_minimal()

gg_invlogit

# Code to rescale
wbt_rescale_value_range(i, output, out_min_val, out_max_val, clip_min = None,
  clip_max = None)

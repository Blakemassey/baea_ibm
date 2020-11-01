  OptimizeGlmSigma(covars_scale, covars_fixed,
        sigma_domains, sigma_starts, pop_size, model, model_num)


    mutate(opt_fit = future_pmap(.l = list(covars_scale, covars_fixed,
        sigma_domains, sigma_starts, pop_size, model, model_num),
        .f = possibly(OptimizeGlmSigma, otherwise = NA_real_),
        .progress = TRUE, .options = future_options(seed = TRUE))) %>%
      mutate(fit_aicc = map_dbl(opt_fit, ExtractAICc))
    tbl_models_fitted_list[[i]] <- tbl_models_fitted_i



  tbl_models_fitted <- tbl_models_fitted_list %>%
    reduce(bind_rows)
  print(paste0("Finished ", step_type, " at ", now()))






  print(paste0("Starting ", step_type, " : ", now()))
  tbl_models_fitted_list <- list()

  print(paste0("Model group: ", i, " of ", length(unique(model_grp))))

  tbl_models_fitted_i_test <- tbl_models %>%
    filter(model_grp == i) %>% slice(10)


covars_scale <- tbl_models_fitted_i_test %>% pluck("covars_scale", 1)
covars_fixed <- tbl_models_fitted_i_test %>% pluck("covars_fixed", 1)
sigma_domains <- tbl_models_fitted_i_test %>% pluck("sigma_domains", 1)
sigma_starts <- tbl_models_fitted_i_test %>% pluck("sigma_starts", 1)
pop_size <- tbl_models_fitted_i_test %>% pluck("pop_size", 1)
model <- tbl_models_fitted_i_test %>% pluck("model", 1)


OptimizeGlmSigma <- function(covars_scale, covars_fixed, sigma_domains,
    sigma_starts, pop_size, model, mod_num){
    ua_data <- ua_steps_i
    sigma_n <- length(covars_scale)
    domains <- sigma_domains # domains are min & max for each covariate
    domains_num <- mapply(as.numeric, domains) # Fixes a FRUSTRATING bug!
    dim(domains_num) <- dim(domains) # rgenoud NOW REQUIRES DOMAINS BE NUMERIC!
    starting_values <- sigma_starts # starting values for covariates
    parms <- starting_values
    if (length(covars_scale) == 0){ # not optimizing sigmas
      covars_fixed_0 <- paste0(covars_fixed, "0")
      preds <- paste(c("-1", covars_fixed_0), collapse = " + ")
      glm_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
      opt_fit <- tryCatch({
        suppressMessages(opt_fit <- glm(glm_model_formula,
          data = ua_steps_i, family = 'binomial'))
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
      FitGlmSigma <- function(sigmas, ua_data){
        covars_scale_sigmas <- paste0(covars_scale, sigmas)
        covars_fixed_0 <- paste0(covars_fixed, "0")
        preds <- paste(c("-1", covars_scale_sigmas, covars_fixed_0),
          collapse = " + ")
        glm_model_formula <- as.formula(paste('case', preds, sep = " ~ "))
        glm_fit <- glm(glm_model_formula, data = ua_data,
          family = 'binomial')
        model_aicc = AICc(glm_fit)
        return(model_aicc)
      }
      opt_fit <- tryCatch({
        opt_fit <- genoud(fn = FitGlmSigma, nvars = sigma_n,
          pop.size = pop_size, starting.values = starting_values,
          optim.method = "SANN", max.generations = max_generations,
          hard.generation.limit = FALSE, wait.generations = wait_generations,
          solution.tolerance = 0.0001, P5 = 0, P6 = 0, P7 = 0, P8 = 0,
          BFGSburnin = burnin_generations, print.level = 0,
          boundary.enforcement = 2, ua_data = ua_data, data.type.int = TRUE,
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


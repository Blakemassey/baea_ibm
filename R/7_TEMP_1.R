
Longitude: -68.556534
Latitude: 44.586117




predictors <- seq(-.25, 1.25, by = .01)
intercept <- in_intercept
beta1 <- in_beta1
predictors_logit <- intercept + beta1*(predictors)
df <- data.frame(predictors, probs = plogis(predictors_logit))
(y_mid_int <- (-(1*intercept/beta1)))
rect_df <- data.frame(xmin = c(-.25, 1), ymin=c(0,0), xmax= c(0,1.25),
  ymax = c(1,1))
ggplot(df) + geom_line(aes(predictors, probs), color = "blue") +
  geom_segment(aes(x = y_mid_int, y = 0, xend = y_mid_int, yend = 1),
    color = "red") +
  geom_rect(data = rect_df, alpha = .5, aes(xmin=xmin, ymin=ymin, xmax=xmax,
    ymax=ymax)) +
  annotate("text", x = y_mid_int + .03, y = .03, label = signif(y_mid_int, 2),
    color = "red") +
  ylim(0,1) + labs(x = "Predictor", y = "Probability") + theme_no_legend +
  ggtitle(paste0("Logistic Regression (", "intercept = ", intercept,
    ", beta1 = ", beta1, ")")) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0))



## This temp file is for creating a script to get covariate values for a datalayer at
## various bandwidths then to fit models for each of them.

# Incoming variables
covar_name
covar_mat
covar_ras
temp_ras
ua_data
cell_nums
cell_size
temp_ras <- raster(covar_ras)

bandwidths <- c(seq(0, 270, by=30), seq(300, 1350, by=30),
  seq(1500, 3000, by=300))  # radius (meters)

covariate_cols <- paste0(rep(covar_name, each=length(bandwidths)),
    rep(bandwidths, times=1))
ua_data[, covariate_cols] <- NA

for(m in seq_along(bandwidths)){
  sigma <- bandwidths[m]/xres(covar_ras)
  col_name <- paste0(covar_name, bandwidths[m])
  print(paste0("Starting:", col_name))
  if(bandwidths[m] == 0){
    ua_data[, col_name] <- covar_ras[cell_nums]
  } else {
    values(temp_ras) <- gauss2dsmooth(covar_mat, lambda=sigma,
      nx=nrow(temp_ras), ny=ncol(temp_ras))
    ua_data[, col_name] <- temp_ras[cell_nums]
    temp_ras[] <- NA
  }
}

all_models <- data.frame(covar_type = covar_name,
  bw = bandwidths, aic = NA, coef = NA, opt_bw = NA)
opt_models <- cbind(data.frame(covar_type = unique(covar_name)), bw = NA)

colnames_i <- str_subset(colnames(ua_data), covar_name)
bandwidths_i <- unique(str_replace_all(colnames_i, "[:^digit:]", ""))
covar_models_list <- vector("list", length(bandwidths_i))
names(covar_models_list) <- as.character(bandwidths_i)
for (j in seq_along(bandwidths_i)){
  covar_bw_name <- paste0(covar_name, bandwidths_i[j])
  model_formula <- as.formula(paste("case ~ ", covar_bw_name))
  covar_model <- glm(model_formula, family=binomial, data = ua_data)
  covar_models_list[[j]] <- covar_model
  row_num = which(all_models$covar_type == covar_name &
      all_models$bw == bandwidths[j])
  print(row_num)
  covar_mod_aic <- AIC(covar_model) #$aic
  covar_mod_coef <- as.numeric(coef(covar_model)[1])
  all_models[row_num, "aic"] <- ifelse(is.null(covar_mod_aic), NA, covar_mod_aic)
  all_models[row_num, "coef"] <- ifelse(is.null(covar_mod_coef), NA, covar_mod_coef)
}
#lapply(covar_models_list, summary)
bandwidths_x <- seq(0, 3000, by=300)
aic_table <- aictab(covar_models_list, second.ord = FALSE) %>% arrange(AIC)
aic_table$covar_type <- covar_name
aic_table$opt_bw <- as.numeric(as.character(aic_table[1,1]))
opt_bw <- as.numeric(as.character(aic_table[1,1]))
all_models[all_models$covar_type == covar_name, "opt_bw"] <- opt_bw
opt_models[opt_models$covar_type == covar_name, "bw"] <- opt_bw
g <- ggplot(aic_table, aes(x = as.numeric(as.character(Modnames)), y = AIC)) +
  #geom_line(color="red") +
  geom_point() + xlab("Bandwidth") +
  ggtitle(covar_name) +
  theme_no_legend
if (nrow(aic_table) > 1){
  g <- g +
    scale_x_continuous(breaks = as.numeric(bandwidths_x)) +
    geom_vline(xintercept = opt_bw, color="blue", linetype='dashed') +
    geom_line(color="red") +
    annotate("text",
      x = opt_bw + (diff(range(as.numeric(aic_table$Modnames)))*.025),
      y = max(aic_table$AIC), label = as.character(opt_bw), color = "blue")
}
g

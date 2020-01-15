## Script for using 'rgeound' optiminzation method to fit sigma values.
##

# Load packages, helpers, and functions
source("R/Assets/Assets_Sigma_Optimization.R")
file_dir <- "C:/Work/R/Projects/Sigma_Opt"

# Load Covar Rasters (if needed) -----------------------------------------------

load_covars <- TRUE
if (isTRUE(load_covars)){
  covar1 <- raster("Data/covar1.tif")
  covar2 <- raster("Data/covar2.tif")
  covar3 <- raster("Data/covar3.tif")
  names(covar1) <- "elev"
  names(covar2) <- "develop"
  names(covar3) <- "gauss"
}

# Set Opt Sigma Seq ------------------------------------------------------------

opt_sigmas <- seq(0, 40, by = 1)
length(opt_sigmas)^3 # Number of sigma combinations

# Create Covar Sigma Rasters Brick ---------------------------------------------

covar_brick <- brick(c(
  tibble(sigma = opt_sigmas, covar = "covar1") %>% pmap(., SmoothRaster),
  tibble(sigma = opt_sigmas, covar = "covar2") %>% pmap(., SmoothRaster),
  tibble(sigma = opt_sigmas, covar = "covar3") %>% pmap(., SmoothRaster)
  ))

covar_matrix <- raster::as.matrix(covar_brick)
covar_cols <-  setNames(seq_len(ncol(covar_matrix)), colnames(covar_matrix))
covar_names <- c(names(covar1), names(covar2), names(covar3))
rm(covar_brick)

# Fit Sigma Combo Models -------------------------------------------------------

# Make Clusters
plan(multiprocess)

tic("Fit Optimization Models 0")
df_opt_fit0 <- df_pa_data %>% slice(1:1000) %>%
  mutate(opt_fit = future_map2(number, pa_data, FitSigmaOpt3, .progress = TRUE))
toc()
saveRDS(df_opt_fit0, file.path(file_dir, paste0("df_opt_fit0_", today(),
  ".rds")))
rm(df_opt_fit0)

tic("Fit Optimization Models 1")
df_opt_fit1 <- df_pa_data %>% slice(1001:2000) %>%
  mutate(opt_fit = future_map2(number, pa_data, FitSigmaOpt3, .progress = TRUE))
toc()
saveRDS(df_opt_fit1, file.path(file_dir, paste0("df_opt_fit1_", today(),
  ".rds")))
rm(df_opt_fit1)

tic("Fit Optimization Models 2")
df_opt_fit2 <- df_pa_data %>% slice(2001:3000) %>%
  mutate(opt_fit = future_map2(number, pa_data, FitSigmaOpt3, .progress = TRUE))
toc()
saveRDS(df_opt_fit2, file.path(file_dir, paste0("df_opt_fit2_", today(),
  ".rds")))
rm(df_opt_fit2)

tic("Fit Optimization Models 3")
df_opt_fit3 <- df_pa_data %>% slice(3001:4000) %>%
  mutate(opt_fit = future_map2(number, pa_data, FitSigmaOpt3, .progress = TRUE))
toc()
saveRDS(df_opt_fit3, file.path(file_dir, paste0("df_opt_fit3_", today(),
  ".rds")))
rm(df_opt_fit3)

tic("Fit Optimization Models")
df_opt_fit4 <- df_pa_data %>% slice(4001:5000) %>%
  mutate(opt_fit = future_map2(number, pa_data, FitSigmaOpt3, .progress = TRUE))
toc()
saveRDS(df_opt_fit4, file.path(file_dir, paste0("df_opt_fit4_", today(),
  ".rds")))
rm(df_opt_fit4)

tic("Fit Optimization Models")
df_opt_fit5 <- df_pa_data %>% slice(5001:nrow(df_pa_data)) %>%
  mutate(opt_fit = future_map2(number, pa_data, FitSigmaOpt3, .progress = TRUE))
toc()
saveRDS(df_opt_fit5, file.path(file_dir, paste0("df_opt_fit5_", today(),
  ".rds")))
rm(df_opt_fit5)

future:::ClusterRegistry("stop")

df_opt_fit <- bind_rows(df_opt_fit0, df_opt_fit1, df_opt_fit2, df_opt_fit3,
  df_opt_fit4, df_opt_fit5)

# Time elapsed = 64887 Seconds

saveRDS(df_opt_fit, file.path(file_dir, paste0("df_opt_fit_", today(),
  ".rds")))

saveRDS(df_opt_fit, file.path("Output/Analysis", paste0("df_opt_fit_", today(),
  ".rds")))


theme_update(plot.title = element_text(hjust = 0.5))
xy_theme <- theme(panel.background = element_rect(fill = "grey90",
  colour = "black", size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
    colour = "white"),
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
    colour = "white"))
id_colors <- CreateColorsByAny(by="id", output=TRUE)
theme_legend <- theme(plot.title=element_text(size=24, face="bold", vjust=1.5))+
  theme(axis.title=element_text(size=20, face="bold")) +
  theme(axis.text=element_text(colour="black")) +
  theme(axis.text.x=element_text(size=16, angle=50, vjust=0.5)) +
  theme(axis.text.y=element_text(size=16, vjust=0.5))
theme_no_legend <- theme_legend + theme(legend.position="none")


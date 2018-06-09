# Load libraries, scripts, and input parameters
suppressPackageStartupMessages(library(AICcmodavg))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(optimx))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(reproducible))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(smoothie))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(survival))
suppressPackageStartupMessages(library(tictoc))
suppressPackageStartupMessages(library(velox))
library(baear)
library(gisr)
library(ibmr)
options(stringsAsFactors=FALSE)
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

tic_msg <- function(tic, msg) {
  if (is.null(msg) || is.na(msg) || length(msg) == 0){
    outmsg <- paste(lubridate::duration(round(toc - tic)))
  } else {
    outmsg <- paste0("Starting: ", msg)
  }
}
toc_msg <- function(tic, toc, msg, info) {
  tt_duration <- lubridate::duration(round(toc - tic))
  if (is.null(msg) || is.na(msg) || length(msg) == 0) {
    outmsg <- paste0(tt_duration)
  } else {
    outmsg <- paste0(info,": ", tt_duration)
  }
}

# Projection data
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Landscape file locations
      base_file <- "C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"
        lc_file <- "C:/ArcGIS/Data/Landcover/Landcover/lc_30mc.tif"
 developed_file <- "C:/ArcGIS/Data/R_Input/BAEA/developed_30mc.tif"
    forest_file <- "C:/ArcGIS/Data/R_Input/BAEA/forest_30mc.tif"
open_water_file <- "C:/ArcGIS/Data/R_Input/BAEA/open_water_30mc.tif"
   pasture_file <- "C:/ArcGIS/Data/R_Input/BAEA/pasture_30mc.tif"
shrub_herb_file <- "C:/ArcGIS/Data/R_Input/BAEA/shrub_herb_30mc.tif"
   wetland_file <- "C:/ArcGIS/Data/R_Input/BAEA/wetland_30mc.tif"

# BAEA and Movement Parameters
baea_steps_file <- "Data/BAEA/baea_steps.rds"
move_pars_file <- "Results/Analysis/Tables/Movements/move_pars.RDS"

# Subsetting Variables
subsetting_bandwidths <- TRUE
subsetting_covars <- TRUE
subsetting_ids <- FALSE
subsetting_step_types <- TRUE

##########  IMPORT BASE RASTER, STEPS DATA, AND MOVEMENT PARAMETERS ############

base <- raster(base_file)
baea_steps <- readRDS(file = baea_steps_file)
move_pars <- readRDS(file = move_pars_file)
rm(baea_steps_file, move_pars_file)

#######################  IMPORT COVARIATE RASTERS ##############################

# Import rasters
lc <- raster(lc_file)
developed <- raster(developed_file)
forest <- raster(forest_file)
open_water <- raster(open_water_file)
pasture <- raster(pasture_file)
shrub_herb <- raster(shrub_herb_file)
wetland <- raster(wetland_file)
  # plot(developed$as.RasterLayer(band = 1))
rm(base_file, developed_file, forest_file, lc_file, open_water_file,
  pasture_file, shrub_herb_file, wetland_file)

############ SPECIFY LANDSCAPE COVARIATES AND BANDWIDTHS  ######################

cell_size <- 30
cell_radius <- cell_size/2
bandwidths <- c(seq(15, 750, by=15), seq(825, 3000, by=75)) # radius (meters)
covar_stack <- stack(developed, forest, open_water, pasture, shrub_herb,wetland)
names(covar_stack) <- str_replace_all(names(covar_stack), "_30mc", "")
covar_types <- names(covar_stack)

if (subsetting_bandwidths == TRUE){  # subset data for testing
  rm(bandwidths)
  bandwidths <- c(seq(0,90, by=15))
}

if (subsetting_covars == TRUE){  # subset data for testing
  rm(covar_types)
  covar_types <- c("developed", "forest", "open_water")
  covar_stack <- stack(developed, forest, open_water)
  names(covar_stack) <- str_replace_all(names(covar_stack), "_30mc", "")
}

if (subsetting_step_types == TRUE){  # subset data for testing
  baea_steps_all <- baea_steps
  unique(baea_steps_all$behavior_behavior)
  baea_steps <- baea_steps_all %>% filter(behavior_behavior %in%
    c("Perch -> Perch"))
  unique(baea_steps$behavior_behavior)
}

if (subsetting_ids == TRUE){  # subset data for testing
  baea_steps <- baea_steps %>% filter(id %in% c("Sandy", "Norway"))
  #i <- j <- k <- m <- 1
}

covariate_cols <- paste0(rep(covar_types, each=length(bandwidths)),
  rep(bandwidths,times=length(covar_types)))

rm(developed, forest, lc, open_water, pasture, shrub_herb, wetland)

#### ----------------------------------------------------------------------- ###
####       CALCULATE KERNEL-WEIGHTED COVARIATE VALUES FOR AVAILABLE          ###
#### ----------------------------------------------------------------------- ###

# Start of Large 'for loop' for Each 'step_type' (i.e. behavior_behavior)

for (i in seq_along(unique(baea_steps$behavior_behavior))){
  tic.clearlog()
  # Subset 'baea_steps' to Step Type -------------------------------------------
  step_type_i <- unique(baea_steps$behavior_behavior)[i]
  step_type_i_name <- str_to_lower(str_replace_all(step_type_i, " -> ", "_"))
  tic(paste0(step_type_i_name, "-NA-NA-NA"), quiet = FALSE,
    func.tic = tic_msg)
  steps_i <- baea_steps %>%
    filter(behavior_behavior == step_type_i) %>%
    tibble::rowid_to_column(., "step_id")

  # Create Movement Kernel for 'step_type' -------------------------------------
  move_pars_i <- move_pars %>% filter(behavior_behavior == step_type_i)
  ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise", "Flight"),
    FALSE, TRUE)
  kernel_i <- CreateMoveKernelWeibullVonMises(max_r = NULL, cellsize =cell_size,
    pars = move_pars_i, ignore_von_mises = ignore_von_mises)
  r <- (cell_size*((nrow(kernel_i)-1)/2))+(cell_size/2)
  kernel_raster <- raster::raster(kernel_i, xmn=-r, xmx=r, ymn=-r, ymx=r)
  prob_raster <- kernel_raster/raster::cellStats(kernel_raster, stat = "sum")
  prob_raster[prob_raster <= .000001] <- 0
  rm(kernel_i, kernel_raster, move_pars_i, ignore_von_mises, r)

  # Create Movement Probability Rasters for Each Step --------------------------
  move_prob_rasters <- list(rep(NA, nrow(steps_i)))
  for (step_i in seq_len(nrow(steps_i))){
    cat(paste0("Calculating: '", step_type_i_name, "' Move_Prob Raster: ",
      step_i, " of ", nrow(steps_i),"\n"))
    exp_angle <- steps_i[step_i,] %>% pull(exp_angle)
    prob_raster_rotated <- RotateRaster(prob_raster, Rad2Deg(exp_angle))
    prob_raster_resampled <- resample(prob_raster_rotated, prob_raster,
      method="bilinear")
    prob_raster_resampled[is.na(prob_raster_resampled)] <- 0
    move_prob_rasters[[step_i]] <- trim(prob_raster_resampled, values = 0)
    rm(exp_angle, prob_raster_rotated, prob_raster_resampled, step_i)
  }
  names(move_prob_rasters) <- paste0("step_", steps_i$step_id)

  # Create Dataframes for Available and Used Values ----------------------------
  covariate_df <- setNames(data.frame(matrix(ncol = length(covariate_cols),
    nrow = nrow(steps_i), NA)), covariate_cols)
  used_steps_i <- cbind(case = 1, steps_i, covariate_df)
  avail_steps_i <- cbind(case = 0, steps_i, covariate_df)
  n_total <- length(unique(steps_i$id))*length(covar_types)*length(bandwidths)
  counter <- 0

  # Calculate Kernel-Weighted Covariate Values For Available and Used ----------
  for (j in seq_along(unique(steps_i$id))){
    id_j <- unique(steps_i$id)[j]
    tic(paste0(step_type_i_name, "-", id_j, "-NA-NA"), quiet = TRUE,
      func.tic = tic_msg)
    avail_steps_ij <- avail_steps_i %>% filter(id == id_j)
    used_steps_ij <- used_steps_i %>% filter(id == id_j)
    steps_ij <- steps_i %>% filter(id == id_j)
    steps_ij_range <- c(range(steps_ij$long_utm), range(steps_ij$lat_utm))
    extent_ij <- extend(extent(alignExtent(extent(steps_ij_range), base,
      snap='out')), 9990)
    for (k in seq_along(covar_types)){
      covar_type_k <- covar_types[k]
      tic(paste0(step_type_i_name, "-", id_j, "-", covar_type_k, "-NA"),
        quiet = TRUE, func.tic = tic_msg)
      covar_layer_k <- which(names(covar_stack) == covar_type_k)
      covar_raster_k <- crop(raster(covar_stack, layer=covar_layer_k),extent_ij)
      covar_matrix_k <- as.matrix(covar_raster_k)
      for(m in seq_along(bandwidths)){
        bw_meters <- bandwidths[m]
        str_ijkm <- paste0(step_type_i_name, "-", id_j, "-", covar_type_k, "-",
          bw_meters)
        cat(paste0("Starting: (", (counter <- counter + 1), " of ", n_total,
          ") ", str_ijkm,"\n"))
        tic(str_ijkm, quiet = TRUE, func.tic = tic_msg)
        col_name <- paste0(covar_type_k, bw_meters)
        col_num_used <- which(colnames(used_steps_i) == col_name)
        col_num_avail <- which(colnames(used_steps_i) == col_name)
        if(bw_meters == 0){
          covar_raster_smooth_m <- covar_raster_k
        } else {
          covar_matrix_smooth_m <- kernel2dsmooth(covar_matrix_k,
            kernel.type="gauss", nx=nrow(covar_matrix_k),
            ny=ncol(covar_matrix_k), sigma = bandwidths[m]/cell_radius) #pixel
          covar_raster_smooth_m <- raster(covar_matrix_smooth_m,
            template = covar_raster_k)
          rm(covar_matrix_smooth_m)
        }
        for (p in seq_len(nrow(avail_steps_ij))){
          avail_steps_ijp_xy <- c(avail_steps_ij[p,] %>% pull(long_utm),
            avail_steps_ij[p,] %>% pull(lat_utm))
          step_id_p <- paste0("step_", avail_steps_ij[p, "step_id"])
          move_prob_raster_p <- shift(move_prob_rasters[[step_id_p]],
            x = avail_steps_ijp_xy[1], y = avail_steps_ijp_xy[2])
          covar_raster_smooth_p <- crop(covar_raster_smooth_m,
            move_prob_raster_p)
          covar_kernel_sum <- sum(as.matrix(covar_raster_smooth_p) *
              as.matrix(move_prob_raster_p))
          row_num <- which(avail_steps_i$step_id == avail_steps_ij[p,"step_id"])
          avail_steps_i[row_num, col_num_avail] <- covar_kernel_sum
          rm(avail_steps_ijp_xy, covar_raster_smooth_p, covar_kernel_sum,
            move_prob_raster_p, p, row_num, step_id_p)
        }
        for (q in seq_len(nrow(used_steps_ij))){
          used_steps_ijq_xy <- data.frame(x = used_steps_ij[q,] %>%
            pull(long_utm_end), y = avail_steps_ij[q,] %>% pull(lat_utm_end))
          covar_value <- extract(covar_raster_smooth_m, used_steps_ijq_xy)
          row_num <- which(used_steps_i$step_id == used_steps_ij[q, "step_id"])
          used_steps_i[row_num, col_num_used] <- covar_value
          rm(used_steps_ijq_xy, covar_value, row_num)
        }
        rm(bw_meters, str_ijkm, col_num_avail, col_num_used,
          covar_raster_smooth_m)
        toc(quiet = FALSE, log = TRUE, func.toc = toc_msg, info = "Finished")
      }
      rm(covar_type_k, covar_layer_k, covar_raster_k, covar_matrix_k)
      toc(quiet = FALSE, log = TRUE, func.toc = toc_msg, info = "Finished")
    }
    rm(id_j, avail_steps_ij, used_steps_ij, steps_ij, steps_ij_range, extent_ij)
    toc(quiet = FALSE, log = TRUE, func.toc = toc_msg, info = "Finished")
  }
  rm(counter, n_total,  covariate_df, steps_i, move_prob_rasters)
  toc(quiet = FALSE, log = TRUE, func.toc = toc_msg, info = "Finished")
  ua_steps_i <- rbind(avail_steps_i, used_steps_i) %>% arrange(step_id, case)
  saveRDS(ua_steps_i, paste0("Results/Analysis/Tables/SSF/ua_steps_",
    step_type_i_name, ".rds"))
  steps_i_tictoc_df <- data.frame(stringr::str_split(unlist(tic.log(format =
    TRUE)), "\\-|\\:", simplify = TRUE))
  steps_i_tictoc_df[steps_i_tictoc_df == "NA"] <- NA
  saveRDS(steps_i_tictoc_df, file.path("Results/Analysis/Tables/SSF/Timing",
    paste0("tictoc_", step_type_i_name, ".rds")))
  rm(avail_steps_i, used_steps_i, step_type_i_name)
  tic.clearlog()
}

############## FIT UNIVARIATE MODELS AT FIXED BANDWIDTHS #######################

ua_data <- readRDS(file.path("Results/Analysis/Tables/Step_Selection",
  "Model_Fit_01_Subsetted/ua_steps_perch_perch.rds"))

# Fit univariate models to find the "approximate" AIC-best bandwidth
# (i.e., stage-one of a two-stage analysis)

start_covar_cols  <- which(colnames(ua_data) == "lat_utm_end")+1
covar_cols <- colnames(ua_data)[start_covar_cols:length(ua_data)]
covar_types <- unique(str_replace_all(covar_cols, "[:digit:]", ""))
bandwidths <- unique(str_replace_all(covar_cols, "[:^digit:]", ""))

opt_models <- cbind(data.frame(covar_type=covar_types), bw=NA)

all_models <- data.frame(covar_type = rep(covar_types, each =
  length(bandwidths)), bw = rep(as.numeric(bandwidths), times = length(covar_types)),
  aic = NA, coef = NA, opt_bw = NA)


for (i in seq_along(covar_types)){
  covar_type_i <- covar_types[i]
  covar_models_list <- vector("list", length(bandwidths))
  names(covar_models_list) <- as.character(bandwidths)
  for (j in seq_along(bandwidths)){
    covar_bw_name <- paste0(covar_type_i, bandwidths[j])
    model_formula <- as.formula(paste("case ~ ", covar_bw_name, " + strata(step_id)"))
    covar_model <- survival::clogit(model_formula, data = ua_data, iter.max=10000)
    ## covar_mod <- survival::clogit(case ~ developed30 + strata(step_id), data = ua_data, iter.max=1000)
    ## covar_mod = glm(fmla, data=ua_data, family="binomial")

    covar_models_list[[j]] <- covar_model
    (row_num = which(all_models$covar_type == covar_type_i &
          all_models$bw == bandwidths[j]))
    covar_mod_aic <- AIC(covar_model) #$aic
    covar_mod_coef <- as.numeric(coef(covar_model)[1])
    all_models[row_num, "aic"] <- ifelse(is.null(covar_mod_aic), NA, covar_mod_aic)
    all_models[row_num, "coef"] <- ifelse(is.null(covar_mod_coef), NA, covar_mod_coef)
  }
  #lapply(covar_models_list, summary)
  aic_table <- aictab(covar_models_list, second.ord = FALSE)
  aic_table$covar_type <- covar_type_i
  aic_table$opt_bw <- as.numeric(as.character(aic_table[1,1]))
  opt_bw <- as.numeric(as.character(aic_table[1,1]))
  all_models[all_models$covar_type == covar_type_i, "opt_bw"] <- opt_bw
  opt_models[opt_models$covar_type == covar_type_i, "bw"] <- opt_bw
  g <- ggplot(aic_table, aes(x = as.numeric(as.character(Modnames)), y = AIC)) +
    geom_line(color="red") + geom_point() + xlab("Bandwidth") +
    ggtitle(covar_type_i) + geom_vline(xintercept = opt_bw, color="blue") +
    annotate("text", x = opt_bw + 100, y = min(aic_table$AIC),
    label = as.character(opt_bw), color = "blue") +
    scale_x_continuous(breaks = as.numeric(bandwidths)) +
    theme_no_legend
  print(g)
  #SaveGGPlot(file = file.path("Results/Analysis/Plots/Step_Selection/AIC",
  #  paste0("AIC_BW_", covar_type_i, ".svg")), bg = "transparent")
}

save(all_models, file=file.path("Results/Analysis/Tables/Step_Selection",
  "All_models.RData"))
save(opt_models, file=file.path("Results/Analysis/Tables/Step_Selection",
  "Opt_models.RData"))

all_models_sort <- all_models %>% arrange(covar_type, aic)
all_models_top5 <- all_models_sort %>% group_by(covar_type) %>% slice(1:5)
write.csv(all_models_top5, file=file.path("Results/Analysis/Tables",
  "Step_Selection/All_models_top5.csv"))

# Plot all AIC by bandwidths in facet_wrap
g <- ggplot(all_models, aes(x=bw, y = aic)) +
  geom_line(color="red") + geom_point() + xlab("Bandwidth") + theme_no_legend +
  facet_wrap(~covar_type, scales = "free_y") +
  geom_vline(data = ddply(all_models, "covar_type", summarize,
    opt_bw_i = min(opt_bw)), aes(xintercept=opt_bw_i), color= "blue") +
  geom_text(data = ddply(all_models, "covar_type", summarize,
    opt_bw_i = min(opt_bw), min_aic = min(aic)),
    mapping=aes(x = opt_bw_i + 300, y = min_aic, label = opt_bw_i),
    size = 4, color = "blue") +

  theme(axis.title=element_text(size=12, face="bold")) +
  theme(axis.text.x=element_text(size=8, angle=50, vjust=0.5)) +
  theme(axis.text.y=element_text(size=8, vjust=0.5))
print(g)
SaveGGPlot(file = file.path("Results/Analysis/Plots/Step_Selection/AIC",
    paste0("AIC_BW_ALL.svg")), bg = "transparent")

### Ended 2018-06-08
### STILL THINKING ABOUT HOW TO SPLIT OUT RESULTS?? BY TYPE OR TOPIC???

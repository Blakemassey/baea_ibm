
############################# ModelFit_SSF #####################################
# Load libraries, scripts, and input parameters --------------------------------
pacman::p_load(AICcmodavg, plyr, dplyr, ggplot2, ggthemes, optimx, raster,
  reproducible, rgdal, smoothie, stringr, survival, tictoc) #spatialfil
pacman::p_load(baear, gisr, ibmr)
options(stringsAsFactors=FALSE)
#setwd("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm")


# Output paths
ua_data_dir <- "Output/Analysis/SSF/UA_Data"

ua_data_files <- list.files(ua_data_dir, full.names = TRUE)

for (i in seq_along(ua_data_files)){
  ua_data_i <- readRDS(ua_data_files[i])
  unique(ua_data_i$behavior_behavior)
  identical(head(ua_data_i$tpi0), head(ua_data_i$tri0), head(ua_data_i$roughness0))
  identical(ua_data_i$tpi0, ua_data_i$tpi30)
  identical(ua_data_i$tri0, ua_data_i$tri30)
  identical(ua_data_i$roughness0, ua_data_i$roughness30)
  ua_data_i_out <- ua_data_i %>%
    mutate(tpi0 = tpi30,
           tri0 = tri30,
           roughness0 = roughness30)
  updated <- all(identical(ua_data_i_out$tpi0, ua_data_i_out$tpi30),
      identical(ua_data_i_out$tri0, ua_data_i_out$tri30),
      identical(ua_data_i_out$roughness0, ua_data_i_out$roughness30))
  if(updated) saveRDS(ua_data_i_out, ua_data_files[i])
}

  saveRDS(ua_steps_i, file.path(ua_data_dir, paste0("ua_steps_",
    step_type_i_name, ".rds")))



# # Rename files
# fit_files <- list.files(file.path(mod_fit_dir, "cruise_perch"),
#   full.names = TRUE)
# for (i in seq_along(fit_files)){
#   fit_file_i <- fit_files[i]
#   fit_file_i_new <- str_replace(fit_file_i, "models", "ssf")
#   file.rename(fit_file_i, fit_file_i_new)
# }

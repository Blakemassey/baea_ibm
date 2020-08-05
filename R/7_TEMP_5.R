for (i in 1:nrow(preds_terrain)){
  preds_terrain_i <- preds_terrain %>%
    slice(i)
  raster_terrain_i <- preds_terrain_i %>%
    mutate(raster_layer = map2(.x = sigma, .y = covar,
      .f = CalculateTerrainMetricWithSigma)) %>%
    pluck("raster_layer", 1)
  raster_terrain_i_name <- preds_terrain_i %>% pull(covar_sigma)
  names(raster_terrain_i) <- raster_terrain_i_name
  writeRaster(raster_terrain_i, file.path(file_dir, "SSF_Rasters", paste0(
    raster_terrain_i_name, ".tif")), format = "raster", overwrite = TRUE)
  rm(raster_terrain_i)
}

for (i in 1:nrow(preds_terrain)){
  preds_terrain_i <- preds_terrain %>%
    slice(i)
  raster_terrain_i <- preds_terrain_i %>%
    mutate(raster_layer = map2(.x = sigma, .y = covar,
      .f = CalculateTerrainMetricWithSigma)) %>%
    pluck("raster_layer", 1)
  raster_terrain_i_name <- preds_terrain_i %>% pull(covar_sigma)
  names(raster_terrain_i) <- raster_terrain_i_name
  writeRaster(raster_terrain_i, file.path(file_dir, "SSF_Rasters", paste0(
    raster_terrain_i_name, "_3")), format = "raster", overwrite = TRUE)
  rm(raster_terrain_i)
}


preds_terrain <- preds_tbl %>%
  filter(raster_class == "terrain_class") %>%

  for (i in 1:nrow(preds_terrain)){
  preds_terrain_i <- preds_terrain %>%
    slice(i)
  sigma <- preds_terrain_i %>% pull(sigma)
  covar <- preds_terrain_i %>% pull(covar)
  raster_file <- file.path(file_dir, paste0(covar, "_30mc.tif"))
  print(paste0("Starting: ", covar, sigma, " (", i, " of ", nrow(preds_terrain),
    ") at: ", lubridate::now()))

  top_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Kernel",
    paste0(covar, sigma, "_top.grd"))
  mid_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Kernel",
    paste0(covar, sigma, "_mid.grd"))
  bot_file <- file.path(file_dir, "SSF_Rasters/Processed_Sections/Kernel",
    paste0(covar, sigma, "_bot.grd"))
  out_file <- file.path(file_dir, "SSF_Rasters", paste0(covar, sigma, ".tif"))

  # Covar TOP
  print("top")

  raster_terrain_i <- preds_terrain_i %>%
    mutate(raster_layer = map2(.x = sigma, .y = covar,
      .f = CalculateTerrainMetricWithSigma))

  covar_top <- crop(raster(raster_file), ext_top, snap = "near")
  covar_top_smooth <- raster::raster(covar_top) # creates blank raster
  values(covar_top_smooth) <-
    smoothie::gauss2dsmooth(raster::as.matrix(covar_top),
    lambda = sigma, nx = DescTools::RoundTo(nrow(covar_top), 2),
    ny = DescTools::RoundTo(ncol(covar_top), 2))
  writeRaster(covar_top_smooth, top_file, format = "raster", overwrite = TRUE)
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
  writeRaster(covar_mid_smooth, mid_file, format = "raster", overwrite = TRUE)
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
  writeRaster(covar_bot_smooth, bot_file, format = "raster", overwrite = TRUE)
  rm(covar_bot, covar_bot_smooth)
  gc()

  # Merge together rasters
  top_raster <- raster(top_file)
  mid_raster <- raster(mid_file)
  bot_raster <- raster(bot_file)
  out_raster <- raster::merge(mid_raster, top_raster, bot_raster)

  # Write out merged raster
  writeRaster(out_raster, out_file, format = "GTiff", overwrite = TRUE)

}

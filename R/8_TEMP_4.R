## Create Grand Lake Turbines Distance Rasters ---------------------------------

# Import base_50km
grand_lake_base_50km <- raster(file.path(gis_exp_dir, "grand_lake_base_50km.tif"))

# Import wind turbine data
grand_lake_n_turbines_sf <- readRDS(file.path(wind_output_dir,
  "grand_lake_n_turbines.rds"))
grand_lake_s_turbines_sf <- readRDS(file.path(wind_output_dir,
  "grand_lake_s_turbines.rds"))

grand_lake_n_turbines_30mc <- rasterize(as_Spatial(grand_lake_n_turbines_sf),
  y = grand_lake_base_50km, field = 'id', small = TRUE)
grand_lake_s_turbines_30mc <- rasterize(as_Spatial(grand_lake_s_turbines_sf),
  y = grand_lake_base_50km, field = 'id', small = TRUE)

grand_lake_ns_turbines_30mc <- sum(grand_lake_n_turbines_30mc,
  grand_lake_s_turbines_30mc, na.rm=TRUE)
grand_lake_ns_turbines_30mc[grand_lake_ns_turbines_30mc == 0] <- NA

grand_lake_n_dist_30mc <- distance(grand_lake_n_turbines_30mc, doEdge = TRUE)
grand_lake_s_dist_30mc <- distance(grand_lake_s_turbines_30mc, doEdge = TRUE)
grand_lake_ns_dist_30mc <- distance(grand_lake_ns_turbines_30mc, doEdge = TRUE)

# Rescale max distance to 20km (New step added in 2021-06)
grand_lake_n_dist_30mc[grand_lake_n_dist_30mc > 20000] <- 20000
grand_lake_n_dist_30mc[is.na(grand_lake_n_dist_30mc[])] <- 20000
grand_lake_s_dist_30mc[grand_lake_s_dist_30mc > 20000] <- 20000
grand_lake_s_dist_30mc[is.na(grand_lake_s_dist_30mc[])] <- 20000
grand_lake_ns_dist_30mc[grand_lake_ns_dist_30mc > 20000] <- 20000
grand_lake_ns_dist_30mc[is.na(grand_lake_ns_dist_30mc[])] <- 20000

writeRaster(grand_lake_n_dist_30mc, file.path(gis_exp_dir,
  "dist_turbines_grand_lake_n.tif"), progress = "text", datatype = 'INT2U',
  overwrite = TRUE)
writeRaster(grand_lake_s_dist_30mc, file.path(gis_exp_dir,
  "dist_turbines_grand_lake_s.tif"), progress = "text", datatype = 'INT2U',
  overwrite = TRUE)
writeRaster(grand_lake_ns_dist_30mc, file.path(gis_exp_dir,
  "dist_turbines_grand_lake_ns.tif"), progress = "text", datatype = 'INT2U',
  overwrite = TRUE)

# Create 'control' layer with all turbine_dist = 20km
grand_lake_c_dist_30mc <- grand_lake_ns_dist_30mc
grand_lake_c_dist_30mc[grand_lake_c_dist_30mc >= 0] <- 20000
plot(grand_lake_c_dist_30mc)

writeRaster(grand_lake_c_dist_30mc, file.path(gis_exp_dir,
  "dist_turbines_grand_lake_c.tif"), progress = "text", datatype = 'INT2U',
  overwrite = TRUE)


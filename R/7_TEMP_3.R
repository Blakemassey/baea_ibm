rasterOptions(maxmem=Inf, memfrac=.8)
canProcessInMemory(elev_org, verbose=TRUE)

## Roughness, TPI, TRI ----
# Roughness: difference between the maximum and the minimum value of a cell
#  and its surrounding cells.
# Topographic Position Index (TPI): is the difference between the value of a
#  cell and the mean value of its surrounding cells.
# Terrain Ruggedness Index (TRI): the mean of the absolute differences between
#  the value of a cell and the value of its surrounding cells.

rough_dir <- "C:/ArcGIS/Data/Elevation/Terrain_Metrics/Roughness"
tpi_dir <-"C:/ArcGIS/Data/Elevation/Terrain_Metrics/Topographic_Postition_Index"
tri_dir <- "C:/ArcGIS/Data/Elevation/Terrain_Metrics/Terrain_Ruggedness_Index"
r_input <- "C:/ArcGIS/Data/R_Input/BAEA"

tri_scale_meters <- c(30, 60) #seq(30, 300, by = 30)
tri_windows <- ((tri_scale_meters/30)*2)+1

# Needed to create 3 over-lapping rasters for initial data analysis (cannot
# allocate vector sufficient for an analysis run on the entire state)

x_min <- xmin(elev)
x_max <- xmax(elev)
y_min <- ymin(elev)
y_max <- ymax(elev)
(y_half <- (ymax(elev) - ymin(elev))*(1/2))
(y_third <- (ymax(elev) - ymin(elev))*(1/3))

elev_1 <- crop(elev, extent(x_min, x_max, y_min, y_max - y_half))
elev_2 <- crop(elev, extent(x_min, x_max, y_min + y_third,
  y_min + y_third + y_half))
elev_3 <- crop(elev, extent(x_min, x_max, y_min + y_half, y_max))
rm(elev)

for(i in seq_along(tri_windows)){
  tri1_name <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_1.tif"))
  print(paste0("Working on: ", basename(tri1_name)))
  tri_1 <- CalculateTerrainMetric(elev_1, tri_windows[i], metric = "tri")
  writeRaster(tri_1, file.name = tri1_name, overwrite = TRUE)
  rm(tri_1, tri1_name)
}
rm(elev_1)

for(i in seq_along(tri_windows)){
  tri2_name <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_2.tif"))
  print(paste0("Working on: ", basename(tri2_name)))
  tri_2 <- CalculateTerrainMetric(elev_2, tri_windows[i], metric = "tri")
  writeRaster(tri_2, file.name = tri1_name, overwrite = TRUE)
  rm(tri_2, tri2_name)
}
rm(elev_2)

for(i in seq_along(tri_windows)){
  tri3_name <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_3.tif"))
  print(paste0("Working on: ", basename(tri3_name)))
  tri_3 <- CalculateTerrainMetric(elev_3, tri_windows[i], metric = "tri")
  writeRaster(tri_3, file.name = tri1_name, overwrite = TRUE)
  rm(tri_3, tri3_name)
}
rm(elev_3)

for(i in seq_along(tri_windows)){
  tri_1 <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_1.tif"))
  tri_2 <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_2.tif"))
  tri_3 <- file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_3.tif"))
  tri_30mc <-  file.path(tri_dir, paste0("tri_", str_pad(tri_scale_meters[i], 3,
    side="left", pad = c("0")), "_30mc.tif"))
  merge_132 <- merge(raster(tri_1), raster(tri_3), raster(tri_2),
    filename=tri_30mc, overwrite = TRUE)
}


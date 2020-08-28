
# Checking for issues with the step type Nest -> Perch
# Currently, all of the simulated birds seem to file to the edges/corners of the
# redistribution_kernel

base <- sim$spatial$base


# Create Move Kernels for 'Nest -> Perch' around each nest
for (i in c(1, 2)){
  print(i)
  start_x <- sim$agents$input$start_x[i]
  start_y <- sim$agents$input$start_y[i]
  move_org <- sim_out$run_1$spatial$classes$male$move_kernels$`3_4`
  plot(move_org)
  move_rotated <- suppressWarnings(RotateRaster(move_org, Rad2Deg(0), resolution=raster::res(base)))
  plot(move_rotated)
  move_crop <- raster::crop(move_rotated, move_org, snap = "near")
  plot(move_crop)
  move_resample <- raster::resample(move_rotated, move_org, method = "ngb")
  plot(move_resample)
  move_shift <- raster::shift(move_resample, dx = start_x,  dy = start_y)
  raster::crs(move_shift) <- raster::crs(base)
  move_kernel <- raster::crop(move_shift, base, snap="in")
  raster::writeRaster(move_kernel, paste0("C:/TEMP/Sim3/move_kernel", i, ".tif"), overwrite = TRUE)
  land <- sim$spatial$landscape$land[[sim$agents$input$nest_id[i]]]
  land_kernel <- raster::crop(land, move_kernel, snap = "in")
  land_kernel <- raster::extend(land_kernel, move_kernel, value = 0)
  land_kernel <- raster::mask(land_kernel, move_kernel)
  raster::writeRaster(land_kernel, paste0("C:/TEMP/Sim3/land_kernel", i, ".tif"), overwrite = TRUE)
}

# Create Land Kernels for 'Nest -> Perch' around each nest


# Create SSF Kernels for 'Nest -> Perch' around each nest
for (i in c(1, 3, 5, 7)){
  print(i)
  start_x <- sim$agents$input$start_x[i]
  start_y <- sim$agents$input$start_y[i]
  move_org <- sim_out$
  plot(move_org)
  move_rotated <- suppressWarnings(RotateRaster(move_org, Rad2Deg(0), resolution=raster::res(base)))
  plot(move_rotated)
  move_crop <- raster::crop(move_rotated, move_org, snap = "near")
  plot(move_crop)
  move_resample <- raster::resample(move_rotated, move_org, method = "ngb")
  plot(move_resample)
  move_shift <- raster::shift(move_resample, dx = start_x,  dy = start_y)
  raster::crs(move_shift) <- raster::crs(base)
  move_kernel <- raster::crop(move_shift, base, snap="in")
  raster::writeRaster(move_kernel, paste0("C:/TEMP/Sim2/ssf_kernel", i, ".tif"))
}


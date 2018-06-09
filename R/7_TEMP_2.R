r <- raster(nrow=19, ncol=19)
m <- matrix(1:ncell(r), nrow=19)
r[] <- as.vector(t(m))
extent(r) <- extent(-9.5, 9.5, -9.5, 9.5)
plot(r)
rr <- RotateRaster(r, 315)
plot(rr)


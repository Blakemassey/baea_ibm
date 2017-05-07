




#' CreateRedistKernel
#'
#' Create a redistribution kernel matrix based on a wrapped Cauchy distribution
#'   for direction and a Pareto distribution for distance.
#'
#' @usage CreateRedistKernel(max_r, cellsize, mu, rho, shape, scale,
#'    ignore_cauchy, ignore_pareto)
#'
#' @param max_r maximum radius of kernel in meters, default = 300
#' @param cellsize cell size in meters, default = 30
#' @param mu mu parameter of wrapped Cauchy distribution, 0 radians is due east
#'   because everything is based on the Unit Circle
#' @param rho rho parameter of wrapped Cauchy distribution
#' @param shape shape parameter of Pareto distribution
#' @param scale scale parameter of Pareto distribution
#' @param ignore_cauchy logical, removes cauchy kernel's contribution to output
#'   raster. Default is FALSE.
#' @param ignore_pareto logical, removes pareto kernel's contribution to output
#'    raster. Default is FALSE.
#'
#' @return matrix
#' @export
CreateRedistKernel <- function(max_r = 300,
                               cellsize = 30,
                               mu,
                               rho,
                               shape,
                               scale,
                               ignore_cauchy = FALSE,
                               ignore_pareto = FALSE) {
  # Create the empty kernel objects
  max_r_cells <- ceiling(max_r/cellsize)
  size <- max_r_cells * 2 + 1
  center <- max_r_cells + 1
  wrpc_kernel <- new("matrix", 0, size, size)
  gpd_kernel <- new("matrix", 0, size, size)
  for (i in 1:size) {
    for (j in 1:size) {
      r = sqrt((i - center)^2 + (j - center)^2) * cellsize
      b = AngleToPoint(center, center, j, i)
      if(r <= max_r){
        wrpc_kernel[i, j] <- round(suppressWarnings(circular::dwrappedcauchy(b,
          mu=mu, rho=rho)), 5)
        gpd_kernel[i, j] <- texmex::dgpd(r, sigma=scale, xi=shape, log=FALSE)
      }
    }
  }
  wrpc_kernel <- apply(wrpc_kernel, 2, rev)
  gpd_kernel[center, center] <- 1/scale
  # This last part deletes the cells at the edge if they are all zero
  if (all(wrpc_kernel[1, ] == 0, wrpc_kernel[, 1] == 0,
    wrpc_kernel[nrow(wrpc_kernel),] == 0, wrpc_kernel[, ncol(wrpc_kernel)] ==0))
    wrpc_kernel <- wrpc_kernel[2:(nrow(wrpc_kernel) - 1), 2:(ncol(wrpc_kernel)
      - 1)]
  if (all(gpd_kernel[1, ] == 0, gpd_kernel[, 1] == 0,
    gpd_kernel[nrow(gpd_kernel),] == 0, gpd_kernel[, ncol(gpd_kernel)] == 0))
    gpd_kernel <- gpd_kernel[2:(nrow(gpd_kernel) - 1), 2:(ncol(gpd_kernel) - 1)]
  # Multiply the two kernels together and re-normalize
  if (ignore_cauchy) wrpc_kernel <- 1
  if (ignore_pareto) gpd_kernel <- 1
  redist_kernel <- gpd_kernel*wrpc_kernel
  redist_kernel <- redist_kernel/sum(redist_kernel)
  return(redist_kernel)
}











max_r = 12000
cellsize = 30
mu = 0
rho = .5
scale = 1.172086
shape = 0.7081443

redist <- CreateRedistKernelWeibull(max_r=step_max_r, cellsize=cellsize,
  mu=mu, rho=rho, shape=shape, scale=scale)

r <- (cellsize*((nrow(redist)-1)/2))+(cellsize/2)
redist_raster <- raster::raster(redist, xmn=-r, xmx=r, ymn=-r, ymx=r)
plot(redist_raster)
redist_shift <- raster::shift(redist_raster, x=step_data$x[i],
  y=step_data$y[i])


redist_shift <- raster::crop(redist_shift, base, snap="in")
con_nest <- CreateConNestProb(con_nest_raster,
  gamma_shape=connest_gamma_shape, gamma_rate=connest_gamma_rate,
  x=step_data$x[i], y=step_data$y[i], max_r=step_max_r, cellsize=cellsize,
  base=base)
print(paste0("con_nest:", as.vector(raster::extent(con_nest)),
  "redist_shift:", as.vector(raster::extent(redist_shift))))
con_nest_crop <- raster::crop(con_nest, redist_shift, snap="out")
prob_raster <- raster::overlay(redist_shift, con_nest_crop,
  fun=function(a,b){return(a*b)}, recycle=FALSE)
prob_raster <- prob_raster/raster::cellStats(prob_raster, stat="sum")
print("prob_min:", raster::minValue(prob_raster))
raster::crs(prob_raster) <- raster::crs(sim$spatial$base)





max_r_cells <- 10

  size <- max_r_cells * 2 + 1
  center <- max_r_cells + 1
  angle_matrix <- new("matrix", 0, size, size)
  row_matrix <- new("matrix", 0, size, size)
  col_matrix <- new("matrix", 0, size, size)
  distance_matrix <- new("matrix", 0, size, size)

  i <- j <-  1:size

  row_matrix[] <- rep(i, times  = max(j))
  col_matrix[] <- as.vector(row_matrix)
  identical(row_matrix, col_matrix)
  col_matrix <- t(angle_matrix)
  col_matrix
  col_matrix + row_matrix

  dx <- row_matrix - center
  dx
  dy <- col_matrix - center
  dy
  abs_angle <- atan2(dy, dx)
  abs_angle <- ifelse(abs_angle < 0, (2*pi) + abs_angle, abs_angle)
  abs_angle




library(RandomFields)
library(geostatsp)


model <- c(var=5, range=1,shape=0.5)
myraster = raster(nrows=20,ncols=30,xmn=0,ymn=0,xmx=6,ymx=4,
crs="+init=epsg:2081")
set.seed(NULL)
simu <- RFsimulate(model, x=myraster, n=3)

raster <- simu[['sim2']]
plot(raster, col=plot3D::gg.col())

par(mfrow=c(1,1))

devtools::reload("C:/Work/R/Packages/gisr")
Plot3DRaster(raster, main = "Raster")

azimuth = 45;
coaltitude = 30;
col = NULL;
border="black";
x_lab = NULL;
y_lab = NULL;
z_lab = NULL;
z_lim = NULL;
main = NULL;
legend_lab = NULL;
rgl = TRUE;
rgl_window = "screen";
spin = FALSE;
movie = FALSE;
movie_name = "RasterSpin"

  raster <- raster
  x <- raster::xFromCol(raster, col=1:ncol(raster))
  y <- raster::yFromRow(raster, row=1:nrow(raster))
  z <- t(raster::as.matrix(raster))
  z[is.na(z)] <- 0  # otherwise the hist3d() plot does not work properly
  if (is.null(col)) col <- plot3D::gg.col(length(unique(z)))
  if (is.null(x_lab)) x_lab <- "Longitude"
  if (is.null(y_lab)) y_lab <- "Latitude"
  if (is.null(z_lab)) z_lab <- ""
  if (is.null(z_lim)) z_lim <- range(z, na.rm = TRUE)
  if (is.null(main)) main <- deparse(substitute(raster))
  if (is.null(legend_lab)) legend_lab <- z_lab
  plot3D::hist3D(x=x, y=y, z=z, shade=0, nticks=5, ticktype="detailed", col=col,
    bty="b2", expand=.25, phi=coaltitude, theta=azimuth, border=border,
    facets=TRUE, axes=TRUE, image=FALSE, contour=FALSE, panel.first=NULL,
    ltheta=-135, lphi=0, space=0, add=FALSE, plot=TRUE, clab=legend_lab,
    main=main, xlab="Longitude", ylab="Latitude", zlab="", zlim=z_lim,
    colkey=list(side=4, line.clab=1,  length=.5, width=.5, adj.clab=0.1,
      dist=-.03))
  ResetGraphics <- function(){
    rgl::rgl.clear(type = "bboxdeco")
    text_ids <- subset(rgl::rgl.ids(), type=="text", select="id")
    for (i in 1:nrow(text_ids)){
      rgl::rgl.pop(id=text_ids[i,"id"])
    }
    par(mar=c(2, 2, 2, 2)+.01, las=2)
    rgl::axis3d('x--', ntick=7)  # can be adjust to add more or fewer tick marks
    rgl::axis3d('y+-', ntick=7)  # can be adjust to add more or fewer tick marks
    rgl::axis3d('z--', ntick=4)  # can be adjust to add more or fewer tick marks
    rgl::mtext3d(x_lab, edge='x--', line=2)
    rgl::mtext3d(y_lab, edge='y+-', line=2)
    rgl::mtext3d(z_lab, edge='z--', line=2.5)
    r1 <- rgl::rotationMatrix((coaltitude + 270) * (pi / 180), 1, 0, 0)  #
    r2 <- rgl::rotationMatrix(-azimuth * pi / 180, 0, 0, 1)  #
    r <- r1 %*% r2
    rgl::rgl.viewpoint(interactive=TRUE, userMatrix=r) # rotate
    rgl::observer3d(-0.075, -0.15, 3)
    Sys.sleep(.5)
    rgl::bgplot3d({
      par(omd=c(0.75, 1.0000000, 0, 0.3), ps=20)
  #    par(cra=2)
      min_z <- ifelse(min(z) < 0, min(z), 0)
      plot3D::colkey(side = 4, clim = c(min_z, max(z)), add = FALSE, cex.clab=1,
        line.clab=.75, width = 2, length = 1.5, clab = legend_lab,
        col=col, adj.clab = 0.05, cex.axis = 1)
      par(omd = c(0, 1, 0, .975), ps=35)
      title(main=main, font.main=2, cex.main=1)
    })
  }
  if (rgl == TRUE) {
    plot3D::hist3D(x=x, y=y, z=z, shade=0, nticks=5, ticktype="detailed",
      col=col, bty="b2", expand=.25, phi=coaltitude, theta=azimuth,
      border=border, facets=TRUE, axes=TRUE, image=FALSE, contour=FALSE,
      panel.first=NULL, ltheta=-135, lphi=0, space=0, add=FALSE, plot=TRUE,
      clab=legend_lab, main=main, xlab="Longitude", ylab="Latitude", zlab="",
      zlim=z_lim, colkey=FALSE)
    plot3Drgl::plotrgl(new = TRUE, colkey=FALSE) # new window
    if (rgl_window == "image") par3d(windowRect=c(150, 22, 1174, 790))  #
    if (rgl_window == "screen") par3d(windowRect=c(0, 28, 1920, 1080))  #
    ResetGraphics()
    if (spin == TRUE) {
      rgl::play3d(rgl::spin3d(axis=c(0,0,1), rpm=6), duration=10)
    }
    if (movie == TRUE) {
      if (rgl_window == "image") {
        cat("Creating a movie file, this will take a few seconds", "\n")
        rgl::movie3d(rgl::spin3d(axis=c(0,0,1), rpm=6), fps = 32, duration=10,
        movie=movie_name, dir=getwd(), clean=TRUE)
        cat(paste0("Created movie file: ", movie_name, ".gif"), "\n")
      }
      if (rgl_window == "screen") {
        org <- as.numeric(rgl::rgl.cur())
        cat("Opening new rgl device with proper dimensions for a movie.", "\n")
        plotrgl(new = TRUE)
        rgl::par3d(windowRect=c(150, 22, 1174, 790))  # dimensions: 1028 X 768
        ResetGraphics()
        cat("Creating a movie file - this will take a few seconds.")
        movie3d(rgl::spin3d(axis=c(0,0,1), rpm=6), fps = 32, duration=10,
          movie=movie_name, dir=getwd(), clean=TRUE, verbose=FALSE)
        cat(paste0("Created movie file: ", movie_name, ".gif"), "\n")
        cat("Returning to previous rgl device.")
        rgl::rgl.close()
        rgl::rgl.set(which=org)
      }
    }
  }
}

plot3Drgl::plotrgl(new = TRUE, colkey=FALSE) # new window
if (rgl_window == "screen") par3d(windowRect=c(0, 28, 1920, 1080))  #
rgl::rgl.clear(type = "bboxdeco")
text_ids <- subset(rgl::rgl.ids(), type=="text", select="id")
for (i in 1:nrow(text_ids)){
    rgl::rgl.pop(id=text_ids[i,"id"])
}
par(mar=c(2, 2, 2, 2)+.01, las=2)
rgl::axis3d('x--', ntick=7)  # can be adjust to add more or fewer tick marks
rgl::axis3d('y+-', ntick=7)  # can be adjust to add more or fewer tick marks
rgl::axis3d('z--', ntick=4)  # can be adjust to add more or fewer tick marks
rgl::mtext3d(x_lab, edge='x--', line=2)
rgl::mtext3d(y_lab, edge='y+-', line=2)
rgl::mtext3d(z_lab, edge='z--', line=2.5)
r1 <- rgl::rotationMatrix((coaltitude + 270) * (pi / 180), 1, 0, 0)  #
r2 <- rgl::rotationMatrix(-azimuth * pi / 180, 0, 0, 1)  #
r <- r1 %*% r2
rgl::rgl.viewpoint(interactive=TRUE, userMatrix=r) # rotate
rgl::observer3d(-0.075, -0.15, 3)
Sys.sleep(.5)

bgplot3d_HD <- function(expression){
    viewport <- par3d("viewport")
    width <- viewport["width"]*2
    height <- viewport["height"]*2
    if (width > 0 && height > 0) {
        filename <- tempfile(fileext = ".png")
        png(filename = filename, width = width, height = height, res = 200)
        value <- try(expression)
        dev.off()
        result <- bg3d(texture = filename, col = "white", lit = FALSE)
    }
    else {
        value <- NULL
        result <- bg3d(col = "white")
    }
    lowlevel(structure(result, value = value))
}

bgplot3d_HD({
  par(omd=c(0.75, 1.0000000, 0, 0.3), ps=20)
  #    par(cra=2)
  min_z <- ifelse(min(z) < 0, min(z), 0)
  plot3D::colkey(side = 4, clim = c(min_z, max(z)), add = FALSE, cex.clab=1,
    line.clab=.75, width = 2, length = 1.5, clab = legend_lab,
    col=col, adj.clab = 0.05, cex.axis = 1)
  par(omd = c(0, 1, 0, .975), ps=35)
  title(main=main, font.main=2, cex.main=1)
})


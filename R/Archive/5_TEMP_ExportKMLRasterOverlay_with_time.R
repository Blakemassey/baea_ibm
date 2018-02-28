# ExportKMLRasterOverlayWithTime Function --------------------------------------

###  Export KML Raster function 
###  Usage: ExportKMLRasterOverlayWithTime(raster, time, color_pal, alpha, 
###    maxpixels, blur, colNA, outfile, output_dir) 
###  Arguments: raster = a Raster* object
###             time = Interval* object, default = NULL
###             color_pal = color palette, can be a color ramp (e.g., c("white",
###               "red") or a specific palette (e.g., "SAGA_pal[[1]]")
###             alpha = numeric (0-1), transparency level of the .kml. 
###               Default is 1. 
###             method = method used to compute values for the new RasterLayer.
###               Either 'ngb' (nearest neighbor), which is useful for 
###               categorical variables, or 'bilinear' (bilinear interpolation; 
###               the default value), which is appropriate for continuous 
###               variables.
###             maxpixels = maximum number of pixels. If ncell(raster) > 
###               maxpixels, sample is used to reduce the number of pixels. 
###             blur = integer (default=10). Higher values help avoid blurring 
###               of isolated pixels (at the expense of a png file that is 
###               blur^2 times larger)
###             colNA = color to use for the background (default is transparent)          
###             outfile = name of KML, default is to use name of raster 
###             output_dir = output folder location, default is getwd()
###             zip = logical, whether or not to convert .kml to .kmz
###  Returns: KML of a Raster
###  Notes: Modified from functions in the 'kml' and 'raster' packages
###  Blake Massey
###  2015.03.04

ExportKMLRasterOverlayWithTime <- function(raster = raster, 
                                           time = NULL,
                                           color_pal = rev(terrain.colors(255)), 
                                           alpha = 1,
                                           method = "ngb",
                                           overwrite = TRUE, 
                                           maxpixels = 500000,
                                           blur = 10,
                                           colNA = "transparent",
                                           outfile = NULL, 
                                           output_dir= getwd(),
                                           zip = TRUE) {
  suppressPackageStartupMessages(require(raster))
  x <- raster
  if (nlayers(x) > 1) {
    x <- x[[1]]
  }
  if(!is.null(outfile)){
    name <- outfile
    outfile <- paste(output_dir, "/", name, ".kml", sep="")
  } else {
    name <- names(x)
    if (name == "layer") {
      name <- deparse(substitute(raster))
    }
    outfile <- paste(output_dir, "/", name, ".kml", sep="")  
  }
  stopifnot(hasValues(x))
  x <- projectRaster(x, crs="+proj=longlat +datum=WGS84", method=method)
  unique_x <- length(unique(getValues(x)))
  col <- colorRampPalette(color_pal, alpha=TRUE)(unique_x)
  cols <- adjustcolor(col, alpha)
  #  if (unique_x > 250) unique_x <- 250  
  filename <- extension(outfile, ".kml")
  x <- sampleRegular(x, size=maxpixels, asRaster=TRUE, useGDAL=TRUE)
  imagefile <- filename
  extension(imagefile) <- ".png"
  kmlfile <- kmzfile <- filename
  extension(kmlfile) <- ".kml"
  if (file.exists(kmlfile)) {
    if (overwrite) {
      file.remove(kmlfile)
    } else {
      stop("kml file exists, use \"overwrite=TRUE\" to overwrite it")
    }
  }
  png(filename=imagefile, width=max(480, blur * ncol(x)), height=max(480, 
    blur * nrow(x)), bg="transparent", type="cairo-png")
  if (!is.na(colNA)) {
    par(mar = c(0, 0, 0, 0), bg = colNA)
  } else {
    par(mar = c(0, 0, 0, 0))
  }
  x[x== 0] <- NA
  image(x, col=cols, axes=FALSE, useRaster=TRUE, maxpixels=maxpixels)
  #plot(x, colNA="transparent")
  dev.off()
  kml <- c("<?xml version=\"1.0\" encoding=\"UTF-8\"?>", 
    "<kml xmlns=\"http://www.opengis.net/kml/2.2\">", "<GroundOverlay>")
  kmlname <- paste("<name>", name, "</name>", sep="")
  if(!is.null(time)){   
    start_time <- paste0(strftime(int_start(time), "%Y-%m-%d", tz = time@tzone), 
      "T", strftime(int_start(time), "%H:%M", tz=time@tzone), "Z")
    end_time <- paste0(strftime(int_end(time), "%Y-%m-%d", tz = time@tzone),
      "T", strftime(int_end(time), "%H:%M", tz=time@tzone), "Z")
    timespan <- paste0("<TimeSpan>", "<begin>", start_time, "</begin>", "<end>", 
      end_time, "</end>", "</TimeSpan>")
  } else {
    timespan <- ""
  }
  icon <- paste("<Icon><href>", basename(imagefile),"</href><viewBoundScale>",
    "0.75</viewBoundScale></Icon>", sep="")
  e <- extent(x)
  latlonbox <- c("\t<LatLonBox>", paste("\t\t<north>", e@ymax,"</north><south>", 
    e@ymin, "</south><east>", e@xmax, "</east><west>", e@xmin, "</west>", 
    sep = ""), "\t</LatLonBox>")
  footer <- "</GroundOverlay></kml>"
  kml <- c(kml, kmlname, timespan, icon, latlonbox, footer)
  cat(paste(kml, sep = "", collapse = "\n"), file = kmlfile, sep = "")
  if(zip) ZipKML(kmlfile, imagefile)
}
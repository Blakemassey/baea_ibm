library(RgoogleMaps)

ir.osm <- GetMapTiles(lonR=xlim, latR=ylim, zoom=7, verbose=1,


                        type = "osm", tileDir= TRUE)
  xlim = c(-7, -3.5)
  ylim = c(51.35, 55.35)
  Dublin = c(lon=-6.266155,lat=53.350140)
  DublinMerc = geosphere_mercator(Dublin)

  ir.osm <- GetMapTiles(lonR=xlim, latR=ylim, zoom=7, verbose=1,
                        type = "osm", tileDir= TRUE)

class(ir.osm)

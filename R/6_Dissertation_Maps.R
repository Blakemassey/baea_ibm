suppressPackageStartupMessages(library(cartography))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(mapview))
suppressPackageStartupMessages(library(OpenStreetMap))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(prettymapr))
suppressPackageStartupMessages(library(prettymapr))
suppressPackageStartupMessages(library(raster))
suppressPackageStartupMessages(library(rosm))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(tmap))
suppressPackageStartupMessages(library(tmaptools))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(webshot))
library(gisr)
library(baear)
cols <- viridis(100)

# Rasters
base <- raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

# Directories
baea_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Data/BAEA"
nests_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Nests/Nests_rds")
maps_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Products/Maps"
report_maps_dir <- file.path("C:/Users/blake/Documents/PhD Program/Reports",
  "BAEA Project - 2018/Maps")

# Nests
nests_study_org <- readRDS(file.path(nests_dir, "nests_study.rds"))
nests_study <- st_as_sf(x = nests_study_org, coords = c("long", "lat"),
  crs = "+proj=longlat +datum=WGS84")

# Bald Eagle Data
baea_org <- readRDS(file.path(baea_dir, "baea.rds"))
baea <- st_as_sf(x = baea_org, coords = c("long_utm", "lat_utm"),
  crs = 32619) #  crs = "+proj=longlat +datum=WGS84")

# Maine Outline
maine <- read_sf(file.path("C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp")) %>%
  st_transform(., crs = 4326) %>%
  mutate(state = "Maine")  %>%
  dplyr::select(state)

## ----------------------------- CREATE MAPS -------------------------------- ##

tmap_mode("plot")

## Maine Overview Map ------------------------------------------------------- ##

# Mapbox Baselayers
mapbox_url <- "https://api.mapbox.com/styles/v1/mapbox/"
mapbox_tile <- "/tiles/256/{z}/{x}/{y}"
mapbox_key <- paste0("?access_token=pk.eyJ1IjoiYmxha2VtYXNzZXkiLCJhIjoi",
  "Y2pseTYxYW56MDE4eDNwcXZxdmNtNmJ1eiJ9.cguQx1N8bIpciBnc2h3v_w")
om_type <- paste0(mapbox_url, "streets-v10", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "outdoors-v10", mapbox_tile, mapbox_key)
om_type <- paste0(mapbox_url, "light-v9", mapbox_tile, mapbox_key)

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_type <- paste0(esri_url, "NatGeo_World_Map", esri_tile)


#### ----------------------- NESTS OVERVIEW MAPS -------------------------- ####

maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1.15, width = 2))
maine_bb_ext <- CreateOSMBaseBB(maine_bb_sf, type = "om_type")

# Use "Tmap_baselayers.R" script to get other baselayers
maine_down = OpenStreetMap::openmap(maine_bb_ext[[1]], maine_bb_ext[[2]],
  minNumTiles = 9, type = om_type)
maine_om <- RasterizeOsMDownload(maine_down)

maine_overview <-  tm_layout(asp = .75) +
  tm_shape(maine_om) +
    tm_rgb() +
  tm_shape(maine,
    bbox = bb(maine, ext = 1.15), is.master = TRUE) + # this sets map crs
    tm_borders(col = "black")
maine_overview

nests_overview <- maine_overview +
  tm_layout(
    main.title = " Trapping Sites Overview",
    main.title.position = "center",
    main.title.size = 1.15,
    title.position = c(.65, .02),
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85, outside = FALSE,
    position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), size = .65, position = c(.7, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 1.9,
    position = c(.85, .88))+
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_shape(maine) + tm_borders(col = "black") +  # ME outline overlays grid
  tm_shape(nests_study %>% filter(nest_site != "446R01")) +
  tm_symbols("red", size = .5) +
	tm_text("name", shadow = TRUE, auto.placement = TRUE, size = .5) +
  tm_xlab("") + tm_ylab("")
nests_overview

tmap_save(tm = nests_overview, filename = file.path(report_maps_dir,
  "Trapping_Sites_Overview.svg"), unit = "in", dpi = 300, height = 5, width = 5)

#### ---------------------- BAEA INDIVIDUAL MAPS -------------------------- ####

# Getting the ratio and background correct requires 3 components:
# 1) Getting enough coverage of basemap by adjusting bb() 'height'/'weight' args
# 2) Adjusting the openmap() 'zoom' if needed
# 3) Setting the tm_layout() 'asp' arg to a reasonable ratio

# Select id and year
table(baea$id, baea$year) # Determine available individual/year combos
id_i <- "Ellis"
year_i <- 2018


### Flightpath Maps ------------------------------------------------------------

# Filter data, create fightpaths
baea_i <- baea %>% filter(id == id_i) %>% filter(year == year_i) %>%
  st_transform(., crs = as.character(OpenStreetMap::osm()))

baea_i_lines <- baea_i %>%
  group_by(id) %>%
  arrange(datetime) %>%
  summarize(m = mean(year), do_union = FALSE) %>%
  st_cast("LINESTRING")

# Get osm baselayer for baea_i
baea_i_bb_sf <- st_as_sfc(bb(baea_i, relative = TRUE, height = 3,
  width = 2))
baea_i_bb_ext <- CreateOSMBaseBB(baea_i_bb_sf, type = "om_type")
baea_i_down = OpenStreetMap::openmap(baea_i_bb_ext[[1]], baea_i_bb_ext[[2]],
  minNumTiles = 21, type = om_type)  # may need to add and adjust 'zoom' arg
baea_i_om <- RasterizeOsMDownload(baea_i_down)

baea_i_x_dist <- as.numeric(approx_distances(bb(baea_i, ext = 1.15))[1])/1000/5
baea_i_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0, baea_i_x_dist),
  scales::pretty_breaks(3))[1]))

# All flight paths and points
baea_i_paths <-
  tm_layout(asp = 1) +
  tm_shape(baea_i_om) +
    tm_rgb() +
  tm_shape(baea_i_lines) +
    tm_lines("yellow", lwd = 2, alpha = .5) +
  tm_shape(baea_i,
    bbox = bb(baea_i, ext = 1.15), is.master = TRUE) +
    tm_dots(size = 0.075, col = "darkmagenta") +
  tm_layout(main.title = paste0("GPS Locations: ", id_i),
    main.title.position = "center",
    main.title.size = 1.15,
    title.snap.to.legend = TRUE) +
  tm_legend(title.size = 1, text.size = .85,
    outside = TRUE, position = c("right", "bottom")) +
  tm_scale_bar(size = .75, width = .2,
    breaks = baea_i_x_breaks,
    position = c(.05, .01)) +
  tm_compass(type = "4star",  show.labels = 1, size = 2.5,
    position = c(.875, .875)) +
  tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
    labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
    labels.inside.frame = FALSE) +
  tm_xlab("") + tm_ylab("")
baea_i_paths

baea_i_bb = gisr::CreateMapExtentBB(baea_i, asp = 1, ext = 1.15)

# Use "Tmap_baselayers.R" script to get other baselayers
maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1,
  width = 2))
maine_baea_i <- sf::st_union(x = maine_bb_sf, y = baea_i_bb %>%
  st_transform(st_crs(maine_bb_sf)))
maine_i_bb <- bb_poly(bb(maine_baea_i, ext = 1.15))
maine_i_bb_ext <- CreateOSMBaseBB(maine_i_bb, type = "om_type")
maine_i_down = OpenStreetMap::openmap(maine_i_bb_ext[[1]],
  maine_i_bb_ext[[2]], zoom = 5, minNumTiles = 9, type = om_type)
maine_i_om <- RasterizeOsMDownload(maine_i_down)

maine_i_overview <-
  tm_shape(maine_i_om) +
    tm_rgb() +
  tm_shape(maine) + # setting this as master sets lat/long
    tm_borders(col = "black") +
  tm_shape(baea_i_bb) +
    tm_borders(col = "red")
maine_i_overview

tmap_save(tm = baea_i_paths, filename = file.path(report_maps_dir,
  paste0(year_i, "_", id_i, ".png")), insets_tm = maine_i_overview,
  insets_vp =  viewport(x = 0.855, y = 0.145, width = 0.2, height = 0.2),
  unit = "in", dpi = 300, height = 6, width = 6)
# Create 2d kernel density data - isopleth lines, polygons, and rasters
baea_i_smooth <- smooth_map(baea_i, cover = as(CreateExtentSF(baea_i, 1),
  "Spatial"), nlevels = 10)

### Isopleth Maps --------------------------------------------------------------

# Drop lowest density polygon
baea_i_smooth_polys <- st_intersection(baea_i_smooth$polygons,
  baea_i_smooth$polygons %>% arrange(level) %>% slice(-1))

mapview(baea_i_smooth_polys)

# Isolate highest density polygon
baea_i_smooth_poly1 <- st_intersection(baea_i_smooth$polygons,
  baea_i_smooth$polygons %>% arrange(rev(level)) %>% slice(1))

# Download om for polys_i
polys_i_bb_sf <- st_as_sfc(bb(baea_i_smooth_polys, relative = TRUE, height = 1.15,
  width = 1.15))
polys_i_bb_ext <- CreateOSMBaseBB(polys_i_bb_sf, type = "om_type")
polys_i_down = OpenStreetMap::openmap(polys_i_bb_ext[[1]], polys_i_bb_ext[[2]],
  minNumTiles = 21, type = om_type)
polys_i_om <- RasterizeOsMDownload(polys_i_down)

# Download om for poly1_i
poly1_i_bb_sf <- st_as_sfc(bb(baea_i_smooth_poly1, relative = TRUE, height = 2,
  width = 2))
poly1_i_bb_ext <- CreateOSMBaseBB(poly1_i_bb_sf, type = "om_type")
poly1_i_down = OpenStreetMap::openmap(poly1_i_bb_ext[[1]], poly1_i_bb_ext[[2]],
  minNumTiles = 21, type = om_type)
poly1_i_om <- RasterizeOsMDownload(poly1_i_down)

# All but lowest density isopleth
tm_shape(polys_i_om) +
  tm_raster() +
tm_shape(baea_i_smooth$raster) +
  tm_raster("count", alpha = .5) +
tm_shape(baea_i_smooth$iso) +
  tm_iso("black", size = .5, fontcolor="black")

# Highest density isopleth
tm_shape(poly1_i_om) +
  tm_rgb() +
tm_shape(baea_i_smooth_polys) +
  tm_fill("level", alpha = .5) +
tm_borders()

# ---------------------------------------------------------------------------- #
################################ OLD CODE ######################################
# ---------------------------------------------------------------------------- #




#
# baea_i, asp = 4, ext = 1.15
# sf_object = baea_i
# ext = 1.15
# asp = .5
#
# CreateMapExtentBB2 <- function(sf_object,
#                               ext = 1.15,
#                               asp = 1){
#   map_asp <- asp
#   sf_asp <- tmaptools::get_asp_ratio(sf_object, ext = 1.15)
# #  bb_point <- sf::st_centroid(sf::st_as_sfc(tmaptools::bb(sf_object, ext =ext)))
# #  bb_x <- sf::st_coordinates(bb_point)[1]
# #  bb_y <- sf::st_coordinates(bb_point)[2]
# #  bb_dists <- tmaptools::approx_distances(tmaptools::bb(sf_object, ext = ext),
# #    projection = sf::st_crs(sf_object))
# #  bb_width <- as.numeric(bb_dists)[1]
# #  bb_height <- as.numeric(bb_dists)[2]
# #  bb_asp <- bb_width / bb_height
#   if (sf_asp >= map_asp){
#     bb_width = ext     # width should not be reduced
#     bb_height = (sf_asp/map_asp)*ext
#   } else {
#     bb_width = (map_asp/sf_asp)*ext
#     bb_height = ext # height should not be reducted
#   }
#   map_bb <- sf::st_as_sfc(tmaptools::bb(sf_object, relative = TRUE,
#     width = bb_width, height = bb_height))
#   sf::st_crs(map_bb) <- sf::st_crs(sf_object)
#   return(map_bb)
# }
# mapview(map_bb) + mapview(baea_i_lines)
#
# maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1.15, width = 2))
# maine_bb_ext <- CreateOSMBaseBB(maine_bb_sf, type = "om_type")
#

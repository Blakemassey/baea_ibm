


# Mapbox Baselayers
mapbox_url <- "https://api.mapbox.com/styles/v1/mapbox/"
mapbox_tile <- "/tiles/256/{z}/{x}/{y}"
mapbox_key <- paste0("?access_token=pk.eyJ1IjoiYmxha2VtYXNzZXkiLCJhIjoi",
  "Y2pseTYxYW56MDE4eDNwcXZxdmNtNmJ1eiJ9.cguQx1N8bIpciBnc2h3v_w")
#om_type <- paste0(mapbox_url, "streets-v10", mapbox_tile, mapbox_key)
#om_type <- paste0(mapbox_url, "outdoors-v10", mapbox_tile, mapbox_key)
#om_type <- paste0(mapbox_url, "light-v9", mapbox_tile, mapbox_key)

# ESRI Baselayers
#esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
#esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_type <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

# Select id and year
table(baea$id, baea$year) # Determine available individual/year combos
id_i <- "Sandy"
year_i <- 2018

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

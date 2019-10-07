# THIS TEMP FILE IS FOR:
# Trying to get a consistent map template for all of the potential individuals

homerange_akde <- readRDS(file.path("Output/Analysis/Homerange",
  "homerange_akde.rds"))
baea_hr <- readRDS("Data/BAEA/baea_homerange.rds")

library(dplyr)
library(gisr)
library(plotKML)
baea_hr_mush <- baea_hr %>% filter(id == "Musquash")
ExportKMLTelemetryBAEA(df = baea_hr_mush %>% filter(year == 2016), file = "Musquash_2016.kmz")
ExportKMLTelemetryBAEA(df = baea_hr_mush %>% filter(year == 2018), file = "Musquash_2018.kmz")

# Use "Tmap_baselayers.R" script to get other baselayers
maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1,
                            width = 2))
maine_bb <- bb_poly(bb(maine_bb_sf, ext = 1.15))
maine_bb_ext <- CreateOSMBaseBB(maine_bb, type = "om_type")
maine_down = OpenStreetMap::openmap(maine_bb_ext[[1]],
                                    maine_bb_ext[[2]], zoom = 5, minNumTiles = 9, type = om_type)
maine_om <- RasterizeOMDownload(maine_down)

table(baea_hr$id, baea_hr$year)
i <- "Branch"; j <- 2015
i <- "Crooked"; j <- 2015
i <- "Ellis"; j <- 2015
i <- "Wilson"; j <- 2015
i <- "Ellis"; j <- 2017

# For mapping
for (i in unique(baea_hr$id)){
  baea_hr_i <- baea_hr %>% filter(id == i) %>% arrange(datetime)
  homerange_akde_i <- homerange_akde %>% filter(id == i)
  for (j in unique(baea_hr_i$year)){
    print(paste0("ID:", i, "; ", "Year:", j))
    baea_hr_k <- baea_hr_i %>% filter(year == j)
    homerange_akde_k <- homerange_akde_i %>% filter(year == j)
    akde_k <- homerange_akde_k %>% pull(hr_akde) %>% pluck(1)
    ud_95_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.95,
      level = 0.95)
    ud_95_sf_k <- st_as_sf(ud_95_sp_k) %>% slice(2)
    ud_50_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.5,
      level = 0.95)
    ud_50_sf_k <- st_as_sf(ud_50_sp_k) %>% slice(2)
    baea_sf_k <- st_as_sf(baea_hr_k, coords = c("long_utm", "lat_utm"),
      crs = 32619, agr = "constant")
    # mapview(list(ud_95_sf_k, ud_50_sf_k, baea_sf_k),
    #   zcol = list(NULL, NULL, NULL),
    #   legend = list(TRUE, FALSE, FALSE), homebutton = list(FALSE, TRUE, TRUE))
    # Filter data, create fightpaths
    baea_k <- baea_hr_k %>% st_as_sf(., coords = c("long_utm", "lat_utm"),
      crs = 32619)  %>%
      st_transform(., crs = as.character(OpenStreetMap::osm()))
    baea_k_lines <- baea_k %>% group_by(id) %>% arrange(datetime) %>%
      summarize(m = mean(year), do_union = FALSE) %>%
      st_cast("LINESTRING")

    # Get osm baselayer for baea_i
    baea_k_bb_sf <- st_as_sfc(bb(baea_k, relative = TRUE, height = 4, width =4))
    baea_k_bb_ext <- CreateOSMBaseBB(baea_k_bb_sf, type = "om_type")
    baea_k_down = OpenStreetMap::openmap(baea_k_bb_ext[[1]], baea_k_bb_ext[[2]],
      minNumTiles = 21, type = om_type)  # may need to add and adjust 'zoom' arg
    baea_k_om <- RasterizeOMDownload(baea_k_down)
    baea_dist_sf <- st_as_sfc(bb(baea_k, relative = TRUE, height = 1, width =1))
    baea_k_x_dist <- as.numeric(approx_distances(bb(baea_dist_sf,
      ext = 1.15))[1])/1000/5
    baea_k_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0, baea_k_x_dist),
      scales::pretty_breaks(2))[1]))
    print(baea_k_x_breaks)

    # Home range, points, and flight paths
    baea_k_hr_paths <-
      tm_layout(asp = 1) +
      tm_shape(baea_k_om) +
        tm_rgb() +
      tm_shape(baea_k_lines) +
        tm_lines("#ffffff", lwd = 2, alpha = .25) + # brewer.pal(5, "RdGy")[3]
      tm_shape(baea_k,
        bbox = bb(baea_k, ext = 1.15), is.master = TRUE) +
        tm_dots(size = 0.075, col = "#700074", alpha = .5) + #"#404040"  # brewer.pal(5, "RdGy")[5]
      tm_shape(ud_95_sf_k) +
        tm_polygons(col = "yellow", alpha = .15) +
      tm_shape(ud_95_sf_k) +
        tm_borders(col= "yellow", lwd = 2) +
      tm_shape(ud_50_sf_k) +
        tm_polygons(col = "red", alpha = .15) +
      tm_shape(ud_50_sf_k) +
        tm_borders(col= "red", lwd = 2) +
      tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
        main.title.position = "center",
        main.title.size = 1.15,
        title.snap.to.legend = TRUE) +
      tm_legend(title.size = 1, text.size = .85,
        outside = TRUE, position = c("right", "bottom")) +
      tm_scale_bar(text.size = .75, width = .2,
        breaks = baea_k_x_breaks,
        position = c(.05, .01)) +
      tm_compass(type = "4star",  show.labels = 1, size = 2.5,
        position = c(.875, .875)) +
     tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = 0,
       labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
       labels.inside.frame = FALSE) +
      tm_xlab("") + tm_ylab("")
    baea_k_hr_paths
    baea_k_bb = gisr::CreateMapExtentBB(baea_k, asp = 1, ext = 1.15)
    maine_overview <-
      tm_shape(maine_om) +
        tm_rgb() +
      tm_shape(maine) + # setting this as master sets lat/long
        tm_borders(col = "black") +
      tm_shape(baea_k_bb) +
        tm_borders(col = "red")
    maine_overview
    tmap_save(tm = baea_k_hr_paths, filename = file.path("C:/Temp",
      paste0(i, "_", j, ".svg")),
      insets_tm = maine_overview,
      insets_vp =  viewport(x = 0.881, y = 0.147, width = 0.2, height = 0.2),
      unit = "in", dpi = 300, height = 6, width = 6)
    # tmap_save(tm = baea_i_paths, filename = file.path(tex_dir,
    #   "Figures/Ch2/HR_Maps", paste0(i, "_", j, ".svg")),
    #   insets_tm = maine_overview,
    #   insets_vp =  viewport(x = 0.855, y = 0.145, width = 0.2, height = 0.2),
    #   unit = "in", dpi = 300, height = 6, width = 6)
  }
}

#
# # For mapping
# for (i in unique(baea_hr$id)){
#   baea_hr_i <- baea_hr %>% filter(id == i) %>% arrange(datetime)
#   for (j in unique(baea_hr_i$year)){
#     baea_hr_ij <- baea_hr_i %>% filter(year == j)
#     print(paste0("ID:", i, "; ", "Year:", j))
#     move_k <- move(x = baea_hr_ij$long_utm, y = baea_hr_ij$lat_utm,
#       time = as.POSIXct(baea_hr_ij$datetime, format = "%Y-%m-%d %H:%M:%S",
#       tz = "NYC"), data = baea_hr_ij, proj = wgs84n19, animal = i,
#       sensor = "GPS")
#     # Compute movement models
#     telemetry_k <- as.telemetry(move_k)
#     guess_k <- ctmm.guess(telemetry_k, interactive = FALSE)
#     fit_k <- ctmm.fit(telemetry_k, guess_k)
#     # Compute akde object
#     akde_k <- akde(telemetry_k, fit_k)
#     ud_95_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.95,
#       level = 0.95)
#     ud_95_sf_k <- st_as_sf(ud_95_sp_k) %>% slice(2)
#     ud_50_sp_k <- SpatialPolygonsDataFrame.UD(akde_k, level.UD = 0.5,
#       level = 0.95)
#     ud_50_sf_k <- st_as_sf(ud_50_sp_k) %>% slice(2)
#     baea_sf_k <- st_as_sf(baea_hr_ij, coords = c("long_utm", "lat_utm"),
#       crs = 32619, agr = "constant")
#     # mapview(list(ud_95_sf_k, ud_50_sf_k, baea_sf_k),
#     #   zcol = list(NULL, NULL, NULL),
#     #   legend = list(TRUE, FALSE, FALSE), homebutton = list(FALSE, TRUE, TRUE))
#
#     # Filter data, create fightpaths
#     baea_i <- baea_hr_ij %>% st_as_sf(., coords = c("long_utm", "lat_utm"),
#       crs = 32619)  %>%
#       st_transform(., crs = as.character(OpenStreetMap::osm()))
#     baea_i_lines <- baea_i %>%
#       group_by(id) %>%
#       arrange(datetime) %>%
#       summarize(m = mean(year), do_union = FALSE) %>%
#       st_cast("LINESTRING")
#
#     # Get osm baselayer for baea_i
#     baea_i_bb_sf <- st_as_sfc(bb(baea_i, relative = TRUE, height = 4,
#       width = 4))
#     baea_i_bb_ext <- CreateOSMBaseBB(baea_i_bb_sf, type = "om_type")
#     (as.numeric(approx_distances(bb(baea_i_bb_sf, ext = 1.15))[1])/1000/5)
#     baea_i_down = OpenStreetMap::openmap(baea_i_bb_ext[[1]], baea_i_bb_ext[[2]],
#       minNumTiles = 21, type = om_type)  # may need to add and adjust 'zoom' arg
#     baea_i_om <- RasterizeOMDownload(baea_i_down)
#     baea_dist_sf <- st_as_sfc(bb(baea_i, relative = TRUE, height = 1, width = 1))
#     (as.numeric(approx_distances(bb(baea_dist_sf, ext = 1.15))[1])/1000)
#     baea_i_x_dist <- as.numeric(approx_distances(bb(baea_dist_sf, ext = 1.15))[1])/1000/5
#     print(baea_i_x_dist)
#     baea_i_x_breaks <- as.numeric(unlist(scales::cbreaks(c(0, baea_i_x_dist),
#       scales::pretty_breaks(2))[1]))
#     print(baea_i_x_breaks)
#
#     # All flight paths and points
#     baea_i_paths <-
#       tm_layout(asp = 1) +
#       tm_shape(baea_i_om) +
#         tm_rgb() +
#       tm_shape(ud_95_sf_k) +
#         tm_polygons(col = "yellow", alpha = .15) +
#       tm_shape(ud_95_sf_k) +
#         tm_borders(col= "yellow", lwd = 2, alpha = .8) +
#       tm_shape(ud_50_sf_k) +
#         tm_polygons(col = "red", alpha = .15) +
#       tm_shape(ud_50_sf_k) +
#         tm_borders(col= "red", lwd = 2, alpha = .8) +
#       tm_shape(baea_i_lines) +
#         tm_lines("#ffffff", lwd = 2, alpha = .3) + # brewer.pal(5, "RdGy")[3]
#       tm_shape(baea_i,
#         bbox = bb(baea_i, ext = 1.15), is.master = TRUE) +
#         tm_dots(size = 0.075, col = "#700074", alpha = .8) +   # brewer.pal(5, "RdGy")[5]
#       tm_layout(main.title = NULL, #paste0("GPS Locations: ", id_i),
#         main.title.position = "center",
#         main.title.size = 1.15,
#         title.snap.to.legend = TRUE) +
#       tm_legend(title.size = 1, text.size = .85,
#         outside = TRUE, position = c("right", "bottom")) +
#       tm_scale_bar(text.size = .75, width = .2,
#         breaks = baea_i_x_breaks,
#         position = c(.05, .01)) +
#       tm_compass(type = "4star",  show.labels = 1, size = 2.5,
#         position = c(.875, .875)) +
# #      tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey15", alpha = 1,
# #        labels.col = "grey15", lines = FALSE, #labels.format = list(format = "f", big.mark = ""),
#  #       labels.inside.frame = FALSE, ticks = TRUE) +
#       tm_xlab("") + tm_ylab("")
#     baea_i_paths
#     baea_i_bb = gisr::CreateMapExtentBB(baea_i, asp = 1, ext = 1.15)
#
#     maine_overview <-
#       tm_shape(maine_om) +
#         tm_rgb() +
#       tm_shape(maine) + # setting this as master sets lat/long
#         tm_borders(col = "black") +
#       tm_shape(baea_i_bb) +
#         tm_borders(col = "red")
#     maine_overview
#     tmap_save(tm = baea_i_paths, filename = file.path("C:/Temp",
#       paste0(i, "_", j, ".svg")),
#       insets_tm = maine_overview,
#       insets_vp =  viewport(x = 0.881, y = 0.097, width = 0.2, height = 0.2),
#       unit = "in", dpi = 300, height = 6, width = 6)
#     # tmap_save(tm = baea_i_paths, filename = file.path(tex_dir,
#     #   "Figures/Ch2/HR_Maps", paste0(i, "_", j, ".svg")),
#     #   insets_tm = maine_overview,
#     #   insets_vp =  viewport(x = 0.855, y = 0.145, width = 0.2, height = 0.2),
#     #   unit = "in", dpi = 300, height = 6, width = 6)
#   }
# }
#
#
#

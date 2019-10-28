baea = baea_terr
nest_set = nests_active
base = base
output_dir = "Output/Analysis/Territorial"
max_r = 100
write_home_dist = FALSE
write_con_dist = FALSE
write_con_dist_nest = TRUE

  id <- "nest_site"
  name <- "name"
  if (!dir.exists(output_dir)) dir.create(output_dir)
  if (write_home_dist | write_con_dist | write_con_dist_nest){
    for (k in sort(unique(baea$year))) dir.create(file.path(output_dir, k),
      showWarnings = FALSE)
  }
  distance_df <- data.frame(year = numeric(), id=character(),
    home_dist_min=numeric(), home_dist_max=numeric(), con_dist_min=numeric(),
    con_dist_max=numeric(), edge_dist_min=numeric(), edge_dist_max=numeric())
  cellsize <- res(base)[1]
  max_r_cells <- ceiling(max_r/cellsize)
  xmin <- xmin(base)
  ymin <- ymin(base)
  for (i in 1:nrow(nest_set)){
    nest_set[i, "x"] <- CenterXYInCell(nest_set[i,"long_utm"],
      nest_set[i,"lat_utm"], xmin, ymin, cellsize)[1]
    nest_set[i, "y"] <- CenterXYInCell(nest_set[i,"long_utm"],
      nest_set[i,"lat_utm"], xmin, ymin, cellsize)[2]
  }
  nest_set_sf <- st_as_sf(x = nest_set, coords = c("x", "y"),
    crs = 32619)
  i = "Ellis"; j = 2016
  for (i in unique(baea$id)) {
    baea_i <- baea %>% dplyr::filter(id == i)
    for (j in sort(unique(baea_i$year))){
      baea_k <- baea %>% dplyr::filter(year == j)
      nest_k_xy <- baea_k %>% slice(1) %>% dplyr::select(nest_long_utm,
        nest_lat_utm) %>% as.vector()
      nest_k_id <- baea_k %>% slice(1) %>% dplyr::select(nest_site) %>% pull()
      # Home Nest
      home_k_x <- CenterXYInCell(nest_k_xy[1], nest_k_xy[2], xmin, ymin,
        cellsize)[[1]]
      home_k_y <- CenterXYInCell(nest_k_xy[1], nest_k_xy[2], xmin, ymin,
        cellsize)[[2]]
      home_k_xy <- tibble(x = home_k_x, y = home_k_y)
      home_k_sf <- st_as_sf(x = home_k_xy, coords = c("x", "y"), crs = 32619)
      cell_extent <- raster::extent(home_k_x - (cellsize/2),
        home_k_x + (cellsize/2), home_k_y - (cellsize/2),
        home_k_y + (cellsize/2))
      cell <- setValues(raster(cell_extent,crs=projection(base),res=cellsize),j)
      home_ext <- raster::extend(cell, c(max_r_cells, max_r_cells), value=NA)
      home_dist <- distance(home_ext)

      mapview(home_dist) + mapview(home_k_sf)
      home_dist[cellFromXY(home_dist, c(home_k_x,home_k_y))]
      home_dist[cellFromXY(home_dist, c(home_k_x,home_k_y)) + 2]


      summary(home_dist)
      #home_dist <- raster::distanceFromPoints(home_ext, home[,c("x","y")])
      plot(home_dist)
      mapview(home_ext) + mapview(home_k_sf)
      # Conspecific Nests
      filter_quo <- paste0("active_", j, " == TRUE")
      nest_set_sf_j <- nest_set_sf %>%
        filter_se(filter_quo) %>%
        filter(nest_site != nest_k_id)
      nests_k <- st_contains(st_as_sfc(bb(home_dist)), nest_set_sf_j)
      nest_set_sf_k <- nest_set_sf_j %>% slice(unlist(nests_k))


      st_coordinates(nest_set_sf_k)
      con_dist <- raster::distanceFromPoints(home_ext,
        st_coordinates(nest_set_sf_k))
      plot(con_dist)


      test <- fasterize::raster(x = home_k_sf)
      raster::raster()


      cellStats(home_dist, min)
      cellStats(home_dist, max)

      cellStats(con_dist, min)
      cellStats(con_dist, max)

      raster::extract(con_dist, home_k_xy)
      home_con_dist <- raster::extract(con_dist, as_Spatial(home_k_sf))

      st_join(home_k_sf, con_dist)

      fun <- function(x){home_con_dist - x}
      con_dist_nest <- calc(con_dist, fun)
      plot(con_dist_nest)
      mapview(home_dist) +  mapview(cell) +  mapview(home_k_sf)


      a = st_sf(a = 1:3,
 geom = st_sfc(st_point(c(1,1)), st_point(c(2,2)), st_point(c(3,3))))
b = st_sf(a = 11:14,
 geom = st_sfc(st_point(c(10,10)), st_point(c(2,2)), st_point(c(2,2)), st_point(c(3,3))))

plot(a)
plot(b)
st_join(a, b)
st_join(a, b, left = FALSE)


      home_con_dist <- overlay(home_dist, con_dist, fun = function(x,y){round(x+y)})
      plot(home_con_dist)

      df_all <- nest_set[which(nest_set[,col] == TRUE), ]
      year_nest <- unique(baea_year$nest_site)
      df_home <- df_all %>% dplyr::filter(nest_site %in% year_nest)
      df_all_sp <- SpatialPointsDataFrame(df_all[,c("x","y")], df_all,
        proj4string=crs(base))
      homerange_ids <- df_home[,id]
      total <- length(homerange_ids)
      for (j in 1:nrow(df_home)) {
        hr_nest_site <- df_home[j,] %>% dplyr::select(id) %>% dplyr::pull()
        id_year_xy <- baea %>%
          dplyr::filter(nest_site == hr_nest_site) %>%
          dplyr::filter(year == i) %>%
          dplyr::mutate(x = long_utm) %>%
          dplyr::mutate (y = lat_utm) %>%
          dplyr::select(x,y)
        sv <- baea$nest_site == hr_nest_site & baea$year == i
        sv <- ifelse(is.na(sv), FALSE, sv)
        home <- df_home[j,]
        ifelse(is.null(name), home_name <- j, home_name <- home[,name])
        writeLines(noquote(paste0("Calculating nest and edge distances for ",
          i, ": ", home_name, " (", j, " of ", total, ").")))
        home_sp <- sp::SpatialPointsDataFrame(home[,c("x","y")], home,
          proj4string=crs(base))
        xy <- c(home_sp@coords[,"x"], home_sp@coords[,"y"])
        cell_extent <- raster::extent(xy[1]-(cellsize/2), xy[1]+(cellsize/2),
          xy[2]-(cellsize/2), xy[2]+(cellsize/2))
        cell <- setValues(raster(cell_extent,crs=projection(base),res=cellsize),j)
        home_ext <- raster::extend(cell, c(max_r_cells, max_r_cells), value=NA)
        home_dist <- raster::distanceFromPoints(home_ext, home[,c("x","y")])
        plot(home_dist)
        if (write_home_dist == TRUE) {
          filename <- file.path(output_dir, i, paste0("HomeDist_", home_name,
            ".tif"))
          raster::writeRaster(home_dist, filename=filename, format="GTiff",
            overwrite=TRUE)
          writeLines(noquote(paste("Writing:", filename)))
        }
        base_crop <- raster::raster(raster::extent(home_ext), resolution=30,
          crs=raster::crs(home_ext))
        rm(home_ext)
        base_crop <- raster::setValues(base_crop, runif(ncell(base_crop), 1, 10))
        con_dist <- raster::distanceFromPoints(base_crop,
          df_all_sp[which(df_all_sp$nest_area != home$nest_area),])
        # raster::plot(con_dist)
        if (write_con_dist == TRUE) {
          filename <- file.path(output_dir, i, paste0("ConDist_", home_name,
            ".tif"))
          writeLines(noquote(paste0("Writing: ", filename)))
          writeRaster(con_dist, filename=filename, format="GTiff",
            overwrite=TRUE)
        }
        fun <- function(x){ raster::extract(con_dist, home_sp) - x  }
        con_dist_nest <- calc(con_dist, fun)
        if (write_con_dist_nest == TRUE) {
          filename <- file.path(output_dir, i, paste0("ConDistNest_", home_name,
              ".tif"))
          writeLines(noquote(paste0("Writing: ", filename)))
          writeRaster(con_dist_nest, filename=filename, format="GTiff",
            overwrite=TRUE)
        }
        baea[sv, "con_dist"] <- raster::extract(con_dist, id_year_xy)
        baea[sv, "con_dist_min"] <- cellStats(con_dist, min)
        baea[sv, "con_dist_nest"] <- raster::extract(con_dist, home_sp)
        k <- nrow(distance_df) + 1
        distance_df[k, "con_dist_min"] <- cellStats(con_dist, min)
        distance_df[k, "con_dist_max"] <- cellStats(con_dist, max)
        distance_df[k, "con_dist_max"] <- raster::extract(con_dist, home_sp)
        rm(con_dist)
        global_dist_crop <- distanceFromPoints(base_crop, df_all_sp)
        rm(base_crop)
        cent_dist <- overlay(home_dist, global_dist_crop, fun = function(x,y){
          ifelse(x != y, NA, x)})
        baea[sv, "home_dist"] <- raster::extract(home_dist, id_year_xy)
        distance_df[k, "home_dist_min"] <- cellStats(home_dist, min)
        distance_df[k, "home_dist_max"] <- cellStats(home_dist, max)
        cent_bounds <- boundaries(cent_dist)
        rm(home_dist)
        if (write_terr_edge == TRUE) {
          terr_edge <- cent_bounds
          terr_edge[terr_edge == 0] <- NA
          terr_edge_poly <- rasterToPolygons(terr_edge, n=4, # fun=function(x){x==1},
            digits = 8, dissolve=FALSE)
          file_dir <- file.path(output_dir, i, "TerrEdge_Shapefiles")
          if(!dir.exists(file_dir)) dir.create(file_dir)
          writeLines(noquote(paste0("Writing: ", file.path(file_dir, paste0(
            "TerrEdge_", home_name)))))
          rgdal::writeOGR(terr_edge_poly, dsn = file_dir, layer = paste0(
            "TerrEdge_", home_name), driver = "ESRI Shapefile", overwrite_layer =
            TRUE)
          rm(terr_edge_poly)
        }
        cent_bounds <- subs(cent_bounds, data.frame(from=c(0,1), to=c(NA,1)))
        edge_dist_abs <- distance(cent_bounds)
        rm(cent_bounds) # new
        edge_dist <- overlay(cent_dist, edge_dist_abs, fun=function(x,y)
          {ifelse(!is.na(x), y*-1, y*1)})
        rm(cent_dist, edge_dist_abs)
        if (write_edge_dist == TRUE) {
          filename <- file.path(output_dir, i, paste0("EdgeDist_", home_name,
            ".tif"))
          writeLines(noquote(paste0("Writing: ", filename)))
          writeRaster(edge_dist, filename=filename, format="GTiff",
            overwrite=TRUE)
          edge_dist_shift <- calc(edge_dist, function(x) x - cellStats(edge_dist,
            min))
          filename <- file.path(output_dir, i, paste0("EdgeDistShift_", home_name,
            ".tif"))
          writeLines(noquote(paste0("Writing: ", filename)))
          writeRaster(edge_dist_shift, filename=filename, format="GTiff",
            overwrite=TRUE)
          rm(edge_dist_shift)
        }
        baea[sv, "edge_dist"] <- raster::extract(edge_dist, id_year_xy)
        baea[sv, "edge_dist_min"] <- cellStats(edge_dist, min)
        rm(sv, id_year_xy)
        distance_df[k, "year"] <- i
        distance_df[k, "id"] <- home_name
        distance_df[k, "edge_dist_min"] <- cellStats(edge_dist, min)
        distance_df[k, "edge_dist_max"] <- cellStats(edge_dist, max)
        rm(edge_dist)
      }
  }
  filepath <- file.path(output_dir, "Raster_Distance_Metrics.csv")
  writeLines(noquote(paste0("Writing: ", filepath)))
  write.csv(distance_df, filepath)
  baea$con_dist_shift <- baea$con_dist - baea$con_dist_min
  baea$edge_dist_shift <- baea$edge_dist - baea$edge_dist_min
  return(baea)
  }


# example of largest = TRUE:
nc <- st_transform(st_read(system.file("shape/nc.shp", package="sf")), 2264)
gr = st_sf(
    label = apply(expand.grid(1:10, LETTERS[10:1])[,2:1], 1, paste0, collapse = " "),
    geom = st_make_grid(nc))
gr$col = sf.colors(10, categorical = TRUE, alpha = .3)
# cut, to check, NA's work out:
gr = gr[-(1:30),]
nc_j <- st_join(nc, gr, largest = TRUE)
# the two datasets:
opar = par(mfrow = c(2,1), mar = rep(0,4))
plot(st_geometry(nc_j))
plot(st_geometry(gr), add = TRUE, col = gr$col)
text(st_coordinates(st_centroid(gr)), labels = gr$label)
# the joined dataset:
plot(st_geometry(nc_j), border = 'black', col = nc_j$col)
text(st_coordinates(st_centroid(nc_j)), labels = nc_j$label, cex = .8)
plot(st_geometry(gr), border = 'green', add = TRUE)
par(opar)


r <- raster(ncol=36,nrow=18)
values(r) <- 1:(36*18)
r[500] <- 1
plot(r)
dist <- distance(r)
plot(dist / 1000)


r <- raster(ncols=10, nrows=10)
values(r) <- 1:(10*10)+1
plot(r)
cellFromRowCol(r, 5, 5)
cellFromRowCol(r, 1:2, 1:2)
cellFromRowColCombine(r, 1:3, 1:2)
cellFromCol(r, 1)
cellFromRow(r, 1)

colFromX(r, 0.5)
rowFromY(r, 0.5)
plot(r)
cellFromXY(r, cbind(c(0.5,5), c(15, 88)))
fourCellsFromXY(r, cbind(c(0.5,5), c(15, 88)))

cds1 <- rbind(c(-180,-20), c(-160,5), c(-60, 0), c(-160,-60), c(-180,-20))
cds2 <- rbind(c(80,0), c(100,60), c(120,0), c(120,-55), c(80,0))
pols <- SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), Polygons(list(Polygon(cds2)), 2)))
cellFromPolygon(r, pols)

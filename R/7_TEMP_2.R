# Function for ViewProbsNestMap  -----------------------------------------------
ViewProbNestsMap <- function(nest_names,
                             step_type,
                             model_id,
                             opacity = .6){
  nests_point <- nests_sim %>% slice(which(nests_sim$name %in% nest_names))
  writeLines(paste0("Nests: ", nest_names))
  nests_buffer <- st_buffer(nests_point, dist = 6000)
  nests_bbox <- st_as_sfc(st_bbox(nests_buffer))
  step_type_i <- step_type
  step_type_i_numeric <- step_type_i %>%
    str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
    "perch" = "4", "roost" = "5"))
  step_type_i_text <- step_type_i_numeric %>%
    str_replace_all(c("1" = "Cruise", "2" = "Flight", "3" = "Nest",
      "4" = "Perch", "5" = "Roost"))
  step_type_i_arrow_html <- step_type_i_text %>%
    str_replace_all("_", " &#8594; ")
  writeLines(paste0("Step Type: ", as.character(step_type_i_text)))
  ssf_prob_file <- list.files(file.path(ssf_prob_model_dir, model_id),
     pattern = paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
  ssf_prob <- raster(ssf_prob_file) #%>% slice(1)
  ssf_prob_crop <- crop(ssf_prob, nests_buffer)
  ssf_prob_mask <- mask(ssf_prob_crop, nests_buffer)
  step_type_list <- vector(mode = "list", length = nrow(nests_point))
  for (i in seq_len(nrow(nests_point))){
    nest_i <- nests_point %>% slice(i)
    nest_i_buffer <- st_buffer(nest_i, dist = 6000)
    ssf_prob_i_crop <- crop(ssf_prob, nest_i_buffer)
    ssf_prob_i_mask <- mask(ssf_prob_i_crop, nest_i_buffer)
    names(ssf_prob_i_mask) <- step_type_i_text
    step_type_list[[i]] <- ssf_prob_i_mask
  }
  tmap_i <-
    tm_basemap("Esri.NatGeoWorldMap") +  #"OpenStreetMap","Stamen.Watercolor"
    tm_shape(nests_buffer, name = "Nests Buffer", is.master = TRUE) +
    tm_borders(col = "grey", alpha = .25) +
    tm_facets(by = "name", as.layers = FALSE, ncol = 3) +
    tm_shape(nests_point, name = "Nests") +
    tm_dots(col = "red", size = .25)
#  i <- 1
 # while (i <= length(step_type_list)) {
  #  print(i)
  ssf_raster <- step_type_list[[1]]

  tmap_i <- tmap_i +
      tm_shape(ssf_raster, name = "SSF Probability") +
      tm_raster(alpha = opacity, palette = "viridis", legend.reverse = TRUE,
        title = paste0(step_type_i_arrow_html, "<br>Probability"),
        breaks = seq(0, 1, .1), legend.show = ifelse(i == 1, TRUE, FALSE)) +
      tm_layout(title = "TEST", # Bug in tmap version keeps title from showing
      title.size = 1,
      title.snap.to.legend = FALSE,
      title.position = c("left", "top"))

  tmap_i_leaflet <- tmap_i %>%
     tmap_leaflet

  k <- str_which(map(tmap_i_leaflet[[1]][[1]]$children[[1]]$x$calls, "method"),
     "addRasterImage")
  m <- suppressWarnings(str_which(
     pluck(tmap_i_leaflet[[1]][[1]]$children[[1]]$x$calls, k, "args"),
     "SSF Probability"))
  tmap_i_leaflet[[1]][[1]]$children[[1]]$x$calls$args[[m]] <- 'SSF Probability'

  tmap_i_leaflet_map <- tmap_i_leaflet %>%
     leafem::addImageQuery(ssf_raster, project=TRUE, layerId='SSF Probability',
   			digits = 5, prefix='')

  for (i in k){
     print(i)
  }


  }
  tmap_i <- tmap_i +
    tm_mouse_coordinates()  +
    tm_scale_bar(position=c("left", "bottom")) +
    tm_layout(
      legend.title.size = .75,
      legend.text.size = .5)
  return(tmap_i)
}

# Plot Multiple Probability Layers at Nest in Viewer----------------------------
nest_names <- nests_sim %>% slice(1:4) %>% pull(name)
step_type <- ssf_fits_best %>% slice(4) %>% pull(step_type)

ViewProbNestsMap(nest_name, step_types, model_id, opacity = .6)

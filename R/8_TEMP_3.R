  # Nest SSF Maps --------------------------------------------------------------

  # For Individual Scenario Maps
  for (j in seq_len(length(exp_scenarios))){
    exp_scenario_j <- exp_scenarios[j]
    exp_scenario_j_name <- basename(exp_scenarios[j])
    ssf_tmap_list <- vector(mode = "list", length = 20)
    for (i in seq_len(nrow(ssf_fits_best))){
      step_type_i_numeric <- ssf_fits_best %>% slice(i) %>% pull(step_type) %>%
        str_replace_all(c("cruise" = "1", "flight" = "2", "nest" = "3",
          "perch" = "4", "roost" = "5"))
      ssf_prob_dir <- file.path(exp_scenario_j, "Step_Types_Prob")
      ssf_prob_file <- list.files(ssf_prob_dir, pattern =
        paste0(step_type_i_numeric, "\\.tif$"), full.names = TRUE)
      if (i ==  1){
        # Use "Tmap_baselayers.R" script to get other baselayers
        nest_bbox <- st_as_sfc(st_bbox(st_buffer(nest, dist = 10000)))
        nest_buffer <- st_buffer(nest, dist = 10000)
        #nest_bb_sf <- st_as_sfc(bb(nest_buffer, relative = TRUE, height = 1.35,
        #  width = 1.35))
        nest_bb_sf <- st_as_sfc(bb(nest_buffer, relative = TRUE, height = 1.35,
          width = 1.35))
        Sys.sleep(1)
        nest_om = read_osm(nest_bb_sf, type = om_nat_geo, zoom = 11)
          #type = "osm", minNumTiles=9,
        nest_om_bb <- bb_poly(nest_om)
      }
      ssf_prob_i <- raster(ssf_prob_file) #%>% slice(1)
      ssf_prob_i_crop <- crop(ssf_prob_i, nest_buffer)
      ssf_prob_i_mask <- mask(ssf_prob_i_crop, nest_buffer)

      ssf_prob_i_max <- round(maxValue(ssf_prob_i_mask), 2)
      ssf_prob_i_min <- round(minValue(ssf_prob_i_mask), 2)
      if((ssf_prob_i_max) >= .75 & (ssf_prob_i_min) <= .5){
        raster_breaks <- seq(0, 1, by = .5)
        legend_label <- "A"
        legend_a_only <- tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
          tm_raster(palette = viridis(20, direction = 1),
            breaks = raster_breaks,
            legend.reverse = TRUE, style = "cont",
            title = "A) Probability") +
          tm_layout(legend.only = TRUE,
            legend.title.size = 1.25,
            legend.text.size = .9,
            panel.label.size = .5,
            fontfamily = "Latin Modern Roman")
        legend_a_only
        legend_a_file <- file.path("C:/TEMP/TEMP_Images",
          paste0("Legend_A.png"))
        if(!file.exists(legend_a_file)){
          tmap_save(tm = legend_a_only, filename = legend_a_file, unit = "in",
            dpi = 300, height = .9, width = 1)
        }
      }
      if((ssf_prob_i_max) >= .75 & (ssf_prob_i_min) >= .5) {
        raster_breaks <- seq(.5, 1, by = .25)
        legend_label <- "B"
        legend_b_only <- tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
          tm_raster(palette = viridis(20, direction = 1),
            breaks = raster_breaks,
            legend.reverse = TRUE, style = "cont",
            title = "B) Probability") +
          tm_layout(legend.only = TRUE,
            legend.title.size = 1.5,
            legend.text.size = .9,
            panel.label.size = .5,
            fontfamily = "Latin Modern Roman")
        legend_b_file <- file.path("C:/TEMP/TEMP_Images",
          paste0("Legend_B.png"))
        if(!file.exists(legend_b_file)){
          tmap_save(tm = legend_b_only, filename = legend_b_file, unit = "in",
            dpi = 300, height = .9, width = 1)
        }
      }
      if((ssf_prob_i_max) <= .5){
        raster_breaks <- seq(0, .5, by = .25)
        legend_label <- "C"
        legend_c_only <- tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
          tm_raster(palette = viridis(20, direction = 1),
            breaks = raster_breaks,
            legend.reverse = TRUE, style = "cont",
            title = "C) Probability") +
          tm_layout(legend.only = TRUE,
            legend.title.size = 1.5,
            legend.text.size = .9,
            panel.label.size = .5,
            fontfamily = "Latin Modern Roman")
        legend_c_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_C.png"))
        if(!file.exists(legend_c_file)){
          tmap_save(tm = legend_c_only, filename = legend_c_file, unit = "in",
            dpi = 300, height = .9, width = 1)
        }
      }

      step_type_i_text <- step_type_i_numeric %>%
        str_replace_all("1", "Cruise") %>%
        str_replace_all("2", "Flight") %>%
        str_replace_all("3", "Nest") %>%
        str_replace_all("4", "Perch") %>%
        str_replace_all("5", "Roost")
      writeLines(paste0("Mapping: ", step_type_i_text))
      step_type_i_arrow <- step_type_i_text %>%
        str_replace_all("_", "$\\\\rightarrow$ ") %>%
        latex2exp::TeX(.)
      ssf_prob_i_nest_map <-
        tm_shape(nest_om) +
          tm_rgb() +
       tm_shape(ssf_prob_i_mask, raster.downsample = FALSE) +
       tm_raster(palette = viridis(20, direction = 1), alpha = .6,
         legend.reverse = TRUE, style = "cont", title = "Probability") +
        tm_scale_bar(breaks = c(0, 5, 10), text.size = .65, lwd = .25,
          position = c(.03, .0)) +
        tm_compass(type = "4star", text.size = 0.75, show.labels = 1,
          size = 1.5, position = c(.795, .675), lwd = .25) +
        tm_shape(nest) +
        tm_symbols(shape = 21, border.col = "black", border.lwd = 1,
          col = nest_color, size = .125) +
        tm_layout(asp = .8,
          fontfamily = "Latin Modern Roman",
          frame = NA,
          title.color = "black",
          title.bg.color = NA, #"ivory3",
          title.bg.alpha = .85,
          title.position = c(.215,.95),
          title.fontfamily = "Latin Modern Roman",
          title.fontface = "bold",
          title = step_type_i_arrow,
          title.size = .75,
          title.snap.to.legend = FALSE,
          legend.show = FALSE)  +
          tm_credits(legend_label, bg.color = "white", position = c(.875, .05),
            just = "center", size = 0.7, width = .105)

      tmap_position <- switch(step_type_i_numeric,
        "1_1" = 1,  "1_2" = 2,  "1_4" = 3,
        "2_1" = 5,  "2_2" = 6,  "2_4" = 7,  "2_5" = 8,
        "3_1" = 9,  "3_2" = 10, "3_4" = 11, "3_5" = 12,
        "4_1" = 13, "4_2" = 14, "4_4" = 15, "4_5" = 16,
                    "5_2" = 18, "5_4" = 19)
      writeLines(as.character(tmap_position))
      ssf_tmap_list[[tmap_position]] <- ssf_prob_i_nest_map
    }

    tmap_blank <-
      tm_shape(nest_om_bb, is.master = TRUE) +
        tm_fill(col = "white") +
      tm_shape(nest_buffer, is.master = TRUE) +
        tm_polygons(col = "white", border.col = "white") +
      tm_layout(asp = .8, legend.show = FALSE, frame = FALSE)

    for (i in seq_len(length(ssf_tmap_list))){
      if(is.null(ssf_tmap_list[[i]])) ssf_tmap_list[[i]] <- tmap_blank
    }

    # Arrange map of probability surfaces for testing
    ssf_tmap_nest_arrange <- tmap_arrange(
      ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]],
      ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
      ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
      ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
      ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
      ssf_tmap_list[[16]], ssf_tmap_list[[17]], ssf_tmap_list[[18]],
      ssf_tmap_list[[19]], ssf_tmap_list[[20]], ncol = 4)

    map_file <- file.path("C:/TEMP/TEMP_Images", "SSF_Maps.png")

    tmap_save(tm = ssf_tmap_nest_arrange, filename = map_file, unit = "in",
      dpi = 300, height = 8, width = 8*.8)

    map_img <- map_file %>% image_read(.) %>% image_trim(.)
    legend_a_img <- legend_a_file %>% image_read(.) %>% image_trim(.)
    legend_a_title <- legend_a_img %>% image_crop(., "300x50+0+0")
    legend_a_scale <- legend_a_img %>% image_crop(., "150x155+0+70") %>%
      image_scale(., "x119")

    legend_b_img <- legend_b_file %>% image_read(.) %>% image_trim(.)
    legend_b_title <- legend_b_img %>% image_crop(., "300x50+0+0")
    legend_b_scale <- legend_b_img %>% image_crop(., "150x155+0+70")

    legend_c_img <- legend_c_file %>% image_read(.) %>% image_trim(.)
    legend_c_title <- legend_c_img %>% image_crop(., "300x50+0+0")
    legend_c_scale <- legend_c_img %>% image_crop(., "150x155+0+70")

    ssf_maps_fig <- image_blank(1900, 2395, color = "white") %>%
      image_composite(., map_img, offset = "+0+0") %>%
      image_composite(., legend_a_img, offset = "+1460+1925") %>%
      image_composite(., legend_b_img, offset = "+1460+2170") %>%
      image_composite(., legend_c_img, offset = "+1670+2047")
    ssf_maps_fig

    ssf_nest_map_fig_file <- file.path(tex_dir,
      "Figures/Ch4/Maps_SSF_Probability", nest_str,
      paste0(exp_scenario_j_name, ".png"))
    image_write(ssf_maps_fig, path = ssf_nest_map_fig_file, format = ".png")

    file.remove(map_file)
    file.remove(legend_a_file)
    file.remove(legend_b_file)
    file.remove(legend_c_file)

  }

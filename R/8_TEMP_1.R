# SSF for Maine - Combined -----------------------------------------------------

# This process assumes that the 'final' version of the SSF probablity layers
# is in the ssf_prob_dir

for (i in seq_len(length(ssf_prob_files))){
  print(i)
  ssf_prob_i <- read_stars(ssf_prob_files[i])
  ssf_prob_i_max <- round(maxValue(raster(ssf_prob_files[i])), 2)
  ssf_prob_i_min <- round(minValue(raster(ssf_prob_files[i])), 2)
  if((ssf_prob_i_max) >= .75 & (ssf_prob_i_min) <= .5){
    raster_breaks <- seq(0, 1, by = .5)
    legend_label <- "A"
    legend_a_only <- tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
        legend.reverse = TRUE, style = "cont",
        title = "A) Probability") +
      tm_layout(legend.only = TRUE,
        legend.title.size = 1.25,
        legend.text.size = .9,
        panel.label.size = .5,
        fontfamily = "Latin Modern Roman")
    legend_a_only
    legend_a_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_A.png"))
    if(!file.exists(legend_a_file)){
      tmap_save(tm = legend_a_only, filename = legend_a_file, unit = "in",
        dpi = 300, height = .9, width = 1)
    }
  }
  if((ssf_prob_i_max) >= .75 & (ssf_prob_i_min) >= .5) {
    raster_breaks <- seq(.5, 1, by = .25)
    legend_label <- "B"
    legend_b_only <- tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
        legend.reverse = TRUE, style = "cont",
        title = "B) Probability") +
      tm_layout(legend.only = TRUE,
        legend.title.size = 1.5,
        legend.text.size = .9,
        panel.label.size = .5,
        fontfamily = "Latin Modern Roman")
    legend_b_file <- file.path("C:/TEMP/TEMP_Images", paste0("Legend_B.png"))
    if(!file.exists(legend_b_file)){
      tmap_save(tm = legend_b_only, filename = legend_b_file, unit = "in",
        dpi = 300, height = .9, width = 1)
    }
  }
  if((ssf_prob_i_max) <= .5){
    raster_breaks <- seq(0, .5, by = .25)
    legend_label <- "C"
    legend_c_only <- tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
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
  step_type_numeric <- basename(tools::file_path_sans_ext(ssf_prob_files[i]))
  step_type_latex <- step_type_numeric %>% # This is no longer used
    str_replace_all("1", "Cruise") %>%
    str_replace_all("2", "Flight") %>%
    str_replace_all("3", "Nest") %>%
    str_replace_all("4", "Perch") %>%
    str_replace_all("5", "Roost") %>%
    str_replace_all("_", "$\\\\rightarrow$ ") %>%
    latex2exp::TeX(.)
  step_type <- step_type_numeric %>%
    str_replace_all("1", "Cruise") %>%
    str_replace_all("2", "Flight") %>%
    str_replace_all("3", "Nest") %>%
    str_replace_all("4", "Perch") %>%
    str_replace_all("5", "Roost")
  print(paste0(step_type, " ", ssf_prob_i_min, " ", ssf_prob_i_max))

  start_behavior <- str_split(step_type, "_") %>% pluck(., 1) %>% .[1]
  end_behavior <- str_split(step_type, "_") %>% pluck(., 1) %>% .[2]

  # Old way of having 2 rows of text (however there's too much space btwn lines)
  step_type <- paste0("$\\overset{", start_behavior,
    "$\\rightarrow$ phantom(x)}{", end_behavior, "}$") %>%
    latex2exp::TeX(.)

  # Had to do this manually - couldn't find way to use bquote with tmap
  if(start_behavior == "Cruise"){
    step_type_top <- expression(Cruise %->% phantom(x))
  }
  if(start_behavior == "Flight"){
    step_type_top <- expression(Flight %->% phantom(x))
  }
  if(start_behavior == "Nest"){
    step_type_top <- expression(Nest %->% phantom(x))
  }
  if(start_behavior == "Perch"){
    step_type_top <- expression(Perch %->% phantom(x))
  }
  if(start_behavior == "Root"){
    step_type_top <- expression(Roost %->% phantom(x))
  }

  # Had to do this manually - couldn't find way to use bquote with tmap
  if(end_behavior == "Cruise"){
    step_type_bottom <- expression(Cruise)
  }
  if(end_behavior == "Flight"){
    step_type_bottom <- expression(Flight)
  }
  if(end_behavior == "Nest"){
    step_type_bottom <- expression(Nest)
  }
  if(start_behavior == "Perch"){
    step_type_bottom <- expression(Perch)
  }
  if(start_behavior == "Root"){
    step_type_bottom <- expression(Roost)
  }

  ssf_prob_i_map <-
    tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1), breaks = raster_breaks,
        legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_layout(asp = .8,
        fontfamily = "Latin Modern Roman",
        title.position = c(.65, .1),
        title.fontfamily = "Latin Modern Roman",
        title = legend_label,
        title.size = .7,
        title.snap.to.legend =  FALSE,
        legend.show = FALSE,
        legend.position = c("RIGHT", "BOTTOM"),
        legend.height = .5,
        legend.title.size = .5,
        legend.text.size = .75,
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman",
        frame = FALSE) +
      tm_credits(step_type_top, size = 0.8, fontface = "bold",
        position = c(.05, .89)) +
      tm_credits(step_type_bottom, size = 0.8, fontface = "bold",
        position = c(.075, .79))

  ssf_prob_i_map
  ssf_tmap_list[[i]] <- ssf_prob_i_map
}

tmap_blank <- tm_shape(ssf_prob_i, raster.downsample = TRUE) +
  tm_raster(palette = "white", style = "cont") +
  tm_layout(asp = .75, legend.show = FALSE, frame = FALSE)

ssf_tmap_arrange <- tmap_arrange(
  ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]], tmap_blank,
  tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, ssf_tmap_list[[16]], ssf_tmap_list[[17]], tmap_blank,
  ncol = 4)

# ORIGINAL
ssf_tmap_arrange <- tmap_arrange(
  ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]], tmap_blank,
  ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]],
  ssf_tmap_list[[7]], ssf_tmap_list[[8]], ssf_tmap_list[[9]],
  ssf_tmap_list[[10]], ssf_tmap_list[[11]], ssf_tmap_list[[12]],
  ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
  tmap_blank, ssf_tmap_list[[16]], ssf_tmap_list[[17]], tmap_blank,
  ncol = 4)

maps_file <- file.path("C:/TEMP/TEMP_Images", "SSF_Maps.png")
#tmap_save(tm = ssf_tmap_arrange, filename = maps_file, unit = "in",
#  dpi = 300, height = 8, width = 8*(.8))

map_img <- maps_file %>% image_read(.) %>% image_trim(.)
legend_a_img <- legend_a_file %>% image_read(.) %>% image_trim(.)
legend_a_title <- legend_a_img %>% image_crop(., "300x50+0+0")
legend_a_title
legend_a_scale <- legend_a_img %>% image_crop(., "150x155+0+70") %>%
  image_scale(., "x119")
legend_a_scale

legend_b_img <- legend_b_file %>% image_read(.) %>% image_trim(.)
legend_b_title <- legend_b_img %>% image_crop(., "300x50+0+0")
legend_b_title
legend_b_scale <- legend_b_img %>% image_crop(., "150x155+0+70")
legend_b_scale

legend_c_img <- legend_c_file %>% image_read(.) %>% image_trim(.)
legend_c_title <- legend_c_img %>% image_crop(., "300x50+0+0")
legend_c_title
legend_c_scale <- legend_c_img %>% image_crop(., "150x155+0+70")
legend_c_scale

ssf_maps_fig <- image_blank(1850, 2395, color = "white") %>%
  image_composite(., map_img, offset = "+0+0") %>%
  image_composite(., legend_a_title, offset = "+1380+1930") %>%
  image_composite(., legend_a_scale, offset = "+1440+1975") %>%
  image_composite(., legend_c_title, offset = "+1605+2060") %>%
  image_composite(., legend_c_scale, offset = "+1665+2105") %>%
  image_composite(., legend_b_title, offset = "+1380+2190") %>%
  image_composite(., legend_b_scale, offset = "+1440+2235")

ssf_maps_fig

# Export
ssf_maps_fig_file <- file.path(tex_dir, "Figures/Ch2",
  "SSF_Prob_Raster_Maps", "SSF_Probability_Maps_Overview.png")
image_write(ssf_maps_fig, path = ssf_maps_fig_file, format = ".png")

file.remove(maps_file)
file.remove(legend_a_file)
file.remove(legend_b_file)
file.remove(legend_c_file)

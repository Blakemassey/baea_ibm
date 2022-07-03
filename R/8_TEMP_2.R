# Movement Kernels -------------------------------------------------------------

move_dens <- data.frame(grp = character(), x = numeric(), y = numeric(),
  dens = numeric())
for (i in 1:nrow(move_pars)){
  move_pars_i <- move_pars[i, ]
  ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise", "Flight"),
    FALSE, TRUE)
  kernel_i <- CreateMoveKernelWeibullVonMises(
      max_r = NULL,
      cellsize = 30,
      mu1 = move_pars_i$mvm_mu1[1],
      mu2 = move_pars_i$mvm_mu2[1],
      kappa1 = move_pars_i$mvm_kappa1[1],
      kappa2 = move_pars_i$mvm_kappa2[1],
      mix = move_pars_i$mvm_prop[1],
      shape = move_pars_i$weibull_shape[1],
      scale = move_pars_i$weibull_scale[1],
      ignore_von_mises = ignore_von_mises)
  r <- (30*((nrow(kernel_i)-1)/2))+(30/2)
  kernel_raster <- raster::raster(kernel_i, xmn=-r, xmx=r, ymn=-r, ymx=r)
  #print(raster::freq(kernel_raster))
  df <- data.frame(raster::rasterToPoints(kernel_raster)) # NA cells removed.
  names(df)[3] <- "dens"
  df$behavior_behavior <- move_pars_i$behavior_behavior
  move_dens <- rbind(move_dens, df)
}

# All plots on one figure
ind_list = lapply(sort(unique(move_dens$behavior_behavior)), function(i){
  grp_i = str_replace_all(i, "->", "$\\\\rightarrow$")
  gg_move_kernel <- move_dens %>%
      filter(behavior_behavior == i) %>%
    ggplot(., aes(x = x, y = y)) +
    geom_raster(aes(fill = dens)) +
    coord_fixed(ratio = 1) +
    scale_fill_viridis_c(name = "Probability", option = "C", direction = -1,
      labels = scales::scientific) +
    scale_x_continuous(expand = c(0.005, 0.005), labels = MetersToKilometers) +
    scale_y_continuous(expand = c(0.005, 0.005), labels = MetersToKilometers) +
    theme_minimal() +
    theme_latex +
    theme(plot.margin = margin(3, 6, 3, 6, "pt")) +
    #theme(axis.text = element_text(size = 7)) +
    theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
    #theme(legend.text = element_text(size = 7)) +
    #theme(legend.title = element_text(size = 8)) +
    #theme(plot.title = element_text(size = 9, vjust = -1, hjust = 0.5)) +
    #guides(fill = guide_colourbar(barwidth = .5, barheight = 3)) +
    theme(legend.position = "none") +
    ggtitle(NULL) + labs(x = NULL, y = NULL)
  return(gg_move_kernel)
})

layout <- '
ABC#
DEXG
HIJK
LMNO
#PQ#
'
movements_kernel_plots <- wrap_plots(A = ind_list[[1]],
  B = ind_list[[2]], C = ind_list[[3]], D = ind_list[[4]], E = ind_list[[5]],
  X = ind_list[[6]], G = ind_list[[7]], H = ind_list[[8]], I = ind_list[[9]],
  J = ind_list[[10]], K = ind_list[[11]], L = ind_list[[12]], M =ind_list[[13]],
  N = ind_list[[14]], O = ind_list[[15]], P = ind_list[[16]], Q =ind_list[[17]],
  design = layout)
movements_kernel_plots

# Save Temp (No Label) File
movements_kernel_no_label_fig_file = file.path("C:/TEMP/TEMP_Images",
  "Movements_Kernel_NO_LABEL.png")
ggsave(filename = basename(movements_kernel_no_label_fig_file),
  plot =  movements_kernel_plots,
  path = dirname(movements_kernel_no_label_fig_file), scale = 1, width = 5,
  height = 5.75, units = "in", dpi = 300)


# Create Tex Strings
tex_head <- tibble(
  tex_str = c("Distance (km)"),
  tex_name = c("lab_distance"))
tex_head_df <- bind_rows(
  bind_rows(tex_head) %>% mutate(title_size = 11))

# Create Tex Strings
tex_state <- tibble(
  tex_str = c("Start State", "End State"),
  tex_name = c("start", "end"))
tex_state_df <- bind_rows(
  bind_rows(tex_state) %>% mutate(title_size = 12))

# Create Tex Strings
tex_behavior <- tibble(
  tex_str = c("Cruise", "Flight", "Nest", "Perch", "Roost"),
  tex_name = c("cruise", "flight", "nest", "perch", "roost"))
tex_behavior_df <- bind_rows(
  bind_rows(tex_behavior) %>% mutate(title_size = 11))

tex_df <- bind_rows(tex_head_df, tex_state_df, tex_behavior_df)

# Create Tex Text Plots
for (i in seq_len(nrow(tex_df))){
  tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
  tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
  title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
  gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
    theme(plot.title = element_text(size = title_size_i))
  ggsave(file = "C:/TEMP/TEMP_Images/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("C:/TEMP/TEMP_Images/TEMP.png"))
  file.remove("C:/TEMP/TEMP_Images/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}

rm(i, tex_head, tex_head_df, tex_state, tex_state_df, tex_behavior,
  tex_behavior_df, tex_df, tex_name_i, tex_str_i, tex_i)

# Create Manual Legend
legend_tbl <- tibble(x = c(0, 1), y = c(.5, .5))
gg_legend <- legend_tbl %>%
  ggplot(., aes(x = x, y = y)) +
  geom_point(aes(fill = x)) +
  coord_fixed(ratio = 1) +
  guides(fill = guide_colourbar(ticks = FALSE,barwidth = .5, barheight = 2.75)) +
  scale_fill_viridis_c(name = "Probability", option = "C", direction = -1,
    breaks = c(min(move_dens_i$x), max(move_dens_i$x)),
    labels = c("Min (varies)", "Max (varies)")) +
  theme_latex

gg_legend_only <- as_ggplot(get_legend(gg_legend))
gg_legend_only

# Save Temp (No Label) File
movements_kernel_legend_fig_file = file.path("C:/TEMP/TEMP_Images",
  "Movements_Kernel_Legend.png")
ggsave(filename = basename(movements_kernel_legend_fig_file),
  plot =  gg_legend_only,
  path = dirname(movements_kernel_no_label_fig_file), scale = 1, width = 1,
  height = 1, units = "in", dpi = 300)

# Image background
backgrd <- image_blank(1660, 1800, color = "white")

# Create Final Plot and Export to Dissertation
movements_kernel_fig <- image_read(movements_kernel_no_label_fig_file)
movements_legend_fig <- image_read(movements_kernel_legend_fig_file)

movements_kernel_labels_fig_ver_1 <- backgrd %>%
  image_composite(., movements_kernel_fig, offset = "+45+65") %>%
  image_composite(., image_rotate(tex_lab_distance, 270),
    offset = "+10+750") %>%
  image_composite(., tex_lab_distance, offset = "+750+1755") %>%
  image_composite(., tex_end, offset = "+750+0") %>%
  image_composite(., image_rotate(tex_start, 90), offset = "+1620+790") %>%
  image_composite(., tex_cruise, offset = "+250+65") %>%
  image_composite(., tex_flight, offset = "+625+65") %>%
  image_composite(., tex_nest, offset = "+985+65") %>%
  image_composite(., tex_perch, offset = "+1305+65") %>%
  image_composite(., image_rotate(tex_cruise, 90), offset = "+1550+150") %>%
  image_composite(., image_rotate(tex_flight, 90), offset = "+1550+510") %>%
  image_composite(., image_rotate(tex_nest, 90), offset = "+1550+850") %>%
  image_composite(., image_rotate(tex_perch, 90), offset = "+1550+1150") %>%
  image_composite(., image_rotate(tex_roost, 90), offset = "+1550+1490") %>%
  image_composite(., movements_legend_fig, offset = "+1200+1400")

movements_kernel_labels_fig_ver_1a <- backgrd %>%
  image_composite(., movements_kernel_fig, offset = "+45+65") %>%
  image_composite(., image_rotate(tex_lab_distance, 270),
    offset = "+10+750") %>%
  image_composite(., tex_lab_distance, offset = "+750+1755") %>%
  image_composite(., tex_end, offset = "+750+0") %>%
  image_composite(., image_rotate(tex_start, 90), offset = "+1620+790") %>%
  image_composite(., tex_cruise, offset = "+250+65") %>%
  image_composite(., tex_flight, offset = "+625+65") %>%
  image_composite(., tex_nest, offset = "+985+65") %>%
  image_composite(., tex_perch, offset = "+1305+65") %>%
  image_composite(., image_rotate(tex_cruise, 90), offset = "+1550+150") %>%
  image_composite(., image_rotate(tex_flight, 90), offset = "+1550+510") %>%
  image_composite(., image_rotate(tex_nest, 90), offset = "+1550+850") %>%
  image_composite(., image_rotate(tex_perch, 90), offset = "+1550+1150") %>%
  image_composite(., image_rotate(tex_roost, 90), offset = "+1550+1490") %>%
  image_composite(., movements_legend_fig, offset = "+90+1435")

movements_kernel_labels_fig_ver_2 <- backgrd %>%
  image_composite(., movements_kernel_fig, offset = "+45+65") %>%
  image_composite(., image_rotate(tex_lab_distance, 270),
    offset = "+10+750") %>%
  image_composite(., tex_lab_distance, offset = "+750+1755") %>%
  image_composite(., tex_end, offset = "+750+0") %>%
  image_composite(., image_rotate(tex_start, 90), offset = "+1620+790") %>%
  image_composite(., tex_cruise, offset = "+250+65") %>%
  image_composite(., tex_flight, offset = "+625+65") %>%
  image_composite(., tex_nest, offset = "+985+65") %>%
  image_composite(., tex_perch, offset = "+1315+390") %>%
  image_composite(., image_rotate(tex_cruise, 90), offset = "+1210+160") %>%
  image_composite(., image_rotate(tex_flight, 90), offset = "+1550+510") %>%
  image_composite(., image_rotate(tex_nest, 90), offset = "+1550+850") %>%
  image_composite(., image_rotate(tex_perch, 90), offset = "+1550+1150") %>%
  image_composite(., image_rotate(tex_roost, 90), offset = "+1210+1490") %>%
  image_composite(., movements_legend_fig, offset = "+1330+1480")

movements_kernel_labels_fig_ver_1
movements_kernel_labels_fig_ver_1a
movements_kernel_labels_fig_ver_2

movements_kernel_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Movements_Kernel.png")
image_write(movements_kernel_labels_fig_ver_1a, path =movements_kernel_fig_file,
  format = ".png")
file.remove(movements_kernel_no_label_fig_file)
file.remove(movements_kernel_legend_fig_file)




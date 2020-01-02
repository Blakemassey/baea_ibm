# Plotting (von Mises) ---------------------------------------------------------

bin_width = (2*pi)/24
breaks <- seq(0, (2*pi), by=((2*pi)/12))
labels <- c(0, "", "", expression(pi / 2), "", "",
   expression(pi), "", "", expression(1.5*pi), "", "",
   expression(2*pi))
minor_breaks <- seq(0, 2*pi, by=bin_width)
minor_labels <- c(0, "", "", expression(pi / 4), "", "", expression(pi / 2),
  "", "", expression(3/4*pi), "", "", expression(pi),
  "", "", expression(5/4*pi), "", "", expression(1.5*pi),
  "", "", expression(7/4*pi), "", "")
limits <- c(0, 2*pi)
vec_length <- 100
von_mises_dens <- data.frame(grp=factor(), pred=numeric(), dens=numeric())

for (i in 1:nrow(move_pars)){
  pars_i <- move_pars[i,]
  grp = rep(pars_i$behavior_behavior, vec_length)
  pred = seq(limits[1], limits[2], length = vec_length)
  dens = dmixedvm(pred, mu1 = pars_i$mvm_mu1, mu2 = pars_i$mvm_mu2,
    kappa1 = pars_i$mvm_kappa1, kappa2 = pars_i$mvm_kappa2,
    p = pars_i$mvm_prop)
  von_mises_dens <- rbind(von_mises_dens, data.frame(grp, pred, dens))
}

# Polar Coordinates ------------------------------------------------------------
# All plots on one figure
ind_list = lapply(sort(unique(baea_movements_vm$behavior_behavior)),
  function(i){
    grp_i = str_replace_all(i, "->", "$\\\\rightarrow$")
    ggplot(baea_movements_vm[baea_movements_vm$behavior_behavior == i, ],
      aes(x = turn_angle)) +
    geom_histogram(aes(y = ..density.., weight=weights), fill = "grey20",
      color = "black", boundary = 0, binwidth = (2*pi)/24) +
    geom_line(data = von_mises_dens[von_mises_dens$grp == i, ],
      aes(x = pred, y = dens), size = 1, colour = "red") +
    coord_polar(start = (1.5*pi), direction = -1) +
    scale_y_continuous(labels = NULL) +
    scale_x_continuous(limits = limits, labels = minor_labels,
      breaks = minor_breaks[-25], minor_breaks = minor_breaks, expand = c(0,0))+
    theme(axis.ticks = element_blank()) +
    facet_grid(. ~ behavior_behavior) +
    theme_minimal() + theme_latex +
    theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
    theme(axis.text = element_text(size = 7)) +
    theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
    theme(plot.title = element_text(size = 8, vjust = -2)) +
    theme(legend.position="none") +
    theme(panel.grid.major = element_line(colour = "grey90"))  +
    theme(panel.grid.minor = element_line(colour = "grey90"))  +
    ggtitle(NULL) +
    labs(x = NULL, y = NULL)
})

layout <- c(
  patchwork::area(t = 1, l = 1),
  patchwork::area(t = 1, l = 2),
  patchwork::area(t = 1, l = 3),
  patchwork::area(t = 2, l = 1),
  patchwork::area(t = 2, l = 2),
  patchwork::area(t = 2, l = 3),
  patchwork::area(t = 2, l = 4),
  patchwork::area(t = 3, l = 1),
  patchwork::area(t = 3, l = 2),
  patchwork::area(t = 3, l = 3),
  patchwork::area(t = 3, l = 4),
  patchwork::area(t = 4, l = 2),
  patchwork::area(t = 4, l = 3)
)
#plot(layout)

movements_direction_plots <- ind_list[[1]] + ind_list[[2]] + ind_list[[3]] +
  ind_list[[4]] + ind_list[[5]] + ind_list[[6]] + ind_list[[7]] + ind_list[[8]]+
  ind_list[[9]] + ind_list[[10]] + ind_list[[11]] + ind_list[[12]] +
  ind_list[[13]] + plot_layout(design = layout)

# Save Temp (No Label) File
movements_direction_no_lab_fig_file = file.path("Output/Analysis/Movements",
  "Movements_Direction_NO_LABEL.png")
ggsave(filename = basename(movements_direction_no_lab_fig_file),
  plot =  movements_direction_plots,
  path = dirname(movements_direction_no_lab_fig_file), scale = 1, width = 6,
  height = 5.9,units = "in", dpi = 300)

# Create Tex Strings
tex_head <- tibble(
  tex_str = c("Probability", "Direction (radians)"),
  tex_name = c("lab_density", "lab_direction"))
tex_df <- bind_rows(
  bind_rows(tex_head) %>% mutate(title_size = 11))

# Create Tex Text Plots
for (i in seq_len(nrow(tex_df))){
  tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
  tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
  title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
  gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
    theme(plot.title = element_text(size = title_size_i))
  ggsave(file = "Output/Analysis/Movements/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("Output/Analysis/Movements/TEMP.png"))
  file.remove("Output/Analysis/Movements/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}
rm(i, tex_df, tex_name_i, tex_str_i, tex_i)

# Image background
backgrd <- image_blank(1800, 1800, color = "white")

# Create Final Plot and Export to Dissertation
movements_direction_fig <- image_read(movements_direction_no_lab_fig_file)
movements_direction_labels_fig <- backgrd %>%
  image_composite(., movements_direction_fig, offset = "+20+00") %>%
  image_composite(., image_rotate(tex_lab_density, 270), offset = "+20+700") %>%
  image_composite(., tex_lab_direction, offset = "+800+1745")
movements_direction_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Movements_Direction.png")
image_write(movements_direction_labels_fig,
  path = movements_direction_fig_file, format=".png")
file.remove(file.path(movements_direction_no_lab_fig_file))

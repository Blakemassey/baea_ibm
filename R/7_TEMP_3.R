
ua_steps_stats_squared <- ua_steps_stats %>% filter(covar_squared == "^2")

for (i in unique(ua_steps_stats_squared %>% pull(behavior_behavior))){
  #i <- "Perch -> Perch"
  gg_covar_land <-  ggplot(ua_steps_stats_squared %>%
      filter(behavior_behavior == i) %>% filter(covar_alpha %in% landcover)) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    facet_grid(~ covar_alpha, labeller = labeller(covar_alpha = RelabelCovar)) +
    labs(x = "", y = "Step-Selection Covariate Difference\n(Used - Available)")+
    theme_latex

  gg_covar_land

  gg_covar_directions <-  ggplot(ua_steps_stats_squared %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha %in% directions)) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    facet_grid(. ~ covar_alpha, labeller = labeller(covar_alpha = Capitalize)) +
    labs(x = "", y = "Step-Selection Covariate Difference\n(Used - Available)")

  gg_covar_rough <-  ggplot(ua_steps_stats_squared %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha == "roughness")) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    ggtitle("Roughness") + labs(x = "", y = "") +
    theme(plot.title = element_text(vjust = -12, hjust = .5, size = 9))

  gg_covar_tpi <-  ggplot(ua_steps_stats_squared %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha == "tpi")) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    ggtitle("TPI") + labs(x = "", y = "") +
    theme(plot.title = element_text(vjust = -12, hjust = .5, size = 9))

  gg_covar_tri <-  ggplot(ua_steps_stats_squared %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha == "tri")) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    ggtitle("TRI") + labs(x = "", y = "") +
    theme(plot.title = element_text(vjust = -12, hjust = .5, size = 9))

  gg_covar_windclass <-  ggplot(ua_steps_stats_squared %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha == "wind_class")) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    ggtitle("Wind Class") + labs(x = "", y = "") +
    theme(plot.title = element_text(vjust = -12, hjust = .5, size = 9))

  gg_covar_dist <-  ggplot(ua_steps_stats_squared %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha %in% dist) %>%
      mutate(covar_alpha = Capitalize(str_replace_all(covar_alpha, "_dist",
        "")))) +
    geom_errorbar(aes(x = covar_alpha, y = median, ymin = prob_25,
      ymax = prob_75), color = ribbon_color2, width = 0.15) +
    geom_errorbar(aes(x = covar_alpha, y = median, ymin = prob_05,
      ymax = prob_95), color = ribbon_color1,  width = 0.2) +
    geom_point(aes(x = covar_alpha, y = median), color = "gray20", size = 2)+
    geom_point(aes(x = covar_alpha, y = median), color = point_color, size=1.5)+
    ggtitle("Euclidean Distance to Nearest") + labs(x = "", y = "") +
    theme(plot.title = element_text(vjust = -12, hjust = .5, size = 9)) +
    theme(axis.text.x.bottom = element_text(angle = 0, hjust = .4,
      vjust = 0.75))

  layout <-
    'AAAAAAAA
     BBCDEGHH'

  # Theme (for LaTeX font)
  theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
    theme(axis.text = element_text(size = 6)) +
    theme(axis.title = element_text(size = 7)) +
    theme(plot.title = element_text(size = 7, hjust = .5)) +
    theme(strip.text = element_text(size = 7)) +
    theme(strip.background = element_rect(colour = NA, fill = NA)) +
    theme(plot.margin = margin(t = 1, r = 2, b = 1, l = 1, unit = "pt")) +
    theme(panel.background = element_rect(fill = NA, colour = NA,
       size = 2, linetype = "solid")) +
    theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
      colour = "grey92")) +
    theme(panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
      colour = "grey95")) +
    theme(axis.ticks = element_line(size = 0.5, linetype = 'solid',
      colour = "grey92")) +
    theme(axis.ticks.length = unit(1, "pt")) +
    theme(axis.title.x.bottom = element_text(vjust = 3))

  gg_covar_stats <- wrap_plots(
    A = gg_covar_land,
    B = gg_covar_directions,
    C = gg_covar_rough,
    D = gg_covar_tpi,
    E = gg_covar_tri,
    G = gg_covar_windclass,
    H = gg_covar_dist,
    design = layout) & theme_latex
  gg_covar_stats

  i_name <- str_replace_all(i, " -> ", "_") %>% paste0(., "_Squared")
  gg_covar_stats_fig_no_lab_file = paste0("C:/TEMP/Covar_Figures/", i_name,
    "_no_label.png")

  ggsave(filename = basename(gg_covar_stats_fig_no_lab_file),
    plot =  gg_covar_stats,
    path = dirname(gg_covar_stats_fig_no_lab_file), scale = 1, width = 9,
    height = 6, units = "in", dpi = 300)

  # Create Tex Strings
  i_name_tex = str_replace_all(i_name, "_", " $\\\\rightarrow$ ")
  tex_head <- tibble(
    tex_str = c("Covariate Bandwidth (Sigma)", "Covariate", i_name_tex),
    tex_name = c("lab_sigma", "lab_covar", "lab_steptype"))
  tex_df <- bind_rows(bind_rows(tex_head) %>% mutate(title_size =
      c(rep(7, times = 2), rep(9, times = 1))))

  # Create Tex Text Plots
  for (i in seq_len(nrow(tex_df))){
    tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
    tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
    title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
    gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
      theme(plot.title = element_text(size = title_size_i))
    ggsave(file = "C:/TEMP/TEMP.png", plot = gg_tex, width = 5, height = .75)
    tex_i <- image_trim(image_read("C:/TEMP/TEMP.png"))
    file.remove("C:/TEMP/TEMP.png")
    assign(paste0("tex_", tex_name_i), tex_i)
  }
  rm(i, tex_df, tex_name_i, tex_str_i, tex_i, tex_head)

  # Image background
  backgrd <- image_blank(2700, 1900, color = "white")
  # Create Final Plot and Export to Dissertation
  covar_stats_no_label_fig <- image_read(gg_covar_stats_fig_no_lab_file)
  covar_stats_label_fig <- backgrd %>%
    image_composite(., covar_stats_no_label_fig, offset = "+000+035") %>%
    image_composite(., image_rotate(tex_lab_sigma, 0), offset = "+1200+890") %>%
    image_composite(., image_rotate(tex_lab_sigma, 0), offset = "+975+1815")%>%
    image_composite(., image_rotate(tex_lab_covar, 0), offset = "+2380+1815")%>%
    image_composite(., image_rotate(tex_lab_steptype, 0), offset = "+1290+015")

  covar_stats_label_fig

  covar_stats_label_fig_file <- paste0("C:/TEMP/Covar_Figures/", i_name, ".png")
  graphs_file <- file.path(graphs_dir, paste0(i_name, ".png"))

  image_write(covar_stats_label_fig, path = covar_stats_label_fig_file,
    format = ".png")

  covar_stats_label_fig_file <- paste0("C:/TEMP/Covar_Figures/", i_name, ".png")
  file.copy(covar_stats_label_fig_file, graphs_file)

  file.remove(gg_covar_stats_fig_no_lab_file)
}

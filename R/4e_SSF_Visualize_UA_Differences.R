################## ModelFit_SSF_Calculate_Covariates ###########################
# Load libraries, scripts, and input parameters --------------------------------
pacman::p_load(plyr, dplyr, forcats, ggplot2, ggthemes, gridExtra, magick,
  patchwork, purrr, stringr, tidyr, tibble)
pacman::p_load(baear, gisr, ibmr)
suppressMessages(extrafont::loadfonts(device="win"))

# Directories
ua_data_diff_dir <- "Output/Analysis/SSF/UA_Data_Diff"

# Themes
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14, hjust = .5)) +
  theme(strip.background = element_rect(fill = NA)) +
  theme(strip.text = element_text(colour = 'black', size = 14))
theme_blank <- theme(legend.position = "none",
  text = element_text(family = "Latin Modern Roman"),
  plot.title = element_text(size=14),
  panel.grid = element_blank(), axis.title = element_blank(),
  axis.text = element_blank(), axis.ticks = element_blank(),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA))
theme_update(plot.title = element_text(hjust = 0.5))

RelabelCovar <- function(string){
  out <- string %>%
    str_replace_all(., "_", " ") %>%
    str_to_title(.)
  return(out)
}

## Graph UA Differences --------------------------------------------------------

# Load ua_step_diff
ua_steps_diff <- list.files(path = ua_data_diff_dir,
    pattern = paste0("ua_steps_diff_*")) %>%
  map(~ readRDS(file.path(ua_data_diff_dir, .))) %>%
    reduce(bind_rows) %>%
    as_tibble(.)

# Check histogram of a test value
ua_steps_diff %>%
  filter(behavior_behavior == "Nest -> Perch") %>%
  pull(road_dist0) %>%
  hist()

# Summarize mean values for each of the differences
ua_steps_mean <- ua_steps_diff %>%
  group_by(behavior_behavior) %>%
  summarise(across(matches("[0-9]"), mean), .groups = "drop") %>%
  pivot_longer(!behavior_behavior, names_to = "covar", values_to = "mean")

# Summarize median values for each of the differences
ua_steps_median <- ua_steps_diff %>%
  group_by(behavior_behavior) %>%
  summarise(across(matches("[0-9]"), median), .groups = "drop") %>%
  pivot_longer(!behavior_behavior, names_to = "covar", values_to = "median")

# Summarize quantiles (.05, .25, etc.) for each of the differences
ua_steps_prob_05 <- ua_steps_diff %>%
  group_by(behavior_behavior) %>%
  summarise(across(matches("[0-9]"), ~quantile(.x, probs = .05)),
    .groups = "drop") %>%
  pivot_longer(!behavior_behavior, names_to = "covar", values_to = "prob_05")

ua_steps_prob_25 <- ua_steps_diff %>%
  group_by(behavior_behavior) %>%
  summarise(across(matches("[0-9]"), ~quantile(.x, probs = .25)),
    .groups = "drop") %>%
  pivot_longer(!behavior_behavior, names_to = "covar", values_to = "prob_25")

ua_steps_prob_50 <- ua_steps_diff %>%
  group_by(behavior_behavior) %>%
  summarise(across(matches("[0-9]"), ~quantile(.x, probs = .50)),
    .groups = "drop") %>%
  pivot_longer(!behavior_behavior, names_to = "covar", values_to = "prob_50")

ua_steps_prob_75 <- ua_steps_diff %>%
  group_by(behavior_behavior) %>%
  summarise(across(matches("[0-9]"), ~quantile(.x, probs = .75)),
    .groups = "drop") %>%
  pivot_longer(!behavior_behavior, names_to = "covar", values_to = "prob_75")

ua_steps_prob_95 <- ua_steps_diff %>%
  group_by(behavior_behavior) %>%
  summarise(across(matches("[0-9]"), ~quantile(.x, probs = .95)),
    .groups = "drop") %>%
  pivot_longer(!behavior_behavior, names_to = "covar", values_to = "prob_95")

ua_steps_stats <- ua_steps_mean %>%
  left_join(., ua_steps_median) %>%
  left_join(., ua_steps_prob_05) %>%
  left_join(., ua_steps_prob_25) %>%
  left_join(., ua_steps_prob_50) %>%
  left_join(., ua_steps_prob_75) %>%
  left_join(., ua_steps_prob_95) %>%
  mutate(
    covar_alpha = str_replace_all(covar, "[0-9]", ""),
    covar_num = as.numeric(str_replace_all(covar, "[^0-9]", "")),
    covar_sigma = covar_num/30,
    covar = paste0(covar_alpha, covar_num))

rm(ua_steps_prob_05, ua_steps_prob_25, ua_steps_prob_50, ua_steps_prob_75,
  ua_steps_prob_95)

# Group covariate types
landcover <- c("developed", "forest", "open_water", "pasture", "road",
  "shrub_herb", "wetland")
directions <- c("eastness", "northness")
terrain <- c("roughness", "tpi", "tri", "wind_class")
dist <- c("developed_dist", "hydro_dist", "road_dist", "turbine_dist")

# Create graph for each of the step_type and covariates showing the differences
# between the used and available values


line_color = viridis::viridis(5)[5]
point_color = viridis::viridis(5)[5]

ribbon_fill1 <-  "grey95"
ribbon_color1 <- viridis::viridis(5)[1]
ribbon_alpha1 <- .5

ribbon_fill2 <-  NA
ribbon_color2 <- viridis::viridis(5)[4]
ribbon_alpha2 <- .5

for (i in unique(ua_steps_stats %>% pull(behavior_behavior))){
  #i <- "Nest -> Perch"
  gg_covar_land <-  ggplot(ua_steps_stats %>%
      filter(behavior_behavior == i) %>% filter(covar_alpha %in% landcover)) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    facet_grid(~ covar_alpha, labeller = labeller(covar_alpha = RelabelCovar)) +
    labs(x = "", y = "Median Difference\n(Used - Available)") + theme_latex

  gg_covar_land

  gg_covar_directions <-  ggplot(ua_steps_stats %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha %in% directions)) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    facet_grid(. ~ covar_alpha, labeller = labeller(covar_alpha = Capitalize)) +
    labs(x = "", y = "Median Difference\n(Used - Available)")

  gg_covar_rough <-  ggplot(ua_steps_stats %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha == "roughness")) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    ggtitle("Roughness") + labs(x = "", y = "") +
    theme(plot.title = element_text(vjust = -12, hjust = .5, size = 9))

  gg_covar_tpi <-  ggplot(ua_steps_stats %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha == "tpi")) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    ggtitle("TPI") + labs(x = "", y = "") +
    theme(plot.title = element_text(vjust = -12, hjust = .5, size = 9))

  gg_covar_tri <-  ggplot(ua_steps_stats %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha == "tri")) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    ggtitle("TRI") + labs(x = "", y = "") +
    theme(plot.title = element_text(vjust = -12, hjust = .5, size = 9))

  gg_covar_windclass <-  ggplot(ua_steps_stats %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha == "wind_class")) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_05, ymax = prob_95),
      fill = ribbon_fill1, color = ribbon_color1, alpha = ribbon_alpha1) +
    geom_ribbon(aes(x = covar_sigma, ymin = prob_25, ymax = prob_75),
      fill = ribbon_fill2, color = ribbon_color2, alpha = ribbon_alpha2) +
    geom_line(aes(x = covar_sigma, y = median), color = line_color, size = .75)+
    ggtitle("Wind Class") + labs(x = "", y = "") +
    theme(plot.title = element_text(vjust = -12, hjust = .5, size = 9))

  gg_covar_dist <-  ggplot(ua_steps_stats %>%
      filter(behavior_behavior == i)  %>% filter(covar_alpha %in% dist) %>%
      mutate(covar_alpha = Capitalize(str_replace_all(covar_alpha, "_dist",
        "")))) +
    geom_errorbar(aes(x = covar_alpha, y = median, ymin = prob_05,
      ymax = prob_95), width = 0.2) +
    geom_point(aes(x = covar_alpha, y = median), color = point_color, size = 1)+
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

  i_name <- str_replace_all(i, " -> ", "_")
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

  image_write(covar_stats_label_fig, path = covar_stats_label_fig_file,
    format = ".png")

  file.remove(gg_covar_stats_fig_no_lab_file)
}

### ------------------------------------------------------------------------ ###
############################### OLD CODE #######################################
### ------------------------------------------------------------------------ ###

# ua_steps_all <- list.files(path = file.path(ua_data_dir),
#     pattern = "^ua_steps_*")

# for (i in seq_along(ua_steps_all)){
#   ua_steps_i <- readRDS(file.path(ua_data_dir, ua_steps_all[i]))
#   print(paste0(ua_steps_all[i], " Columns = ", dim(ua_steps_i)[2]))}


# ua_steps_i_rd <- readRDS(file.path(ua_data_dir, ua_steps_i[2]))
#
# ua_steps_i_combine <- left_join(ua_steps_i_org, ua_steps_i_rd)
#
# saveRDS(ua_steps_i_combine, file.path(ua_data_dir, ua_steps_i[1]))
# rm(ua_steps_i, ua_steps_i_org, ua_steps_i_rd, ua_steps_i_combine)

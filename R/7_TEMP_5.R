
# Transition Probabilities NEW -------------------------------------------------

df_trans_new <- readRDS(file = "Output/Analysis/Transitions/df_trans_NEW.rds") %>%
  mutate(date = as.Date(julian_date, origin=as.Date("2015-01-01")))
state_names <- c('1' = "Cruise", '2' = "Flight", '3' = "Nest", '4' = "Perch",
  '5' = "Roost")

# Tex Strings
tex_head <- tibble(
  tex_str = c("Start State", "End State"),
  tex_name = c("start", "end"))

# Tex Dataframe
tex_df <- bind_rows(
  bind_rows(tex_head) %>% mutate(title_size = 11))

# Create Tex Text Plots
for (i in seq_len(nrow(tex_df))){
  tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
  tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
  title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
  gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
    theme(plot.title = element_text(size = title_size_i))
  ggsave(file = "Output/Analysis/Territorial/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("Output/Analysis/Territorial/TEMP.png"))
  file.remove("Output/Analysis/Territorial/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}

rm(i, tex_df, tex_name_i, tex_str_i, tex_i)

# Image background
backgrd <- image_blank(1800, 1800, color = "white")

# Metrics for figure labels

CalculateMode(df_trans %>% pull(time_proportion)) # 0.5030271
CalculateMode(df_trans %>% pull(date)) #"2015-06-02"
range(df_trans$date) #"2015-03-21" "2015-08-17"

# Plot Transitions by Date
gg_trans_date <- ggplot(
  data = df_trans %>%
    filter(
      time_proportion == CalculateMode(df_trans %>% pull(time_proportion))) %>%
    filter(time_proportion > 0 && time_proportion < 1),
    aes(date, prob)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = 'grey90', color = 'grey50') +
  geom_line() + labs(x = "Date", y = "Probability") +
  facet_grid(rows = vars(end_st), cols = vars(start_st),
    labeller = as_labeller(state_names)) +
  theme_minimal() + theme_latex +
#  scale_x_continuous(limits = c(75, 235)) +
  scale_y_continuous(limits = c(-0.001, 1.001), expand = expand_scale(add = 0))+
  theme(panel.spacing = unit(1, "lines")) +
  theme(strip.text = element_text(size = 9, vjust = 0)) +
  theme(axis.text = element_text(size = 8, color = 'black')) +
  theme(axis.text.y.left = element_text(angle = 0, hjust = .6)) +
  theme(axis.text.x.bottom = element_text(angle = 45, vjust = .7)) +
  theme(axis.title.x = element_text(size = 9, vjust = 0)) +
  theme(axis.title.y = element_text(size = 9, vjust = 0)) +
  theme(plot.margin = unit(c(.6,.75,0.2,0.2), "cm"))+
  theme(axis.ticks = element_line(color = "grey50", size = .75)) +
  theme(axis.ticks.length = unit(5, "pt"))
gg_trans_date

# Save Plot
ggsave(filename = "Trans_Probs_Date_NO_LABEL.png", plot = gg_trans_date,
  path = file.path(tex_dir, "Figures/Ch2"), scale = 1, width = 6, height = 6,
  units = "in", dpi = 300)

# Plot Transitions by Time
gg_trans_time <- ggplot(
    data = df_trans %>%
      filter(time_proportion > 0 && time_proportion < 1) %>%
      filter(julian_date == CalculateMode(df_trans %>% pull(julian_date))),
    aes(time_proportion, prob))+
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = 'grey90', color = 'grey50') +
  geom_line() +
  labs(x = "Time (Daily Proportion)", y = "Probability") +
  facet_grid(rows = vars(end_st), cols = vars(start_st),
    labeller = as_labeller(state_names)) +
  scale_x_continuous(limits = c(-0.001, 1.001), expand = expand_scale(add = 0))+
  scale_y_continuous(limits = c(-0.001, 1.001), expand = expand_scale(add = 0))+
  theme_minimal() + theme_latex +
  theme(panel.spacing = unit(1, "lines")) +
  theme(strip.text = element_text(size = 9, vjust = 0)) +
  theme(axis.title = element_text(size = 9, vjust = 0)) +
  theme(axis.text = element_text(size = 8, color = 'black')) +
  theme(axis.text.y.left = element_text(angle = 0, hjust = .6)) +
  theme(axis.text.x.bottom = element_text(angle = 45, vjust = .7)) +
  theme(plot.margin = unit(c(.6,.75,0.2,0.2), "cm")) +
  theme(axis.ticks = element_line(color = "grey50", size = .5)) +
  theme(axis.ticks.length = unit(5, "pt"))
gg_trans_time

# Save Plot
ggsave(filename = "Trans_Probs_Time_NO_LABEL.png",
  path = file.path(tex_dir, "Figures/Ch2"), scale = 1, width = 6, height = 6,
  units = "in", dpi = 300)
#theme_get()

# Composite Graph
plot_trans_date <- image_read(file.path(tex_dir, "Figures/Ch2",
  "Trans_Probs_Date_NO_LABEL.png"))
trans_prob_date_fig <- backgrd %>%
  image_composite(., plot_trans_date, offset = "+0+0") %>%
  image_composite(., tex_start, offset = "+775+35") %>%
  image_composite(., image_rotate(tex_end, 90), offset = "+1720+815")
trans_prob_date_fig

plot_trans_time <- image_read(file.path(tex_dir, "Figures/Ch2",
  "Trans_Probs_Time_NO_LABEL.png"))
trans_prob_time_fig <- backgrd %>%
  image_composite(., plot_trans_time, offset = "+0+0") %>%
  image_composite(., tex_start, offset = "+775+35") %>%
  image_composite(., image_rotate(tex_end, 90), offset = "+1720+815")
trans_prob_time_fig

# Export to Dissertation
trans_prob_time_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Trans_Prob_Time.png")
image_write(trans_prob_time_fig, path = trans_prob_time_fig_file, format=".png")
file.remove(file.path(tex_dir, "Figures/Ch2",
   "Trans_Probs_Time_NO_LABEL.png"))

trans_prob_date_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Trans_Prob_Date.png")
image_write(trans_prob_date_fig, path = trans_prob_date_fig_file, format=".png")
file.remove(file.path(tex_dir, "Figures/Ch2",
  "Trans_Probs_Date_NO_LABEL.png"))







# Transition Probabilities -----------------------------------------------------

df_trans <- readRDS(file = "Output/Analysis/Transitions/df_trans.rds") %>%
  mutate(date = as.Date(julian_date, origin=as.Date("2015-01-01")))
state_names <- c('1' = "Cruise", '2' = "Flight", '3' = "Nest", '4' = "Perch",
  '5' = "Roost")

# Tex Strings
tex_head <- tibble(
  tex_str = c("Start State", "End State"),
  tex_name = c("start", "end"))

# Tex Dataframe
tex_df <- bind_rows(
  bind_rows(tex_head) %>% mutate(title_size = 11))

# Create Tex Text Plots
for (i in seq_len(nrow(tex_df))){
  tex_str_i <- tex_df %>% slice(i) %>% pull(tex_str)
  tex_name_i <- tex_df %>% slice(i) %>% pull(tex_name)
  title_size_i <- tex_df %>% slice(i) %>% pull(title_size)
  gg_tex <- ggplot() + theme_blank + labs(title = latex2exp::TeX(tex_str_i)) +
    theme(plot.title = element_text(size = title_size_i))
  ggsave(file = "Output/Analysis/Territorial/TEMP.png", plot = gg_tex,
         width = 5, height = .75)
  tex_i <- image_trim(image_read("Output/Analysis/Territorial/TEMP.png"))
  file.remove("Output/Analysis/Territorial/TEMP.png")
  assign(paste0("tex_", tex_name_i), tex_i)
}

rm(i, tex_df, tex_name_i, tex_str_i, tex_i)

# Image background
backgrd <- image_blank(1800, 1800, color = "white")

# Metrics for figure labels

CalculateMode(df_trans %>% pull(time_proportion)) # 0.5030271
CalculateMode(df_trans %>% pull(date)) #"2015-06-02"
range(df_trans$date) #"2015-03-21" "2015-08-17"

# Plot Transitions by Date
gg_trans_date <- ggplot(
  data = df_trans %>%
    filter(
      time_proportion == CalculateMode(df_trans %>% pull(time_proportion))) %>%
    filter(time_proportion > 0 && time_proportion < 1),
    aes(date, prob)) +
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = 'grey90', color = 'grey50') +
  geom_line() + labs(x = "Date", y = "Probability") +
  facet_grid(rows = vars(end_st), cols = vars(start_st),
    labeller = as_labeller(state_names)) +
  theme_minimal() + theme_latex +
#  scale_x_continuous(limits = c(75, 235)) +
  scale_y_continuous(limits = c(-0.001, 1.001), expand = expand_scale(add = 0))+
  theme(panel.spacing = unit(1, "lines")) +
  theme(strip.text = element_text(size = 9, vjust = 0)) +
  theme(axis.text = element_text(size = 8, color = 'black')) +
  theme(axis.text.y.left = element_text(angle = 0, hjust = .6)) +
  theme(axis.text.x.bottom = element_text(angle = 45, vjust = .7)) +
  theme(axis.title.x = element_text(size = 9, vjust = 0)) +
  theme(axis.title.y = element_text(size = 9, vjust = 0)) +
  theme(plot.margin = unit(c(.6,.75,0.2,0.2), "cm"))+
  theme(axis.ticks = element_line(color = "grey50", size = .75)) +
  theme(axis.ticks.length = unit(5, "pt"))
gg_trans_date

# Save Plot
ggsave(filename = "Trans_Probs_Date_NO_LABEL.png", plot = gg_trans_date,
  path = file.path(tex_dir, "Figures/Ch2"), scale = 1, width = 6, height = 6,
  units = "in", dpi = 300)

# Plot Transitions by Time
gg_trans_time <- ggplot(
    data = df_trans %>%
      filter(time_proportion > 0 && time_proportion < 1) %>%
      filter(julian_date == CalculateMode(df_trans %>% pull(julian_date))),
    aes(time_proportion, prob))+
  geom_ribbon(aes(ymin = lci, ymax = uci), fill = 'grey90', color = 'grey50') +
  geom_line() +
  labs(x = "Time (Daily Proportion)", y = "Probability") +
  facet_grid(rows = vars(end_st), cols = vars(start_st),
    labeller = as_labeller(state_names)) +
  scale_x_continuous(limits = c(-0.001, 1.001), expand = expand_scale(add = 0))+
  scale_y_continuous(limits = c(-0.001, 1.001), expand = expand_scale(add = 0))+
  theme_minimal() + theme_latex +
  theme(panel.spacing = unit(1, "lines")) +
  theme(strip.text = element_text(size = 9, vjust = 0)) +
  theme(axis.title = element_text(size = 9, vjust = 0)) +
  theme(axis.text = element_text(size = 8, color = 'black')) +
  theme(axis.text.y.left = element_text(angle = 0, hjust = .6)) +
  theme(axis.text.x.bottom = element_text(angle = 45, vjust = .7)) +
  theme(plot.margin = unit(c(.6,.75,0.2,0.2), "cm")) +
  theme(axis.ticks = element_line(color = "grey50", size = .5)) +
  theme(axis.ticks.length = unit(5, "pt"))
gg_trans_time

# Save Plot
ggsave(filename = "Trans_Probs_Time_NO_LABEL.png",
  path = file.path(tex_dir, "Figures/Ch2"), scale = 1, width = 6, height = 6,
  units = "in", dpi = 300)
#theme_get()

# Composite Graph
plot_trans_date <- image_read(file.path(tex_dir, "Figures/Ch2",
  "Trans_Probs_Date_NO_LABEL.png"))
trans_prob_date_fig <- backgrd %>%
  image_composite(., plot_trans_date, offset = "+0+0") %>%
  image_composite(., tex_start, offset = "+775+35") %>%
  image_composite(., image_rotate(tex_end, 90), offset = "+1720+815")
trans_prob_date_fig

plot_trans_time <- image_read(file.path(tex_dir, "Figures/Ch2",
  "Trans_Probs_Time_NO_LABEL.png"))
trans_prob_time_fig <- backgrd %>%
  image_composite(., plot_trans_time, offset = "+0+0") %>%
  image_composite(., tex_start, offset = "+775+35") %>%
  image_composite(., image_rotate(tex_end, 90), offset = "+1720+815")
trans_prob_time_fig

# Export to Dissertation
trans_prob_time_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Trans_Prob_Time.png")
image_write(trans_prob_time_fig, path = trans_prob_time_fig_file, format=".png")
file.remove(file.path(tex_dir, "Figures/Ch2",
   "Trans_Probs_Time_NO_LABEL.png"))

trans_prob_date_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Trans_Prob_Date.png")
image_write(trans_prob_date_fig, path = trans_prob_date_fig_file, format=".png")
file.remove(file.path(tex_dir, "Figures/Ch2",
  "Trans_Probs_Date_NO_LABEL.png"))

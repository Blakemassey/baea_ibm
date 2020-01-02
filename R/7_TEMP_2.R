# Individual plots
library(latex2exp)
library(tmaptools)
MetersToKilometers <- function(x){
  x_out <- x/1000
  return(x_out)
}
Multiplier100 <- function(x){
  x_100 <- x*100
  x_100 <- str_pad(x_100, 5, side = "right", pad = 0)
  x_100 <- if_else(x_100 == "00000", "0", x_100)
  return(x_100)
}

palette_explorer()
line_color <- inferno(5, direction = 1)[4]

ind_list <- lapply(sort(unique(baea_movements_wb$behavior_behavior)),
    function(i){
  grp_i = str_replace_all(i, "->", "$\\\\rightarrow$")
  ggplot(baea_movements_wb[baea_movements_wb$behavior_behavior == i,],
    aes(x = step_length)) +
  geom_histogram(aes(y = ..density.., weight=weights), binwidth = 500, #binwidth = 30,
    size = .1, boundary = 0, color = "black", fill = "grey80") +
  geom_line(data = weibull_dens[weibull_dens$grp == i, ], aes(x = pred,
    y = dens), size = .8, color = line_color) +
  xlab(NULL) + ylab(NULL) + ggtitle(TeX(grp_i)) +
  theme_minimal() + theme_latex +
  scale_y_continuous(labels = Multiplier100)  +
  scale_x_continuous(labels = MetersToKilometers)  +
  theme(plot.margin = margin(0, 6, 3, 6, "pt")) +
  theme(axis.text = element_text(size = 7)) +
  theme(axis.text.x = element_text(angle = 0, vjust = .5, hjust = 0.5)) +
  theme(plot.title = element_text(size = 8, vjust = -2))
  # Old code for annotations of Weibull parameters
  #annotate("text", Inf, Inf, hjust = 1.1, vjust = 1.1, size = 5,
    #label = paste0("Weibull Distribution\n", "shape = ",
    #  signif(move_pars[move_pars$behavior_behavior == i, "weibull_shape"],
    #    3), "\n", "scale = ",
    #  signif(move_pars[move_pars$behavior_behavior == i, "weibull_scale"],
    #    3)))  +
})
#theme_get()

layout <- '
ABC#
DEXG
HIJK
LMNO
#PQ#
'

movements_step_length_plots <- wrap_plots(A = ind_list[[1]],
  B = ind_list[[2]],
  C = ind_list[[3]],
  D = ind_list[[4]],
  E = ind_list[[5]],
  X = ind_list[[6]],
  G = ind_list[[7]],
  H = ind_list[[8]],
  I = ind_list[[9]],
  J = ind_list[[10]],
  K = ind_list[[11]],
  L = ind_list[[12]],
  M = ind_list[[13]],
  N = ind_list[[14]],
  O = ind_list[[15]],
  P = ind_list[[16]],
  Q = ind_list[[17]],
  design = layout)
movements_step_length_plots

# Save Temp (No Label) File
movements_step_length_no_label_fig_file = file.path("Output/Analysis/Movements",
  "Movements_Step_Length_NO_LABEL.png")
ggsave(filename = basename(movements_step_length_no_label_fig_file),
  plot =  movements_step_length_plots,
  path = dirname(movements_step_length_no_label_fig_file), scale = 1, width = 6,
  height = 5.9, units = "in", dpi = 300)

# Create Tex Strings
tex_head <- tibble(
  tex_str = c("Probability", "Step Length (km)"),
  tex_name = c("lab_density", "lab_step_length"))
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
rm(i, tex_df, tex_name_i, tex_str_i, tex_i, tex_head)

# Image background
backgrd <- image_blank(1800, 1800, color = "white")

# Create Final Plot and Export to Dissertation
movements_step_length_fig <- image_read(movements_step_length_no_label_fig_file)
movements_step_length_labels_fig <- backgrd %>%
  image_composite(., movements_step_length_fig, offset = "+20+00") %>%
  image_composite(., image_rotate(tex_lab_density, 270), offset = "+20+700") %>%
  image_composite(., tex_lab_step_length, offset = "+800+1745")
movements_step_length_fig_file = file.path(tex_dir, "Figures/Ch2",
  "Movements_Step_Length.png")
image_write(movements_step_length_labels_fig,
  path = movements_step_length_fig_file, format=".png")
file.remove(movements_step_length_no_label_fig_file)

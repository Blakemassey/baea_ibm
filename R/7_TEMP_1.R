#------------------------- MOVEMENTS ANALYSIS ---------------------------------#
# Script for analyzing baea movement patterns, fitting parameters, and plotting
# step, turn angle, and redististribution kernels
#------------------------------------------------------------------------------#
pacman::p_load(CircStats, circular, devtools, dplyr, fitdistrplus, ggplot2,
  ggthemes, gridExtra, patchwork, latex2exp, lubridate, magick, movMF, raster,
  reshape2, scales, tidyverse, stringr, zoo)
pacman::p_load(baear, gisr, ibmr)
options(stringsAsFactors = FALSE)
suppressMessages(extrafont::loadfonts(device="win"))
theme_update(plot.title = element_text(hjust = 0.5))
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Load packages, helpers, and functions
pacman::p_load(tidyverse,  ggplot2, gridExtra, patchwork)

pkg_dir <- "C:/Users/Blake/OneDrive/Work/R/Projects/multiscale_optim"
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"

# Theme (for LaTeX font)
theme_latex <- theme(text = element_text(family = "Latin Modern Roman")) +
  theme(axis.text = element_text(size = 10)) +
  theme(axis.title = element_text(size = 12)) +
  theme(plot.title = element_text(size = 14))

theme_blank <- theme(legend.position = "none",
  text = element_text(family="Latin Modern Roman"),
  plot.title = element_text(size=14),
  panel.grid = element_blank(), axis.title = element_blank(),
  axis.text = element_blank(), axis.ticks = element_blank(),
  panel.background = element_rect(fill = "transparent", colour = NA),
  plot.background = element_rect(fill = "transparent", colour = NA))

baea_behavior_perch_perch <- readRDS("Data/BAEA/baea_behavior_perch_perch.rds")
baea_movements <- readRDS("Data/BAEA/baea_movements.rds")
baea_movements_wb <- readRDS("Data/BAEA/baea_movements_wb.rds")
baea_movements_vm <- readRDS("Data/BAEA/baea_movements_vm.rds")
move_pars <- readRDS("Output/Analysis/Movements/move_pars.rds")

################################ PLOTTING  #####################################

# ggplot(baea_behavior_perch_perch, aes(step_length)) +
#   geom_histogram(aes(y = ..density..), binwidth = 42.5, boundary = 0,
#     fill = "blue", color = "white") +
#   xlab("Step Length (m)") + ylab("Density") +
#   xlim(0, 5000) +
#   theme_latex +
#   theme(axis.text=element_text(colour="black")) +
#   theme(axis.title.x = element_text(size = 16, angle = 0, vjust = 0, hjust=0.5)) +
#   theme(axis.title.y = element_text(size = 16, angle = 90, vjust = 0.5, hjust=0.5)) +
#   theme(axis.text = element_text(size = 14, colour = "black")) +
#   theme(panel.grid = element_blank()) +
#   theme(panel.background = element_rect(fill = NA, size=2, color = "grey90")) +
#   scale_y_continuous(labels = comma) +  ggtitle("Perch -> Perch") +
#   theme(title = element_text(size = 20))
# SaveGGPlot(filename = "Perch_Perch_StepLengths.png",
#   path="Products/Graphs/Step_Length")

### ALL DATA -------------------------------------------------------------------
# Plotting (Weibull) -----------------------------------------------------------

vec_length <- 100
weibull_dens <- data.frame(grp=factor(), pred=numeric(), dens=numeric())
for (i in 1:nrow(move_pars)){
  pars_i <- move_pars[i,]
  grp = rep(pars_i$behavior_behavior, vec_length)
  pred = seq(pars_i$min_step, pars_i$max_step, length = vec_length)
  dens = dweibull(pred, shape=pars_i$weibull_shape, scale=pars_i$weibull_scale)
  weibull_dens <- rbind(weibull_dens, data.frame(grp, pred, dens))
}

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


## WHERE  Temp_2 script should go.


















# All plots on one figure

str_replace_all(weibull_dens$grp, "->", "")

pointSize = 2; textSize = 5; spaceLegend = 1
ggplot(data = baea_movements_wb %>% mutate(grp = behavior_behavior),
    aes(x = step_length)) +
  geom_histogram(aes(y = ..density.., weight = weights), binwidth = 30,
    boundary = 0) +
  geom_line(data = weibull_dens, aes(x = pred, y = dens), size = 1,
    colour = "blue") +
  xlab("Step Length (m)") + ylab("Density") +
  theme(axis.text=element_text(colour="black")) +
  scale_fill_viridis_d(option = "C", direction = -1) +
  guides(fill = guide_legend(title = "Utilization\nDistribution")) +
  ggtitle("") + guides(NA) + theme_latex + theme(legend.position = "none") +
  theme_minimal() +
  #facet_grid(rows = vars(behavior), cols = vars(behavior_next),
  #  labeller = as_labeller(vars(grp))) +
  facet_wrap(~ grp, scales = "free") +
  #  labeller = as_labeller(appender, default = label_parsed)) +
  # facet_wrap(~ grp_arrow, , scales = "free",
  #   labeller = as_labeller(appender, default = label_parsed)) +
  theme_latex +
    theme(axis.text = element_text(size = 7)) +
    theme(axis.title = element_text(size = 9)) +
    theme(plot.title = element_text(size = 11)) +
    theme(panel.grid.major.x = element_blank())

  theme(axis.title.x = element_text(angle = 0, vjust = 0, hjust=0.5)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = NA, color = "grey80")) +
  theme(strip.background = element_rect(fill="white", color=NULL),
    strip.text.x = element_text(size=10, colour="black",
      margin = margin(1,0,1,0, "pt"))) +
  geom_line(data = weibull_dens, aes(x = pred, y = dens), size = 1,
    colour = "blue") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.5)) +
  scale_y_continuous(labels = comma)


SaveGGPlot(filename = "Step Lengths with Fitted Weibull.png",
  path="Output/Plots/Step_Length")

# Individual plots
ind_list <- lapply(sort(unique(baea_movements_wb$behavior_behavior)),
    function(i){
  ggplot(baea_movements_wb[baea_movements_wb$behavior_behavior == i,],
    aes(x = step_length)) +
  geom_histogram(aes(y = ..density.., weight=weights), binwidth = 30,
    boundary = 0) +
  geom_line(data = weibull_dens[weibull_dens$grp == i, ], aes(x = pred,
    y = dens), size = 2, colour = "blue") +
  annotate("text", Inf, Inf, hjust = 1.1, vjust = 1.1, size = 5,
    label = paste0("Weibull Distribution\n", "shape = ",
      signif(move_pars[move_pars$behavior_behavior == i, "weibull_shape"],
        3), "\n", "scale = ",
      signif(move_pars[move_pars$behavior_behavior == i, "weibull_scale"],
        3)))  +
  xlab("Step Length (m)") + ylab("Density") +  ggtitle(paste(i, "(all)")) +
  theme(title = element_text(size = 16)) +
  theme(axis.text = element_text(size = 12, colour = "black")) +
  theme(axis.title.x = element_text(angle = 0, hjust=0.5)) +
  theme(axis.title.y = element_text(angle = 90, hjust=0.5)) +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = NA, color = "grey80")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  scale_y_continuous(labels = comma)  +
  theme(plot.margin = margin(15, 15, 15, 15, "pt"))

#SaveGGPlot(filename = paste(str_replace(i, ">", ""), ".png"),
#    path="Output/Plots/Step_Length/Individual")
  })

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

# Cartesian Coordinates --------------------------------------------------------
# All plots on one figure

p_list = lapply(sort(unique(baea_movements_vm$behavior_behavior)), function(i){
    ggplot(data =
      baea_movements_vm[baea_movements_vm$behavior_behavior ==i,],
      aes(x=turn_angle)) +
    geom_histogram(aes(y = ..density.., weight=weights), fill = "grey20",
      color = "black", boundary = 0, binwidth = bin_width) +
    geom_line(data = von_mises_dens[von_mises_dens$grp == i, ],
      aes(x = pred, y = dens), size = 1, colour = "red") +
    scale_x_continuous(limits = limits, labels = labels,
      breaks = breaks, minor_breaks = minor_breaks, expand = c(0,0)) +
    facet_grid(. ~ behavior_behavior) +
    theme(strip.background = element_rect(fill="white", color=NULL),
      strip.text.x = element_text(size=10, colour="black",
      margin = margin(1, 0, 1, 0, "pt"))) +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(colour="grey20", size=8, vjust=.5, angle=0,
      margin=margin(1, 1, 0, 1, "pt"))) +
    theme(axis.text.y = element_text(colour="grey20", size=8)) +
    theme(panel.grid.major = element_blank())  +
    theme(panel.grid.minor = element_blank())  +
    theme(panel.background = element_rect(fill = NA, color = "black")) +
    theme(plot.margin = margin(1, 5, 1, 5, "pt")) +
    ggtitle(NULL) +
    labs(x=NULL, y=NULL)
})
do.call(grid.arrange, c(p_list, ncol=5, nrow=4, left = "Density",
  bottom="Direction"))
SavePlot(filename = "Turn Angles with Fitted von Mises - Cartesian.png",
  path="Output/Plots/Turn_Angle")

# Individual plots
ind_list <- lapply(sort(unique(baea_movements_vm$behavior_behavior)),
  function(i){
    ggplot(baea_movements_vm[baea_movements_vm$behavior_behavior == i, ],
    aes(x=turn_angle)) +
    geom_histogram(aes(y = ..density.., weight=weights), fill = "grey20",
      color = "black", boundary = 0, binwidth = bin_width) +
    geom_line(data = von_mises_dens[von_mises_dens$grp == i, ],
      aes(x = pred, y = dens), size = 1, colour = "red") +
    xlab("Turn Angle (radians)") + ylab("Density") + ggtitle(paste(i))+
    annotate("text", Inf, Inf, hjust = 1, vjust = 1, size = 5,
      label = paste0("von Mises Distribution\n",
      "mu1 (radians) = ", signif(move_pars[move_pars$behavior_behavior == i,
        "mvm_mu1"], 3), "\n",
      "mu2 (radians) = ", signif(move_pars[move_pars$behavior_behavior == i,
        "mvm_mu2"], 3), "\n",
      "kappa1 = ", signif(move_pars[move_pars$behavior_behavior == i,
        "mvm_kappa1"], 3), "\n",
      "kappa2 = ", signif(move_pars[move_pars$behavior_behavior == i,
        "mvm_kappa2"], 3))) +
    scale_x_continuous(limits = limits, labels = labels,
      breaks = breaks, minor_breaks = minor_breaks, expand = c(0,0)) +
    theme(legend.position="none") +
    theme(title = element_text(size = 16)) +
    theme(axis.text.x = element_text(colour = "grey20", size = 12, vjust = .5,
      angle = 0, margin=margin(1, 1, 0, 1, "pt"))) +
    theme(axis.text.y = element_text(colour = "grey20", size = 12)) +
    theme(panel.grid.major = element_blank())  +
    theme(panel.grid.minor = element_blank())  +
    theme(panel.background = element_rect(fill = NA, color = "black")) +
    theme(plot.margin = margin(15, 15, 15, 15, "pt"))
  SaveGGPlot(filename = paste(str_replace(i, ">", ""), "(all).png"),
    path="Output/Plots/Turn_Angle/Individual/Cartesian")
})

# Polar Coordinates ------------------------------------------------------------
# All plots on one figure
p_list = lapply(sort(unique(baea_movements_vm$behavior_behavior)), function(i){
  ggplot(baea_movements_vm[baea_movements_vm$behavior_behavior == i, ],
      aes(x = turn_angle)) +
    geom_histogram(aes(y = ..density.., weight=weights), fill = "grey20",
      color = "black", boundary = 0, binwidth = bin_width) +
    geom_line(data = von_mises_dens[von_mises_dens$grp == i, ],
      aes(x = pred, y = dens), size = 1, colour = "red") +
    coord_polar(start = (1.5*pi), direction = -1) +
    scale_y_continuous(labels = NULL) +
    scale_x_continuous(limits = limits, labels = minor_labels,
      breaks = minor_breaks[-25], minor_breaks = minor_breaks, expand = c(0,0))+
    theme(axis.ticks = element_blank()) +
    facet_grid(. ~ behavior_behavior) +
    theme(strip.background = element_rect(fill = "white", color = NULL),
      strip.text.x = element_text(size = 10, colour = "black",
      margin = margin(1, 0, 1, 0, "pt"))) +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(colour = "grey20", size = 7))+
    theme(panel.grid.major = element_line(colour = "grey90"))  +
    theme(panel.grid.minor = element_line(colour = "grey90"))  +
    theme(panel.background = element_rect(fill = NA, color = "black")) +
    theme(plot.margin = margin(1, 1, 1, 1, "pt")) +
    ggtitle(NULL) +
    labs(x = NULL, y = NULL)
})
do.call(grid.arrange, c(p_list, ncol=5, nrow=4, left = "Density",
  bottom="Direction"))
SavePlot(filename = "Turn Angles with Fitted von Mises - Polar.png",
  path="Output/Plots/Turn_Angle")

# Individual plots
ind_list = lapply(sort(unique(baea_movements_vm$behavior_behavior)),
  function(i){
  ggplot(baea_movements_vm[baea_movements_vm$behavior_behavior == i, ],
    aes(x = turn_angle)) +
    geom_histogram(aes(y = ..density.., weight=weights), fill = "grey20",
      color = "black", boundary = 0, binwidth = bin_width) +
    geom_line(data = von_mises_dens[von_mises_dens$grp == i, ],
      aes(x = pred, y = dens), size = 1, colour = "red") +
    labs(x = "Direction", y = "Density") + ggtitle(paste(i, "(all)"))+
    coord_polar(start = (1.5*pi), direction = -1)  +
    scale_y_continuous(labels = NULL) +
    scale_x_continuous(limits = limits, labels = minor_labels,
      breaks = minor_breaks[-25], minor_breaks = minor_breaks, expand = c(0,0))+
    theme(title = element_text(size = 16)) +
    theme(axis.ticks = element_blank()) +
    theme(legend.position="none") +
    theme(axis.text.x = element_text(colour = "grey20", size = 12))+
    theme(panel.grid.major = element_line(colour = "grey90"))  +
    theme(panel.grid.minor = element_line(colour = "grey90"))  +
    theme(panel.background = element_rect(fill = NA, color = "black")) +
    theme(plot.margin = margin(15, 15, 15, 15, "pt"))
  SaveGGPlot(filename = paste(str_replace(i, ">", ""), ".png"),
    path="Output/Plots/Turn_Angle/Individual/Polar")
})

# Plotting Move Kernel -------------------------------------------------------

move_dens <- data.frame(grp=character(), x=numeric(), y=numeric(),
  dens=numeric())
for (i in 1:nrow(move_pars)){
  move_pars_i <- move_pars[i, ]
  ignore_von_mises <- ifelse(move_pars_i$behavior[1] %in% c("Cruise",
    "Flight"), FALSE, TRUE)
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
  df <- data.frame(raster::rasterToPoints(kernel_raster))
  names(df)[3] <- "dens"
  df$behavior_behavior <- move_pars_i$behavior_behavior
  move_dens <- rbind(move_dens, df)
}

# All plots on one figure
p_list = lapply(sort(unique(move_dens$behavior_behavior)), function(i){
  ggplot(move_dens[move_dens$behavior_behavior == i, ], aes(x = x, y = y)) +
    geom_raster(aes(fill = dens)) +
    coord_fixed(ratio = 1) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Oranges")) +
    theme(legend.position = "none") +
    scale_x_continuous(expand = c(0.005, 0.005)) +
    scale_y_continuous(expand = c(0.005, 0.005)) +
    facet_grid(. ~ behavior_behavior) +
    theme(strip.background = element_rect(fill="white", color=NULL),
      strip.text.x = element_text(size=10, colour="black",
      margin = margin(0,0,1,0, "pt"))) +
    theme(legend.position="none") +
    theme(axis.text = element_text(colour="grey20", size=7)) +
    theme(axis.text.x = element_text(angle=30)) +
    theme(panel.grid.major = element_blank())  +
    theme(panel.grid.minor = element_blank())  +
    theme(panel.background = element_rect(fill = NA, color = "black")) +
    theme(plot.margin = margin(1, 1, 1, 1, "pt")) +
    ggtitle(NULL) + labs(x=NULL, y=NULL)
})
do.call(grid.arrange, c(p_list, ncol=5, nrow=4, left="Y", bottom="X"))
SavePlot(filename = "Move Kernels with Fitted Distributions.png",
  path="Output/Plots/Move_Kernels")

# Individual plots
ind_list = lapply(sort(unique(move_dens$behavior_behavior)), function(i){
  ggplot(move_dens[move_dens$behavior_behavior == i,], aes(x = x, y = y)) +
    geom_raster(aes(fill = dens)) +
    labs(x = "X", y = "Y", title = paste(i)) +
    coord_fixed(ratio = 1) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "Oranges")) +
    labs(fill = "Probability") +
    scale_x_continuous(expand = c(0.005, 0.005)) +
    scale_y_continuous(expand = c(0.005, 0.005)) +
    theme(title = element_text(size = 16)) +
    theme(axis.text = element_text(colour = "grey20", size = 12)) +
    theme(axis.title.y = element_text(angle=0, vjust=0.5)) +
    theme(panel.grid.major = element_blank())  +
    theme(panel.grid.minor = element_blank())  +
    theme(panel.background = element_rect(fill = NA, color = "black")) +
    theme(plot.margin = margin(15, 15, 15, 15, "pt"))
  SaveGGPlot(filename = paste(str_replace(i, ">", ""), ".png"),
    path = "Output/Plots/Move_Kernels/Individual")
})

table(baea_movements$behavior_behavior)


#------------------------------------------------------------------------------#
################################## OLD CODE ####################################
#------------------------------------------------------------------------------#

# library(fitdistrplus)
# library(dplyr)
# library(extraDistr)
# library(texmex)
#
# movements <- baea %>% group_by("id") %>%
#   filter(step_length > 42.5) %>%
#   filter(step_time <= 20) %>%
#   mutate(step_length = step_length/1000)
#   ungroup()
#
# nests <- baea %>% group_by(id) %>% slice(1) %>%
#     dplyr::select(nest_long_utm, nest_lat_utm)  %>%
#     transmute(long = nest_long_utm, lat = nest_lat_utm)
#
#   home_dist_gg <- ConvertRasterForGGPlot(home_dist)
#
#   ggplot(home_dist_gg, aes(x, y)) +
#     geom_raster(aes(fill = value), interpolate=TRUE) +
#     coord_fixed(ratio = 1) +
#     scale_fill_distiller(breaks = seq(0,40000, by=5000), name="Meters",
#       palette = "Blues", direction=-1) +
#     geom_point(data = nests_2016, aes(long, lat), shape=24, alpha=.9,
#       color="red", fill= "black", size=2, stroke=2) +
#     geom_point(data = nests, aes(long, lat), shape=24, alpha=.9,
#       color="blue", fill= "floralwhite", size=2, stroke=2) +
#     geom_point(data = movements, aes(long_utm, lat_utm), shape=4, alpha=.9,
#       color="chartreuse2", size=1, stroke=2) +
#     geom_point(data = nests, aes(long, lat), shape=24, alpha=.9,
#       color="blue", fill= "floralwhite", size=2, stroke=2) +
#     theme_legend +
#     ggtitle(paste("Movement Locations")) + xlab("Longitude") + ylab("Latitude")
#   SaveGGPlot("Movement Locations.png", image_output, bg = "white")
#
#
#     geom_raster(aes(fill = value), interpolate=TRUE) + theme_legend +
#     scale_x_continuous(expand=c(0, 0)) + scale_y_continuous(expand=c(0, 0)) +
#     scale_fill_distiller(breaks = seq(0,40000, by=5000), name="Meters",
#       palette = "Blues", direction=-1) +
#     ggtitle(paste(i, "- Home Distance")) +
#     xlab("Longitude") + ylab("Latitude") +
#     geom_point(data = nests_2016_i, aes(long, lat), shape=24, alpha=.9,
#       color="red", fill= "black", size=2, stroke=2) +
#     geom_point(data = nest_i, aes(long, lat), shape=24, alpha=.9,
#       color="blue", fill= "white", size=2, stroke=2)
#   SaveGGPlot(paste0(i, " - Home Distance.png"),
#     file.path(image_output), bg = NA)
#
#
# descdist(movements$step_length, boot=100)
#
# fits_movements <- list(
#   exponential = fitdist(movements$step_length, "exp"),
#   halfnorm = fitdist(movements$step_length, "hnorm", start=list(sigma=
#     sqrt(pi/2))),
#   gamma = fitdist(movements$step_length, "gamma"),
#   pareto = fitdist(movements$step_length, "gpd", start=list(sigma=2, xi=2)),
#   weibull = fitdist(movements$step_length, "weibull")
# )
#
# save(fits_movements, file = "Output/fits_movements.RData")
#
# sapply(fits_movements, function(i) summary(i))
# sapply(fits_movements, function(i) coef(i))
#
# plot(fits_movements$exponential)
# plot(fits_movements$halfnorm)
# plot(fits_movements$gamma)
# plot(fits_movements$pareto)
# plot(fits_movements$weibull)
#
# ggplot(movements, aes(step_length)) + stat_ecdf(geom = "step") +
#   xlab("Step Length Distance (km)") + ylab("ECD") + theme_no_legend
#
# movements_lines <- data.frame(x=movements$step_length,
#   HalfNorm=dhnorm(movements$step_length,
#     fits_movements$halfnorm$estimate["sigma"]),
#   Exponential=dexp(movements$step_length,
#     fits_movements$exponential$estimate["rate"]),
#   Pareto=texmex::dgpd(movements$step_length,
#     fits_movements$pareto$estimate["sigma"],
#     fits_movements$pareto$estimate["xi"]),
#   Gamma=dgamma(movements$step_length,
#     fits_movements$gamma$estimate["shape"],
#     fits_movements$gamma$estimate["rate"]),
#   Weibull=stats::dweibull(movements$step_length,
#     fits_movements$weibull$estimate["shape"],
#     fits_movements$weibull$estimate["scale"]))
#
# ggplot(movements) +
#   geom_histogram(aes(x=step_length, y=..density..), color="black", fill="grey",
#     breaks = seq(0, max(movements$step_length), by=.25)) +
#   ggtitle("Step Length (km)") +
#   xlab("Kilometers") + ylab("Density") + theme_legend +
# #  geom_line(data = movements_lines, aes(x, HalfNorm, color = "HalfNorm"), size = 1.1) +
# #  geom_line(data = movements_lines, aes(x, Exponential, color = "Exponential"), size = 1.1) +
# #  geom_line(data = movements_lines, aes(x, Gamma, color = "Gamma"), size = 1.1) +
#   geom_line(data = movements_lines, aes(x, Weibull, color = "Weibull"), size = 1.1) +
# #  geom_line(data = movements_lines, aes(x, Pareto,  color="Pareto"), size = 1.1) +
#   scale_color_manual(name = "Fitted \nDistributions",
#     values = c("Exponential" = "green", "HalfNorm" = "darkorchid1",
#       "Pareto" = "red1", "Gamma" = "blue1",
#       "Weibull" = "yellow"))
#
# SaveGGPlot(paste0("Movements Distribution Fits.png"),
#   file.path(image_output), bg = "black")
#
# # Using Weibull Fit
#
# max_r <- qweibull(.995, fits_movements$weibull$estimate["shape"],
#   fits_movements$weibull$estimate["scale"]) * 1000
#
# nestcon_gamma_shape <- 1.1
# nestcon_gamma_rate <- 0.495
#
# (step_weibull_scale = fits_movements$weibull$estimate["scale"])
# (step_weibull_shape = fits_movements$weibull$estimate["shape"])
#
#
# con_nest_Sandy <- overlay(home_dist_Sandy, con_dist_nest_Sandy,
# #  fun=function(x,y){round(x+y)})
#
# plot(con_nest_Sandy, col=terrain.colors(255), main= "Sandy - ConNest Distance")
# #  legend.args=list(text="Con D", cex=1, side=3, line=1))
# points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
#
#
# loc_pts <- data.frame(
#   x = c(500015, 474995),
#   y = c(4919965, 4930015),
#   title = c("Near Edge", "Above Nest"))
# points(loc_pts$x, loc_pts$y, size=2, pch=4, lwd=2, col="black")
#
#
# CenterXYInCell(x = c(500000, 475000),y = c(4920000, 4930000), xmin(base),
#   ymin(base), 30)
#
# step_max_r = 15000
#
# cellsize = 30
# mu = 0
# rho = .5
# scale = 1.172086
# shape = 0.7081443
#
# move_kernel <- CreateMoveKernelWeibull(max_r=step_max_r, cellsize=cellsize,
#   mu=mu, rho=rho, shape=shape, scale=scale)
#
# r <- (cellsize*((nrow(move_kernel)-1)/2))+(cellsize/2)
# redist_raster <- raster::raster(move_kernel, xmn=-r, xmx=r, ymn=-r, ymx=r)
# plot(redist_raster, main= "Movement Kernel")
# SavePlot("Movement Kernel.jpeg", image_output)
#
# i <- 2
# redist_shift <- raster::shift(redist_raster, x=loc_pts$x[i],
#   y=loc_pts$y[i])
# plot(redist_shift, main= "Sandy - Movement Kernel")
# points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
# points(loc_pts$x[i], loc_pts$y[i], size=2, pch=4, lwd=2, col="black")
# SavePlot(paste0("Sandy - Movement Kernel - ", i, ".jpeg"), image_output)
#
# redist_shift <- raster::crop(redist_shift, base, snap="in")
# con_nest <- CreateConNestProb(con_nest_raster = con_nest_Sandy,
#   gamma_shape=nestcon_gamma_shape, gamma_rate=nestcon_gamma_rate,
#   x=loc_pts$x[i], y=loc_pts$y[i], max_r=step_max_r, cellsize=cellsize,
#   base=base)
# plot(con_nest, main= "Sandy - ConNest Probability")
# points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
# points(loc_pts$x[i], loc_pts$y[i], size=2, pch=4, lwd=2, col="black")
# SavePlot(paste0("Sandy - ConNest Probability - ", i, ".jpeg"), image_output)
#
# con_nest_crop <- raster::crop(con_nest, redist_shift, snap="out")
# redist_shift_crop <- raster::crop(redist_shift, con_nest, snap="out")
#
# redist_shift_crop
# con_nest_crop
#
# prob_raster <- raster::overlay(redist_shift_crop, con_nest_crop,
# #  fun=function(a,b){return(a*b)}, recycle=FALSE)
#
# prob_raster <- prob_raster/raster::cellStats(prob_raster, stat="sum")
# plot(prob_raster, main= "Sandy - Movement Kernel")
# points(nest_Sandy[1], nest_Sandy[2], pch=17, cex=1.5, col="blue")
# points(loc_pts$x[i], loc_pts$y[i], size=2, pch=4, lwd=2, col="black")
# SavePlot(paste0("Sandy - Movement Kernel - ", i, ".jpeg"), image_output)
#
# print("prob_min:", raster::minValue(prob_raster))
# raster::crs(prob_raster) <- raster::crs(sim$spatial$base)
#
# #CreateMoveKernelWeibull <- function(max_r = 300,
#                                       cellsize = 30,
#                                       mu,
#                                       rho,
#                                       shape,
#                                       scale,
#                                       ignore_cauchy = FALSE,
#                                       ignore_weibull = FALSE) {
#
# max_r <- qweibull(.99, fits_movements$weibull$estimate["shape"],
#   fits_movements$weibull$estimate["scale"]) * 1000
# #max_r <- 100
# cellsize <- 30
# mu = 0
# rho = .5
# scale = step_weibull_scale
# shape = step_weibull_shape
# ignore_cauchy = FALSE
# ignore_weibull = FALSE
#
# ptm <- proc.time()
#
#   if (is.null(max_r)) max_r <- qweibull(.99, shape, scale) * 1000
#   # Create the empty kernel objects
#   max_r_cells <- ceiling(max_r/cellsize)
#   size <- max_r_cells * 2 + 1
#   center <- max_r_cells + 1
#   angle_matrix <- row_matrix <- col_matrix <- new("matrix", 0, size, size)
#   distance_matrix <- new("matrix", 0, size, size)
#   i <- j <-  1:size
#   row_matrix[] <- rep(i, times  = max(j))
#   col_matrix <- t(row_matrix)
#   dx <- row_matrix - center
#   dy <- col_matrix - center
#   abs_angle <- atan2(dx, dy)
#   angle_matrix <- ifelse(abs_angle < 0, (2*pi) + abs_angle, abs_angle)
#   wrpc_kernel <- suppressWarnings(dwrappedcauchy(angle_matrix, mu=mu, rho=rho))
#   distance_matrix <- (sqrt((row_matrix - center)^2 + (col_matrix - center)^2) *
#       cellsize) / 1000
#   weibull_kernel <- dweibull(distance_matrix, scale=scale, shape=shape)
#   weibull_kernel[center, center] <- 0  # Forces agent to move from current cell
#   # This last part deletes the cells at the edge if they are all zero
#   if (all(wrpc_kernel[1, ] == 0, wrpc_kernel[, 1] == 0,
#     wrpc_kernel[nrow(wrpc_kernel),] == 0, wrpc_kernel[, ncol(wrpc_kernel)] ==0))
#     wrpc_kernel <- wrpc_kernel[2:(nrow(wrpc_kernel) - 1), 2:(ncol(wrpc_kernel)
#       - 1)]
#   if (all(weibull_kernel[1, ] == 0, weibull_kernel[, 1] == 0,
#     weibull_kernel[nrow(weibull_kernel),] == 0, weibull_kernel[,
#       ncol(weibull_kernel)] == 0))
#     weibull_kernel <- weibull_kernel[2:(nrow(weibull_kernel) - 1),
#       2:(ncol(weibull_kernel) - 1)]
#   # Multiply the two kernels together and re-normalize
#   if (ignore_cauchy) wrpc_kernel <- 1
#   if (ignore_weibull) weibull_kernel <- 1
#   redist_kernel <- weibull_kernel*wrpc_kernel
#   redist_kernel <- redist_kernel/sum(redist_kernel)
# #  return(redist_kernel)
#
# proc.time() - ptm
#
# ##
# move_kernel <- redist_kernel
# r <- (cellsize*((nrow(move_kernel)-1)/2))+(cellsize/2)
#
# redist_raster <- raster(move_kernel, xmn=-r, xmx=r, ymn=-r, ymx=r)
# redist_shift <- shift(redist_raster, x=50000, y=50000)
#
# plot(redist_shift)
# Plot3DRaster(redist_shift, main="Move Kernel", border=NA)
#
#
#
#   ptm <- proc.time()
#   for (i in 1:size) {
#     for (j in 1:size) {
#       r = (sqrt((i - center)^2 + (j - center)^2) * cellsize) / 1000
#       b = AngleToPoint(center, center, j, i)
#       if(r <= max_r){
# #        distance_kernel[i, j] <- r
#         wrpc_kernel[i, j] <- round(suppressWarnings(dwrappedcauchy(b,
#           mu=mu, rho=rho)), 5)
#         weibull_kernel[i, j] <- stats::dweibull(r, scale=scale, shape=shape,
#           log=FALSE)
#       }
#     }
#   }
#   proc.time() - ptm
#   wrpc_kernel <- apply(wrpc_kernel, 2, rev)
#   weibull_kernel[center, center] <- 0  # Forces agent to move from current cell
#   # This last part deletes the cells at the edge if they are all zero
#   if (all(wrpc_kernel[1, ] == 0, wrpc_kernel[, 1] == 0,
#     wrpc_kernel[nrow(wrpc_kernel),] == 0, wrpc_kernel[, ncol(wrpc_kernel)] ==0))
#     wrpc_kernel <- wrpc_kernel[2:(nrow(wrpc_kernel) - 1), 2:(ncol(wrpc_kernel)
#       - 1)]
#   if (all(weibull_kernel[1, ] == 0, weibull_kernel[, 1] == 0,
#     weibull_kernel[nrow(weibull_kernel),] == 0, weibull_kernel[,
#       ncol(weibull_kernel)] == 0))
#     weibull_kernel <- weibull_kernel[2:(nrow(weibull_kernel) - 1),
#       2:(ncol(weibull_kernel) - 1)]
#   # Multiply the two kernels together and re-normalize
#   if (ignore_cauchy)
#     wrpc_kernel <- 1
#   if (ignore_weibull)
#     weibull_kernel <- 1
#   redist_kernel <- weibull_kernel*wrpc_kernel
#   redist_kernel <- redist_kernel/sum(redist_kernel)
#   return(redist_kernel)
# }
#
#
#

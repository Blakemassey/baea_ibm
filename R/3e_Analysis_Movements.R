#------------------------- MOVEMENTS ANALYSIS ---------------------------------#
# Script for analyzing baea movement patterns, fitting parameters, and plotting
# step, turn angle, and redististribution kernels
#------------------------------------------------------------------------------#
pacman::p_load(CircStats, circular, devtools, dplyr, fitdistrplus, ggplot2,
  ggthemes, gridExtra, lubridate, movMF, raster, scales, stringr, zoo)
pacman::p_load(baear, gisr, ibmr)
options(stringsAsFactors = FALSE)
theme_update(plot.title = element_text(hjust = 0.5))
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

############################  IMPORT FILES  ####################################

## Import Baea behavior --------------------------------------------------------
baea_behavior <- readRDS(file="Data/Baea/baea_behavior.rds")

######################## ANALYZE MOVEMENTS  ####################################

baea_behavior_transitions <- baea_behavior %>%
  arrange(id, datetime) %>%
  group_by(id) %>%  #  slice(1:150) %>%
  mutate(behavior_next = lead(behavior)) %>%
  mutate(step_time2 = lead(datetime) - datetime) %>%
  filter(step_time2 <= 20)  %>%
  filter(!is.na(turn_angle)) %>%
  ungroup() %>%
  mutate(behavior_behavior = paste(behavior, "->", behavior_next))

# Create behavior transition summary matrix
baea_behavior_transitions_sum <- baea_behavior_transitions %>%
  group_by(behavior_behavior) %>%
  summarize(count = n())
baea_behavior_transitions_sum

# Extract 'Perch -> Perch' movement for Bernoulli trail in MovementSubModel
baea_behavior_perch_perch <- baea_behavior_transitions %>%
  filter(behavior_behavior == "Perch -> Perch")  %>%
  dplyr::select(id, datetime, step_time, step_time2, long_utm, lat_utm,
    speed, alt, agl, dx, dy, step_length, behavior, behavior_next,
    behavior_behavior, turn_angle)

perch_perch_move <- baea_behavior_perch_perch %>% filter(step_length >= 42.5)
perch_perch_Bern <- nrow(perch_perch_move)/nrow(baea_behavior_perch_perch)
perch_perch_Bern

# Subset data for analysis of movement parameters (distance and direction)
# Note: cruise -> roost and roost -> cruise are not included b/c of small sample
baea_movements <- baea_behavior_transitions %>%
  filter(step_length > 42.43) %>%
  filter(behavior_next != "Nest",
      behavior_behavior != "Roost -> Roost",
      behavior_behavior != "Cruise -> Roost",
      behavior_behavior != "Roost -> Cruise") %>%
  dplyr::select(id, datetime, step_time, step_time2, long_utm, lat_utm, speed,
    alt, agl, dx, dy, step_length, behavior, behavior_next, behavior_behavior,
    turn_angle)

saveRDS(baea_behavior_perch_perch, "Data/BAEA/baea_behavior_perch_perch.rds")
saveRDS(baea_movements, file = "Data/BAEA/baea_movements.rds")

# All Data parameters (with weights) -------------------------------------------

# Fitting Weibull ####
weibull_pars <- baea_movements %>%
  group_by(behavior_behavior) %>%
  summarize(
    behavior = unique(behavior),
    behavior_next = unique(behavior_next)) %>%
  mutate(count = NA, min_step = NA, max_step = NA)

weibull_weights <- baea_movements %>%
  group_by(behavior_behavior, id) %>%
  summarize(count = n()) %>%
  filter(count > 1)   %>%
  mutate(weights = 1/count) %>%
  dplyr::select(id, count, weights, behavior_behavior) %>%
  ungroup()
#View(weibull_pars)

baea_movements_wb <- baea_movements %>%
  left_join(., weibull_weights, by=c("id", "behavior_behavior")) %>%
  filter(!is.na(weights))

saveRDS(baea_movements_wb, file = "Data/BAEA/baea_movements_wb.rds")

for (i in unique(baea_movements_wb$behavior_behavior)){
  baea_movements_wb_i <- baea_movements_wb %>%
    filter(behavior_behavior == i)
  weibull_pars_i <- fitdist(baea_movements_wb_i$step_length,
    distr = "weibull", method = "mle",
    weights = round(baea_movements_wb_i$weights*10000), lower = c(.01, 1),
    upper = c(100, 10000))
  weibull_pars_row <- which(weibull_pars$behavior_behavior == i)
  weibull_pars[weibull_pars_row, "weibull_shape"] <- weibull_pars_i$estimate[1]
  weibull_pars[weibull_pars_row, "weibull_scale"] <- weibull_pars_i$estimate[2]
  baea_movements_wb_i_sum <-
    baea_movements_wb_i %>%
      summarize(
        count = n(),
        min_step = min(step_length),
        max_step = max(step_length))
  weibull_pars[weibull_pars_row, "count"] <-
    baea_movements_wb_i_sum$count[1]
  weibull_pars[weibull_pars_row, "min_step"] <-
    baea_movements_wb_i_sum$min_step[1]
  weibull_pars[weibull_pars_row, "max_step"] <-
    baea_movements_wb_i_sum$max_step[1]
  rm(baea_movements_wb_i, weibull_pars_i, weibull_pars_row)
}

# Fitting von Mises  ####

von_mises_pars <- baea_movements %>%
  left_join(., baea_movements %>% group_by(behavior_behavior) %>%
      summarize(count = n()), by = "behavior_behavior") %>%
  group_by(behavior_behavior) %>%
  filter(step_length > 0 & count > 1) %>%
  summarize(
    behavior = unique(behavior),
    behavior_next = unique(behavior_next),
    count = n()) %>%
  ungroup() %>%
  mutate(mvm_mu1 = NA,
    mvm_mu2 = NA,
    mvm_kappa1 = NA,
    mvm_kappa2 = NA,
    mvm_prop = NA)

von_mises_weights <- baea_movements %>%
  group_by(behavior_behavior, id) %>%
  summarize(count = n()) %>%
  filter(count > 1)   %>%
  mutate(weights = 1/count) %>%
  dplyr::select(id, count, weights, behavior_behavior) %>%
  ungroup()
#View(von_mises_weights)

baea_movements_vm <- baea_movements %>%
  left_join(., von_mises_weights, by=c("id", "behavior_behavior")) %>%
  filter(!is.na(weights))

for (i in unique(baea_movements$behavior_behavior)){
  baea_movements_vm_i <- baea_movements_vm %>%
    filter(behavior_behavior == i)
  mvm_pars <- mledist(baea_movements_vm_i$turn_angle, "mixedvonmises",
    weights = round(baea_movements_vm_i$weights*1000), silent=TRUE,
    start = list(mu1=pi/2, mu2=1.5*pi, kappa1=10, kappa2=10, prop=.5),
    lower=c(mu1= 0, mu2=0, kappa1=0, kappa2=0, prop=0),
    upper=c(mu1= 2*pi, mu2=2*pi, kappa1=100, kappa2=100, prop=1))
  rows_i <- which(von_mises_pars$behavior_behavior == i)
  von_mises_pars[rows_i, "mvm_mu1"] <- mvm_pars$estimate[1]
  von_mises_pars[rows_i, "mvm_mu2"] <- mvm_pars$estimate[2]
  von_mises_pars[rows_i, "mvm_kappa1"] <- mvm_pars$estimate[3]
  von_mises_pars[rows_i, "mvm_kappa2"] <- mvm_pars$estimate[4]
  von_mises_pars[rows_i, "mvm_prop"] <- mvm_pars$estimate[5]
  rm(baea_movements_vm_i, mvm_pars, rows_i)
}

move_pars <- full_join(weibull_pars, von_mises_pars, by=c("behavior",
  "behavior_next", "behavior_behavior")) %>%
  mutate(behavior_fac = as.factor(behavior),
    behavior_next_fac = as.factor(behavior_next),
    bern_p = NA) %>%
  dplyr::select(-c(count.x, count.y))

perch_to_perch <- which(move_pars[,"behavior_behavior"] == "Perch -> Perch")
move_pars[perch_to_perch, "bern_p"] <- perch_perch_Bern # needed move prob

saveRDS(baea_movements_wb, file = "Data/BAEA/baea_movements_wb.rds")
saveRDS(baea_movements_vm, file = "Data/BAEA/baea_movements_vm.rds")
write.csv(move_pars, file="Products/Tables/move_pars.csv") # for Powerpoint
saveRDS(move_pars, file="Output/Analysis/Movements/move_pars.rds")

################################ PLOTTING  #####################################


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

# All plots on one figure
ggplot(data=baea_movements_wb %>% mutate(grp = behavior_behavior),
    aes(x = step_length)) +
  geom_histogram(aes(y = ..density.., weight=weights), binwidth = 30,
    boundary = 0) +
  xlab("Step Length (m)") + ylab("Density") +
  theme(axis.text=element_text(colour="black")) +
  theme(axis.title.x = element_text(angle = 0, vjust = 0, hjust=0.5)) +
  theme(axis.title.y = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
  theme(axis.text = element_text(size = 8, colour = "black")) +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = NA, color = "grey80")) +
  facet_wrap(~grp, scales = "free") +
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
  SaveGGPlot(filename = paste(str_replace(i, ">", ""), ".png"),
    path="Output/Plots/Step_Length/Individual")
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

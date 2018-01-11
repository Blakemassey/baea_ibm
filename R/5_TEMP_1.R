mu1 <- pi/2
mu2 <- pi
kappa1 <-  10
kappa2 <- 5
p <- .25

df_test <- data.frame(turn_angle = rmixedvm(1000, mu1, mu2, kappa1, kappa2, p))
hist(df_test$turn_angle, breaks=15, xlim = c(0, 2*pi))

angle_in_degrees <- deg(df_test$turn_angle)
pts_on_unit_circle <- cbind(cos(angle_in_degrees * pi / 180),
                            sin(angle_in_degrees * pi / 180))

d <- movMF(pts_on_unit_circle, 2)
mu <- atan2(d$theta[,2], d$theta[,1])
kappa <- sqrt(rowSums(d$theta^2))
p1 <- d$alpha[1]
d
mu
kappa

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
wrp_cauchy_dens <- data.frame(grp=factor(), pred=numeric(), dens=numeric())

# Cartesian Coordinates --------------------------------------------------------
# All plots on one figure
  grp = rep("Test", vec_length)
  pred = seq(limits[1], limits[2], length = vec_length)
  dens = dmixedvm(pred, mu[1], mu[2], kappa[1], kappa[2], p1)
  wrp_cauchy_dens <- rbind(wrp_cauchy_dens, data.frame(grp, pred, dens))

ggplot(df_test, aes(x=turn_angle)) +
  geom_histogram(aes(y = ..density..), fill = "grey20", color = "black",
    boundary = 0, binwidth = bin_width) +
  geom_line(data = wrp_cauchy_dens,
    aes(x = pred, y = dens), size = 1, colour = "red") +
  xlab("Turn Angle (radians)") + ylab("Density") + ggtitle(paste("Test"))+
#  annotate("text", Inf, Inf, hjust = 1, vjust = 1, size = 5,
#    label = paste0("Wrapped Cauchy Distribution\n", "mu (radians) = ",
#    signif(redist_pars[redist_pars$behavior_behavior == i,
#      "wrp_cauchy_mu_rad"], 3), "\n", "rho = ",
#    signif(redist_pars[redist_pars$behavior_behavior == i, "wrp_cauchy_rho"],
#        3)))+
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
    path="Output/Plots/Turn_Angle/All/Cartesian")


library(BAMBI)

# illustration only - more iterations needed for convergence
fit.vmsin.20 <- fit_vmsinmix(tim8, ncomp = 3, n.iter = 20,
ncores = 1)
fit.vmsin.20[1]
fit.vmsin.20 <- fit_vmsinmix(pts_on_unit_circle, ncomp = 2, n.iter = 20,
  ncores = 1)
densityplot2d(fit.vmsin.20)
densityplot2d(fit.vmsin.20, theta = 45, phi = 45)


fit.wnorm2.15 <- fit_wnorm2mix(tim8, ncomp = 3, n.iter = 15,
ncores = 1)
fit.wnorm2.15

wrp_cauchy_pars <- baea_movements %>%
  group_by(behavior_behavior) %>%
  filter(!is.na(turn_angle)) %>%
  summarize(
    behavior = unique(behavior),
    behavior_next = unique(behavior_next),
    wrp_cauchy_mu_rad = CircStats::wrpcauchy.ml(turn_angle, 0, .5)[[1]],
    wrp_cauchy_rho = CircStats::wrpcauchy.ml(turn_angle, 0, .5)[[2]]) %>%
  mutate(wrp_cauchy_mu_deg = wrp_cauchy_mu_rad * (180/pi))  %>%
  dplyr::select(behavior, behavior_next, behavior_behavior, wrp_cauchy_mu_rad,
    wrp_cauchy_mu_deg, wrp_cauchy_rho) %>%
  ungroup()

baea_movement_test <- baea_movements %>%
  group_by(behavior_behavior) %>%
  filter(!is.na(turn_angle)) %>%
  filter(behavior_behavior == "Flight -> Nest")

%>%
  summarize(
    behavior = unique(behavior),
    behavior_next = unique(behavior_next),
    fit1 = fit_wnorm2mix(turn_angle, ncomp = 3, n.iter = 15, ncores = 1))


tim8

fit.vmsin.20 <- fit_vmcosmix(pts_on_unit_circle, ncomp = 1, n.iter = 100,
  ncores = 1)
fit.vmsin.20
summary(fit.vmsin.20)

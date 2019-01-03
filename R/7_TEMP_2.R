library(circular)
library(Circstats)
library(fitdistrplus)
library(ggplot2)
library(movMF)
options(stringsAsFactors = FALSE)

# Set arguments for random sample
n <- 300
mu1 <- 0
mu2 <- pi
kappa1 <- 15
kappa2 <- 15
prop <- .75

# Get random sample using package 'circular'
df <- data.frame(sample_dir = as.numeric(rep(NA, n)))
df$sample_dir <- as.numeric(rmixedvonmises(n = n, mu1 = circular(mu1), mu2 = circular(mu2),
  kappa1 = 10, kappa2 = kappa2, prop = 0.5))
plot(sample_dir)
rm(n, mu1, mu2, kappa1, kappa2, prop)

# Fit distribution using "fitdistrplus"
mvm_pars <- mledist(df$sample_dir, distr = "mixedvonmises",
  start = list(mu1 = pi/2, mu2 = 1.5*pi, kappa1 = 10, kappa2 = 10, prop = .5),
  silent = TRUE, lower = c(0, 0, 0, 0, 0), upper = c(2*pi, 2*pi, 100, 100, 1))

# Values used for plotting
bin_width = (2*pi)/24
breaks <- seq(0, (2*pi), by=((2*pi)/12))
labels <- c(0, "", "", expression(pi / 2), "", "", expression(pi), "", "",
  expression(1.5*pi), "", "", expression(2*pi))
minor_breaks <- seq(0, 2*pi, by=bin_width)
minor_labels <- c(0, "", "", expression(pi / 4), "", "", expression(pi / 2),
  "", "", expression(3/4*pi), "", "", expression(pi), "", "",
  expression(5/4*pi), "", "", expression(1.5*pi), "", "",
  expression(7/4*pi), "", "")
limits <- c(0, 2*pi)
vec_length <- 100
von_mises_dens <- data.frame(pred=numeric(), dens=numeric())

# Generate predicted values from fitted model using 'Circstats'
pred = seq(limits[1], limits[2], length = vec_length)
dens = CircStats::dmixedvm(pred, mu1 = mvm_pars$estimate["mu1"],
  mu2 = mvm_pars$estimate["mu2"], kappa1 = mvm_pars$estimate["kappa1"],
  kappa2 = mvm_pars$estimate["kappa2"], p = mvm_pars$estimate["prop"])
von_mises_dens <- rbind(von_mises_dens, data.frame(pred, dens))

# Polar coordinate plot using ggplot2
ggplot(df, aes(x = sample_dir)) +
  geom_histogram(aes(y = ..density..), fill = "grey20",
    color = "black", boundary = 0, binwidth = bin_width) +
  geom_line(data = von_mises_dens, aes(x = pred, y = dens), size = 1,
    colour = "red") +
  labs(x = "Direction", y = "Density") +
  ggtitle(paste("Data (black) and Fitted Distribution (red)")) +
  coord_polar(start = (1.5 * pi), direction = -1)  +
  scale_y_continuous(labels = NULL) +
  scale_x_continuous( limits = limits, labels = minor_labels,
    breaks = minor_breaks[-25], minor_breaks = minor_breaks, expand = c(0, 0)) +
  theme(title = element_text(size = 16)) +
  theme(axis.ticks = element_blank()) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(colour = "grey20", size = 12)) +
  theme(panel.grid.major = element_line(colour = "grey90"))  +
  theme(panel.grid.minor = element_line(colour = "grey90"))  +
  theme(panel.background = element_rect(fill = NA, color = "black")) +
  theme(plot.margin = margin(15, 15, 15, 15, "pt"))

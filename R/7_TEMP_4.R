

# Fit an RSF, then calculate log-RSS to visualize results.
# Load packages
library(amt)
library(ggplot2)
# Load data
data("amt_fisher")
data("amt_fisher_lu")
# Prepare data for RSF

rsf_data <- amt_fisher %>%
filter(burst_ == 1) %>%
make_track(x_, y_, t_) %>%
random_points() %>%
extract_covariates(amt_fisher_lu) %>%
mutate(lu = factor(landuse_study_area))
# Fit RSF
m1 <- rsf_data %>%
fit_rsf(case_ ~ lu)
# Calculate log-RSS
# data.frame of x1s
x1 <- data.frame(lu = sort(unique(rsf_data$lu)))
# data.frame of x2 (note factor levels should be same as model data)
x2 <- data.frame(lu = factor(21, levels = levels(rsf_data$lu)))
# Calculate
logRSS <- log_rss(object = m1, x1 = x1, x2 = x2)
# Plot
ggplot(logRSS$df, aes(x = lu_x1, y = log_rss)) +
geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
geom_point() +
xlab(expression("Land Use Category " * (x[1]))) +
ylab("log-RSS") +
ggtitle(expression("log-RSS" * (x[1] * ", " * x[2]))) +
theme_bw()
# Fit an SSF, then calculate log-RSS to visualize results.
#Prepare data for SSF
ssf_data <- deer %>%
steps_by_burst() %>%
random_steps(n = 15) %>%
extract_covariates(sh_forest) %>%
mutate(forest = factor(sh.forest, levels = 1:2,
labels = c("forest", "non-forest")),
cos_ta = cos(ta_),
log_sl = log(sl_))
# Fit an SSF (note model = TRUE necessary for predict() to work)
m2 <- ssf_data %>%
fit_clogit(case_ ~ forest + strata(step_id_), model = TRUE)
# Calculate log-RSS
# data.frame of x1s
x1 <- data.frame(forest = factor(c("forest", "non-forest")))
# data.frame of x2
x2 <- data.frame(forest = factor("forest", levels = levels(ssf_data$forest)))

# Calculate
logRSS <- log_rss(object = m2, x1 = x1, x2 = x2)
# Plot

ggplot(logRSS$df, aes(x = forest_x1, y = log_rss)) +
geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
geom_point(size = 3) +
xlab(expression("Forest Cover " * (x[1]))) +
ylab("log-RSS") +
ggtitle(expression("log-RSS" * (x[1] * ", " * x[2]))) +
theme_bw()



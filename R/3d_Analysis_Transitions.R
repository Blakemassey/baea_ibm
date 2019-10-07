## BAEA Data -------------------------------------------------------------------

suppressPackageStartupMessages(library(CircStats))
suppressPackageStartupMessages(library(circular))
suppressPackageStartupMessages(library(fitdistrplus))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(momentuHMM))
suppressPackageStartupMessages(library(padr))
theme_update(plot.title = element_text(hjust = 0.5))

library(baear)
library(gisr)
library(ibmr)

baea_behavior_org <- readRDS(file="Data/Baea/baea_behavior.rds")

baea_behavior <- baea_behavior_org %>%
#  filter(id == "Norway" | id == "Ellis") %>%
  group_by(id) %>%
#  slice(1:2000) %>%
  ungroup() %>%
  as.data.frame(.) %>%
  mutate(
    julian_date = yday(datetime),
    behavior = as.factor(behavior),
    behavior_num = as.numeric(behavior),
    ID = as.factor(id)) %>%
  dplyr::select(id, ID, datetime, behavior, behavior_num, julian_date, long_utm,
    lat_utm,
    step_length, step_time, speed, alt, agl, turn_angle, time_proportion)

# Cruise = 1, Flight = 2, Nest = 3, Perch = 4, Roost = 5

PlotLocationSunriseSunset(df=baea_behavior %>% as.data.frame %>%
  filter(id == "Norway"),
  by = "id", color_factor = "behavior",
  individual = "", start = "", end = "", breaks = "3 days", tz = "Etc/GMT+5",
  addsolartimes = TRUE, wrap = TRUE)

# Fitting Weibull  ####

weibull_pars <- baea_behavior %>%
  group_by(behavior) %>%
  summarize() %>%
  mutate(count = NA, min_step = NA, max_step = NA)

weibull_weights <- baea_behavior %>%
  group_by(behavior, id) %>%
  summarize(count = n()) %>%
  filter(count > 1)   %>%
  mutate(weights = 1/count) %>%
  dplyr::select(id, count, weights, behavior) %>%
  ungroup()

baea_behavior_weibull <- baea_behavior %>%
  left_join(., weibull_weights, by=c("id", "behavior")) %>%
  filter(!is.na(weights))

for (i in unique(baea_behavior_weibull$behavior)){
  baea_behavior_weibull_i <- baea_behavior_weibull %>%
    filter(behavior == i)
  weibull_pars_i <- fitdist(baea_behavior_weibull_i$step_length + 1, # no zeroes
    distr = "weibull", method = "mle",
    weights = round(baea_behavior_weibull_i$weights*10000), lower = c(0, 0))
  weibull_pars_row <- which(weibull_pars$behavior == i)
  weibull_pars[weibull_pars_row, "weibull_shape"] <- weibull_pars_i$estimate[1]
  weibull_pars[weibull_pars_row, "weibull_scale"] <- weibull_pars_i$estimate[2]
  baea_behavior_weibull_i_sum <-
    baea_behavior_weibull_i %>%
      summarize(count = n(),
        min_step = max(step_length),
        max_step = min(step_length))
  weibull_pars[weibull_pars_row, "count"] <-
    baea_behavior_weibull_i_sum$count[1]
  weibull_pars[weibull_pars_row, "min_step"] <-
    baea_behavior_weibull_i_sum$min_step[1]
  weibull_pars[weibull_pars_row, "max_step"] <-
    baea_behavior_weibull_i_sum$max_step[1]
  rm(baea_behavior_weibull_i, baea_behavior_weibull_i_sum,
    weibull_pars_i, weibull_pars_row)
}

rm(weibull_weights, baea_behavior_weibull)

# Fitting von Mises  ####

von_mises_pars <- baea_behavior %>%
  left_join(., baea_behavior %>% group_by(behavior) %>%
    summarize(count = n()), by = "behavior") %>%
  group_by(behavior) %>%
  filter(step_length > 0 & count > 1) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  mutate(vm_mu = NA,
    vm_kappa = NA)

von_mises_weights <- baea_behavior %>%
  group_by(behavior, id) %>%
  summarize(count = n()) %>%
  filter(count > 1)   %>%
  mutate(weights = 1/count) %>%
  dplyr::select(id, count, weights, behavior) %>%
  ungroup()
#View(von_mises_weights)

baea_behavior_von_mises <- baea_behavior %>%
  left_join(., von_mises_weights, by=c("id", "behavior")) %>%
  filter(!is.na(weights))

for (i in unique(baea_behavior$behavior)){
  baea_behavior_von_mises_i <- baea_behavior_von_mises %>%
    filter(behavior == i) %>% filter(!is.na(turn_angle))
  vm_pars <- mledist(baea_behavior_von_mises_i$turn_angle, "vm",
    weights = round(baea_behavior_von_mises_i$weights*10000), silent = TRUE,
    start = list(mu = 0.1, kappa = 10), lower = c(mu = 0, kappa = 0),
    upper=c(mu = 2*pi, kappa = 100))
  rows_i <- which(von_mises_pars$behavior == i)
  von_mises_pars[rows_i, "vm_mu"] <- vm_pars$estimate[1]
  von_mises_pars[rows_i, "vm_kappa"] <- vm_pars$estimate[2]
  rm(baea_behavior_von_mises_i, vm_pars, rows_i)
}

redist_pars <- full_join(weibull_pars, von_mises_pars, by=c("behavior"))

RemoveExcept(c("baea_behavior", "redist_pars"))

baea_behavior_prep <- prepData(baea_behavior %>% dplyr::select(-c(id,
  turn_angle)), type="UTM", coordNames = c("long_utm", "lat_utm"),
  covNames = c("julian_date", "time_proportion", "nest_dist"))

if (min(baea_behavior_prep$step, na.rm = TRUE) == 0){
  baea_behavior_prep$step <- baea_behavior_prep$step + 1
}

# Start HMM Model Fitting ------------------------------------------------------
n_states <- 5
state_names <- as.character(levels(baea_behavior$behavior))
#Cruise = 1, Flight = 2, Nest = 3, Perch = 4, Roost = 5
step_dist <- "weibull"
angle_dist <- "vm"
step_par <- c(redist_pars$weibull_shape, redist_pars$weibull_scale)
angle_par <- c(redist_pars$vm_kappa)
formula <- ~cosinor(julian_date, period = 365) + cosinor(time_proportion,
  period = 1)

library(msm)
statetable.msm(behavior, ID, data=baea_behavior_prep)

beta_0 <- rbind(c(0.25, 0.25, 0.25, 0.00,  # Cruise to Roost not allowed
                  0.20, 0.20, 0.20, 0.20,
                  0.20, 0.20, 0.20, 0.20,
                  0.20, 0.20, 0.20, 0.20,
                  0.00, 0.25, 0.25, 0.25)) # Roost to Cruise not allowed

# Initial HMM Model Fit --------------------------------------------------------

# No formula = to get starting estimates for Par0 & beta0
baea_hmm_start <- fitHMM(
  data = baea_behavior_prep,
  nbStates = n_states,
  dist = list(step=step_dist, angle=angle_dist),
  Par0 = list(step=step_par, angle=angle_par),
  beta0 = beta_0,
  knownStates = as.numeric(baea_behavior_prep$behavior),
  stateNames = state_names,
  estAngleMean = list(angle=FALSE))

saveRDS(baea_hmm_start, file = "Data/Models/baea_hmm_start")

Par0_baea_hmm_start <- getPar0(baea_hmm_start, formula = formula)

# Fitting with starting values
library(tictoc)
tic()
baea_hmm_full <- fitHMM(
  data = baea_behavior_prep,
  nbStates = n_states,
  dist = list(step=step_dist, angle=angle_dist),
  Par0 = Par0_baea_hmm_start$Par,
  beta0 = Par0_baea_hmm_start$beta,
  formula = formula,
  knownStates = as.numeric(baea_behavior_prep$behavior),
  stateNames = state_names,
  estAngleMean = list(angle=FALSE),
  verbose = 2)
toc()

saveRDS(baea_hmm_full, file = "Data/Models/baea_hmm_full")
baea_hmm_full <- readRDS(file = "Data/Models/baea_hmm_full.rds")

plot(baea_hmm_full, plotCI = TRUE)
names(baea_hmm_full)

# Transition Matrix Probabilities ----
library(broom)
library(stringr)
betas_full <- CIbeta(baea_hmm_full)
beta_est <- betas_full$beta

#Cruise = 1, Flight = 2, Nest = 3, Perch = 4, Roost = 5

beta_est2 <- beta_est %>% as.data.frame(.) %>%
  rename_all(funs(str_replace_all(., "[\\.\\.\\.\\.]", " "))) %>%
  rename_all(funs(str_replace_all(., "    ", "->"))) %>%
  rename_all(funs(str_replace_all(., "1", "Cruise"))) %>%
  rename_all(funs(str_replace_all(., "2", "Flight"))) %>%
  rename_all(funs(str_replace_all(., "3", "Nest"))) %>%
  rename_all(funs(str_replace_all(., "4", "Perch"))) %>%
  rename_all(funs(str_replace_all(., "5", "Roost"))) %>%
  rename_all(funs(str_replace_all(., "est ", "")))

colnames(beta_est2)

rownames(beta_est2) <- rownames(beta_est2) %>%
  str_replace_all(c("\\(Intercept\\)" = "Intercept", "cosinor" = "")) %>%
  str_replace_all(c(", period = 365" = "", ", period = 1" = ""))

beta_est


(nbStates <- length(baea_hmm_full$stateNames))
(dist <- baea_hmm_full$conditions$dist)
(distnames <- names(dist))
(userBounds <- baea_hmm_full$conditions$bounds)
(stateNames <- baea_hmm_full$stateNames)
(estAngleMean <- baea_hmm_full$conditions$estAngleMean)
(circularAngleMean <- baea_hmm_full$conditions$circularAngleMean)
(DM <- baea_hmm_full$conditions$DM)
(cons <- baea_hmm_full$conditions$cons)
(workcons <- baea_hmm_full$conditions$workcons)
(zeroInflation <- baea_hmm_full$conditions$zeroInflation)
(oneInflation <- baea_hmm_full$conditions$oneInflation)
(formula <- baea_hmm_full$conditions$formula)
(formulaDelta <- baea_hmm_full$condition$formulaDelta)
(Par <- baea_hmm_full$mle[distnames])
(parindex <- c(0, cumsum(unlist(lapply(baea_hmm_full$conditions$fullDM,
  ncol)))[-length(baea_hmm_full$conditions$fullDM)]))
(names(parindex) <- distnames)


library(shiny)
library(cosinor)
cosinor_analyzer(vitamind)

saveRDS(baea_hmm_full, file = "Data/Models/baea_hmm_full")
baea_hmm_full <- readRDS(file = "Data/Models/baea_hmm_full")


baea_hmm_full <- readRDS(file = "Data/Models/baea_hmm_full.rds")
plot(baea_hmm_full)



#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

hist(baea_behavior_prep$step)

stepPar0 <- c(5, 2000, 5, 500) # (mu_1,mu_2,sd_1,sd_2)
# initial angle distribution natural scale parameters
anglePar0 <- c(0,0,1,8) # (mean_1,mean_2,concentration_1,concentration_2)

baea_hmm2 <- fitHMM(
  data=baea_behavior_prep,
  nbStates=2,
  dist = list(step = "gamma", angle = "vm"),
  Par0=list(step=c(5, 2000, 5, 500), angle=c(1, 8)),
  # knownStates = as.numeric(baea_behavior_prep$behavior),
  stateNames = c("stationary", "exploratory"),
  estAngleMean = list(angle=FALSE))


plot(baea_hmm2)

baea_hmm2

baea_hmm <- fitHMM(
  data=baea_behavior_prep,
  nbStates=n_states,
  dist=list(step=step_dist, angle=angle_dist),
  Par0=list(step=step_par, angle=angle_par),
 # knownStates = as.numeric(baea_behavior_prep$behavior),
  stateNames = state_names,
  retryFits = 5)

plot(baea_hmm, plotCI=TRUE)

rename_all(mtcars, toupper)

fit_weibull <- function(x)
{
    xbar <- mean(x)
    varx <- var(x)
    f <- function(b){return(gamma(1+2/b)/gamma(1+1/b)^2 - 1 - varx/xbar^2)}
    bhat <- uniroot(f,c(0.02,50))$root
    ahat <- xbar/gamma(1+1/bhat)
    return(c(ahat,bhat))
}
# using method of moments (see function at top of script)
  # weibull_pars_i <- fit_weibull(baea_behavior_weibull_i$step_length)
  # weibull_pars_row <- which(weibull_pars$behavior == i)
  # weibull_pars[weibull_pars_row, "weibull_shape"] <- weibull_pars_i[1]
  # weibull_pars[weibull_pars_row, "weibull_scale"] <- weibull_pars_i[2]

# WORKED
baea_hmm5 <- fitHMM(
  data = baea_behavior_prep,
  nbStates = n_states,
  dist = list(step=step_dist, angle=angle_dist),
  Par0 = list(step=step_par, angle=angle_par),
  beta0 = beta_0,
  knownStates = as.numeric(baea_behavior_prep$behavior),
  stateNames = state_names,
  estAngleMean = list(angle=FALSE))
# WORKED

saveRDS(baea_hmm5, file = "Data/Models/baea_hmm5")
baea_hmm5 <- readRDS("Data/Models/baea_hmm5")

library(momentuHMM)

baea_hmm5
plot(baea_hmm5)
P <- momentuHMM:::allProbs(baea_hmm5,nbStates=5)
P


# Testing Exponential Plotting
library(ggplot2)
x = seq(-3,2, by = .01)
y2 = 2^x
ye = exp(1)^x
y4 = 4^x
y10 = 10^x

df <- data.frame(x,y2,ye,y4)
ggplot(df) +
  geom_line(aes(x,y2), color = "red") +
  geom_line(aes(x,ye), color = "blue") +
  geom_line(aes(x,y4), color = "green") +
  geom_line(aes(x,y10), color = "yellow") +
  coord_fixed(ratio = 1, xlim = c(-3,2), ylim = c(-1, 4))

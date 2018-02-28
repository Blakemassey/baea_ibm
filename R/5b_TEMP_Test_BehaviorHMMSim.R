suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(maptools))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(varhandle))

library(baear)
library(gisr)
library(ibmr)
devtools::reload("C:/Work/R/Packages/ibmr")
xy_coords <- matrix(c(-67.7778, 44.8012), nrow = 1) # Bangor, ME
time_step_period <- as.period(15, "mins")

baea_hmm_full <- readRDS(file = "Data/Models/baea_hmm_full2")
beta <- baea_hmm_full$mle$beta

# Create dataframe for simulated data ----
start_date <- as.POSIXct("2017-07-20", tz="America/New_York") #3-20
end_date <- as.POSIXct("2017-07-25", tz="America/New_York")  #9-20
dates <- seq(start_date, end_date, by="days")
steps_all <- as.numeric()
for (i in 1:length(dates)){
  date_i <- dates[i] #, tz="America/New_York")
  step_interval_end  <- maptools::sunriset(xy_coords, date_i,
      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunset",
      POSIXct.out= TRUE)[1,2]
  step_interval_start  <- maptools::sunriset(xy_coords, date_i,
      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
      POSIXct.out= TRUE)[1,2]
  end_int <- lubridate::int_end(lubridate::as.interval(time_step_period,
    step_interval_start))
  steps <- lubridate::with_tz(append(step_interval_start, end_int),
    lubridate::tz("America/New_York"))
  while (end_int < step_interval_end) {
    end_int <- end_int + time_step_period
    steps <- lubridate::with_tz(append(steps, end_int),
      lubridate::tz("America/New_York"))
  }
  if (tail(steps, 1) > step_interval_end) {
    steps[length(steps)] <- step_interval_end
  }
  print(i)
  steps_all <- append(steps_all, steps)
}

library(dplyr)
library(tidyr)

df_sim <- as.data.frame(steps_all) %>%
  mutate(hr_before_sunrise =  maptools::sunriset(xy_coords, steps_all,
      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
      POSIXct.out= TRUE)[,2])


hr_before_sunrise <- maptools::sunriset(xy_coords, steps_all,
      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
      POSIXct.out= TRUE)[1,2]


df_sim <- data.frame(id = NA, date_time = steps_all, time_proportion = NA,
  julian = yday(steps_all), behavior = NA)



AddTimeStepProportion(df = df, by = "id", time_step = "15 min",
  tz = "Etc/GMT+5")


n_days <- interval(start_date, end_date) %/% days(1) + 1
time_seq = round(rep(seq(0, 1, by = .02)), 3)
day <- rep(dates, each = length(time_seq))
time_proportion <- rep(time_seq, times= n_days)
df_sim <- data.frame(id = NA, day = day, time_proportion = time_proportion,
  julian = yday(day), behavior = NA)


df_sim[1, "behavior"] <- 4
prob_cols <- vector()
for (i in 1:5) {
  for (j in 1:5) {
    df_sim[, ncol(df_sim) + 1] <- NA
    colnames(df_sim )[ncol(df_sim)] <- paste(i, "->", j)
  }
}

pseudo_ids <- 1:5

for (y in 1:length(pseudo_ids)){
  df <- df_sim
  df$id <- paste0("ID_", str_pad(pseudo_ids[y], 3, pad = "0"))
  for (k in 1:nrow(df)) {
    (gamma <- diag(5))
    g <- beta[1, ]  #  g = state transition probabilities intercepts
    g <- g +
      beta[2, ] * cos(2*pi * (df[k, "julian"]/365)) +
      beta[3, ] * sin(2*pi * (df[k, "julian"]/365)) +
      beta[4, ] * cos(2*pi * (df[k, "time_proportion"]/1)) +
      beta[5, ] * sin(2*pi * (df[k, "time_proportion"]/1))
    exp(g)
    gamma[!gamma] <- exp(g) # insert Beta values into (non-diagonal locations) in transition matrix
    gamma
    gamma2 <- t(gamma) # transpose matrix -> probabilities for state transitions are now in rows
    gamma2
    gamma3 <- gamma2/apply(gamma2, 1, sum) # rows sum to 1
    gamma3
    for(i in 0:4){
      for(j in 1:5){
        df[k, (5 + (i*5) + (j))] <- gamma3[i + 1, j]
      }
    }
    if (k > 1){
      df[k, "behavior"] <- sample(1:5, size = 1,
        prob = gamma3[df[k-1, "behavior"], ])  # trans prob. given behavior at k-1
    }
  }
  ifelse(!exists("df_final"), df_final <- df, df_final <- rbind(df_final, df))
}
df_sim <- df_final

table(df_sim$behavior)/sum(table(df_sim$behavior))
table(baea_hmm_full$data$behavior)/sum(table(baea_hmm_full$data$behavior))

PlotLocationSunriseSunset(
  df = df_sim,
  by = "id", color_factor = "behavior",
  individual = "ID_001", start = "", end = "", breaks = "10 days",
  tz = "Etc/GMT+5", addsolartimes = FALSE, wrap = TRUE)


df2 <- df %>% as.data.frame(.) %>%
  rename_all(funs(str_replace_all(., "1", "Cruise"))) %>%
  rename_all(funs(str_replace_all(., "2", "Flight"))) %>%
  rename_all(funs(str_replace_all(., "3", "Nest"))) %>%
  rename_all(funs(str_replace_all(., "4", "Perch"))) %>%
  rename_all(funs(str_replace_all(., "5", "Roost"))) %>%
  rename_all(funs(str_replace_all(., " -> ", "_")))

# PLOT OF TRANSITIONS OVER THE COURSE OF THE SEASON -------------------------
df3 <- df2 %>%
  filter(time_prop %in% c(0.02, .5, .98)) %>%
  mutate(time_prop = as.factor(time_prop)) %>%
  dplyr::select(-behavior)

df4 <- df3 %>% # select(-julian) %>%
  gather(key = "State_Trans", value = "Prob", -c(time_prop, julian))

ggplot(data = df4) +
  geom_line(mapping = aes(x = julian, y = Prob, color = time_prop)) +
  facet_wrap(~ State_Trans)

# PLOT OF TRANSITIONS OVER THE COURSE OF A DAY ---------------------------
df3 <- df2 %>%
  filter(julian %in% c(90, 150, 210)) %>%
  mutate(julian = as.factor(julian)) %>%
  dplyr::select(-behavior)

df4 <- df3 %>%
  gather(key = "State_Trans", value = "Prob", -c(time_prop, julian))

ggplot(data = df4) +
  geom_line(mapping = aes(x = time_prop, y = Prob, color = julian)) +
  facet_wrap(~ State_Trans)

plot(baea_hmm_full, plotCI = TRUE)


# For comparison ----

baea_behavior <- readRDS(file="Data/Baea/baea_behavior.rds") %>%
  mutate(sex = "female")
PlotBehaviorProportionBar(baea_behavior, breaks = 50, title = "Actual Data")

mean_locations <- baea_behavior %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(id, date) %>%
  summarize(mean_locs = mean(n()))

df_behavior <- df %>%
  mutate(sex = "female") %>%
  mutate(time_proportion = time_prop) %>%
  mutate(behavior = as.factor(behavior))

levels(df_behavior$behavior) <- c("Cruise", "Flight", "Nest", "Perch", "Roost")
df_behavior$behavior <- unfactor(df_behavior$behavior)
PlotBehaviorProportionBar(df_behavior, breaks = 50, title = "Simulated Data")

# Cruise = 1, Flight = 2, Nest = 3, Perch = 4, Roost = 5


id_colors_org <- CreateColorsByAny(by="id", baea_behavior_days)
sex_colors_org <- CreateColorsByAny(by="sex", baea_behavior_days)

PlotColorPie(id_colors_org)
id_colors <- id_colors_org[!is.na(names(id_colors_org))]
PlotColorPie(id_colors)

baea_behavior_days <- baea_behavior %>%
  mutate(julian = yday(datetime))%>%
  group_by(id, julian) %>%
  summarize(locs = n())

ggplot(baea_behavior_days) +
  geom_bar(aes(x=julian, fill = id), position = "stack") +
  scale_fill_manual(values=id_colors)

ggplot(baea_behavior_days) +
  geom_bar(aes(x=julian, fill = id), position = "stack") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(breaks= pretty_breaks()) +
  theme(panel.background = element_rect(fill = NA, colour = NA))

id_colors






length(unique(baea_behavior_days$id))

RColorBrewer::display.brewer.all()

s <- ggplot(mpg, aes(fl, fill = drv))
s + geom_bar(position = "stack")

# Table of duration and start/end dates of birds' location data
baea_dates <- baea_behavior %>%
  group_by(id) %>%
  summarize(start_date = first(as.Date(datetime)), last_date = last(as.Date(datetime)),
    locs = n()) %>%
  mutate(date_period = as.period(interval(start_date, last_date),
    unit="months")) %>%
  ungroup() %>% as.data.frame()
baea_dates

PlotLocationSunriseSunset(
  df=baea_behavior %>% as.data.frame() %>% filter(id == "Three"),
  by = "id", color_factor = "behavior",
  individual = "", start = "2015-06-05", end = "2015-09-21", breaks = "10 days",
  tz = "Etc/GMT+5", addsolartimes = FALSE, wrap = TRUE)

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

# moveHMM's way of creating betas
#nbObs <- 100
#nbStates <- 5
#nbCovs <- 3
#beta2 <- matrix(rnorm(nbStates * (nbStates - 1) * (nbCovs + 1)),
#  nrow = nbCovs + 1)
#dim(beta2)

subCovs <- data.frame(cov1 = rnorm(nbObs))
for (j in 2:nbCovs) {
  c <- data.frame(rnorm(nbObs))
  colnames(c) <- paste("cov", j, sep = "")
  subCovs <- cbind(subCovs, c)
}
subCovs

delta <- rep(1, nbStates)/nbStates # initial starting probabilities ALL EQUAL
Z <- rep(NA, nbObs)  # states sequence
Z[1] <- sample(1:nbStates, size = 1, prob = delta)

# Regular covariates
for (k in 2:nbObs) {
  (gamma <- diag(nbStates))
  (g <- beta[1, ])  #  g = state transition probabilities intercepts
  if (nbCovs == 1)
    g <- g + beta[2, ] * subCovs[k, 1] # beta[2,] = beta for covariate1;  subCovs[k, 1] = covariate1 from data
  if (nbCovs > 1) {
    for (j in 1:nbCovs){
      g <- g + beta[j + 1, ] * subCovs[k, j] # beta[j + 1, ] = beta for covariate 'j' ; subCovs[k, j] = covariate 'j' for observation k
    }
  }
  exp(g)
  gamma[!gamma] <- exp(g) # insert Beta values into (non-diagonal locations) in transition matrix
  gamma
  gamma2 <- t(gamma) # transpose matrix -> probabilities for state transitions are now in rows
  gamma2
  gamma3 <- gamma2/apply(gamma2, 1, sum) # rows sum to 1
  gamma3
  (Z[k] <- sample(1:nbStates, size = 1, prob = gamma3[Z[k-1], ])) # probability of transition given state at [k-1]
}

ggplot(mpg, aes(hwy, cty)) +
 geom_point(aes(color = cyl))

#Cruise = 1, Flight = 2, Nest = 3, Perch = 4, Roost = 5

for (i in 0:4) {
  for (j in 1:5) {
    col <- 3 + (i*5) + (j)
    g_row <- i + 1
    g_col <- j
    print(paste0("col=", col, "; g_row=", g_row, "g_col=", g_col))
  }
}






# Cosinor fitting

fit <- cosinor.lm(Y ~ time(time) + X, data = vitamind)
fit

col <- 3

colnames(beta_est2)[col]
betas <- beta_est2[,col]

hour <- c(seq(0, 1, by=.01))
day <- 180

Y <- betas[1] + betas[2] * cos(2*pi * hour/1) + betas[3] * sin(2*pi * hour/1) +
  betas[4] * cos(2*pi * day/365) + betas[5] * sin(2*pi * day/365)

plot(hour, Y)

ggplot.cosinor.lm(fit, "X")


beta_est2

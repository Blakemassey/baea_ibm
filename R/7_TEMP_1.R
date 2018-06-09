library(broom)
library(stringr)
library(momentuHMM)

beta <- readRDS(file = "Data/Models/behavior_betas.rds")
View(beta)

step_data <- readRDS(file="Data/Simulation/sim_step_data1.rds")
step_data["behavior", 1] <- 3
View(step_data)

step <- step_data[1, "datetime"]

step_row <- which(step_data$datetime == step)
current_behavior <- as.numeric(step_data[step_row, "behavior"])
current_time_prop <- as.numeric(step_data[step_row, "time_proportion"])

gamma <- diag(5)
g <- beta[1, ]  #  g = state transition probabilities intercepts

g <- g +
  beta[2, ] * cos(2*pi * (step_data[step_row, "julian"]/365)) +
  beta[3, ] * sin(2*pi * (step_data[step_row, "julian"]/365)) +
  beta[4, ] * cos(2*pi * (step_data[step_row, "time_proportion"])) +
  beta[5, ] * sin(2*pi * (step_data[step_row, "time_proportion"]))
exp(g)

gamma[!gamma] <- exp(g) # Beta values in non-diagonal locations in matrix
   # At this point: FROM is column, TO is row
gamma2 <- t(gamma) # probabilities for state transitions are now in rows
   # At this point: FROM is row, TO is column
gamma3 <- gamma2/apply(gamma2, 1, sum) # rows sum to 1


if(current_time_prop <= .5 & current_behavior != 5){
  gamma3[, 5] <- 0
  gamma3 <- gamma3/apply(gamma3, 1, sum)
  gamma3[, 5] <- 0
}
if(current_time_prop > .5 & current_behavior == 5){
  gamma3[, 1:4] <- 0
  gamma3[, 5] <- 1
  #gamma3 <- gamma3/apply(gamma3, 1, sum)
}
if(current_behavior == 1){ # prevents 1->5 (Cruise to Roost)
  gamma3[, 5] <- 0
  gamma3 <- gamma3/apply(gamma3, 1, sum)
  gamma3[, 5] <- 0
}
if(current_behavior == 5){ # prevents 5->1 (Roost to Cruise)
  gamma3[, 1] <- 0
  gamma3 <- gamma3/apply(gamma3, 1, sum)
  gamma3[, 1] <- 0
}


baea_hmm_full <- readRDS(file = "Data/Models/baea_hmm_full.rds")

plot(baea_hmm_full, plotCI = TRUE)
names(baea_hmm_full)

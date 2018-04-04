# BAEA Data
library(msm)
library(dplyr)
library(gdata)

baea_behavior <- baea_behavior %>%
  group_by(id) %>%
  mutate(min_datetime = min(datetime))

baea_behavior$behavior_f <- as.numeric(as.factor(baea_behavior$behavior))
head(baea_behavior$behavior_f)

statetable.msm(behavior, id, data=baea_behavior)

Q <- rbind (c(0.25, 0.25, 0.25, 0.25, 0.00),
            c(0.20, 0.20, 0.20, 0.20, 0.20),
            c(0.20, 0.20, 0.20, 0.20, 0.20),
            c(0.20, 0.20, 0.20, 0.20, 0.20),
            c(0.00, 0.25, 0.25, 0.25, 0.25))

Q.crude <- crudeinits.msm(behavior_f ~ datetime, subject=id,
  data=baea_behavior, qmatrix=Q)

baea_data_sub <- baea_behavior %>%
  filter(id == "Ellis" || id == "Norway") %>%
  group_by(id) %>% slice(1:1000) %>% ungroup()

baea_behavior_msm <- msm(behavior_f ~ datetime, subject=id,
  data=baea_data_sub, qmatrix=Q, covariates = ~ time_proportion,
  control=list(fnscale=10000000, maxit=1000, trace=1, REPORT=1))

print(baea_behavior_msm)
baea_behavior_msm
qmatrix.msm(baea_behavior_msm)
Q.crude

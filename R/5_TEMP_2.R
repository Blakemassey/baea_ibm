


xy_coords <- matrix(c(-71, -67.75, -71, -67.75,-67.7778, 47.5, 47.5, 43, 43, 44.8012), nrow = 5)

xy_sppt <- sp::SpatialPoints(xy_coords, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

sunrise <- maptools::sunriset(xy_sppt, as.POSIXct("2018-07-20", tz = "America/New_York"),
  direction ="sunrise", POSIXct.out= TRUE)
sunrise

[1,2]

sunset <- maptools::sunriset(xy_coords, interval_end_East,
  proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunset",
  POSIXct.out= TRUE)[1,2]


hels <- matrix(c(24.97, 60.17), nrow=1)
Hels <- SpatialPoints(hels, proj4string=CRS("+proj=longlat +datum=WGS84"))
xy_coords <- matrix(c(44.8012, 68.7778), nrow = 1)

xy_coords <- matrix(c(-67.7778, 44.8012), nrow = 1)


date_x <- as.POSIXct("2018-02-16", tz="America/New_York")

sunriset(xy_coords, date_x,
      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
      POSIXct.out= TRUE)

sunriset(xy_coords, date_x,
      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunset",
      POSIXct.out= TRUE)


## Astronomical dawn
crepuscule(hels, d041224, solarDep=18, direction="dawn", POSIXct.out=TRUE)


Hels_seq <- seq(from=d041224, length.out=365, by="days")
up <- sunriset(xy_coords, Hels_seq, direction="sunrise", POSIXct.out=TRUE)

  step_interval_end  <- maptools::sunriset(xy_coords, as.POSIXct("2004-12-24", tz="EET"),
      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunset",
      POSIXct.out= TRUE)[1,2]



install.packages("msm")
library(msm)
cav[1:21,]
statetable.msm(state, PTNUM, data=cav)

Q <- rbind (c(0,    0.5,    0,   0.5),
            c(0.25,0.25,  0.25,  0.25),
            c(0,    0.5,    0,   0.5),
            c(0,      0,    0,    0))

Q.crude <- crudeinits.msm(state ~ years, PTNUM, data=cav, qmatrix=Q)
Q.crude

cav.msm <- msm( state ~ years, subject=PTNUM, data = cav,
  qmatrix = Q, deathexact = 4)

cav.msm

cavsex.msm <- msm( state ~ years, subject=PTNUM, data = cav,
  qmatrix = Q, deathexact = 4, covariates = ~ sex)

cavsex.msm







# BAEA Data
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

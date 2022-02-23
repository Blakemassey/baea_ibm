
move_org_shift <- raster::shift(move_org, dx = step_data$x[i],
  dy = step_data$y[i])



rands1 <- sampleRast(prob_raster_reclass, 1000)
rands2 <- sampleRast(prob_raster_reclass, 1000, adjArea=FALSE)
rands3 <- sampleRast(prob_raster_reclass, 1000, prob=FALSE)
rands4 <- sampleRast(prob_raster_reclass, 1000, adjArea=FALSE, prob=FALSE)
par(mfrow=c(1, 1))
plot(prob_raster_reclass, main='adjArea = TRUE & prob = TRUE', colNA = "blue")
points(rands1, pch='.')
plot(prob_raster_reclass, main='adjArea = FALSE & prob = TRUE')
points(rands2, pch='.')
plot(prob_raster_reclass, main='adjArea = TRUE & prob = FALSE')
points(rands3, pch='.')
plot(prob_raster_reclass, main='adjArea = FALSE & prob = FALSE')
points(rands4, pch='.')


random.raster(
  r = prob_raster_reclass,
n.layers = 1,
x = seq(1, 10),
min = 0,
max = 1,
mean = 0,
sd = 1,
p = 0.5,
s = 1.5,
distribution = c("random", "normal", "seq", "binomial", "gaussian")
)



prob_sample <- sampling::strata(data = data.frame(
  cell = rep(1:10, 1000)), stratanames = NULL,
  size = 1000, method = "systematic",
  pik = seq(.05, .95, by = .1))

hist(prob_sample$Prob)

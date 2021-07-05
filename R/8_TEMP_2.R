library(sf)
library(windfarmGA)

Polygon1 <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(cbind(
    c(4651704, 4651704, 4654475, 4654475, 4651704),
    c(2692925, 2694746, 2694746, 2692925, 2692925)))),
  crs = 3035
))
plot(Polygon1, col = "blue", axes = TRUE)

wind_df <- data.frame(ws = 12, wd = 0)
windrosePlot <- plot_windrose(data = wind_df, spd = wind_df$ws,
                             dir = wind_df$wd, dirres = 10, spdmax = 20)
Rotor <- 20
fcrR <- 9

source("R/6d_Experiment_Fix_WindfarmGA.R", echo=TRUE)

Grid <- grid_area_fixed(shape = Polygon1, size = (Rotor*fcrR), prop = 1, plotGrid = TRUE)

result <- genetic_algorithm_fixed(Polygon1 = sp_polygon,
                            n = 20,
                            Rotor = Rotor, fcrR = fcrR,
                            iteration = 50,
                            vdirspe = wind_df,
                            referenceHeight = 50, RotorHeight = 100)

# The following function will execute all plotting function further below:
plot_windfarmGA_fixed(result, Polygon1, whichPl = "all", best = 1, plotEn = 1)


filter_col <- "EnergyOverall"
listind <- 2
title <- "Energy"
energy_order <- unlist(lapply(result[, listind], function(x) x[, filter_col][[1]]))
energy_order <- order(energy_order, decreasing = FALSE)
result <- result[, listind][energy_order]
ledup <- length(result)
rectid <- lapply(result, function(x) x[, "Rect_ID"])
rectidt <- !duplicated(rectid)
result <- result[rectidt]
best = 1
result <- result[(length(result) - best + 1):(length(result))]
best_result <- data.frame(result[[1]])

best_results_sf <- st_as_sf(best_result, coords = c("X", "Y"), crs = 3035)
mapview::mapview(best_results_sf)

plot_leaflet(result, Polygon1, which = 1, orderitems = TRUE)

# The plotting functions can also be called individually:
plot_result(result, Polygon1, best = 1, plotEn = 1, topographie = FALSE)
plot_evolution(result, ask = TRUE, spar = 0.1)
plot_parkfitness(result, spar = 0.1)
plot_fitness_evolution(result)
plot_cloud(result, pl = TRUE)
plot_heatmap(result = result, si = 5)
plot_leaflet(result = result, Polygon1 = Polygon1, which = 1)

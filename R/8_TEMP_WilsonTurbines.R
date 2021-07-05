library(sf)
library(windfarmGA)
library(weathermetrics)
library(clifro)

wind_wilson_n_union <- wind_wilson_n_union
plot(wind_wilson_n_union, col = "blue", axes = TRUE)

# Create windspeed data for Greenville
greenville_wind_org <- read_csv("Data/Wind/Greenville_WindRose.csv")

greenville_wind <- greenville_wind_org %>%
  mutate(Direction = if_else(Direction == "355-004", "0-0", Direction)) %>%
  separate(Direction, c("dir_min", "dir_max"), "-") %>%
  mutate(dir_min = as.integer(dir_min)) %>%
  mutate(dir_max = as.integer(dir_max)) %>%
  mutate(wd = ((dir_min+dir_max)/2)+.5) %>%
  mutate(wd = if_else(wd == .5, 0, wd)) %>% # wd "355-004" should be 0
  dplyr::select(-c(dir_min, dir_max)) %>%
  rename('0' = Calm,
         '3.5' = "2.0  4.9",
         '6' = "5.0  6.9",
         '8.5' = "7.0  9.9",
         '12.5' = "10.0 14.9",
         '17.5' = "15.0 19.9",
         '21' = "20.0+") %>%
  pivot_longer(!wd, names_to = "ws") %>%
  mutate(wd = as.numeric(wd)) %>%
  mutate(ws = as.numeric(ws)) %>%
  replace_na(., list(value = 0)) %>%
  rename(probab = value) %>%
  group_by(wd) %>%
  mutate(ws = convert_wind_speed(ws, old_metric = "mph", new_metric = "mps"))%>%
  dplyr::select(ws, wd, probab) %>%
  ungroup()

wind_df = data.frame(wind_speeds = c(rweibull(80, 2, 4), rweibull(20, 3, 9)),
                     wind_dirs = c(rnorm(80, 135, 55), rnorm(20, 315, 35)) %% 360,
                     station = rep(rep(c("Station A", "Station B"), 2),
                                   rep(c(40, 10), each = 2)))

# Plot a simple windrose using all the defaults, ignoring any facet variable
data <- greenville_wind

x_location <- pi # x location of the labels
dirres <- 10

# Get the percentage
  T_data <- data %>%
    dplyr::group_by(wd) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(y = count/sum(count))

  labels <- data.frame(x = x_location,
                       y = scales::extended_breaks()(range(T_data$y)))
  # Create figure
  p.windrose <- ggplot() +
    geom_bar(data = data,
             aes(x = wd, y = (..count..)/sum(..count..),
                 fill = ws))+
    geom_text(data = labels,
              aes(x=x, y=y, label = scales::percent(y, 1))) +
    scale_y_continuous(breaks = waiver(),labels=NULL)+
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    ylab("")+xlab("")+
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)",
                      values = viridis(36),
                      drop = FALSE)+
    theme_bw(base_size = 12, base_family = "Helvetica") +
    theme(axis.ticks.y = element_blank(), # Disables default y-axis
          axis.text.y = element_blank())
  p.windrose

greenville_wind_formatted <- windata_format(greenville_wind)[[1]]
plot_windrose(data = greenville_wind)

windrosePlot <- plot_windrose(data = greenville_wind, spd = greenville_wind$ws,
   dir = greenville_wind$wd, dirres = 10, spdmax = 25)

Rotor <- 10
fcrR <- 9
Grid <- grid_area(shape = wind_wilson_n_union, size = (Rotor*fcrR), prop = 1, plotGrid = TRUE)

result <- genetic_algorithm(Polygon1 = wind_wilson_n_union,
                            n = 20,
                            Rotor = Rotor, fcrR = fcrR,
                            iteration = 50,
                            vdirspe = wind_df,
                            referenceHeight = 50, RotorHeight = 100)
result[1, ]

result_which <- result[, "bestPaEn"][[which]]

xysp <- st_as_sf(data.frame(result), coords = c("X",
        "Y"))


# The following function will execute all plotting function further below:
plot_windfarmGA(result, wind_wilson_n_union, whichPl = "all", best = 1, plotEn = 1)

# The plotting functions can also be called individually:
plot_result(result, wind_wilson_n_union, best = 1, plotEn = 1, topographie = FALSE)
plot_evolution(result, ask = TRUE, spar = 0.1)
plot_parkfitness(result, spar = 0.1)
plot_fitness_evolution(result)
plot_cloud(result, pl = TRUE)
plot_heatmap(result = result, si = 5)
plot_leaflet(result = result, Polygon1 = wind_wilson_n_union, which = 1)

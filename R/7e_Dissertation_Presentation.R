#---------------------- DISSERTATION PRESENTATION -----------------------------#
# This script is for testing and archiving interactive figures and maps for my
# dissertation presentation
#------------------------------------------------------------------------------#

library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras2)

baea_behavior <- readRDS("Data/BAEA/baea_behavior.rds")
i <- "Ellis"; j <- 2015

baea_points <- baea_behavior %>%
  filter(id == i) %>%
  arrange(datetime) %>%
  filter(year == j) %>%
  st_as_sf(., coords = c("long", "lat"), crs = 4326, remove = FALSE)  %>%
  dplyr::select(id, long, lat, behavior, year, first, date, datetime) %>%
  slice(1:500)

baea_xy <- baea_points %>%
  st_drop_geometry(.) %>%
  mutate(next_long = lead(long)) %>%
  mutate(next_lat = lead(lat)) %>%
  mutate(datetime = lead(datetime)) %>%
  dplyr::select(long, lat, next_long, next_lat, datetime) %>%
  na.omit()
head(baea_xy)
tail(baea_xy)

make_line <- function(long, lat, next_long, next_lat) {
    st_linestring(matrix(c(long, next_long, lat, next_lat), 2, 2))
}

baea_lines <- baea_xy %>%
  dplyr::select(long, lat, next_long, next_lat) %>%
  pmap(make_line) %>%
  st_as_sfc(crs = 4326) %>%
  {cbind(baea_xy, geom = .)} %>%
  st_sf()

baea_sf <- bind_rows(baea_points, baea_lines)

mapview::mapview(baea_sf)

# Behavior colors (for Ch2/3, GetColors(5, "muted", gray = TRUE))
behavior_colors <- c("#44AA99", "#332288", "#DDCC77", "#117733", "#AA4499")
behavior_names <-  c("Cruise", "Flight", "Nest", "Perch", "Roost")

pal <- colorFactor(behavior_colors, domain = behavior_names)

leaflet(data = baea_points) %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addTimeslider(data = baea_sf,
    radius = 4,
    stroke = TRUE,
    color = "black",
    weight = 1,
    opacity = 1,
    fill = TRUE,
    fillColor = ~pal(behavior),
    fillOpacity = 1,
    options = timesliderOptions(
      alwaysShowDate = TRUE,
      position = "topright",
      timeAttribute = "datetime",
      range = TRUE)) %>%
  setView(-70.68667, 44.66776, 12)

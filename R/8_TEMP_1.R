
## Not run:
library(leaflet)
library(leaflet.extras2)
library(sf)
library(geojsonsf)

data <- sf::st_as_sf(leaflet::atlStorms2005[1,])
data <- st_cast(data, "POINT")
data$time = as.POSIXct(
  seq.POSIXt(Sys.time() - 1000, Sys.time(), length.out = nrow(data)))

leaflet() %>%
  addTiles() %>%
  addTimeslider(data = data,
             options = timesliderOptions(
               position = "topright",
               timeAttribute = "time",
               range = TRUE)) %>%
  setView(-70, 44, 10)


df <- baea_k

 df_split <- df %>%
    as.data.frame(.) %>%
    group_by(id) %>%
    mutate(datetime_lead = lead(datetime, 1)) %>%
    mutate(step_time2 = as.integer(difftime(datetime_lead, datetime,
      units = "mins"))) %>%
    mutate(step_time_prev = lag(step_time2, 1, default = 0)) %>%
    mutate(split_time = if_else(step_time_prev > 20, 1, 0)) %>%
    mutate(split_time_grp = cumsum(split_time)) %>%
    group_by(split_time_grp) %>%
    mutate(behavior_lead = lead(behavior, 1)) %>%
    mutate(start_flight = if_else(behavior_lead == "Flight" &
        behavior != "Flight", 1, 0)) %>%
    mutate(behavior_lag = lag(behavior, 1)) %>%
    mutate(end_flight = if_else(behavior_lag == "Flight" &
        behavior != "Flight", 1, 0))  %>%
    mutate(bh_flight_tf = if_else(behavior == "Flight", TRUE, FALSE),
      bh_flight_seq = (sequence(rle(bh_flight_tf)$lengths) * bh_flight_tf)) %>%
    ungroup() %>%
    dplyr::select(-c(datetime_lead, step_time2, split_time, split_time_grp,
      step_time_prev, behavior_lead, behavior_lag, bh_flight_tf))
  df_path <- df_split %>%
    group_by(id) %>%
    filter(start_flight != 0 | end_flight != 0 | bh_flight_seq != 0) %>%
    mutate(datetime_lead = lead(datetime, 1)) %>%
    mutate(step_time2 = as.integer(difftime(datetime_lead, datetime,
      units = "mins"))) %>%
    mutate(step_time_prev = lag(step_time2, 1, default = 0)) %>%
    mutate(split_time = if_else(step_time_prev > 20, 1, 0)) %>%
    mutate(split_time_grp = cumsum(split_time)) %>%
    group_by(id, split_time_grp) %>%
    mutate(first_loc = ifelse(row_number()==1, 1, 0)) %>%
    ungroup(.) %>%
    dplyr::select(-c(datetime_lead, step_time2, split_time, step_time_prev))
  df_dup <- df_path %>%
    filter(start_flight == 1 & end_flight == 1) %>%
    mutate(first_loc = 1)
  df_out <- rbind(df_path, df_dup) %>%
    arrange(id, datetime, first) %>%
    group_by(id) %>%
    mutate(path_seg = cumsum(first_loc)) %>%
    ungroup(.) %>%
    dplyr::select(-c(start_flight, end_flight, first_loc))
  return(df_out)

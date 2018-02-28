df <- baea_roost
at_roost_distance_threshold = 50
depart_timediff_max = 1000
arrive_timediff_max = 1000

library(dplyr)

  df <- df
  df_departure <- df %>% filter(datetime < solarnoon)
  df_arrival <- df %>% filter(datetime > solarnoon)
  depart <- df_departure %>% dplyr::select(date, datetime, id, bh_nest, dist_first) %>%
    group_by(date, id) %>%
    mutate(away_from_roost =
      if_else(dist_first > at_roost_distance_threshold | bh_nest == "Nest",
        1, 0)) %>%
    mutate(depart_roost = away_from_roost == 1 &
      !duplicated(away_from_roost == 1)) %>%
    mutate(lead_depart = lead(depart_roost, 1)) %>%
    mutate(roost = ifelse(lead_depart == 1, "Depart", NA)) %>%
    mutate(pos_depart = which(roost == "Depart")[1]) %>%
    mutate(pos_all = 1) %>%
    mutate(pos_cumsum = cumsum(pos_all)) %>%
    mutate(roost = ifelse(pos_cumsum < pos_depart, "Roost", roost)) %>%
    ungroup() %>%
    dplyr::select(id, datetime, roost, bh_nest) %>%
    mutate(roost_depart = TRUE)
  arrive <- df_arrival %>% dplyr::select(date, datetime, id, bh_nest, dist_last) %>%
    group_by(date, id) %>%
    arrange(desc(datetime)) %>%
    mutate(away_from_roost =
        if_else(dist_last > at_roost_distance_threshold | bh_nest == "Nest", 1, 0)) %>%
    mutate(arrive_roost = away_from_roost == 1 &
      !duplicated(away_from_roost == 1)) %>%
    mutate(lead_arrive = lead(arrive_roost, 1)) %>%
    mutate(roost = ifelse(lead_arrive == 1, "Arrive", NA)) %>%
    mutate(pos_arrive = which(roost == "Arrive")[1]) %>%
    mutate(pos_all = 1) %>%
    mutate(pos_cumsum = cumsum(pos_all)) %>%
    mutate(roost = ifelse(pos_cumsum < pos_arrive, "Roost", roost)) %>%
    arrange(id, datetime) %>%
    ungroup() %>%
    dplyr::select(id, datetime, roost, bh_nest) %>%
    mutate(roost_arrive = TRUE)
  df_roost <- full_join(arrive, depart, by = c("id", "datetime")) %>%
    arrange(id, datetime) %>%
    mutate(bh_roost = coalesce(roost.x, roost.y)) %>%
    dplyr::select(id, datetime, bh_roost)
  df_final <- left_join(df, df_roost, by = c("id", "datetime"))
  return(df_final)

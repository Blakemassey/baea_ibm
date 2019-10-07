## Create "baea_steps" from baea_movements -------------------------------------

baea <- readRDS(file="Data/BAEA/baea.rds")
baea_movements <- readRDS(file="Data/BAEA/baea_movements.rds")

baea_locs <- baea %>%
  dplyr::select(date, datetime, id, long_utm, lat_utm, dx:step_time) %>%
  arrange(id, datetime) %>%
  group_by(id) %>%
  mutate(exp_angle = lag(abs_angle)) %>%
  mutate(long_utm_end = lead(long_utm)) %>%
  mutate(lat_utm_end = lead(lat_utm)) %>%
  ungroup() %>%
  dplyr::select(id, datetime, exp_angle, long_utm_end, lat_utm_end)

baea_steps <- left_join(baea_movements, baea_locs, by = c("id", "datetime")) %>%
  mutate(x = long_utm, y = lat_utm) %>%
  CenterXYWithBase(., base) %>%
  mutate(long_utm = x, lat_utm = y) %>%
  dplyr::select(id, datetime, behavior_behavior, long_utm, lat_utm, exp_angle,
    long_utm_end, lat_utm_end)

saveRDS(baea_steps, file = "Data/BAEA/baea_steps.rds")
rm(baea, baea_steps, baea_locs)

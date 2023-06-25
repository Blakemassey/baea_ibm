# Load Packages
pacman::p_load(tidyr, dplyr, lubridate)

# Load Data

baea_org <- readRDS(file="Data/BAEA/baea.rds")

baea <- baea_org %>%
  rename(birdid = bird_ID) %>%
  rename(bird_id = birdid) %>%
  select(bird_id, sex, gps_serial = serial,
    datetime, long, lat, alt, speed, fix, hdop,
    vdop) %>%
  arrange(bird_id, datetime)

# The original script that imported the telemetry data, used the
# gisr::CompileDownloads function and set the datetime is set to "Etc/GMT+5"
# For MOVEBANK, this translates to a timezone offset of UTC-5

write.csv(baea, "Output/Movebank/Bald_Eagle_GPS_Telemetry_Maine.csv")

sort(unique(baea$bird_ID))

# Get first and last deployment

gps_first <- baea %>%
  group_by(bird_id) %>%
  select(bird_id, sex, datetime) %>%
  mutate(first_utc = with_tz(datetime, "UTC")) %>%
  rename(first_datetime = datetime) %>%
  slice(1) %>%
  ungroup(.)

gps_last <- baea %>%
  group_by(bird_id) %>%
  select(bird_id, datetime) %>%
  mutate(last_utc = with_tz(datetime, "UTC")) %>%
  rename(last_datetime = datetime) %>%
  slice(n()) %>%
  ungroup()

gps_first_last <- first %>%
  left_join(., last)


str_to_title("HABITAT USE AND INDIVIDUAL-BASED MODELING OF BALD EAGLES IN MAINE NEAR CURRENT AND POTENTIAL WIND ENERGY FACILITIES")

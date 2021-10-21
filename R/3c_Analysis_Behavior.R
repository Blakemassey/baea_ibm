#-------------------------- Analysis Behaviors --------------------------------#
# This script models the behavior transitions of GPS location data.
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------

# Load packages
pacman::p_load(devtools, dplyr, fitdistrplus, ggplot2, ggthemes, lubridate,
  raster, zoo)
theme_update(plot.title = element_text(hjust = 0.5))
pacman::p_load(baear, gisr, ibmr)
if(FALSE) devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/gisr")

# Variables
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# Identify Nest, Perch, Flight, Cruise, and Roost Behavior ---------------------

# Import baea, nests, and base
baea_hr <- readRDS("Data/BAEA/baea_homerange.rds")

# Table of duration and start/end dates of birds' location data
baea_dates <- baea_hr %>%
  group_by(id) %>%
  summarize(start_date = first(date), last_date = last(date), locs = n()) %>%
  mutate(date_period = as.period(interval(start_date, last_date),
    unit="months")) %>%
  ungroup() %>% as.data.frame()
baea_dates

# Filter location by criteria and add behaviors
baea_nest <- FilterByNestCriteria(baea_hr, min_daily_nest_dist = 250,
  seasons = c("spring","summer"))
baea_nest <- AddNestBehavior(baea_nest, distance_threshold = 75)
baea_roost <- FilterByRoostCriteria(baea_nest,
  overnight_distance_threshold = 100, number_pm_loc = 7, number_am_loc = 7)
baea_roost <- AddRoostBehavior(baea_roost, at_roost_distance_threshold = 50,
  depart_timediff_max = 1000, arrive_timediff_max = 1000)
baea_flight <- AddFlightBehavior(baea_roost, min_speed = 5, min_step_length =50,
  max_step_time = 30, threshold_agl = 100) %>% filter(!is.na(elev))
baea_cruise <- AddCruiseBehavior(baea_flight, min_agl = 200, min_speed = 5,
  threshold_agl = 200)
baea_perch <- baea_cruise %>%
  mutate(behavior = coalesce(bh_nest, bh_roost, bh_cruise, bh_flight)) %>%
  mutate(behavior = ifelse(behavior == "Arrive" | behavior == "Depart", "Roost",
      behavior)) %>%
  mutate(behavior = ifelse(is.na(behavior), "Perch", behavior)) # %>%
baea_behavior <- baea_perch
baea_behavior_simple <- baea_behavior %>%
  dplyr::select(datetime, id, bh_nest:behavior)

baea_flights <- CreateFlightPathSegments(baea_behavior) # %>% filter(id == "")

table(baea_behavior$id)
table(baea_behavior$behavior)
baea_behavior %>% group_by(id) %>% summarize(sex = first(sex))
saveRDS(baea_behavior, file = "Data/Baea/baea_behavior.rds")

# Export kml of flight segments
if(FALSE){
  ExportKMLTelemetryBAEA(baea_flights, behavior= "behavior",
    point_color = "behavior", file = "BAEA - Flights.kml")
}

# Table behaviors' consecutive lengths
table(data.frame(unclass(rle(baea_behavior$bh_nest))) %>%
    filter(values == "Nest"))
table(data.frame(unclass(rle(baea_behavior$bh_roost))) %>%
    filter(values == "Roost"))
table(data.frame(unclass(rle(baea_behavior$bh_cruise))) %>%
    filter(values == "Cruise"))
table(data.frame(unclass(rle(baea_behavior$bh_flight))) %>%
    filter(values == "Flight"))

PlotBehaviorProportionBar(baea_behavior, title = "")
SaveGGPlot("Products/Graphs/Behavior/Proportion_Bar.png", bg = "transparent")

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#
#
# baea_extract <- baea_flights %>% filter(id == "Musquash") %>%
#   filter(date == "2016-04-14") %>%
#   dplyr::select(date:id, alt, step_time, step_length, agl, bh_nest:bh_cruise,
#     behavior)
#
# baea_behavior_sum <- baea_behavior %>%
#   group_by(behavior) %>%
#   summarize(
#     n = n(),
#     min_agl = min(agl, na.rm = TRUE),
#     median_agl = median(agl, na.rm = TRUE),
#     max_agl = max(agl, na.rm = TRUE),
#     IQR_agl = IQR(agl, na.rm = TRUE),
#     IQR_agl = IQR(agl, na.rm = TRUE),
#     min_speed = min(speed, na.rm = TRUE),
#     median_speed = median(speed, na.rm = TRUE),
#     max_speed = max(speed, na.rm = TRUE))
#
# baea_high_agl <- baea_behavior %>%
#   filter(behavior == "Perch" | behavior == "Roost") %>%
#   filter(agl >= 100)  #%>%
#   dplyr::select(date:id, alt, step_time, step_length, agl, speed,
#   bh_nest:bh_cruise, behavior)
#
#
# baea_high_agl2 <- baea_high_agl %>%
#     plyr::mutate(
#       bh_flight = ifelse(agl >= 100, "Flight", bh_flight))
#
# ExportKMLTelemetryBAEA(baea_high_agl, behavior= "behavior", path = FALSE,
#   point_color = "behavior", file = "BAEA - High AGL.kml")
#
# %>%
#   dplyr::select(id, datetime, speed, agl, behavior, step_time, step_length,
#   first, last)
#
# AddFlightPathSegments <- function(df){
#   df <- as.data.frame(df)
#   df2 <- df %>%
#     mutate(behavior_lead = lead(behavior, 1),
#       behavior_lag = lag(behavior, 1)) %>%
#     filter(behavior == "Flight" | behavior_lead == "Flight" |
#         behavior_lag == "Flight" ) %>%
#     mutate(bh_flight_tf = ifelse(!is.na(bh_flight), TRUE, FALSE),
#       bh_flight_seq = (sequence(rle(bh_flight_tf)$lengths) * bh_flight_tf),
#       bh_flight_lead = lead(bh_flight_seq, 1),
#       bh_flight_lag = lag(bh_flight_seq, 1)) %>%
#     mutate(roll_sum = RcppRoll::roll_sum(bh_flight_seq, 2, align = "right",
#       fill = NA))    %>%
#     mutate(path_end = ifelse(roll_sum == 0, 1, 0)) %>%
#     mutate(path_end = ifelse(is.na(path_end), 0, path_end)) %>%
#     group_by(id) %>%
#     mutate(path_seg = cumsum(path_end)) %>%
#     ungroup() %>%
#     mutate(path_seg = path_seg + 1) %>%
#     dplyr::select(id, datetime, path_seg)
#   df_out <- left_join(df, df2)
#   return(df_out)
# }
#
# by_colors <- CreateColorsByAny(by="id", df=baea, output=TRUE)
# study_years <- c(2013:2017)
#
# ################### PLOT 3-DAY RUNNING MEAN NEST DISTANCE ################## #
#
# # Plot 3-day running mean nest distance for all eagles
# ggplot(data = baea_terr_sum) +
#   annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-03-20"))),
#     xmax = c(as.POSIXct(paste0(study_years, "-06-21"))),
#     ymin = 0, ymax = Inf, alpha = 0.2, fill = "green") +
#   annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-06-21"))),
#     xmax = c(as.POSIXct(paste0(study_years, "-09-23"))),
#     ymin = 0, ymax = Inf, alpha = 0.2, fill = "yellow") +
#   annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-09-23"))),
#     xmax = c(as.POSIXct(paste0(study_years, "-12-21"))),
#     ymin = 0, ymax = Inf, alpha = 0.2, fill = "darkgoldenrod3") +
#   annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-12-21"))),
#     xmax = c(as.POSIXct(paste0(study_years+1, "-03-20"))),
#     ymin = 0, ymax = Inf, alpha = 0.2, fill = "ivory1") +
#   geom_line(aes(x=as.POSIXct(date), y=nest_dist_run_3dy/1000, color=id),
#     size=.5) +
#   scale_colour_manual(values = by_colors) +
#   scale_y_continuous(expand = c(0, 0, 0.025, 0)) +
#   geom_hline(aes(yintercept = 5), color = "red") +
#   coord_cartesian(xlim = c(min(as.POSIXct(baea_terr_sum$date)),
#     max(as.POSIXct(baea_terr_sum$date)))) +
#   scale_x_datetime(breaks=scales::date_breaks("3 month"), labels =
#     scales::date_format("%b %Y"), expand = c(0.02, 0.02, 0, 0)) +
#   labs(x = "Date", y = paste0("Daily Mean Nest Distance (km)"),
#       title = "All Territorial Data") +
#   theme(text = element_text(color="black")) +
#   theme(plot.title = element_text(size=18, face="bold", vjust=1.5, hjust=0.5)) +
#   theme(axis.title = element_text(size=14, face="bold")) +
#   theme(axis.text.x = element_text(size=12, angle=50, vjust=1, hjust=1)) +
#   theme(axis.text.y = element_text(size=12, vjust=0.5))
#
# # Plot 3-day running mean nest distance for each eagle
# for (i in unique(baea_terr_sum$id)){
#   cat(noquote(paste(i,"\n")))
#   baea_terr_i <- baea_terr_sum %>% filter(id == i)
#   min_date <-  as.POSIXct(baea_terr_i$date[min(which(!is.na(
#     baea_terr_i$nest_dist_run_3dy)))])
#   max_date <- as.POSIXct(baea_terr_i$date[max(which(!is.na(
#     baea_terr_i$nest_dist_run_3dy)))])
#   g <- ggplot(data = baea_terr_i) +
#     annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-03-20"))),
#       xmax = c(as.POSIXct(paste0(study_years, "-06-21"))),
#       ymin = 0, ymax = Inf, alpha = 0.2, fill = "green") +
#     annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-06-21"))),
#       xmax = c(as.POSIXct(paste0(study_years, "-09-23"))),
#       ymin = 0, ymax = Inf, alpha = 0.2, fill = "yellow") +
#     annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-09-23"))),
#       xmax = c(as.POSIXct(paste0(study_years, "-12-21"))),
#       ymin = 0, ymax = Inf, alpha = 0.2, fill = "darkgoldenrod3") +
#     annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-12-21"))),
#       xmax = c(as.POSIXct(paste0(study_years+1, "-03-20"))),
#       ymin = 0, ymax = Inf, alpha = 0.2, fill = "ivory1") +
#     geom_line(aes(x=as.POSIXct(date), y=nest_dist_run_3dy/1000),
#       color = "blue", size = .75, show.legend = FALSE) +
#     scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
#     geom_hline(aes(yintercept = 5), color = "red") +
#     coord_cartesian(xlim = c(min_date, max_date)) +
#     scale_x_datetime(breaks=scales::date_breaks("1 month"), labels =
#       scales::date_format("%b %Y"), expand = c(0.02, 0.02, 0, 0)) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
#     labs(x = "Date", y = paste0("Running Mean 3-Day Nest Distance (km)"),
#       title = i) +
#     theme(text = element_text(color="black")) +
#     theme(plot.title = element_text(size=18, face="bold", vjust=1.5, hjust=0.5)) +
#     theme(axis.title = element_text(size=14, face="bold")) +
#     theme(axis.text.x = element_text(size=12, angle=50, vjust=1, hjust=1)) +
#     theme(axis.text.y = element_text(size=12, vjust=0.5))
#   plot(g)
# }
#
# ####################### PLOT DAILY MIN NEST DISTANCE ####################### #
#
# # Plot daily minimum nest distance for all eagles
# ggplot(data = baea_terr_sum)  +
#   annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-03-20"))),
#     xmax = c(as.POSIXct(paste0(study_years, "-06-21"))),
#     ymin = 0, ymax = Inf, alpha = 0.2, fill = "green") +
#   annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-06-21"))),
#     xmax = c(as.POSIXct(paste0(study_years, "-09-23"))),
#     ymin = 0, ymax = Inf, alpha = 0.2, fill = "yellow") +
#   annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-09-23"))),
#     xmax = c(as.POSIXct(paste0(study_years, "-12-21"))),
#     ymin = 0, ymax = Inf, alpha = 0.2, fill = "darkgoldenrod3") +
#   annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-12-21"))),
#     xmax = c(as.POSIXct(paste0(study_years+1, "-03-20"))),
#     ymin = 0, ymax = Inf, alpha = 0.2, fill = "ivory1") +
#   geom_line(aes(x=as.POSIXct(date), y=nest_dist_min/1000, color=id),
#     size=.5) +
#   scale_colour_manual(values = by_colors) +
#   scale_y_continuous(expand = c(0, 0, 0.025, 0)) +
#   geom_hline(aes(yintercept = 5), color = "red") +
#   coord_cartesian(xlim = c(min(as.POSIXct(baea_terr_sum$date)),
#     max(as.POSIXct(baea_terr_sum$date)))) +
#   scale_x_datetime(breaks=scales::date_breaks("3 month"), labels =
#     scales::date_format("%b %Y"), expand = c(0.02, 0.02, 0, 0)) +
#   labs(x = "Date", y = paste0("Daily Mean Nest Distance (km)"),
#       title = "All Territorial Data") +
#   theme(text = element_text(color="black")) +
#   theme(plot.title = element_text(size=18, face="bold", vjust=1.5, hjust=0.5)) +
#   theme(axis.title = element_text(size=14, face="bold")) +
#   theme(axis.text.x = element_text(size=12, angle=50, vjust=1, hjust=1)) +
#   theme(axis.text.y = element_text(size=12, vjust=0.5))
#
# # Plot daily minimum nest distance for each eagle
# for (i in unique(baea_terr_sum$id)){
#   cat(noquote(paste(i,"\n")))
#   baea_terr_i <- baea_terr_sum %>% filter(id == i)
#   min_date <-  as.POSIXct(baea_terr_i$date[min(which(!is.na(
#     baea_terr_i$nest_dist_min)))])
#   max_date <- as.POSIXct(baea_terr_i$date[max(which(!is.na(
#     baea_terr_i$nest_dist_min)))])
#   g <- ggplot(data = baea_terr_i) +
#     annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-03-20"))),
#       xmax = c(as.POSIXct(paste0(study_years, "-06-21"))),
#       ymin = 0, ymax = Inf, alpha = 0.2, fill = "green") +
#     annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-06-21"))),
#       xmax = c(as.POSIXct(paste0(study_years, "-09-23"))),
#       ymin = 0, ymax = Inf, alpha = 0.2, fill = "yellow") +
#     annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-09-23"))),
#       xmax = c(as.POSIXct(paste0(study_years, "-12-21"))),
#       ymin = 0, ymax = Inf, alpha = 0.2, fill = "darkgoldenrod3") +
#     annotate("rect", xmin = c(as.POSIXct(paste0(study_years, "-12-21"))),
#       xmax = c(as.POSIXct(paste0(study_years+1, "-03-20"))),
#       ymin = 0, ymax = Inf, alpha = 0.2, fill = "ivory1") +
#     geom_line(aes(x=as.POSIXct(date), y=nest_dist_min/1000),
#       color = "green", size = .75, show.legend = FALSE) +
#     scale_y_continuous(expand = c(0, 0, 0.05, 0)) +
#     geom_hline(aes(yintercept = 5), color = "red") +
#     coord_cartesian(xlim = c(min_date, max_date)) +
#     scale_x_datetime(breaks=scales::date_breaks("1 month"), labels =
#       scales::date_format("%b %Y"), expand = c(0.02, 0.02, 0, 0)) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
#     labs(x = "Date", y = paste0("Daily Minimum Nest Distance (km)"),
#       title = i) +
#     theme(text = element_text(color="black")) +
#     theme(plot.title = element_text(size=18, face="bold", vjust=1.5, hjust=0.5)) +
#     theme(axis.title = element_text(size=14, face="bold")) +
#     theme(axis.text.x = element_text(size=12, angle=50, vjust=1, hjust=1)) +
#     theme(axis.text.y = element_text(size=12, vjust=0.5))
#   plot(g)
# }
#
# # Subset locations that are near the nest (spring/summer only)
#
# baea_nest <- baea_terr_sum %>%
#   filter(nest_dist_lt_100m == 1,
#     season %in% c("Spring", "Summer")) %>%
#   mutate(behavior = as.character(NA))
#
# table(baea_nest$id)
#
# # Find roosting locations
#
# baea_roost <- AddRoostBehavior(baea_nest, overnight_distance_threshold = 100,
#   at_roost_distance_threshold = 50, depart_timediff_max = 1000,
#   arrive_timediff_max = 1000)
#
# default_tz = "America/New_York"
# tz = "Etc/GMT+5"
# overnight_distance_threshold = 100
# at_roost_distance_threshold = 50
# depart_timediff_max = 1000
# arrive_timediff_max = 1000
# df = baea_nest
# df <- df %>%
#   mutate(
#     two_hr_after_sunrise = hr_before_sunrise + hours(2),
#     two_hr_before_sunset = hr_after_sunset - hours(2),
#     sunrise_window_loc = datetime <= two_hr_after_sunrise,
#     sunset_window_loc = datetime >= two_hr_before_sunset)
# df <- df %>% group_by(id, date) %>%
#   summarize(total_loc = n(),
#     am_loc = sum(sunrise_window_loc, na.rm=TRUE),
#     pm_loc = sum(sunset_window_loc, na.rm=TRUE)) %>%
#   mutate(nextdayGPS = lead(date),
#     next_day_GPS = difftime(nextdayGPS, date, units="days"),
#     next_am_loc = lead(am_loc)) %>%
#   left_join(df, ., by = c("id", "date"))
# # At this point, sumstats has: "id", "date", "total_locs", "nextdayGPS",
# # "am_loc", "pm_loc", and "next_am_loc"
# # This has all the data needed to cull by nextDayGPS, and am/pm locations
#
# df2 <- df %>% dplyr::select(id, date, last, step_length, next_day_GPS, pm_loc,
#   next_am_loc)
#
# table(df2$pm_loc)
#
# last_roost_confirmed <- df %>%
#   filter(last == "Last",
#     step_length <= overnight_distance_threshold,
#     next_day_GPS == 1,
#     pm_loc >= 7,
#     next_am_loc >= 7) %>%
#   dplyr::select(id, date)
#
# first_roost_confirmed <- last_roost_confirmed %>%
#   mutate(date = date + 1)
#
# # This culls by overnight segment length, if next day has GPS locations, if
# # there are at least 7 locations within an hour of either side of sunset
# # and if there are at least 7 locations within an hour of either side of
# # sunrise on the following morning.
#
# roost_arrival_filtered <- dplyr::inner_join(df, last_roost_confirmed) %>%
#   dplyr::select(id, date, datetime, dist_last) %>%
#   group_by(id, date) %>%
#   arrange(desc(datetime)) %>%
#   mutate(at_roost_loc = dist_last <= at_roost_distance_threshold) %>%
#   mutate(pm_roosting = cumall(at_roost_loc == TRUE)) %>%
#   arrange(datetime) %>%
#   dplyr::select(-c(at_roost_loc))
#
# roost_departure_filtered <- dplyr::inner_join(df, first_roost_confirmed) %>%
#   dplyr::select(id, date, datetime, dist_first)  %>%
#   group_by(id, date) %>%
#   mutate(at_roost_loc = dist_first <= at_roost_distance_threshold) %>%
#   mutate(am_roosting = cumall(at_roost_loc == TRUE)) %>%
#   dplyr::select(-c(at_roost_loc))
#
# roost_all <- full_join(roost_arrival_filtered, roost_departure_filtered) %>%
#   mutate(roosting = am_roosting == TRUE | pm_roosting == TRUE) %>%
#   arrange(id, date, datetime)
#
# roost_all_complete <- roost_all %>%
#   filter(!is.na(am_roosting)  && !is.na(pm_roosting)) %>%
#   group_by(id) %>%
#   summarize(days = n_distinct(date))
#
# df$loaf <- NA
# df$loaf <- ifelse(df$datetime <= df$dep_dist_threshold_datetime, "loaf", NA)
# df$loaf <- ifelse(is.na(df$loaf) & df$datetime >=
#     df$arr_dist_threshold_datetime, "loaf", df$loaf)
# df$depart <- ifelse(df$datetime == df$dep_datetime, "depart", NA)
# df$arrive <- ifelse(df$datetime == df$arr_datetime, "arrive", NA)
# df$depart <- ifelse(is.na(df$depart) & !is.na(df$dep_datetime) &
#     df$datetime < df$dep_datetime, "roost", df$depart)
# df$arrive <- ifelse(is.na(df$arrive) & !is.na(df$arr_datetime) &
#     df$datetime > df$arr_datetime, "roost", df$arrive)
# df$roost <- ifelse(is.na(df$depart),df$arrive,df$depart)
# df$roost_loaf <- ifelse(!is.na(df$roost), df$roost,
#   df$loaf)
# if (!("behavior" %in% colnames(df))) df$behavior<-NA
# df$behavior <- ifelse(!is.na(df$roost_loaf) & is.na(df$behavior),
#   df$roost_loaf, df$behavior)
# drops <- c("arrive", "depart", "total_loc", "nextdayGPS", "dep_datetime",
#   "two_hr_after_sunrise", "two_hr_before_sunset", "sunrise_window_loc",
#   "arr_datetime", "sunset_window_loc", "am_loc", "pm_loc", "next_am_loc",
#   "roost", "loaf", "roost_loaf" ,"dep_dist_threshold_datetime",
#   "arr_dist_threshold_datetime")
# df <- df[ ,!(names(df) %in% drops)]
# row.names(df) <- NULL
#
# movements <- baea_terr %>%
#   group_by("id") %>%
#   filter(step_length > 42.5) %>%
#   filter(step_time <= 20) %>%
#   mutate(step_length = step_length/1000) %>%
#   ungroup()
#
#
# min_speed = 5
# min_step_length = 50
# max_step_time = 20
# threshold_agl = 100
# df_flight <- baea_roost %>%
#     plyr::mutate(bh_flight = as.character(NA)) %>%
#     plyr::mutate(bh_flight = dplyr::if_else(speed >= min_speed &
#         step_time < max_step_time & step_length > min_step_length, "Flight",
#         bh_flight)) %>%
#     plyr::mutate(bh_flight = dplyr::if_else(agl >= threshold_agl, "Flight",
#       bh_flight))
#
#
# baea_flight <- df_flight %>% filter(agl > 100) %>%
# #  dplyr::filter(is.na(bh_flight)) %>%
#   dplyr::select(date:id, alt, step_time, step_length, agl, bh_nest:bh_flight)
#
# min_agl = 200
# min_speed = 5
# threshold_agl = 200
#
#  df_cruise <- baea_flight %>%
#     dplyr::mutate(
#       bh_cruise = as.character(NA),
#       bh_cruise = dplyr::if_else(agl >= min_agl & speed >= min_speed, "Cruise",
#         bh_cruise))
#
#       bh_cruise = dplyr::if_else(agl >= threshold_agl, "Cruise", bh_cruise),
#       bh_cruise_tf = dplyr::if_else(!is.na(bh_cruise), TRUE, FALSE),
#       bh_cruise_seq = sequence(rle(bh_cruise_tf)$lengths) * bh_cruise_tf) %>%
#     dplyr::select(-bh_cruise_tf)
#

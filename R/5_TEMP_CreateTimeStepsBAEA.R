#step_interval <- step_intervals[[1]]
#time_step_period <- sim$pars$global$time_step_period
#xy_coords <- matrix(c(-68.7778, 44.8012), nrow = 1)

#step_interval <- interval(as.Date("2018-03-05"), as.Date("2018-03-06"))

CreateTimeStepsBAEA <- function(step_interval = step_interval,
                                sim = sim) {
  time_step_period = sim$pars$global$time_step_period
  options(lubridate.verbose=FALSE)
  xy_coords <- matrix(c(-67.7778, 44.8012), nrow = 1)
  interval_start_East <- force_tz(lubridate::int_start(step_interval),
    tzone = "US/Eastern")
  interval_end_East <- force_tz(lubridate::int_end(step_interval)-minutes(1),
    tzone = "US/Eastern") #subtracting one minute to make it the same day
  sunrise <- maptools::sunriset(xy_coords, interval_start_East,
    proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
    POSIXct.out= TRUE)[1,2]
  sunset <- maptools::sunriset(xy_coords, interval_end_East,
    proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunset",
    POSIXct.out= TRUE)[1,2]
  step_interval_start <- round_date(sunrise, "minute") - hours(1)
  step_interval_end <- round_date(sunset, "minute") + hours(1)
  end_int <- lubridate::int_end(lubridate::as.interval(time_step_period,
    step_interval_start))
  steps <- append(step_interval_start, end_int)
  while (end_int < step_interval_end) {
    end_int <- end_int + time_step_period
    steps <- append(steps, end_int)
  }
  if (tail(steps, 1) > step_interval_end) {
    steps[length(steps)] <- step_interval_end
  }
  time_step_list <- list()
  for (i in 1:(length(steps)-1)) {
    interval <- lubridate::as.interval(steps[i], steps[i+1])
    interval@tzone  <- "US/Eastern"
    time_step_list[[length(time_step_list)+1]] <- interval
  }
  return(time_step_list)
}


#
# suppressPackageStartupMessages(library(dplyr))
# suppressPackageStartupMessages(library(ggplot2))
# suppressPackageStartupMessages(library(lubridate))
# suppressPackageStartupMessages(library(maptools))
# suppressPackageStartupMessages(library(stringr))
# suppressPackageStartupMessages(library(tidyr))
# suppressPackageStartupMessages(library(varhandle))
#
# library(baear)
# library(gisr)
# library(ibmr)
# devtools::reload("C:/Work/R/Packages/ibmr")
# xy_coords <- matrix(c(-67.7778, 44.8012), nrow = 1) # Bangor, ME
# time_step_period <- as.period(15, "mins")
#
# baea_hmm_full <- readRDS(file = "Data/Models/baea_hmm_full2")
# beta <- baea_hmm_full$mle$beta
#
# # Create dataframe for simulated data ----
# start_date <- as.POSIXct("2017-07-20", tz="America/New_York") #3-20
# end_date <- as.POSIXct("2017-07-25", tz="America/New_York")  #9-20
# dates <- seq(start_date, end_date, by="days")
# steps_all <- as.numeric()
# for (i in 1:length(dates)){
#   date_i <- dates[i] #, tz="America/New_York")
#   step_interval_start  <- maptools::sunriset(xy_coords, date_i,
#       proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
#       POSIXct.out= TRUE)[1,2]
#   step_interval_end  <- maptools::sunriset(xy_coords, date_i,
#       proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunset",
#       POSIXct.out= TRUE)[1,2]
#   end_int <- lubridate::int_end(lubridate::as.interval(time_step_period,
#     step_interval_start))
#   steps <- lubridate::with_tz(append(step_interval_start, end_int),
#     lubridate::tz("America/New_York"))
#   while (end_int < step_interval_end) {
#     end_int <- end_int + time_step_period
#     steps <- lubridate::with_tz(append(steps, end_int),
#       lubridate::tz("America/New_York"))
#   }
#   if (tail(steps, 1) > step_interval_end) {
#     steps[length(steps)] <- step_interval_end
#   }
#   print(i)
#   steps_all <- append(steps_all, steps)
# }
#
# library(dplyr)
# library(tidyr)
#
# df_sim <- as.data.frame(steps_all) %>%
#   mutate(hr_before_sunrise =  maptools::sunriset(xy_coords, steps_all,
#       proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
#       POSIXct.out= TRUE)[,2])
#
# hr_before_sunrise <- maptools::sunriset(xy_coords, steps_all,
#       proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
#       POSIXct.out= TRUE)[1,2]
#
# df_sim <- data.frame(id = NA, date_time = steps_all, time_proportion = NA,
#   julian = yday(steps_all), behavior = NA)
#
# df_sim$id <- "Sam"
#
# df_sim <- AddTimeStepProportion(df = df_sim, by = "id", time_step = "15 min",
#   tz = "Etc/GMT+5")


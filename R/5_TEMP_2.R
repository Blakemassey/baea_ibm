(step_interval <- step_intervals[[1]])
(time_step_period <- sim$pars$global$time_step_period)
xy_coords <- matrix(c(44.8012, 68.7778), nrow = 1)

CreateTimeSteps <- function(step_interval = step_interval,
                            time_step_period =
                              sim$pars$global$time_step_period) {
  options(lubridate.verbose=FALSE)
  step_interval_end <- lubridate::int_end(step_interval)

#  step_interval_end  <- maptools::sunriset(xy_coords, step_interval_end,
#      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunset",
#      POSIXct.out= TRUE)[1,2]

  step_interval_start <- lubridate::int_start(step_interval) # create output list w/start time


#  steps  <- maptools::sunriset(xy_coords, steps,
#      proj4string = sp::CRS("+proj=longlat +datum=WGS84"), direction ="sunrise",
#      POSIXct.out= TRUE)[1,2]

  end_int <- lubridate::int_end(lubridate::as.interval(time_step_period,
    step_interval_start))
  (end_int)

  steps <- lubridate::with_tz(append(step_interval_start, end_int),
    lubridate::tz(lubridate::int_end(step_interval)))
  while (end_int < step_interval_end) {
    end_int <- end_int + time_step_period
    steps <- lubridate::with_tz(append(steps, end_int),
      lubridate::tz(lubridate::int_end(step_interval)))
  }
  if (tail(steps, 1) > step_interval_end) {
    steps[length(steps)] <- step_interval_end
  }
  time_step_list <- list()
  for (i in 1:(length(steps)-1)) {
    interval <- lubridate::as.interval(steps[i], steps[i+1])
    time_step_list[[length(time_step_list)+1]] <- interval
  }
  return(time_step_list)
}

time_step_list






























data=deployed_all
update_only=TRUE
update_gdrive=FALSE


  data <- data
  # Update local individual files
  data <- data
  ids <- unique(data$id)
  current_year <- year(now())
  for (i in  1:length(ids)){
    id <- ids[i]
    df <- data[data$id == id,]
    if(update_only == TRUE){
      df_year <- df[df$year == current_year,]
      if (nrow(df_year) > 1){
        ExportKMLTelemetryBAEA(df_year, file = paste0(id, "_", current_year,
          ".kml"), output_dir = "Data/GPS/KMLs")
      }
    } else {
      years <- year(first(df$datetime)):year(last(df$datetime))
      for (j in 1:length(years)){
        year <- years[j]
        df_year <- df[df$year == year,]
        ExportKMLTelemetryBAEA(df_year, file = paste0(id, "_", year, ".kml"),
          output_dir = "Data/GPS/KMLs")
      }
    }
  }
  if (update_gdrive == TRUE){
    # Copy individual files to Google Drive
    kml_files <- list.files("Data/GPS/KMLs", full.names=TRUE)
    if (update_only) kml_files <- stringr::str_subset(kml_files,
      as.character(current_year))
    output_dir = file.path("C:/Users/Blake/Google Drive/PhD Program",
      "BAEA Project/Telemetry Data/Individuals")
    file.copy(kml_files, output_dir, overwrite=TRUE)
  }
}

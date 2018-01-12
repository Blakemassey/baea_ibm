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

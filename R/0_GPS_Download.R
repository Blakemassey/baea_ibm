### This script is for downloading telemetry data of deployed transmitters from
### the CTT website, compiling the data, creating summary stats and plots, and
### exporting KMLs and Shapefiles.

library(baear)
library(gisr)
library(ibmr)

## Download RECENT Deployed Data -----------------------------------------------
DownloadCTT(units="deployed", download="recent")
deployed_recent <- CompileDownloads(units="deployed", compile="recent")
deployed_all <- ImportUnits(units="deployed", existing=deployed_recent,
  import=TRUE)

## Filter Deployed -------------------------------------------------------------
week_ago <- as.character(lubridate::floor_date(lubridate::now() -
  lubridate::period(1, "week"), "day"))
deployed <- FilterLocations(df=deployed_all, id="id", individual="",
  start=week_ago, end="")

## Export KML of Locations -----------------------------------------------------
ExportKMLTelemetryBAEA(df=deployed, file="BAEA Data.kml")

## Plot Daily Locations by Time ------------------------------------------------
PlotLocationSunriseSunset(df=deployed, by="id", individual="",
  start="", end="", breaks="1 days", tz="Etc/GMT+5", addsolartimes=TRUE,
  wrap=TRUE)

## Update Weekly Data and Weekly KLM files -------------------------------------
RemoveExcept(c("deployed", "deployed_all"))
devtools::reload("C:/Work/R/Packages/baear")
devtools::reload("C:/Work/R/Packages/gisr")
devtools::reload("C:/Work/R/Packages/ibmr")
UpdateWeeklyData(data=deployed_all, date="2017-03-12", send_email=FALSE)
UpdateWeeklyKMLFiles(deployed_all, start="2017-01-01", end="2017-03-18")

## Download ALL Deployed Data and Write a New "deployed.csv" File --------------
#DownloadCTT(units="deployed", download="all")
deployed_all <- CompileDownloads(units="deployed", compile="all")
write.csv(deployed_all, file=file.path("C:/Work/R/Data/BAEA/Telemetry",
  "Deployed/Deployed.csv"), row.names=FALSE)

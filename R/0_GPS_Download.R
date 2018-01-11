#-------------------------- GPS DOWNLOAD --------------------------------------#
# This script is for downloading telemetry data of deployed transmitters from
# the CTT website, compiling the data, creating summary stats and plots, and
# exporting KMLs and Shapefiles.
#------------------------------------------------------------------------------#

suppressPackageStartupMessages(library(lubridate))
library(baear)
library(gisr)
library(ibmr)
#devtools::reload("C:/Work/R/Packages/baear")
#devtools::reload("C:/Work/R/Packages/gisr")
#devtools::reload("C:/Work/R/Packages/ibmr")

## Download RECENT Deployed Data -----------------------------------------------
DownloadCTT(units="deployed", download="recent")
deployed_recent <- CompileDownloads(units="deployed", compile="recent")
deployed_all <- ImportUnits(units="deployed", existing=deployed_recent,import=T)
saveRDS(deployed_all, file="Data/CTT/Deployed/Deployed.rds")
#deployed_all <-  readRDS(file="Data/CTT/Deployed/Deployed.rds")

## Filter Deployed -------------------------------------------------------------
week_ago <- as.character(floor_date(now() - period(1, "week"), "day"))
deployed <- FilterLocations(df=deployed_all, individual="", start=week_ago,
  end="")

## Export KML of Locations -----------------------------------------------------
ExportKMLTelemetryBAEA(df=deployed, file="BAEA Data.kml")

## Plot Daily Locations by Time ------------------------------------------------
PlotLocationSunriseSunset(df=deployed, by="id", individual="",
  start="", end="", breaks="1 days", tz="Etc/GMT+5", addsolartimes=TRUE,
  wrap=TRUE)

## Update Individual By Year KLM files -----------------------------------------
UpdateIndByYearKMLs(data=deployed_all, update_only=TRUE, update_gdrive=FALSE)

## Download ALL Deployed Data and Write a new "deployed.csv" File --------------
#DownloadCTT(units="deployed", download="all")
deployed_all <- CompileDownloads(units="deployed", compile="all")
saveRDS(deployed_all, file="Data/CTT/Deployed/Deployed.rds")

# Send Email to Charlie and Erynn ----------------------------------------------
SendWeeklyData(data=deployed_all, date="2017-03-12", send_email=FALSE)


#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

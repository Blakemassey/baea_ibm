#-------------------------- GPS DOWNLOAD --------------------------------------#
# This script is for downloading telemetry data of deployed transmitters from
# the CTT website, compiling the data, creating summary stats and plots, and
# exporting KMLs and Shapefiles.
#------------------------------------------------------------------------------#

suppressPackageStartupMessages(library(lubridate))
library(baear)#devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/baear")
library(gisr) #devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/gisr")
library(ibmr) #devtools::reload("C:/Users/blake/OneDrive/Work/R/Packages/ibmr")
library(dplyr)

## Download RECENT Deployed Data -----------------------------------------------
DownloadCTT(units = "deployed", download = "recent")
deployed_recent <- CompileDownloads(units = "deployed", compile = "recent")
deployed_all <- ImportUnits(units = "deployed", existing = deployed_recent,
  import = TRUE)
saveRDS(deployed_all, file="Data/CTT/Deployed/Deployed.rds")
#deployed_all <-  readRDS(file="Data/CTT/Deployed/Deployed.rds")

## Filter Deployed -------------------------------------------------------------
filter_date <- as.character(floor_date(now() - period(3, "month"), "day"))
deployed <- deployed_all %>% filter(datetime >= filter_date)
deployed_2019 <- deployed_all %>% filter(year == 2019)

## Export KML of Locations -----------------------------------------------------
ExportKMLTelemetryBAEA(df = deployed, file = "BAEA Data.kmz")
ExportKMLTelemetryBAEA(df = deployed_2019, file = "BAEA Data - 2019.kmz")

## Plot Daily Locations by Time ------------------------------------------------
PlotLocationSunriseSunset(df = deployed, by = "id", individual = "", start = "",
  end = "", breaks = "1 days", tz = "Etc/GMT+5", addsolartimes = TRUE,
  wrap = TRUE)

## Update Individual By Year KLM files -----------------------------------------
UpdateIndByYearKMLs(df = deployed_all, update_year = 2018, update_gdrive =FALSE)

## Download ALL Deployed Data and Write a new "deployed.csv" File --------------
#DownloadCTT(units="deployed", download="all")
deployed_all <- CompileDownloads(units = "deployed", compile = "all")
saveRDS(deployed_all, file = "Data/CTT/Deployed/Deployed.rds")

# Send Email to Charlie and Erynn ----------------------------------------------
SendWeeklyData(data = deployed_all, date = "2017-03-12", send_email = FALSE)

## BRI Download and Compile ----------------------------------------------------
system('C:/Anaconda/envs/ctt/python.exe C:/Work/Python/Scripts/cttpy/Import_BRI.py')
bri <- CompileDownloads(units = "deployed", compile = "BRI")
UpdateIndByYearKMLs(df = bri, update_year = 2018, update_gdrive = FALSE)
bri_2018 <- bri %>% filter(year == 2018)
ExportKMLTelemetryBAEA(df = bri_2018, file = "BRI Data - 2018.kmz")

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

# bad <- which(df[,"id"]=="Cape_Walsh" & df[,"datetime"]=="2017-08-22 19:19:58")
# bad <- which(df[,"id"]=="Cape_Walsh" & df[,"datetime"]=="2017-11-15 07:45:39")
# which(df[,"id"]=="Cape_Walsh" & df[,"datetime"] > "2017-08-22 17:00:00" &
#   df[,"datetime"] < "2017-08-22 19:00:00")
# if (length(bad) > 0) df <- df[-bad, ]
#
# df[5140,]

### This script is for downloading telemetry data of deployed transmitters from
### the CTT website, compiling the data, creating summary stats and plots, and
### exporting KMLs and Shapefiles.

library(baear)
library(gisr)
library(ibmr)

theme_massey <- theme_bw() + theme(plot.title = element_text(size = 22)) +
    theme(text = element_text(size = 18, colour = "black")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")) +
    theme(text = element_text(size = 20, colour = "black")) +
    theme(axis.text = element_text(colour = "black"))  +
    theme(panel.background = element_rect(fill = "gray90")) +
    theme(panel.grid.major = element_line(color = "gray80")) +
    theme(panel.grid.minor = element_line(color = "gray80")) +
    scale_colour_manual(values = by_colors)

## Download RECENT Deployed Data -----------------------------------------------
DownloadCTT(units="deployed", download="recent")
deployed_recent <- CompileDownloads(units="deployed", compile="recent")
deployed_all <- ImportUnits(units="deployed", existing=deployed_recent,
  import=TRUE)
saveRDS(deployed_all, file="Data/CTT/Deployed/Deployed.rds")

## Filter Deployed -------------------------------------------------------------
week_ago <- as.character(lubridate::floor_date(lubridate::now() -
  lubridate::period(1, "week"), "day"))
deployed <- FilterLocations(df=deployed_all, id="id", individual="",
  start=week_ago, end="")

RemoveExcept(c("deployed", "deployed_all"))

## Export KML of Locations -----------------------------------------------------
ExportKMLTelemetryBAEA(df=deployed, file="BAEA Data.kml")

## Plot Daily Locations by Time ------------------------------------------------
PlotLocationSunriseSunset(df=deployed, by="id", individual="",
  start="", end="", breaks="1 days", tz="Etc/GMT+5", addsolartimes=TRUE,
  wrap=TRUE)

## Update Individual By Year KLM files -----------------------------------------
UpdateIndByYearKMLs(data=deployed_all, update_only=TRUE, update_gdrive=TRUE)

## Download ALL Deployed Data and Write a new "deployed.csv" File --------------
#DownloadCTT(units="deployed", download="all")
deployed_all <- CompileDownloads(units="deployed", compile="all")
saveRDS(deployed_all, file="Data/CTT/Deployed/Deployed.rds")

# Send Email to Charlie and Erynn ----------------------------------------------
SendWeeklyData(data=deployed_all, date="2017-03-12", send_email=FALSE)

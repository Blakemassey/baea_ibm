### --------------------- SIM NEST MANAGEMENT --------------------------------------
### This script is for importing IFW, BAEA, Simulation nest data(.csv) 
### and viewing, updating, and exporting as a Shapefile or Raster. 
### The general scheme for handling BAEA nest data is to import IFW Shapefile 
### and Access data (as a .csv) and the Study Nests (as a .csv). These two sets
### of data are then projected, lat/long coordinates appended, and exported as 
### new .csv files into the R/Data/BAEA/Nests folder. Those new .csv files are 
### then converted to geodatabase files as IFW_Nests.gbd/IFW_All, and 
### Study_Nests.gbd/Study_All, respectively. This maintains the relationship 
### between the .csv files and the geodatabase files.
--------------------------------------------------------------------------------
suppressPackageStartupMessages(require(plyr))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(foreign))
suppressPackageStartupMessages(require(rgdal))
suppressPackageStartupMessages(require(sp))
suppressPackageStartupMessages(require(stringr))
options(stringsAsFactors = FALSE)
source('C:/Work/R/Functions/all.R')
kml_output_dir = "C:/ArcGIS/Data/BAEA/Nests/KMLs"

#############  MANAGE SIM NESTS (USING NESTS_STUDY_INTACT_LAST) ################

source('C:/Work/R/Functions/baea.R')
dir <- "C:/Work/R/Data/BAEA/Nests"
nsil <- read.csv(file.path(dir, "Nests_Study_Intact_Last.csv"))
years <- c(paste0("active_",2013:2017))
nsil[, years] <- NA
nsil[, c("active_2013","active_2014","active_2015")] <- TRUE

# Create and Export Study_Set1
nsil[which(nsil$nest_id == "620A"), "active_2014"] <- FALSE
write.csv(nsil, file.path(dir,"Sim_Sets/Study_Set1.csv"), row.names=FALSE)

# Create and Export Study_Set2
nsil[which(nsil$nest_id == ""), "active_2014"] <- FALSE
write.csv(nsil, file.path(dir, "Sim_Sets/Study_Set2.csv"), row.names=FALSE)

# Export KML to "C:/Work/R/Data/BAEA/Nests/Sim_Sets" folder
ExportKMLPoints(nsil, id = "nest_site", file="Nests Study Intact Last.kml", 
  output_dir="C:/Work/R/Data/BAEA/Nests/Sim_Sets")
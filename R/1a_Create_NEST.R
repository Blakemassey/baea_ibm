#--------------------------- CREATE NEST --------------------------------------#
# This script is for importing IFW, BAEA, Simulation nest data for manipulating,
# updating and exporting as a Shapefile or Raster. The general scheme for
# handling BAEA nest data is to import IFW Shapefile and Access data (as a .csv)
# and the Study Nests (as a .csv). These two sets of data are then projected,
# lat/long coordinates appended, and exported as new .rds files into the
# "Data/Nests" folder. Those new .rds files are then converted to geodatabase
# files as IFW_Nests.gbd/IFW_All, and Study_Nests.gbd/Study_All, respectively.
# This maintains the relationship between the .rds files and geodatabases.
#------------------------------------------------------------------------------#
suppressPackageStartupMessages(require(plyr))
suppressPackageStartupMessages(require(dplyr))
suppressPackageStartupMessages(require(foreign))
suppressPackageStartupMessages(require(rgdal))
suppressPackageStartupMessages(require(sp))
suppressPackageStartupMessages(require(stringr))
library(baear)
library(gisr)
options(stringsAsFactors = FALSE)
options(row.names = FALSE)

csv_output_dir = "Data/Nests/Nests_csv"
kml_output_dir = "Data/Nests/Nests_kml"
rds_output_dir = "Data/Nests/Nests_rds"

############################ IFW Nests - All ###################################

# Import latest tabular data from IFW which is missing some spatial locations
# but has attribute columns that are not in the spatial data.
nests_csv = read.csv("Data/Nests/Original_Data/IFW_20150427.csv") %>%
            dplyr::rename(nest_site = PK_NestSite,
              site_name = NestSiteName,
              intact_at_last_obs = IntactDuringLastYearObs.,
              year_last_monitored = YearLastMonitored,
              year_last_intact = YearLastNestIntact,
              year_last_occupancy = YearLastEagleOccupancy,
              year_first_monitored = YearFirstMonitored,
              comments = HistoryComments,
              landscape = LandscapeHabitat,
              county = County,
              state = State.Province,
              ifw_region = IFWRegion,
              nest_evaluation_date = DateSiteEvaluation,
              nest_tree_prominence = NestTreeStandProminence,
              nest_comments = NestTreeComments,
              long = POINT_X,
              lat = POINT_Y) %>%
            filter(!is.na(nest_site)) %>%
            mutate(nest_area = gsub("[^0-9]","", nest_site)) %>%
            dplyr::select(nest_site, nest_area, site_name, intact_at_last_obs,
              year_last_monitored, year_last_intact, year_last_occupancy,
              year_first_monitored, comments, landscape, county, state,
              ifw_region, nest_evaluation_date, nest_tree_prominence,
              nest_comments, long, lat)

# Import latest shapefile data from IFW, which has some locations not in the
# csv data but is missing certain attribute columns.
nests_shp <- readOGR(dsn = "C:/ArcGIS/Data/BAEA/Nests/IFW_Nests",
  "IFW_Nests_2014")@data %>%
  dplyr::rename(nest_site = PK_NestSit,
    lat2 = POINT_Y,
    long2 = POINT_X) %>%
  dplyr::select(nest_site, long2, lat2) %>%
  group_by(nest_site) %>%
  filter(row_number()==n()) %>%
  ungroup() %>% as.data.frame() # there are two 740A

# Combine csv and shapefile nests. The csv file has all of the attributes, but
# csv nests that are missing spatial data will have it inserted from the
# shapefile database
nests_ifw <- dplyr::left_join(nests_csv, nests_shp, by = "nest_site") %>%
             mutate(lat_utm = ifelse(is.na(lat), lat2, lat)) %>%
             mutate(lat = lat_utm) %>%
             mutate(long_utm = ifelse(is.na(long), long2, long)) %>%
             mutate(long = long_utm) %>%
             dplyr::select(-c(long, lat, lat2, long2)) %>% # NAD83 UTM 19N
             filter(!is.na(lat_utm))

# Project to lat/long WGS84 and add coordinates data
nests_ifw_sp_utm83 <- SpatialPoints(coords =nests_ifw[,c("long_utm","lat_utm")],
  proj4string = CRS("+proj=utm +zone=19 +datum=NAD83"))
nests_ifw_sp_utm84 <- spTransform(nests_ifw_sp_utm83,
  CRSobj = CRS("+proj=longlat +datum=WGS84"))
coords <- as.data.frame(coordinates(nests_ifw_sp_utm84))
colnames(coords) <- c("long", "lat")
nests_ifw_spdf_ll84 <- SpatialPointsDataFrame(coords, bbox = NULL,
  data = as.data.frame(bind_cols(nests_ifw, coords)),
  proj4string = CRS("+proj=longlat +datum=WGS84"))
nests_ifw_data <- nests_ifw_spdf_ll84@data %>%
  arrange(nest_area) %>%
  dplyr::select(nest_site:nest_comments, long_utm, lat_utm, long, lat)

# Export CSV and RDS to "C:/Work/R/Data/BAEA/Nests" folder
write.csv(nests_ifw_data, file = file.path(csv_output_dir, "Nests_IFW_All.csv"),
  row.names = FALSE)
saveRDS(nests_ifw_data, file = file.path(rds_output_dir, "nests_ifw_all.rds"))

# Export KML to "C:/ArcGIS/Data/BAEA/Nests/KMLs" folder
ExportKMLPoints(nests_ifw_data, id = "nest_site", file = "Nests IFW All.kml",
  output_dir = kml_output_dir)

######################### Nests Active 2013 ####################################

# Filter to only nests active in 2013 (some of which are no longer intact)
nests_2013 <- readOGR(dsn = "C:/ArcGIS/Data/BAEA/Nests/IFW_Nests",
  "IFW_Nests_2014")@data %>%
  filter(YearLastEa == "2013") %>%
  dplyr::rename(nest_site = PK_NestSit) %>%
  dplyr::select(nest_site) %>%
  ddply(., .(nest_site), function(x) tail(x,1)) %>%  # has two 740A
  arrange(nest_site)

nests_2013_data <- nests_ifw_data %>%
  filter(nest_site %in% nests_2013$nest_site) %>%
  arrange(nest_site)

# Export CSV and RDS to "C:/Work/R/Data/BAEA/Nests" folder
write.csv(nests_2013_data, file = file.path(csv_output_dir,
  "Nests_IFW_2013_Active.csv"), row.names = FALSE)
saveRDS(nests_2013_data, file = file.path(rds_output_dir,
  "nests_ifw_2013_active.rds"))

# Export KML to "C:/ArcGIS/Data/BAEA/Nests/KMLs" folder
ExportKMLPoints(nests_2013_data, id = "nest_site",
  file = "Nests IFW Active 2103.kml", output_dir = kml_output_dir)

############################ Nests Intact ######################################

nests_intact_all <- nests_ifw_data %>% filter(intact_at_last_obs == "Yes")

# Export CSV and RDS to "C:/Work/R/Data/BAEA/Nests" folder
write.csv(nests_intact_all, file = file.path(csv_output_dir,
  "Nests_IFW_Intact_All.csv"), row.names = FALSE)
saveRDS(nests_intact_all, file = file.path(rds_output_dir,
  "nests_ifw_intact_all.rds"))

# Export KML to "C:/ArcGIS/Data/BAEA/Nests/KMLs" folder
ExportKMLPoints(nests_intact_all, id = "nest_site",
  file = "Nests Intact All.kml", output_dir = kml_output_dir)

################ Nests Intact Last Used (For Each Nest Area) ###################

nests_intact_last <- as.data.frame(nests_intact_all %>%
  group_by(nest_area) %>%
  filter(year_last_occupancy == max(year_last_occupancy)))

# Export CSV and RDS to "C:/Work/R/Data/BAEA/Nests" folder
write.csv(nests_intact_last, file = file.path(csv_output_dir,
  "Nests_IFW_Intact_Last.csv"), row.names = FALSE)
saveRDS(nests_intact_last, file = file.path(rds_output_dir,
  "nests_ifw_intact_last.rds"))

# Export KML to "C:/ArcGIS/Data/BAEA/Nests/KMLs" folder
ExportKMLPoints(nests_intact_last, id = "nest_site",
  file = "Nests Intact Last.kml", output_dir = kml_output_dir)

############################# Study Nests ######################################

nests_study_csv <- read.csv("Data/Nests/Original_Data/Study_20170325.csv")
    # Updated MusquashE and Sheepscot on 20160223
    # Updated Upper on 20160317
    # Updated Davis on 20160327
    # Updated Musquash on 20161106
    # Updated Sandy, Hebron, and Musquash on 20170325
    # Updated Sandy, Ellis, and Musquash on 20190901

nests_study <- left_join(nests_study_csv, nests_intact_all %>%
  dplyr::select(nest_site, site_name, comments:nest_comments),
    by = "nest_site") %>%
  mutate(nest_area = as.character(nest_area)) %>%
  dplyr::select(name:nest_area, intact_at_last_obs:nest_comments, long_utm:lat)

nests_study_spdf_utm84 <- SpatialPointsDataFrame(nests_study[c("long_utm",
  "lat_utm")], bbox = NULL, data = nests_study, proj4string =
  CRS("+proj=utm +zone=19 +datum=WGS84"))
nests_study_spdf_ll84 <- spTransform(nests_study_spdf_utm84,
  CRSobj = CRS("+proj=longlat +datum=WGS84"))
coords <- as.data.frame(coordinates(nests_study_spdf_ll84))
colnames(coords) <- c("long", "lat")
nests_study_spdf_ll84 <- SpatialPointsDataFrame(coords, bbox = NULL,
  data = nests_study, proj4string = CRS("+proj=longlat +datum=WGS84"))
nests_study_data <- nests_study_spdf_ll84@data %>%
  arrange(nest_area)

# Export CSV and RDS to "C:/Work/R/Data/BAEA/Nests" folder
write.csv(nests_study_data, file = file.path(csv_output_dir, "Nests_Study.csv"),
  row.names = FALSE)
saveRDS(nests_study_data, file = file.path(rds_output_dir, "nests_study.rds"))

# Export KML to "C:/ArcGIS/Data/BAEA/Nests/KMLs" folder
ExportKMLPoints(nests_study_data, id = "name", point_col_var = "name",
  point_metadata = "Data/Nests/Nests_Color_Metadata.csv",
  file = "Nests Study.kml", output_dir = kml_output_dir)

###################### Nests - Study and All Intact ############################

nests_study_intact <- nests_intact_all %>%
  mutate(name = "") %>%
  mutate(new_2015 = "") %>%
  bind_rows(., nests_study_data) %>%
  group_by(nest_site) %>%
  slice(unique(n())) %>%
  arrange(nest_site) %>%
  ungroup()

# Export CSV and RDS to "C:/Work/R/Data/BAEA/Nests" folder
write.csv(nests_study_intact, file = file.path(csv_output_dir,
  "Nests_Study_Intact.csv"), row.names = FALSE)
saveRDS(nests_study_intact, file = file.path(rds_output_dir,
  "nests_study_intact.rds"))

# Export KML to "C:/ArcGIS/Data/BAEA/Nests/KMLs" folder
ExportKMLPoints(nests_study_intact, id = "nest_site",
  file = "Nests Study Intact.kml", output_dir = kml_output_dir)

############ Nests Study and Intact Last Used (For Each Nest Area) #############

nests_study_intact_last <- as.data.frame(nests_intact_last %>%
                           mutate(name = "") %>%
                           mutate(new_2015 = "") %>%
                           bind_rows(nests_study_data, .) %>%
                           arrange(nest_site) %>%
                           group_by(nest_area) %>%
                           filter(year_last_occupancy ==
                              max(year_last_occupancy)) %>%
                           filter(year_last_monitored ==
                              max(year_last_monitored)))

# Export CSV and RDS to "C:/Work/R/Data/BAEA/Nests" folder
write.csv(nests_study_intact_last, file = file.path(csv_output_dir,
  "Nests_Study_Intact_Last.csv"), row.names = FALSE)
saveRDS(nests_study_intact_last, file = file.path(rds_output_dir,
  "nests_study_intact_last.rds"))

# Export KML to "C:/ArcGIS/Data/BAEA/Nests/KMLs" folder
ExportKMLPoints(nests_study_intact_last, id = "nest_site",
  file = "Nests Study Intact Last.kml", output_dir = kml_output_dir)

################ Create Feature Classes From All .csv Files ####################

# This script converts all of the exported .csv files in feature classes
script <- "C:/Work/Python/Scripts/gispy/Create_Nest_Features_From_CSVs.py"
system(paste0("python ", script))

#############  Manage Active Years In Nests_Study_Intact_Active  ###############

wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

nests_active <- readRDS(file.path(rds_output_dir, "nests_study_intact.rds"))
years <- c(paste0("active_", 2014:2016))
nests_active[, years] <- NA
nests_active[, c("active_2014","active_2015")] <- TRUE
nests_active[which(nests_active$nest_site == "620A"), "active_2014"] <- FALSE

### 2016 Activity
# Ellis(282A) and Neighboring Nests
nests_active[which(nests_active$nest_site == "282A"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "365B"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "720A"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "586A"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "586A"), "active_2016"] <- TRUE

# Hebron (659A) Conspecific Nests
nests_active[which(nests_active$nest_site == "659A"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "434C"), "active_2016"] <-
  TRUE #Wilson (location not exact)
nests_active[which(nests_active$nest_site == "648A"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "543A"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "380B"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "509C"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "301C"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "377A"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "704A"), "active_2016"] <- TRUE

#Sandy (423A) Conspecific Nests
nests_active[which(nests_active$nest_site == "423R01"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "270F"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "270F"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "271B"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "336A"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "731A"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "540B"), "active_2016"] <- TRUE

#Musquash (446R01) Conspecific Nests
nests_active[which(nests_active$nest_site == "446R01"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "729A"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "083B"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "260B"), "active_2016"] <- TRUE
nests_active[which(nests_active$nest_site == "234A"), "active_2016"] <- TRUE

# Convert to SpatialPointsDataFrame
nests_active_spdf <- SpatialPointsDataFrame(nests_active[c("long_utm",
  "lat_utm")], bbox = NULL, data = nests_active, proj4string = wgs84n19)

# IMPORTANT TO RUN THIS !!!
# Export CSV and RData File to "C:/Work/R/Data/BAEA/Nests" folder --------------
write.csv(nests_active, file = file.path(csv_output_dir, "Nests_Active.csv"),
  row.names = FALSE)
saveRDS(nests_active, file = file.path(rds_output_dir, "nests_active.rds"))
saveRDS(nests_active_spdf, file = file.path(rds_output_dir,
  "nests_active_spdf.rds"))

# This script updates the Nests_Study_Intact_Last feature class from .csv file
script <- "C:/Work/Python/Scripts/BAEA_GIS/Update_Nests_Study_Intact_Active.py"
system(paste0("python ", script))

###################  Export Files In Various Formats  ##########################

library(sp)
library(raster)

nests_ifw <- ConvertNestIdToNum(nests_ifw)

# Export RASTER
ifw_nests_30mc <- CreateRasterFromPointsAndBase(nests_ifw, value ="nest_id_num",
  x = "long_utm", y = "lat_utm")
writeRaster(ifw_nests_30mc, filename = file.path("C:/ArcGIS/Data/BAEA",
  "Nests/IFW_Nests/IFW_nests_30mc.tif"), format = "GTiff", overwrite = TRUE)

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#
#
# Test ConvertNestIdToNum and ConcertNestNumToId --------------------------- ##
# test_nests<- data.frame(nest_id = c("001A", "052B", "681A", "620S01", "681R02",
#   "681S01", "1013R01", "1013R01"), stringsAsFactors = FALSE)
# test_nests <- ConvertNestIdToNum(test_nests)
# test_nests$nest_id1 <- test_nests$nest_id
# test_nests$nest_id <- NULL
# test_nests <- ConvertNestNumToId(test_nests)
# test_nests
# identical(test_nests$nest_id1, test_nests$nest_id)

# Load packages
pacman::p_load(gisr, baear, cartography, dplyr, fasterize, ggplot2, ggthemes,
  grid, leaflet, magick, mapview, OpenStreetMap, plotly, prettymapr, purrr,
  raster, rosm, rsvg, sf, stars, stringr, tmap, tmaptools, viridis, units,
  webshot, zoo)
pacman::p_load(baear, gisr, ibmr)
suppressMessages(extrafont::loadfonts(device="win"))
options(stringsAsFactors = FALSE)
theme_update(plot.title = element_text(hjust = 0.5))

# Coordinate systems
wgs84 <- CRS("+init=epsg:4326") # WGS84 Lat/Long
wgs84n19 <- CRS("+init=epsg:32619") # WGS84 UTM 19N

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

# Rasters
base <- raster(file.path("C:/ArcGIS/Data/BlankRaster/maine_30mc.tif"))

# Directories
nests_dir <- file.path("C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm",
  "Data/Nests/Nests_rds")
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"
maps_dir <- "C:/Users/blake/OneDrive/Work/R/Projects/baea_ibm/Products/Maps"
ssf_prob_dir <- "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters/Step_Types_Prob"

# Nests
nests_study_org <- readRDS(file.path(nests_dir, "nests_study.rds"))
nests_study <- st_as_sf(x = nests_study_org, coords = c("long", "lat"),
  crs = "+proj=longlat +datum=WGS84") %>%
  mutate(x = long_utm, y = lat_utm) %>%
  filter(!name %in% c("Davis", "Upper"))  %>% st_transform(crs = 32619)

# Maine Outline
maine <- read_sf(file.path("C:/ArcGIS/Data/BlankPolygon/MaineOutline.shp")) %>%
  st_transform(., crs = 4326) %>%
  mutate(state = "Maine")  %>%
  dplyr::select(state)

tmap_mode("plot")

################################################################################

ssf_prob_files <- list.files(ssf_prob_dir, pattern = "\\.tif", full.names = TRUE)
ssf_tmap_list <- vector(mode = "list", length = length(ssf_prob_files))

# For Individual Maps
for (i in seq_len(length(ssf_prob_files))){
  print(i)
  ssf_prob_i <- read_stars(ssf_prob_files[i])
  if (i ==  1){
    # Use "Tmap_baselayers.R" script to get other baselayers
    maine_bb_sf <- st_as_sfc(bb(maine, relative = TRUE, height = 1.15, width = 1.35))
    maine_om = read_osm(maine_bb_sf, zoom = 7, minNumTiles = 9, type = om_nat_geo)
  }
  step_type_numeric <- basename(tools::file_path_sans_ext(ssf_prob_files[i]))
  step_type_text <- step_type_numeric %>%
    str_replace_all("1", "Cruise") %>%
    str_replace_all("2", "Flight") %>%
    str_replace_all("3", "Nest") %>%
    str_replace_all("4", "Perch") %>%
    str_replace_all("5", "Roost")
  step_type_arrow <- step_type_text %>%
    str_replace_all("_", "$\\\\rightarrow$ ") %>%
    latex2exp::TeX(.)
  ssf_prob_i_map <-
    tm_shape(maine_om) +
      tm_rgb() +
    tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1),
        alpha = .8,
        legend.reverse = TRUE, style = "cont", title = "Probability") +
    tm_scale_bar(breaks = c(0, 50, 100), text.size = .75,
      position = c(.75, .01)) +
    tm_compass(type = "4star",  show.labels = 1, size = 3,
      position = c(.85, .75)) +
    tm_grid(n.x = 4, n.y = 5, projection = 4326, col = "grey85", alpha = .75,
      labels.col = "grey25", labels.format = list(format = "f", big.mark = ""),
      labels.inside.frame = FALSE) +
    tm_shape(maine) + tm_borders(col = "black") +  # ME outline overlays grid
    tm_shape(nests_study %>% filter(nest_site != "446R01")) +
    tm_symbols(shape = 8, col = "black", size = .4) +
    tm_layout(#asp = .75,
      title.bg.color = "white",
      title.position = c("left", "top"),
      title.fontfamily = "Latin Modern Roman",
      title = step_type_arrow,
      #title.size = ,
      title.snap.to.legend = FALSE,
      legend.position = c(.80,.10),
      legend.outside = FALSE,
      legend.bg.color = "white",
      legend.title.fontfamily = "Latin Modern Roman",
      legend.text.fontfamily = "Latin Modern Roman",
      frame = FALSE) #+ tm_credits(step_type_arrow, position=c("right", "top"))
  ssf_prob_i_map
  tmap_save(tm = ssf_prob_i_map, filename = file.path(tex_dir, "Figures/Ch2",
    "SSF_Prob_Raster_Maps", paste0("SSF_Probability_Map_", step_type_text,
    ".svg")), unit = "in", dpi = 300, height = 8, width = 6)
}

# For All Maps Together
for (i in seq_len(length(ssf_prob_files))){
  print(i)
  ssf_prob_i <- read_stars(ssf_prob_files[i])
  step_type_numeric <- basename(tools::file_path_sans_ext(ssf_prob_files[i]))
  step_type <- step_type_numeric %>%
    str_replace_all("1", "Cruise") %>%
    str_replace_all("2", "Flight") %>%
    str_replace_all("3", "Nest") %>%
    str_replace_all("4", "Perch") %>%
    str_replace_all("5", "Roost") %>%
    str_replace_all("_", "$\\\\rightarrow$ ") %>%
    latex2exp::TeX(.)
  ssf_prob_i_map <-
    tm_shape(ssf_prob_i, raster.downsample = FALSE) +
      tm_raster(palette = viridis(20, direction = 1),
        legend.reverse = TRUE, style = "cont", title = "Probability") +
      tm_layout(asp = .75,
        title.position = c("LEFT", "TOP"),
        title.fontfamily = "Latin Modern Roman",
        title = step_type,
        title.size = .5,
        title.snap.to.legend =  FALSE,
        legend.position = c("RIGHT", "BOTTOM"),
        legend.height = .3,
        legend.title.size = .65,
        legend.title.fontfamily = "Latin Modern Roman",
        legend.text.fontfamily = "Latin Modern Roman",
        frame = FALSE)
  ssf_prob_i_map
  ssf_tmap_list[[i]] <- ssf_prob_i_map
}

tmap_blank <-
  tm_shape(ssf_prob_i, raster.downsample = TRUE) +
  tm_raster(palette = "white", style = "cont") +
  tm_layout(asp = .75, legend.show = FALSE, frame = FALSE)

# TEST arrangement and position of main.title, legend, etc.
ssf_tmap_arrange_test <- tmap_arrange(
  tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, ssf_tmap_list[[10]], tmap_blank,
  tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  tmap_blank, tmap_blank, tmap_blank, tmap_blank,
  ncol = 4)

tmap_save(tm = ssf_tmap_arrange_test, filename =  file.path("C:/TEMP",
  "SSF_Probability_Maps_Overview.svg"), unit = "in", dpi = 300, height = 8,
  width = 6)

ssf_tmap_arrange <- tmap_arrange(
  ssf_tmap_list[[1]], ssf_tmap_list[[2]], ssf_tmap_list[[3]], tmap_blank,
  ssf_tmap_list[[4]], ssf_tmap_list[[5]], ssf_tmap_list[[6]], ssf_tmap_list[[7]],
  ssf_tmap_list[[8]], ssf_tmap_list[[9]], ssf_tmap_list[[10]],ssf_tmap_list[[11]],
  ssf_tmap_list[[12]], ssf_tmap_list[[13]], ssf_tmap_list[[14]], ssf_tmap_list[[15]],
  tmap_blank, ssf_tmap_list[[16]], ssf_tmap_list[[17]], tmap_blank,
  ncol = 4)

tmap_save(tm = ssf_tmap_arrange, filename =  file.path("C:/TEMP",
  "SSF_Probability_Maps_Overview.svg"), unit = "in", dpi = 300, height = 8,
  width = 6)

tmap_save(tm = ssf_tmap_arrange, filename = file.path(tex_dir, "Figures/Ch3",
  "SSF_Probability_Maps_Overview.svg"), unit = "in", dpi = 300, height = 8,
  width = 6)

#### ------------------ SSF Maps (WORK IN PROGRESS) ----------------------------

# Directories and files
mod_fit_dir = "Output/Analysis/SSF/Models"
ssf_raster_dir = "C:/ArcGIS/Data/R_Input/BAEA/SSF_Rasters"
covars_crop_dir = file.path(ssf_raster_dir, "Covars_Crop")
step_type_dir = file.path(ssf_raster_dir, "Step_Type")
maine_raster_trim_file = "C:/ArcGIS/Data/BlankRaster/maine_trim.tif"

# ESRI Baselayers
esri_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
esri_tile <- "/MapServer/tile/{z}/{y}/{x}"
om_nat_geo <- paste0(esri_url, "NatGeo_World_Map", esri_tile)

ssf_layers <- list.files(file.path(ssf_raster_dir, "Step_Types_Rescale_Prob"),
  pattern = "tif$")
nests <- ssf_layers %>%
  str_split(., pattern = "_") %>%
  sapply("[", 1) %>%
  unique(.)

i <- nests[1] # FOR TESTING
for (i in nests){
  nest_ssfs_i <- str_subset(ssf_layers, i)
  for (j in nest_ssfs_i){
    j <- nest_ssfs_i[1] # FOR TESTING
    ssf_i_j <- read_stars(file.path(ssf_raster_dir, "Step_Types_Rescale_Prob",
      j))
    if (j ==  nest_i_rasters[1]){
      bb_nest_i <- st_as_sf(ssf_i_j)
      bb_nest_i_om = read_osm(bb_nest_i, type = om_nat_geo)
      #zoom = 13, minNumTiles = 21,
    }

    # NEED TO ADD TO MAP BELOW (Compass, Scales, etc.)

    tm_shape(bb_nest_i_om, raster.downsample = FALSE) +
      tm_rgb() +
    tm_shape(ssf_i_j, raster.downsample = FALSE) +
      tm_raster(n = 20)

    # NEED TO COMBINE MAPS (either by step_type or by est

  }
}



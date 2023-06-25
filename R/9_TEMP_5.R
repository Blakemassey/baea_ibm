
# Load Data --------------------------------------------------------------------

move_pars_file <- "Output/Analysis/Movements/move_pars.rds"
move_pars <- readRDS(file = move_pars_file) %>%
  as.data.frame(.)
move_pars_i <- move_pars %>%
  filter(behavior_behavior == "Cruise -> Cruise")

move_kernel_i <- CreateMoveKernelWeibullVonMises(
  max_r = NULL,
  cellsize = 30,
  mu1 = move_pars_i$mvm_mu1[1],
  mu2 = move_pars_i$mvm_mu2[1],
  kappa1 = move_pars_i$mvm_kappa1[1],
  kappa2 = move_pars_i$mvm_kappa2[1],
  mix = move_pars_i$mvm_prop[1],
  shape = move_pars_i$weibull_shape[1],
  scale = move_pars_i$weibull_scale[1],
  ignore_von_mises = FALSE)

r <- (30*((nrow(move_kernel_i)-1)/2))+(30/2)
move_raster <- raster::raster(move_kernel_i, xmn=-r, xmx=r, ymn=-r, ymx=r)
move_raster_df <- move_raster %>%
  as.data.frame(xy = TRUE)
ggplot(move_raster_df) +
  geom_raster(aes(x = x, y = y, fill = layer))

# Nest conspecific and home nests raster
con_nest_raster <- raster("Output/Analysis/Territorial/2016/ConNest_Ellis.tif")
mapview::mapview(con_nest_raster)

# Ellis nest
nest_x <- 367655
nest_y <- 4946605

ellis_df <- tibble(nest_x, nest_y, nest = "Ellis")
ellis_sf = st_as_sf(ellis_df, coords = c("nest_x", "nest_y"),
  crs = st_crs(con_nest_raster), remove = FALSE)

# Variables
gamma_shape <- 0.420955
gamma_rate <- 0.2358088
y_min <- 0.001
y_max <- 0.999
y_min_new <- -50
y_max_new <- -2

# Calculate x and y distance (based on equal x and y distance from nest)
dist_m <- 15000
side_m <- sqrt((dist_m^2)/2)

# Eagle location
x_org <- nest_x - side_m
y_org <- nest_y - side_m

x <- CenterXYInCell(x_org, y_org, raster::xmin(base), raster::ymin(base),
  raster::res(base)[1])[1]
y <- CenterXYInCell(x_org, y_org, raster::xmin(base), raster::ymin(base),
  raster::res(base)[1])[2]

point_df <- tibble(x, y, eagle = "Ellis")
point_sf = st_as_sf(point_df, coords = c("x", "y"),
  crs = st_crs(con_nest_raster), remove = FALSE)

# Process to get the con_dist raster and the territorial probability layer
cellsize <- raster::res(con_nest_raster)[1]
move_org <- move_raster
move_kernel <- raster::shift(move_org, dx = x, dy = y)
raster_extent <- raster::extent(move_kernel)

con_nest_crop <- raster::crop(con_nest_raster, raster_extent, snap = 'in')
mapview::mapview(con_nest_crop)

xy <- CenterXYInCell(x, y, raster::xmin(base), raster::ymin(base),
  raster::res(base)[1])  # May be unnecessary
xy_pt <- data.frame(x = xy[1], y = xy[2])
xy_con_nest <- raster::extract(con_nest_crop, xy_pt)
#xy_con_nest <- 5000
con_nest_centered <- raster::calc(con_nest_crop,
  fun = function(x){(x - xy_con_nest)/1000})
mapview::mapview(con_nest_centered)
con_nest_centered_ext <- raster::extend(con_nest_centered, move_kernel, value = NA)
con_nest_centered_crop <- raster::crop(con_nest_centered_ext, move_kernel, snap = "in")
con_nest_centered_mask <- raster::mask(con_nest_centered_crop, move_kernel, value = NA)
mapview::mapview(con_nest_centered_mask)

y_diff_new <- y_max_new - y_min_new
y_pgamma <- pgamma(xy_con_nest/1000, shape = gamma_shape, rate = gamma_rate)
y_log_scale <- y_min_new + (((y_pgamma-y_min)/(y_max-y_min)) * (y_diff_new))
LogisticByInflection2 <- function(x){
  x <- LogisticByInflection(x, inflection = 0, scale = y_log_scale)
}
con_nest_prob <- raster::calc(con_nest_centered, fun = LogisticByInflection2)
mapview::mapview(con_nest_prob)

con_nest_ext <- raster::extend(con_nest_prob, move_kernel, value = NA)
con_nest_crop <- raster::crop(con_nest_ext, move_kernel, snap = "in")
con_nest_mask <- raster::mask(con_nest_crop, move_kernel, value = NA)
mapview::mapview(con_nest_mask)

con_nest_kernel <- con_nest_mask/raster::cellStats(con_nest_mask,
  stat = "sum")
con_nest_kernel[con_nest_kernel == 0] <- NA
mapview::mapview(con_nest_kernel)
if(TRUE) plot(con_nest_kernel, colNA = "black")

kernel_bb_sf <- st_as_sfc(bb(con_nest_kernel, relative = TRUE, height = 1.35,
  width = 1.35))
Sys.sleep(1)
kernel_om = read_osm(kernel_bb_sf, type = om_nat_geo, zoom = 11) #minNumTiles=9,
kernel_om_bb <- bb_poly(kernel_om)

kernel_map <-
  tm_shape(kernel_om) +
  tm_rgb() +
  tm_shape(con_nest_mask, raster.downsample = FALSE) +
  tm_raster(palette = turbo(20, direction = -1), alpha = .6,
    legend.reverse = TRUE, style = "cont", title = "Probability") +
  tm_compass(type = "4star", show.labels = 1, size = 2.5, text.size = 1.25,
    position = c(.835, .835)) +
  tm_scale_bar(text.size = 1.25, breaks = c(0, 5, 10),
    position = c(.4, -0.01)) +
  tm_shape(point_sf) +
  tm_symbols(shape = 21, border.col = "black",
    col = "gray50", border.lwd = 2,  size = .35) +
  tm_shape(ellis_sf) +
  tm_symbols(shape = 21, border.col = "black",
    col = nest_color, border.lwd = 2,  size = .35) +
  tm_layout(asp = 1, fontfamily = "Latin Modern Roman") +
  tm_legend(show = FALSE)

kernel_map_file <- file.path("C:/TEMP/TEMP_Images",
  "Kernel_Map.png")
tmap_save(tm = kernel_map, filename = kernel_map_file, unit = "in",
  dpi = 300, height = 6, width = 6)

legend_only <- tm_shape(kernel_om) +
  tm_rgb() +
  tm_shape(con_nest_mask, raster.downsample = FALSE) +
  tm_raster(palette = turbo(20, direction = -1), alpha = .6,
    legend.reverse = TRUE, style = "cont", title = "Probability") +
  tm_legend(legend.only = TRUE,
    title.size = 1.3,
    text.size = 1.1,
    position = c("LEFT", "TOP"),
    frame = FALSE,
    legend.bg.color = "white",
    legend.format = list(format = "f", big.mark = ""),
    fontfamily = "Latin Modern Roman")
legend_file <- file.path("C:/TEMP/TEMP_Images",
  "Kernel_Map_Legend.png")
tmap_save(tm = legend_only, filename = legend_file, unit = "in", dpi = 300,
  height = 4, width = 2.25)

kernel_map_img <- kernel_map_file %>%
  image_read(.) %>%
  image_trim(.)

legend_img <- legend_file %>% image_read(.) %>% image_trim(.)
legend_title <- legend_img %>% image_crop(., "400x70+0+0")
legend_title
legend_scale <- legend_img %>% image_crop(., "250x550+0+80")
legend_scale

backgrd <- image_blank(2150, 1730, color = "white")

kernel_map_fig <- backgrd %>%
  image_composite(., kernel_map_img, offset = "+0+0") %>%
  image_composite(., legend_title, offset = "+1765+665") %>%
  image_composite(., legend_scale, offset = "+1800+740") %>%
  image_flatten(.)
kernel_map_fig

# Export PNG
# IMPORTANT NOTE!!!!  This has to be updated based on the distance value above
con_nest_fig_png_file = file.path(tex_dir, "Figures/Ch3",
  "ConNest_5km_Probability_Map.png")
image_write(kernel_map_fig, path = con_nest_fig_png_file, format = "png")

# Map of Recentered Distance Map
recentered_map <-
  tm_shape(kernel_om) +
  tm_rgb() +
  tm_shape(con_nest_centered_mask, raster.downsample = FALSE) +
  tm_raster(palette = turbo(20, direction = -1), alpha = .6,
    legend.reverse = TRUE, style = "cont", title = "Probability", midpoint = NA) +
  tm_compass(type = "4star", show.labels = 1, size = 2.5, text.size = 1.25,
    position = c(.835, .835)) +
  tm_scale_bar(text.size = 1.25, breaks = c(0, 5, 10),
    position = c(.4, -0.01)) +
  tm_shape(point_sf) +
  tm_symbols(shape = 21, border.col = "black",
    col = "gray50", border.lwd = 2,  size = .35) +
  tm_shape(ellis_sf) +
  tm_symbols(shape = 21, border.col = "black",
    col = nest_color, border.lwd = 2,  size = .35) +
  tm_layout(asp = 1, fontfamily = "Latin Modern Roman") +
  tm_legend(show = TRUE)




    # Calculate probabilty raster - cruise/flight to cruise
    if (behavior_trans %in% c("1_1", "2_1")){
      kernel_weights <- c(1, 3, 2)
      destination_cells_n <- 25
    }
    # Calculate probabilty raster - cruise/flight to flight
    if (behavior_trans %in% c("1_2", "2_2")){
      kernel_weights <- c(4, 2, 1)
      destination_cells_n <- 25
    }
    # Calculate probabilty raster - perch/roost to cruise/flight
    if (behavior_trans %in% c("3_1", "4_1", "3_2", "4_2", "5_2")){
      kernel_weights <- c(4, 2, 1)
      destination_cells_n <- 25
    }
    # Calculate probabilty raster - air to perch/roost
    if (behavior_trans %in% c("1_4", "2_4", "2_5")){
      kernel_weights <- c(5, 1, 3)
      destination_cells_n <- 1000
    }
    # Calculate probabilty raster - nest to perch/roost
    if (behavior_trans %in% c("3_4", "3_5")){
      kernel_weights <- c(5, 2, 3)
      destination_cells_n <- 1000
    }
    # Calculate probabilty raster - perch/roost to perch
    if (behavior_trans %in% c("4_4", "5_4", "4_5")){
      kernel_weights <- c(5, 2, 1)
      destination_cells_n <- 1000
    }

    kernel_stack <- raster::stack(list(ssf_kernel, move_kernel,
      con_nest_kernel))
    WeightedGeoMean <- function(x){
      geomeans <- geometric_mean(x, w = kernel_weights, na.rm = TRUE)
      return(geomeans)
    }
    kernel_geomeans <- raster::calc(kernel_stack, fun = WeightedGeoMean)
    prob_raster <- kernel_geomeans * maine_outline_kernel * land_kernel
    prob_raster <- prob_raster/raster::cellStats(prob_raster, stat = "sum")
    if(plotting) plot(prob_raster, colNA = "black")

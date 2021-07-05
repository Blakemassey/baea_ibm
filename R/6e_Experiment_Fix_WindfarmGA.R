# Had to updates these two functions because the latest version of sf & s2
# caused windfarmGA to break "https://github.com/YsoSirius/windfarmGA/issues/27"

ConvertBestResultsToSF <- function(df){
  sf_out <- df %>%
    as_tibble(.) %>%
    pluck("bestPaEn") %>%
    map(., as_tibble) %>%
    tibble(id = 1:length(.), data = .) %>%
    unnest(data) %>%
    dplyr::select(id, X, Y, EnergyOverall) %>%
    group_by(id) %>%
    nest(locations = c("X", "Y")) %>%
    arrange(., by = desc(EnergyOverall)) %>%
    ungroup() %>%
    slice(1) %>%
    unnest(., cols = locations) %>%
    st_as_sf(., coords = c("X", "Y"), crs = st_crs(wgs84n19))
  return(sf_out)
}

grid_area_fixed <- function(shape,
                      size = 500,
                      prop = 1,
                      plotGrid = FALSE) {
  if (prop < 0.01) prop <- 0.01
  if (prop > 1) prop <- 1
  grid_polys <- sf::st_make_grid(shape, cellsize = size, what = "polygons")
  grid_intersect <- sf::st_intersection(st_geometry(shape),
    grid_polys)
  areadrygrid <- sf::st_area(grid_intersect)
  indx <- as.numeric((areadrygrid/size^2)) >= prop
  if (!any(indx)) {
    cat("\n################### GA ERROR MESSAGE ###################\n")
    stop("A grid cannot be drawn. Reduce the `size` argument ",
      "or define a projection in meters.")
  }
  grid_filtered <- grid_intersect[indx]  #THIS IS WHERE THE NEW CODE IS!!!
  grid_centr <- sf::st_centroid(grid_filtered)
  centpo <- st_coordinates(grid_centr)
  centpo <- cbind(ID = 1:nrow(centpo), X = centpo[, 1], Y = centpo[, 2])
  if (plotGrid) {
    par_grid <- par(ask = FALSE, no.readonly = TRUE)
    on.exit(par_grid)
    plot.new()
    par(mar = c(5, 5, 5, 4), mfrow = c(1, 1))
    plot(st_geometry(shape), col = "orange", main = paste("Cellsize:",
      size, "m and prop: ", prop, "\nTotal area:",
      round(sum(as.numeric(sf::st_area(shape))) * 1e-06, 3), "km^2",
      "\nNumber of Grids:", length(grid_filtered), "\nGrid area:",
      round(sum(as.numeric(sf::st_area(grid_filtered))) * 1e-06, 3), "km^2"))
    plot(grid_filtered, col = "lightgreen", add = TRUE)
    graphics::points(centpo[, "X"], centpo[, "Y"], col = "blue", pch = 20)
    graphics::text(centpo[, "X"], centpo[, "Y"],
      labels = centpo[, "ID"], pos = 2)
  }
  invisible(list(centpo, grid_filtered))
}

hexa_area_fixed <- function(shape,
                      size = 500,
                      plotGrid = FALSE){
  grid_polys <- sf::st_make_grid(shape, cellsize = size, what = "polygons",
    square = FALSE)
  grid_intersect <- sf::st_intersection(st_geometry(shape), grid_polys)
  areadrygrid <- sf::st_area(grid_intersect)
  indx <- as.numeric(areadrygrid) >= (2 * sqrt(3) * (size/2)^2) * 0.99
  if ((!any(indx)) || (length(grid_polys) == 1)) {
    cat("\n################### GA ERROR MESSAGE ###################\n")
    stop("A grid cannot be drawn. Reduce the `size` argument ",
      "or define a projection in meters.")
  }
  grid_filtered <- grid_polys[indx] #THIS IS WHERE THE NEW CODE IS!!!
  grid_centr <- sf::st_centroid(grid_filtered)
  centpo <- st_coordinates(grid_centr)
  centpo <- cbind(ID = 1:nrow(centpo), X = centpo[, 1], Y = centpo[, 2])
  if (plotGrid) {
    par_grid <- par(ask = FALSE, no.readonly = TRUE)
    on.exit(par_grid)
    plot.new()
    par(mar = c(5, 5, 5, 4), mfrow = c(1, 1))
    plot(st_geometry(shape), col = "orange", main = paste("Cellsize:", size,
      "m", "\nTotal area:", round(sum(as.numeric(st_area(st_geometry(shape)))) *
      1e-06, 3), "km^2", "\nNumber of Grids:", length(grid_filtered),
      "\nGrid area:", round(sum(as.numeric(st_area(grid_filtered))) * 1e-06, 3),
      "km^2"))
    plot(grid_filtered, col = "lightgreen", add = TRUE)
    graphics::points(centpo[, "X"], centpo[, "Y"], col = "blue", pch = 20)
    graphics::text(centpo[, "X"], centpo[, "Y"], labels = centpo[, "ID"],
      pos = 2, cex = 0.8)
  }
  invisible(list(centpo, grid_filtered))
}

genetic_algorithm_fixed <- function (Polygon1, GridMethod, Rotor, n, fcrR,
  referenceHeight, RotorHeight, SurfaceRoughness, Proportionality, iteration,
  mutr, vdirspe, topograp, elitism, nelit, selstate, crossPart1,
  trimForce, Projection, sourceCCL, sourceCCLRoughness, weibull,
  weibullsrc, Parallel, numCluster, verbose = FALSE, plotit = FALSE)
{
    if (plotit) {
        oldpar <- graphics::par(no.readonly = TRUE)
        on.exit(par(oldpar))
        plot.new()
        graphics::par(ask = FALSE)
    }
    if (missing(fcrR)) {
        fcrR <- 5
    }
    if (missing(topograp)) {
        topograp <- FALSE
    }
    if (missing(GridMethod)) {
        GridMethod <- "Rectangular"
    }
    if (missing(Parallel)) {
        Parallel <- FALSE
    }
    if (missing(numCluster)) {
        numCluster <- 2
    }
    if (missing(weibull)) {
        weibull <- FALSE
    }
    if (missing(selstate)) {
        selstate <- "FIX"
    }
    if (missing(crossPart1)) {
        crossPart1 <- "EQU"
    }
    if (missing(SurfaceRoughness)) {
        SurfaceRoughness <- 0.3
    }
    if (missing(Proportionality)) {
        Proportionality <- 1
    }
    if (missing(mutr)) {
        mutr <- 0.008
    }
    if (missing(elitism)) {
        elitism <- TRUE
        if (missing(nelit)) {
            nelit <- 7
        }
    }
    if (missing(trimForce)) {
        trimForce <- FALSE
    }
    if (missing(RotorHeight)) {
        stop("The variable 'RotorHeight' is not defined. Assign the turbine heights to 'RotorHeight'.")
    }
    if (missing(referenceHeight)) {
        referenceHeight <- RotorHeight
    }
    if (missing(iteration)) {
        iteration <- 20
    }
    if (missing(Projection)) {
        if (utils::compareVersion(sf::sf_extSoftVersion()[[3]],
            "6") > 0) {
            ProjLAEA <- 3035
        }
        else {
            ProjLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
        }
    }
    else {
        ProjLAEA <- Projection
    }
    if (missing(vdirspe)) {
        stop("No Winddata is given.")
    }
    if (missing(n)) {
        stop("The variable 'n' is not defined. Assign the number of turbines to 'n'.")
    }
    if (missing(Rotor)) {
        stop("The variable 'Rotor' is not defined. Assign the rotor radius to 'Rotor'.")
    }
    selstate <- toupper(selstate)
    crossPart1 <- toupper(crossPart1)
    Polygon1 <- isSpatial(Polygon1, ProjLAEA)
    if (is.na(st_crs(Polygon1))) {
        stop("The input area is not projected.")
    }
    resol2 <- fcrR * Rotor
    CrossUpLimit <- getOption("windfarmGA.max_population")
    if (Parallel) {
        max_cores <- parallel::detectCores()
        if (numCluster > max_cores) {
            warning("Maximum number of cores is: ", max_cores,
                "\n'numCluster' will be set to: ", max_cores -
                  1)
            numCluster <- max_cores - 1
        }
        type_cluster <- "PSOCK"
        cl <- parallel::makeCluster(numCluster, type = type_cluster)
        doParallel::registerDoParallel(cl)
    }
    if (weibull) {
        if (verbose) {
            cat("\nWeibull Distribution is used.")
        }
        if (missing(weibullsrc)) {
            if (verbose)
                cat("\nWeibull Information from package is used.\n")
            if (!file.exists("k120_100m_Lambert.tif") &&
                !file.exists("a120_100m_Lambert.tif")) {
                download.file("http://github.com/YsoSirius/windfarm_data/raw/master/weibulldata.zip",
                  destfile = "weibulldata.zip", method = "auto")
                unzip("weibulldata.zip")
                unlink("weibulldata.zip")
            }
            k_weibull <- raster("k120_100m_Lambert.tif")
            a_weibull <- raster("a120_100m_Lambert.tif")
        }
        else {
            if (verbose) {
                cat("\nWeibull Information from input is used.\n")
            }
            k_weibull <- weibullsrc[[1]]
            a_weibull <- weibullsrc[[2]]
        }
        shape_project <- st_transform(Polygon1, crs = st_crs(a_weibull))
        k_par_crop <- raster::crop(x = k_weibull, y = raster::extent(shape_project))
        a_par_crop <- raster::crop(x = a_weibull, y = raster::extent(shape_project))
        weibl_k <- raster::mask(x = k_par_crop, mask = shape_project)
        weibl_a <- raster::mask(x = a_par_crop, mask = shape_project)
        estim_speed_raster <- weibl_a * (gamma(1 + (1/weibl_k)))
        estim_speed_raster <- raster::projectRaster(estim_speed_raster,
            crs = raster::crs(Polygon1))
    }
    else {
        estim_speed_raster <- FALSE
    }
    if (crossPart1 != "EQU" & crossPart1 != "RAN") {
        crossPart1 <- readinteger()
    }
    if (selstate != "FIX" & selstate != "VAR") {
        selstate <- readintegerSel()
    }
    inputData <- list(Input_Data = rbind(Rotorradius = Rotor,
        `Number of turbines` = n, `Grid Shape Factor` = fcrR,
        Iterations = iteration, `Mutation Rate` = mutr,
        `Percentage of Polygon` = Proportionality, Topographie = topograp,
        Elitarism = elitism, `Selection Method` = selstate,
        `Trim Force Method Used` = trimForce, `Crossover Method Used` = crossPart1,
        `Reference Height` = referenceHeight, `Rotor Height` = RotorHeight,
        Resolution = resol2, `Parallel Processing` = Parallel,
        `Number Clusters` = numCluster, `Active Weibull` = weibull,
        `Grid Method` = GridMethod, Projection = ProjLAEA))
    inputWind <- list(Windspeed_Data = vdirspe)
    if (verbose) {
        print(inputData)
        print(inputWind)
    }
    winddata <- windata_format(vdirspe)
    if (utils::compareVersion(sf::sf_extSoftVersion()[[3]], "6") >
        0) {
        if (suppressWarnings(!isTRUE(all.equal(st_crs(Polygon1),
            st_crs(ProjLAEA))))) {
            Polygon1 <- sf::st_transform(Polygon1, ProjLAEA)
        }
    }
    else {
        if (as.character(raster::crs(Polygon1)) != ProjLAEA) {
            Polygon1 <- sf::st_transform(Polygon1, ProjLAEA)
        }
    }
    GridMethod <- toupper(GridMethod)
    if (GridMethod != "HEXAGON" & GridMethod != "H") {
        Grid1 <- grid_area_fixed(Polygon1, resol2, Proportionality)
        Grid <- Grid1[[1]]
        grid_filtered <- Grid1[[2]]
    }
    else {
        Grid1 <- hexa_area_fixed(Polygon1, resol2)
        Grid <- Grid1[[1]]
        grid_filtered <- Grid1[[2]]
    }
    n_gridcells <- nrow(Grid)
    nStart <- (n_gridcells * n)/iteration
    if (nStart < 100) {
        nStart <- 100
    }
    if (nStart > 300) {
        nStart <- 300
    }
    nStart <- ceiling(nStart)
    startsel <- init_population(Grid, n, nStart)
    maxParkwirkungsg <- 0
    allparkcoeff <- vector("list", iteration)
    bestPaEn <- vector("list", iteration)
    bestPaEf <- vector("list", iteration)
    fuzzycontr <- vector("list", iteration)
    fitnessValues <- vector("list", iteration)
    nindiv <- vector("list", iteration)
    clouddata <- vector("list", iteration)
    selcross <- vector("list", iteration)
    beorwor <- vector("list", iteration)
    mut_rate <- vector("list", iteration)
    allCoords <- vector("list", iteration)
    if (!topograp) {
        if (verbose) {
            cat("Topography and orography are not taken into account.\n")
        }
        srtm_crop <- ""
        cclRaster <- ""
    }
    else {
        if (verbose) {
            cat("Topography and orography are taken into account.\n")
        }
        if (plotit) {
            par(mfrow = c(3, 1))
        }
        if (missing(sourceCCL)) {
            message("\nNo land cover raster ('sourceCCL') was given. It will be downloaded from ",
                "the EEA-website.\n")
            if (!file.exists("g100_06.tif")) {
                download.file("http://github.com/YsoSirius/windfarm_data/raw/master/clc.zip",
                  destfile = "clc.zip", method = "auto")
                unzip("clc.zip")
                unlink("clc.zip")
            }
            ccl <- raster::raster("g100_06.tif")
        }
        else {
            ccl <- raster::raster(sourceCCL)
        }
        cclPoly <- raster::crop(ccl, Polygon1)
        cclPoly1 <- raster::mask(cclPoly, Polygon1)
        Polygon_wgs84 <- sf::st_transform(Polygon1, st_crs(4326))
        srtm <- tryCatch(elevatr::get_elev_raster(verbose = verbose,
            locations = as(Polygon_wgs84, "Spatial"), z = 11),
            error = function(e) {
                stop("\nDownloading Elevation data failed for the given Polygon.\n",
                  e, call. = FALSE)
            })
        srtm_crop <- raster::crop(srtm, Polygon_wgs84)
        srtm_crop <- raster::mask(srtm_crop, Polygon_wgs84)
        srtm_crop <- raster::projectRaster(srtm_crop, crs = raster::crs(Polygon1))
        if (plotit) {
            raster::plot(srtm_crop, main = "Elevation Data")
            plot(Polygon1, add = TRUE, color = "transparent")
            plot(grid_filtered, add = TRUE)
        }
        roughrast <- raster::terrain(srtm_crop, "roughness")
        if (all(is.na(raster::values(roughrast)))) {
            warning("Cannot calculate a surface roughness. \nMaybe the resolution or ",
                "the area is too small. Roughness values are set to 1.\n")
            raster::values(roughrast) <- 1
        }
        srtm_crop <- list(strm_crop = srtm_crop, orogr1 = raster::calc(srtm_crop,
            function(x) {
                x/(raster::cellStats(srtm_crop, mean, na.rm = TRUE))
            }), roughness = roughrast)
        if (missing(sourceCCLRoughness)) {
            path <- paste0(system.file(package = "windfarmGA"),
                "/extdata/")
            sourceCCLRoughness <- paste0(path, "clc_legend.csv")
        }
        else {
            if (verbose) {
                message("You are using your own Corine Land Cover legend.\n")
            }
        }
        rauhigkeitz <- utils::read.csv(sourceCCLRoughness, header = TRUE,
            sep = ";")
        cclRaster <- raster::reclassify(cclPoly1, matrix(c(rauhigkeitz$GRID_CODE,
            rauhigkeitz$Rauhigkeit_z), ncol = 2))
        if (plotit) {
            raster::plot(cclRaster, main = "Surface Roughness from Corine Land Cover")
        }
    }
    if (verbose) {
        cat("\nStart Genetic Algorithm ...\n")
    }
    rbPal <- grDevices::colorRampPalette(c("red", "green"))
    i <- 1
    while (i <= iteration) {
        if (!verbose) {
            cat(".")
        }
        if (i == 1) {
            fit <- fitness(selection = startsel, referenceHeight = referenceHeight,
                RotorHeight = RotorHeight, SurfaceRoughness = SurfaceRoughness,
                Polygon = Polygon1, resol1 = resol2, rot = Rotor,
                dirspeed = winddata, srtm_crop = srtm_crop, topograp = topograp,
                cclRaster = cclRaster, weibull = estim_speed_raster,
                Parallel = Parallel, numCluster = numCluster)
        }
        else {
            getRectV <- get_grids(mut1, Grid)
            fit <- fitness(selection = getRectV, referenceHeight = referenceHeight,
                RotorHeight = RotorHeight, SurfaceRoughness = SurfaceRoughness,
                Polygon = Polygon1, resol1 = resol2, rot = Rotor,
                dirspeed = winddata, srtm_crop = srtm_crop, topograp = topograp,
                cclRaster = cclRaster, weibull = estim_speed_raster,
                Parallel = Parallel, numCluster = numCluster)
        }
        allparks <- do.call("rbind", fit)
        allparksUni <- subset.matrix(allparks, subset = !duplicated(allparks[,
            "Run"]))
        allCoords[[i]] <- allparks
        maxparkfitness <- round(max(allparksUni[, "Parkfitness"]),
            4)
        meanparkfitness <- round(mean(allparksUni[, "Parkfitness"]),
            3)
        minparkfitness <- round(min(allparksUni[, "Parkfitness"]),
            3)
        MaxEnergyRedu <- round(max(allparksUni[, "EnergyOverall"]),
            2)
        MeanEnergyRedu <- round(mean(allparksUni[, "EnergyOverall"]),
            2)
        MinEnergyRedu <- round(min(allparksUni[, "EnergyOverall"]),
            2)
        maxParkwirkungsg <- round(max(allparksUni[, "EfficAllDir"]),
            2)
        meanParkwirkungsg <- round(mean(allparksUni[, "EfficAllDir"]),
            2)
        minParkwirkungsg <- round(min(allparksUni[, "EfficAllDir"]),
            2)
        allparkcoeff[[i]] <- cbind(maxparkfitness, meanparkfitness,
            minparkfitness, MaxEnergyRedu, MeanEnergyRedu, MinEnergyRedu,
            maxParkwirkungsg, meanParkwirkungsg, minParkwirkungsg)
        clouddata[[i]] <- subset.matrix(allparksUni, select = c("EfficAllDir",
            "EnergyOverall", "Parkfitness"))
        if (verbose) {
            cat(c("\n\n", i, ": Round with coefficients ",
                allparkcoeff[[i]], "\n"))
        }
        xd <- max(allparks[, "EnergyOverall"])
        ind <- allparks[, "EnergyOverall"] == xd
        bestPaEn[[i]] <- allparks[ind, ][1:n, ]
        xd1 <- max(allparks[, "EfficAllDir"])
        ind1 <- allparks[, "EfficAllDir"] == xd1
        bestPaEf[[i]] <- allparks[ind1, ][1:n, ]
        afvs <- allparks[allparks[, "EnergyOverall"] ==
            max(allparks[, "EnergyOverall"]), ]
        if (verbose) {
            cat(paste("How many individuals exist: ", length(fit)),
                "\n")
            cat(paste("How many parks are in local Optimum: ",
                (length(afvs[, 1])/n)), "\n")
        }
        nindivfit <- length(fit)
        if (plotit) {
            lebre <- length(unique(bestPaEn[[i]][, "AbschGesamt"]))
            if (lebre < 2) {
                Col <- "green"
            }
            else {
                Col <- rbPal(lebre)[as.numeric(cut(-bestPaEn[[i]][,
                  "AbschGesamt"], breaks = lebre))]
            }
            lebre2 <- length(unique(bestPaEf[[i]][, "AbschGesamt"]))
            if (lebre2 < 2) {
                Col1 <- "green"
            }
            else {
                Col1 <- rbPal(lebre2)[as.numeric(cut(-bestPaEf[[i]][,
                  "AbschGesamt"], breaks = lebre2))]
            }
        }
        x <- round(bestPaEn[[i]][, "EnergyOverall"][[1]],
            2)
        y <- round(bestPaEn[[i]][, "EfficAllDir"][[1]],
            2)
        e <- bestPaEn[[i]][, "EfficAllDir"]
        x1 <- round(bestPaEf[[i]][, "EnergyOverall"][[1]],
            2)
        y1 <- round(bestPaEf[[i]][, "EfficAllDir"][[1]],
            2)
        e1 <- bestPaEf[[i]][, "EfficAllDir"]
        if (plotit) {
            graphics::par(mfrow = c(1, 2))
            plot(st_geometry(Polygon1), col = "lightblue",
                main = paste(i, "Round \n Best Energy Output: ",
                  x, "W/h \n Efficiency: ", y, "%"),
                sub = paste("\n Number of turbines: ",
                  length(e)))
            plot(grid_filtered, add = TRUE)
            graphics::points(bestPaEn[[i]][, "X"], bestPaEn[[i]][,
                "Y"], col = Col, pch = 20, cex = 1.5)
            plot(st_geometry(Polygon1), col = "lightblue",
                main = paste(i, "Round \n Best Efficiency Output: ",
                  x1, "W/h \n Efficiency: ", y1, "%"),
                sub = paste("\n Number of turbines: ",
                  length(e1)))
            plot(grid_filtered, add = TRUE)
            graphics::points(bestPaEf[[i]][, "X"], bestPaEf[[i]][,
                "Y"], col = Col1, pch = 20, cex = 1.5)
        }
        if (i > 20) {
            besPE <- do.call("rbind", lapply(bestPaEn[1:i],
                function(x) {
                  max(x[, "EnergyOverall"])
                }))
            maxBisher <- max(besPE)
            WhichMaxBs <- which(besPE == max(besPE))
            if (length(WhichMaxBs) >= 2) {
                BestForNo <- bestPaEn[sample(WhichMaxBs, 2)]
                BestForNo[[1]][, "Run"] <- length(fit) +
                  1
                BestForNo[[2]][, "Run"] <- length(fit) +
                  2
            }
            else {
                BestForNo <- bestPaEn[WhichMaxBs]
                BestForNo <- append(BestForNo, BestForNo)
                BestForNo[[1]][, "Run"] <- length(fit) +
                  1
                BestForNo[[2]][, "Run"] <- length(fit) +
                  2
            }
            last7 <- besPE[i:(i - 5)]
            if (!any(last7 == maxBisher)) {
                if (verbose) {
                  cat(paste("Park with highest Fitness level to date ",
                    "is replaced in the list.", "\n\n"))
                }
                fit <- append(fit, BestForNo)
            }
        }
        if (i == 1) {
            t0 <- subset.matrix(allparks, !duplicated(allparks[,
                "Run"]))
            t0 <- t0[, "Parkfitness"]
            fitnessValues[[i]] <- t0
            rangeFitnessVt0 <- range(t0)
            maxt0 <- max(t0)
            meant0 <- mean(t0)
            allcoef0 <- c(rangeFitnessVt0, meant0)
            fuzzycontr[[i]] <- rbind(allcoef0)
            colnames(fuzzycontr[[i]]) <- c("Min", "Max",
                "Mean")
            teil <- 2
            if (selstate == "VAR") {
                teil <- 1.35
            }
            u <- 1.1
            beorwor[[i]] <- cbind(0, 0)
        }
        if (i >= 2 && i <= iteration) {
            t0 <- subset.matrix(allparks, !duplicated(allparks[,
                "Run"]))
            t0 <- t0[, "Parkfitness"]
            fitnessValues[[i]] <- t0
            rangeFitnessVt0 <- range(t0)
            maxt0 <- max(t0)
            meant0 <- mean(t0)
            mint0 <- min(t0)
            t1 <- fitnessValues[[i - 1]]
            rangeFitnessVt1 <- range(t1)
            maxt1 <- max(t1)
            meant1 <- mean(t1)
            mint1 <- min(t1)
            maxDif <- maxt0 - maxt1
            meanDif <- meant0 - meant1
            minDif <- mint0 - mint1
            WeightDif <- c(0.8, 0.2, 0)
            maxunt <- (maxDif * WeightDif[1]) + (meanDif * WeightDif[2]) +
                (minDif * WeightDif[3])
            allcoef1 <- c(rangeFitnessVt0, meant0)
            allcoef2 <- c(rangeFitnessVt1, meant1)
            fuzzycontr[[i]] <- rbind(allcoef1, allcoef2)
            colnames(fuzzycontr[[i]]) <- c("Min", "Max",
                "Mean")
            if (maxunt < 0) {
                pri <- "deteriorated"
                teil <- teil - 0.02
                u <- u - 0.06
            }
            else if (maxunt == 0) {
                pri <- "not changed"
                teil <- teil
                u <- u
            }
            else {
                pri <- "improved"
                teil <- teil + 0.017
                u <- u + 0.03
            }
            if (teil > 5) {
                teil <- 5
                u <- u + 0.09
                if (verbose)
                  cat("Min 20% Selected")
                cat(paste("CPR is increased! CPR:", u,
                  "SP: ", teil, "\n"))
            }
            if (trunc(u) < 0) {
                u <- 0.5
                teil <- teil - 0.4
                if (verbose)
                  cat(paste("Min 1 CrossPoints. Selection decreased. CPR:",
                    u, "SP: ", teil, "\n"))
            }
            if (u >= 4) {
                u <- 4
                teil <- 4
                if (verbose)
                  cat(paste("Max 5 CrossPoints. Select fittest 25%. SP: ",
                    teil, "\n"))
            }
            if (teil <= 4/3) {
                teil <- 4/3
                if (verbose)
                  cat(paste("Max 75% selected. SP: ", teil,
                    "\n"))
            }
            if (length(fit) <= 20) {
                teil <- 1
                u <- u + 0.1
                if (verbose)
                  cat(paste("Less than 20 individuals. Select all and increase ",
                    "Crossover-point rate. CPR: ", u, "SP: ",
                    teil, "\n"))
            }
            u <- round(u, 2)
            teil <- round(teil, 3)
            if (verbose) {
                cat(paste("Fitness of this population (",
                  i, "), compared to the prior,", pri,
                  "by", round(maxunt, 2), "W/h \n"))
            }
            meanunt <- meant0 - meant1
            beorwor[[i]] <- cbind(maxunt, meanunt)
        }
        if (selstate == "FIX") {
            if (teil == 1) {
                teil <- 1
            }
            else {
                teil <- 2
            }
        }
        if (crossPart1 == "EQU") {
            u <- round(u, 2)
        }
        selcross[[i]] <- cbind(cross = trunc(u + 1), teil)
        selec6best <- selection(fit = fit, Grid = Grid, teil = teil,
            elitism = elitism, nelit = nelit, selstate = selstate,
            verbose = verbose)
        selec6best_bin <- selec6best[[1]]
        if (verbose) {
            cat(paste("Selection  -  Amount of Individuals: ",
                length(selec6best_bin[1, -1]), "\n"))
        }
        nindivsel <- length(selec6best_bin[1, -1])
        crossOut <- crossover(se6 = selec6best, u = u, uplimit = CrossUpLimit,
            crossPart = crossPart1, verbose = verbose, seed = NULL)
        if (verbose) {
            cat(paste("Crossover  -  Amount of Individuals: ",
                length(crossOut[1, ])))
        }
        nindivcros <- length(crossOut[1, ])
        loOp <- (length(afvs[, 1])/n)
        if (loOp > 2) {
            mutrn <- round(runif(1, 0.03, 0.1), 2)
            t1 <- (loOp * 1.25)/42
            mutrn <- mutrn * (1 + t1)
            mutrn <- round(mutrn + ((i)/(20 * iteration)), 5)
            mut <- mutation(a = crossOut, p = mutrn, seed = NULL)
            mut_rat <- mutrn
            if (verbose) {
                cat(paste("\nVariable Mutation Rate is",
                  mutrn, "\n"))
            }
        }
        else {
            mut <- mutation(a = crossOut, p = mutr, seed = NULL)
            mut_rat <- mutr
        }
        mut_rate[[i]] <- mut_rat
        if (verbose) {
            cat(paste("\nMutation   -  Amount of Individuals: ",
                length(mut[1, ])))
        }
        nindivmut <- length(mut[1, ])
        mut1 <- trimton(mut = mut, nturb = n, allparks = allparks,
            nGrids = n_gridcells, trimForce = trimForce, seed = NULL)
        if (verbose) {
            cat(paste("\nTrimToN    -  Amount of Individuals: ",
                length(mut1[1, ])))
        }
        nindiv[[i]] <- cbind(nindivfit, nindivsel, nindivcros,
            nindivmut)
        if (maxParkwirkungsg == 100) {
            i <- iteration + 1
        }
        else {
            i <- i + 1
        }
    }
    if (Parallel) {
        parallel::stopCluster(cl)
    }
    mut_rate <- mut_rate[lapply(mut_rate, length) != 0]
    beorwor <- beorwor[lapply(beorwor, length) != 0]
    selcross <- selcross[lapply(selcross, length) != 0]
    clouddata <- clouddata[lapply(clouddata, length) != 0]
    allparkcoeff <- allparkcoeff[lapply(allparkcoeff, length) !=
        0]
    bestPaEn <- bestPaEn[lapply(bestPaEn, length) != 0]
    bestPaEf <- bestPaEf[lapply(bestPaEf, length) != 0]
    fuzzycontr <- fuzzycontr[lapply(fuzzycontr, length) != 0]
    fitnessValues <- fitnessValues[lapply(fitnessValues, length) !=
        0]
    nindiv <- nindiv[lapply(nindiv, length) != 0]
    allCoords <- allCoords[lapply(allCoords, length) != 0]
    alldata <- cbind(allparkcoeff, bestPaEn, bestPaEf, fuzzycontr,
        fitnessValues, nindiv, clouddata, selcross, beorwor,
        inputData, inputWind, mut_rate, allCoords)
    return(alldata)
}

  # result
  # Polygon1
  # whichPl = "all"
  # best = 1
  # plotEn = 1

plot_windfarmGA_fixed <- function (result, Polygon1, whichPl = "all", best = 1,
    plotEn = 1, weibullsrc)
{
    parpplotWindGa <- par(ask = FALSE, no.readonly = TRUE)
    on.exit(par(parpplotWindGa))
    if (any(whichPl == "all")) {
        whichPl <- 1:6
    }
    resol <- as.numeric(result[, "inputData"][[1]][, 1]["Resolution"][[1]])
    Polygon1 <- isSpatial(Polygon1)
    if (nrow(result) < 4) {
        if (any(2:5 %in% whichPl)) {
            cat("Cannot plot option 2,3,4,5. \n Only option 1,6 are available.")
            whichPl <- c(1, 6)
        }
    }
    if (any(whichPl == 1)) {
        print("plot_result: Plot the 'best' Individuals of the GA:")
        plot_result_fixed(result = result, Polygon1 = Polygon1, best = best,
            plotEn = plotEn, topographie = FALSE, Grid = TRUE#,
          #  weibullsrc = weibullsrc
          )
        readline(prompt = "Press [enter] to continue")
    }
    if (any(whichPl == 2)) {
        print("plot_evolution: Plot the Evolution of the Efficiency and Energy Values:")
        plot_evolution(result, TRUE, 0.3)
    }
    if (any(whichPl == 3)) {
        print("plot_parkfitness: Plot the Influence of Population Size, Selection, Crossover, Mutation:")
        plot_parkfitness(result, 0.1)
        readline(prompt = "Press [enter] to continue")
    }
    if (any(whichPl == 4)) {
        print("plot_fitness_evolution: Plot the Changes in Fitness Values:")
        plot_fitness_evolution(result)
        readline(prompt = "Press [enter] to continue")
    }
    if (any(whichPl == 5)) {
        print("plot_cloud: Plot all individual Values of the whole Evolution:")
        plot_cloud(result, TRUE)
        readline(prompt = "Press [enter] to continue")
    }
    if (any(whichPl == 6)) {
        print("plot_heatmap: Plot a Heatmap of all Grid Cells:")
        plot_heatmap(result = result, si = 2)
    }
    return()
}

#plot_result_fixed(
# result = result
# Polygon1 = Polygon1
# best = best
# plotEn = plotEn
# topographie = FALSE
# Grid = TRUE
# weibullsrc = weibullsrc



plot_result_fixed <- function(result, Polygon1, best = 3, plotEn = 1,
  topographie = FALSE, Grid = TRUE, sourceCCLRoughness = NULL, sourceCCL = NULL,
  weibullsrc){
    if (!plotEn %in% c(1, 2)) {
        stop("plotEn must be either 1 or 2. \n", "1 - plots the best energy output. \n",
            "2 - plots the best efficiency output.")
    }
    parpplotRes <- par(no.readonly = TRUE)
    par(mfrow = c(1, 1), mar = c(5, 6, 4, 2) + 0.1, mgp = c(5, 1, 0))
    rbPal1 <- grDevices::colorRampPalette(c("green", "red"))
    result_inputs <- result[1, "inputData"][[1]]
    Polygon1 <- isSpatial(Polygon1)
    PROJ6 <- utils::compareVersion(sf::sf_extSoftVersion()[[3]],
        "6") > 0
    Projection <- result_inputs["Projection", ][[1]]
    if (PROJ6) {
        Projection <- tryCatch(as.integer(Projection), warning = function(e) Projection,
            error = function(e) Projection)
    }
    if (is.na(st_crs(Polygon1))) {
        message("Polygon is not projected. The spatial reference WGS 84 (EPSG:4326) is assumed.")
        if (PROJ6) {
            st_crs(Polygon1) <- 4326
        }
        else {
            st_crs(Polygon1) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs "
        }
    }
    Polygon1 <- sf::st_transform(Polygon1, st_crs(Projection))
    if (missing(weibullsrc)) {
        weibullsrc <- NULL
        col2res <- "lightblue"
    } else {
        PolyCrop <- sf::st_transform(Polygon1, sf::st_crs(weibullsrc[[1]]))
        if (class(weibullsrc) == "list" & length(weibullsrc) ==
            2) {
            wblcroped <- lapply(weibullsrc, function(x) {
                raster::crop(x, raster::extent(PolyCrop))
            })
            wblcroped <- lapply(wblcroped, function(x) {
                raster::mask(x, PolyCrop)
            })
            Erwartungswert <- wblcroped[[2]] * (gamma(1 + (1/wblcroped[[1]])))
        }
        else if (class(weibullsrc) == "list" & length(weibullsrc) ==
            1) {
            wblcroped <- raster::crop(weibullsrc[[1]], raster::extent(PolyCrop))
            wblcroped <- raster::mask(weibullsrc[[1]], PolyCrop)
            Erwartungswert <- wblcroped[[1]]
        }
        else if (class(weibullsrc) == "RasterLayer") {
            wblcroped <- raster::crop(weibullsrc, raster::extent(PolyCrop))
            wblcroped <- raster::mask(weibullsrc, PolyCrop)
            Erwartungswert <- wblcroped
        }
        col2res <- "transparent"
        alpha <- 0.9
        Erwartungswert <- raster::projectRaster(Erwartungswert,
            crs = raster::crs(Polygon1))
    }
    if (isTRUE(Grid)) {
        cellsize <- as.numeric(result_inputs["Resolution",
            ][[1]])
        if (result_inputs["Grid Method", ][[1]] == "Rectangular") {
            Grid <- grid_area_fixed(Polygon1, size = cellsize,
              prop = as.numeric(result_inputs["Percentage of Polygon",
                ][[1]]))[[2]]
        } else {
            Grid <- hexa_area_fixed(Polygon1, size = cellsize)[[2]]
        }
    }
    if (topographie == TRUE) {
        Polygonwgs84 <- sf::st_transform(Polygon1, 4326)
        srtm <- tryCatch(elevatr::get_elev_raster(locations = as(Polygonwgs84,
            "Spatial"), z = 11), error = function(e) {
            stop("\nDownloading Elevation data failed for the given Polygon.\n",
                e, call. = FALSE)
        })
        srtm_crop <- raster::crop(srtm, Polygonwgs84)
        srtm_crop <- raster::mask(srtm_crop, Polygonwgs84)
        srtm_crop <- raster::projectRaster(srtm_crop, crs = raster::crs(Polygon1))
        orogr1 <- raster::calc(srtm_crop, function(x) {
            x/(raster::cellStats(srtm_crop, mean, na.rm = TRUE))
        })
        if (is.null(sourceCCL)) {
            if (length(list.files(pattern = "g100_06.tif")) ==
                0) {
                message("\nNo land cover raster ('sourceCCL') was given. It will be downloaded from ",
                  "the EEA-website.")
                download.file("http://github.com/YsoSirius/windfarm_data/raw/master/clc.zip",
                  destfile = "clc.zip", method = "auto")
                unzip("clc.zip")
                unlink("clc.zip")
                ccl <- raster::raster("g100_06.tif")
            }
            else {
                sourceCCL <- list.files(pattern = "g100_06.tif",
                  full.names = TRUE)
                ccl <- raster::raster(x = sourceCCL)
            }
        }
        cclPoly <- raster::crop(ccl, Polygon1)
        cclPoly1 <- raster::mask(cclPoly, mask = Polygon1)
        if (is.null(sourceCCLRoughness)) {
            path <- paste0(system.file(package = "windfarmGA"),
                "/extdata/")
            source_ccl <- paste0(path, "clc_legend.csv")
        }
        else {
            cat("\nYou are using your own Corine Land Cover legend.")
            source_ccl <- sourceCCLRoughness
        }
        rauhigkeitz <- utils::read.csv(source_ccl, header = TRUE,
            sep = ";")
        cclRaster <- raster::reclassify(cclPoly1, matrix(c(rauhigkeitz[,
            "GRID_CODE"], rauhigkeitz[, "Rauhigkeit_z"]),
            ncol = 2))
    }
    if (plotEn == 1) {
        filter_col <- "EnergyOverall"
        listind <- 2
        title <- "Energy"
    }
    if (plotEn == 2) {
        filter_col <- "EfficAllDir"
        listind <- 3
        title <- "Efficiency"
    }
    energy_order <- unlist(lapply(result[, listind], function(x) x[,
        filter_col][[1]]))
    energy_order <- order(energy_order, decreasing = FALSE)
    result <- result[, listind][energy_order]
    ledup <- length(result)
    rectid <- lapply(result, function(x) x[, "Rect_ID"])
    rectidt <- !duplicated(rectid)
    result <- result[rectidt]
    ndif <- length(result)
    cat(paste("N different optimal configurations:", ndif,
        "\nAmount duplicates:", (ledup - ndif)))
    if (ndif < best) {
        cat(paste("\nNot enough unique Optimas. Show first best Half of different configurations."))
        best <- trunc(ndif/2)
    }
    if (best == 0) best = 1
    result <- result[(length(result) - best + 1):(length(result))]
    for (i in (1:length(result))) {
        best_result <- data.frame(result[[i]])
        best_result$EnergyOverall <- round(best_result[, "EnergyOverall"],
            2)
        best_result$EfficAllDir <- round(best_result[, "EfficAllDir"],
            2)
        br <- length(levels(factor(best_result[, "AbschGesamt"])))
        if (br > 1) {
            Col <- rbPal1(br)[as.numeric(cut(as.numeric(best_result[,
                "AbschGesamt"]), breaks = br))]
        }
        else {
            Col <- "green"
        }
        cat(paste("\nPlot ", (best + 1) - i, " Best ",
            title, " Solution:\n"))
        par(mfrow = c(1, 1), ask = FALSE)
        plot(st_geometry(Polygon1), col = col2res, main = paste((best +
            1) - i, "Best ", title, " Windfarm",
            "\nEnergy Output", best_result$EnergyOverall[[1]],
            "kW", "\nEfficiency:", best_result$EfficAllDir[[1]],
            "%"), cex.main = 0.8)
        if (best > 1) {
            if (i > 1) {
                par(ask = TRUE)
            }
        }
        if (!is.null(weibullsrc)) {
            raster::plot(Erwartungswert, alpha = alpha, legend = TRUE,
                axes = FALSE, useRaster = TRUE, add = TRUE, legend.lab = "Mean Wind Speed")
        }
        if (inherits(Grid, "sf") || inherits(Grid, "sfc")) {
            plot(Grid, add = TRUE)
        }
        graphics::mtext("Total Wake Effect in %", side = 2,
            cex = 0.8)
        graphics::points(best_result[, "X"], best_result[,
            "Y"], cex = 2, pch = 20, col = Col)
        graphics::text(best_result[, "X"], best_result[,
            "Y"], round(best_result[, "AbschGesamt"],
            0), cex = 0.8, pos = 1, col = "black")
        distpo <- stats::dist(x = cbind(best_result[, "X"],
            best_result[, "Y"]), method = "euclidian")
        graphics::mtext(paste("minimal Distance", round(min(distpo),
            2)), side = 1, line = 0, cex = 0.8)
        graphics::mtext(paste("mean Distance", round(mean(distpo),
            2)), side = 1, line = 1, cex = 0.8)
        if (topographie == TRUE) {
            par(ask = TRUE)
            sel1 <- best_result[, 1:2]
            plot_terrain(result_inputs, sel1, Polygon1, orogr1,
                srtm_crop, cclRaster)
        }
    }
    par(parpplotRes)
    invisible(best_result)
}


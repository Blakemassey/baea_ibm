momentuHMM::simData
function (nbAnimals = 1, nbStates = 2, dist, Par, beta = NULL,
    delta = NULL, formula = ~1, formulaDelta = ~1, covs = NULL,
    nbCovs = 0, spatialCovs = NULL, zeroInflation = NULL, oneInflation = NULL,
    circularAngleMean = NULL, centers = NULL, centroids = NULL,
    obsPerAnimal = c(500, 1500), initialPosition = c(0, 0), DM = NULL,
    cons = NULL, userBounds = NULL, workcons = NULL, stateNames = NULL,
    model = NULL, states = FALSE, retrySims = 0, lambda = NULL,
    errorEllipse = NULL)
{
    if (!is.null(model)) {
        if (is.miHMM(model)) {
            model <- model$miSum
        }
        model <- delta_bc(model)
        if (is.miSum(model)) {
            model$mle <- lapply(model$Par$real, function(x) x$est)
            model$mle$beta <- model$Par$beta$beta$est
            model$mle$delta <- model$Par$real$delta$est
            model$mod <- list()
            model$mod$estimate <- model$MIcombine$coefficients
        }
        nbStates <- length(model$stateNames)
        dist <- model$conditions$dist
        distnames <- names(dist)
        userBounds <- model$conditions$bounds
        stateNames <- model$stateNames
        estAngleMean <- model$conditions$estAngleMean
        circularAngleMean <- model$conditions$circularAngleMean
        DM <- model$conditions$DM
        cons <- model$conditions$cons
        workcons <- model$conditions$workcons
        zeroInflation <- model$conditions$zeroInflation
        oneInflation <- model$conditions$oneInflation
        formula <- model$conditions$formula
        formulaDelta <- model$condition$formulaDelta
        Par <- model$mle[distnames]
        parindex <- c(0, cumsum(unlist(lapply(model$conditions$fullDM,
            ncol)))[-length(model$conditions$fullDM)])
        names(parindex) <- distnames
        for (i in distnames) {
            if (!is.null(DM[[i]])) {
                Par[[i]] <- model$mod$estimate[parindex[[i]] +
                  1:ncol(model$conditions$fullDM[[i]])]
                names(Par[[i]]) <- colnames(model$conditions$fullDM[[i]])
                cons[[i]] <- rep(1, length(cons[[i]]))
                workcons[[i]] <- rep(0, length(workcons[[i]]))
            }
        }
        for (i in distnames[which(dist %in% angledists)]) {
            if (!estAngleMean[[i]]) {
                estAngleMean[[i]] <- TRUE
                userBounds[[i]] <- rbind(matrix(rep(c(-pi, pi),
                  nbStates), nbStates, 2, byrow = TRUE), userBounds[[i]])
                cons[[i]] <- c(rep(1, nbStates), cons[[i]])
                workcons[[i]] <- c(rep(0, nbStates), workcons[[i]])
                if (!is.null(DM[[i]])) {
                  Par[[i]] <- c(rep(0, nbStates), Par[[i]])
                  if (is.list(DM[[i]])) {
                    DM[[i]]$mean <- ~1
                  }
                  else {
                    tmpDM <- matrix(0, nrow(DM[[i]]) + nbStates,
                      ncol(DM[[i]]) + nbStates)
                    tmpDM[nbStates + 1:nrow(DM[[i]]), nbStates +
                      1:ncol(DM[[i]])] <- DM[[i]]
                    diag(tmpDM)[1:nbStates] <- 1
                    DM[[i]] <- tmpDM
                  }
                }
                model$conditions$estAngleMean[[i]] <- estAngleMean[[i]]
                model$conditions$userBounds[[i]] <- userBounds[[i]]
                model$conditions$cons[[i]] <- cons[[i]]
                model$conditions$workcons[[i]] <- workcons[[i]]
                model$conditions$DM[[i]] <- DM[[i]]
            }
        }
        beta <- model$mle$beta
        delta <- model$mle$delta
        if (!length(attr(terms.formula(formulaDelta), "term.labels"))) {
            delta <- delta[1, ]
            rownames(delta) <- NULL
        }
        else {
            nbCovsDelta <- ncol(model$covsDelta) - 1
            foo <- length(model$mod$estimate) - (nbCovsDelta +
                1) * (nbStates - 1) + 1
            delta <- matrix(model$mod$estimate[foo:length(model$mod$estimate)],
                nrow = nbCovsDelta + 1)
        }
        Par <- lapply(Par, function(x) c(t(x)))
        if (states)
            model$data$states <- NULL
        if (is.null(covs)) {
            p <- parDef(dist, nbStates, estAngleMean, zeroInflation,
                oneInflation, DM, userBounds)
            covNames <- character()
            for (i in distnames) {
                covNames <- c(covNames, getCovNames(model, p,
                  i)$DMterms)
            }
            if (!is.null(model$rawCovs)) {
                covNames <- c(colnames(model$rawCovs), covNames)
            }
            covNames <- c(covNames, colnames(model$covsDelta)[-1])
            covsCol <- unique(covNames)
            factorterms <- names(model$data)[unlist(lapply(model$data,
                is.factor))]
            factorcovs <- paste0(rep(factorterms, times = unlist(lapply(model$data[factorterms],
                nlevels))), unlist(lapply(model$data[factorterms],
                levels)))
            if (length(covsCol)) {
                for (jj in 1:length(covsCol)) {
                  cov <- covsCol[jj]
                  form <- formula(paste("~", cov))
                  varform <- all.vars(form)
                  if (any(varform %in% factorcovs)) {
                    factorvar <- factorcovs %in% varform
                    covsCol[jj] <- rep(factorterms, times = unlist(lapply(model$data[factorterms],
                      nlevels)))[which(factorvar)]
                  }
                }
            }
            covsCol <- unique(covsCol)
            if (length(covsCol))
                covs <- model$data[covsCol]
        }
    }
    else {
        if (!is.list(dist) | is.null(names(dist)))
            stop("'dist' must be a named list")
        if (!is.list(Par) | is.null(names(Par)))
            stop("'Par' must be a named list")
        distnames <- names(dist)
        if (!all(distnames %in% names(Par)))
            stop(distnames[which(!(distnames %in% names(Par)))],
                " is missing in 'Par'")
        Par <- Par[distnames]
        mHind <- (is.null(DM) & is.null(userBounds) & is.null(spatialCovs) &
            is.null(centers) & is.null(centroids) & ("step" %in%
            names(dist)) & all(unlist(initialPosition) == c(0,
            0)) & is.null(lambda) & is.null(errorEllipse) & !is.list(obsPerAnimal) &
            is.null(covs) & !nbCovs & !length(attr(terms.formula(formula),
            "term.labels")) & !length(attr(terms.formula(formulaDelta),
            "term.labels")) & is.null(delta))
        if (all(names(dist) %in% c("step", "angle")) & mHind) {
            zi <- FALSE
            if (!is.null(zeroInflation$step))
                zi <- zeroInflation$step
            if (is.null(dist$angle))
                dist$angle <- "none"
            data <- moveHMM::simData(nbAnimals, nbStates, dist$step,
                dist$angle, Par$step, Par$angle, beta, covs,
                nbCovs, zi, obsPerAnimal, model, states)
            attr(data, "class") <- "data.frame"
            data$ID <- as.factor(data$ID)
            return(momentuHMMData(data))
        }
    }
    Fun <- lapply(dist, function(x) paste("r", x, sep = ""))
    if (nbAnimals < 1)
        stop("nbAnimals should be at least 1.")
    if (nbStates < 1)
        stop("nbStates should be at least 1.")
    if (is.null(zeroInflation)) {
        zeroInflation <- vector("list", length(distnames))
        names(zeroInflation) <- distnames
        for (i in distnames) {
            zeroInflation[[i]] <- FALSE
        }
    }
    else {
        if (!is.list(zeroInflation) | is.null(names(zeroInflation)))
            stop("'zeroInflation' must be a named list")
        for (i in distnames) {
            if (is.null(zeroInflation[[i]]))
                zeroInflation[[i]] <- FALSE
        }
    }
    if (is.null(oneInflation)) {
        oneInflation <- vector("list", length(distnames))
        names(oneInflation) <- distnames
        for (i in distnames) {
            oneInflation[[i]] <- FALSE
        }
    }
    else {
        if (!is.list(oneInflation) | is.null(names(oneInflation)))
            stop("'oneInflation' must be a named list")
        for (i in distnames) {
            if (is.null(oneInflation[[i]]))
                oneInflation[[i]] <- FALSE
        }
    }
    if (!all(unlist(lapply(zeroInflation, is.logical))))
        stop("zeroInflation must be a list of logical objects")
    if (!all(unlist(lapply(oneInflation, is.logical))))
        stop("oneInflation must be a list of logical objects")
    for (i in distnames) {
        if (!(dist[[i]] %in% zeroInflationdists) & zeroInflation[[i]])
            stop(dist[[i]], " distribution cannot be zero inflated")
        if (!(dist[[i]] %in% oneInflationdists) & oneInflation[[i]])
            stop(dist[[i]], " distribution cannot be one inflated")
    }
    estAngleMean <- vector("list", length(distnames))
    names(estAngleMean) <- distnames
    for (i in distnames) {
        if (dist[[i]] %in% angledists)
            estAngleMean[[i]] <- TRUE
        else estAngleMean[[i]] <- FALSE
    }
    inputs <- checkInputs(nbStates, dist, Par, estAngleMean,
        circularAngleMean, zeroInflation, oneInflation, DM, userBounds,
        cons, workcons, stateNames, checkInflation = TRUE)
    p <- inputs$p
    parSize <- p$parSize
    bounds <- p$bounds
    spatialcovnames <- NULL
    if (!is.null(spatialCovs)) {
        if (!is.list(spatialCovs))
            stop("spatialCovs must be a list")
        spatialcovnames <- names(spatialCovs)
        if (is.null(spatialcovnames))
            stop("spatialCovs must be a named list")
        nbSpatialCovs <- length(spatialcovnames)
        if (!("step" %in% distnames))
            stop("spatialCovs can only be included when 'step' distribution is specified")
        else if (!(dist[["step"]] %in% stepdists))
            stop("spatialCovs can only be included when valid 'step' distributions are specified")
        for (j in 1:nbSpatialCovs) {
            if (class(spatialCovs[[j]]) != "RasterLayer")
                stop("spatialCovs$", spatialcovnames[j], " must be of class 'RasterLayer'")
            if (any(is.na(raster::getValues(spatialCovs[[j]]))))
                stop("missing values are not permitted in spatialCovs")
        }
    }
    else nbSpatialCovs <- 0
    if (is.list(obsPerAnimal)) {
        if (length(obsPerAnimal) != nbAnimals)
            stop("obsPerAnimal must be a list of length ", nbAnimals)
        for (i in 1:length(obsPerAnimal)) {
            if (length(which(obsPerAnimal[[i]] < 1)) > 0)
                stop("obsPerAnimal elements should have positive values.")
            if (length(obsPerAnimal[[i]]) == 1)
                obsPerAnimal[[i]] <- rep(obsPerAnimal[[i]], 2)
            else if (length(obsPerAnimal[[i]]) != 2)
                stop("obsPerAnimal elements should be of length 1 or 2.")
        }
    }
    else {
        if (length(which(obsPerAnimal < 1)) > 0)
            stop("obsPerAnimal should have positive values.")
        if (length(obsPerAnimal) == 1)
            obsPerAnimal <- rep(obsPerAnimal, 2)
        else if (length(obsPerAnimal) != 2)
            stop("obsPerAnimal should be of length 1 or 2.")
        tmpObs <- obsPerAnimal
        obsPerAnimal <- vector("list", nbAnimals)
        for (i in 1:nbAnimals) {
            obsPerAnimal[[i]] <- tmpObs
        }
    }
    if (is.list(initialPosition)) {
        if (length(initialPosition) != nbAnimals)
            stop("initialPosition must be a list of length ",
                nbAnimals)
        for (i in 1:nbAnimals) {
            if (length(initialPosition[[i]]) != 2 | !is.numeric(initialPosition[[i]]))
                stop("each element of initialPosition must be a numeric vector of length 2")
        }
    }
    else {
        if (length(initialPosition) != 2 | !is.numeric(initialPosition))
            stop("initialPosition must be a numeric vector of length 2")
        tmpPos <- initialPosition
        initialPosition <- vector("list", nbAnimals)
        for (i in 1:nbAnimals) {
            initialPosition[[i]] <- tmpPos
        }
    }
    if (!is.null(covs) & nbCovs > 0) {
        if (ncol(covs) != nbCovs)
            warning("covs and nbCovs argument conflicting - nbCovs was set to ncol(covs)")
    }
    if (!is.null(covs)) {
        if (!is.data.frame(covs))
            stop("'covs' should be a data.frame")
    }
    if (!is.null(covs)) {
        nbCovs <- ncol(covs)
        if (length(which(is.na(covs))) > 0)
            warning(paste("There are", length(which(is.na(covs))),
                "missing covariate values.", "Each will be replaced by the closest available value."))
        for (i in 1:nbCovs) {
            if (length(which(is.na(covs[, i]))) > 0) {
                if (is.na(covs[1, i])) {
                  k <- 1
                  while (is.na(covs[k, i])) k <- k + 1
                  for (j in k:2) covs[j - 1, i] <- covs[j, i]
                }
                for (j in 2:nrow(covs)) if (is.na(covs[j, i]))
                  covs[j, i] <- covs[j - 1, i]
            }
        }
    }
    allNbObs <- rep(NA, nbAnimals)
    for (zoo in 1:nbAnimals) {
        if (obsPerAnimal[[zoo]][1] != obsPerAnimal[[zoo]][2])
            allNbObs[zoo] <- sample(obsPerAnimal[[zoo]][1]:obsPerAnimal[[zoo]][2],
                size = 1)
        else allNbObs[zoo] <- obsPerAnimal[[zoo]][1]
    }
    cumNbObs <- c(0, cumsum(allNbObs))
    if (!is.null(covs)) {
        covnames <- colnames(covs)
        while (sum(allNbObs) > nrow(covs)) covs <- rbind(covs,
            covs)
        covs <- data.frame(covs[1:sum(allNbObs), ])
        colnames(covs) <- covnames
    }
    allCovs <- NULL
    if (nbCovs > 0) {
        if (is.null(covs)) {
            allCovs <- data.frame(cov1 = rnorm(sum(allNbObs)))
            if (nbCovs > 1) {
                for (j in 2:nbCovs) {
                  c <- data.frame(rnorm(sum(allNbObs)))
                  colnames(c) <- paste("cov", j, sep = "")
                  allCovs <- cbind(allCovs, c)
                }
            }
        }
        else {
            allCovs <- covs
        }
    }
    if (anyDuplicated(colnames(allCovs)))
        stop("covariates must have unique names")
    if (anyDuplicated(spatialcovnames))
        stop("spatialCovs must have unique names")
    if (!is.null(model) & nbSpatialCovs > 0) {
        spInd <- which(!(colnames(allCovs) %in% spatialcovnames))
        if (length(spInd)) {
            allCovs <- allCovs[, spInd, drop = FALSE]
            nbCovs <- ncol(allCovs)
        }
        else {
            allCovs <- NULL
            nbCovs <- 0
        }
    }
    else if (any(colnames(allCovs) %in% spatialcovnames))
        stop("spatialCovs name(s) cannot match other covariate name(s)")
    centerInd <- NULL
    if (!is.null(centers)) {
        if (!is.matrix(centers))
            stop("centers must be a matrix")
        if (dim(centers)[2] != 2)
            stop("centers must be a matrix consisting of 2 columns (i.e., x- and y-coordinates)")
        centerInd <- which(!apply(centers, 1, function(x) any(is.na(x))))
        if (length(centerInd)) {
            if (is.null(rownames(centers)))
                centerNames <- paste0("center", rep(centerInd,
                  each = 2), ".", rep(c("dist", "angle"), length(centerInd)))
            else centerNames <- paste0(rep(rownames(centers),
                each = 2), ".", rep(c("dist", "angle"), length(centerInd)))
            centerCovs <- data.frame(matrix(NA, nrow = sum(allNbObs),
                ncol = length(centerInd) * 2, dimnames = list(NULL,
                  centerNames)))
        }
    }
    else centerNames <- NULL
    centroidInd <- NULL
    if (!is.null(centroids)) {
        if (!is.list(centroids))
            stop("centroids must be a named list")
        centroidNames <- character()
        for (j in 1:length(centroids)) {
            if (!is.data.frame(centroids[[j]]))
                stop("each element of centroids must be a data frame")
            if (dim(centroids[[j]])[1] < max(unlist(obsPerAnimal)) |
                dim(centroids[[j]])[2] != 2)
                stop("each element of centroids must be a data frame consisting of at least ",
                  max(unlist(obsPerAnimal)), " rows (i.e., the maximum number of observations per animal) and 2 columns (i.e., x- and y-coordinates)")
            if (!all(c("x", "y") %in% colnames(centroids[[j]])))
                stop("centroids columns must be named 'x' (x-coordinate) and 'y' (y-coordinate)")
            if (any(is.na(centroids[[j]])))
                stop("centroids cannot contain missing values")
            if (is.null(names(centroids[j])))
                centroidNames <- c(centroidNames, paste0("centroid",
                  rep(j, each = 2), ".", c("dist", "angle")))
            else centroidNames <- c(centroidNames, paste0(rep(names(centroids[j]),
                each = 2), ".", c("dist", "angle")))
        }
        centroidCovs <- data.frame(matrix(NA, nrow = sum(allNbObs),
            ncol = length(centroidNames), dimnames = list(NULL,
                centroidNames)))
        centroidInd <- length(centroidNames)/2
    }
    else centroidNames <- NULL
    if (!is.null(model) & length(centerInd)) {
        cInd <- which(!(colnames(allCovs) %in% centerNames))
        if (length(cInd)) {
            allCovs <- allCovs[, cInd, drop = FALSE]
            nbCovs <- ncol(allCovs)
        }
        else {
            allCovs <- NULL
            nbCovs <- 0
        }
    }
    else if (any(colnames(allCovs) %in% centerNames))
        stop("centers name(s) cannot match other covariate name(s)")
    if (!is.null(model) & length(centroidInd)) {
        cInd <- which(!(colnames(allCovs) %in% centroidNames))
        if (length(cInd)) {
            allCovs <- allCovs[, cInd, drop = FALSE]
            nbCovs <- ncol(allCovs)
        }
        else {
            allCovs <- NULL
            nbCovs <- 0
        }
    }
    else if (any(colnames(allCovs) %in% centroidNames))
        stop("centroids name(s) cannot match other covariate name(s)")
    allNbCovs <- nbCovs + nbSpatialCovs
    if (is.null(delta))
        delta <- rep(1, nbStates)/nbStates
    zeroMass <- oneMass <- vector("list", length(dist))
    names(zeroMass) <- names(oneMass) <- distnames
    allStates <- NULL
    allSpatialcovs <- NULL
    if (all(c("step", "angle") %in% distnames)) {
        distnames <- c("step", "angle", distnames[!(distnames %in%
            c("step", "angle"))])
    }
    data <- data.frame(ID = factor())
    for (i in distnames) {
        data[[i]] <- numeric()
    }
    if ("angle" %in% distnames) {
        if (dist[["angle"]] %in% angledists & ("step" %in% distnames))
            if (dist[["step"]] %in% stepdists) {
                data$x <- numeric()
                data$y <- numeric()
            }
    }
    else if ("step" %in% distnames) {
        if (dist[["step"]] %in% stepdists) {
            data$x <- numeric()
            data$y <- numeric()
        }
    }
    else if (nbSpatialCovs | length(centerInd) | length(centroidInd))
        stop("spatialCovs, centers, and/or centroids cannot be specified without valid step length and turning angle distributions")
    message("=======================================================================")
    message("Simulating HMM with ", nbStates, " states and ",
        length(distnames), " data streams")
    message("-----------------------------------------------------------------------\n")
    for (i in distnames) {
        pNames <- p$parNames[[i]]
        if (inputs$circularAngleMean[[i]])
            pNames[1] <- paste0("circular ", pNames[1])
        if (is.null(DM[[i]])) {
            message(" ", i, " ~ ", dist[[i]], "(", paste0(pNames,
                "=~1", collapse = ", "), ")")
        }
        else if (is.list(DM[[i]])) {
            message(" ", i, " ~ ", dist[[i]], "(", paste0(pNames,
                "=", DM[[i]], collapse = ", "), ")")
        }
        else message(" ", i, " ~ ", dist[[i]], "(", paste0(pNames,
            ": custom", collapse = ", "), ")")
    }
    message("\n Transition probability matrix formula: ", paste0(formula,
        collapse = ""))
    message("\n Initial distribution formula: ", paste0(formulaDelta,
        collapse = ""))
    message("=======================================================================")
    if (length(all.vars(formula)))
        if (!all(all.vars(formula) %in% c("ID", names(allCovs),
            centerNames, centroidNames, spatialcovnames)))
            stop("'formula' covariate(s) not found")
    if (length(all.vars(formulaDelta)))
        if (!all(all.vars(formulaDelta) %in% c("ID", names(allCovs),
            centerNames, centroidNames, spatialcovnames)))
            stop("'formulaDelta' covariate(s) not found")
    if (("ID" %in% all.vars(formula) | "ID" %in% all.vars(formulaDelta)) &
        nbAnimals < 2)
        stop("ID cannot be a covariate when nbAnimals=1")
    stateForms <- terms(formula, specials = paste0("state", 1:nbStates))
    newformula <- formula
    if (nbStates > 1) {
        if (length(unlist(attr(stateForms, "specials")))) {
            newForm <- attr(stateForms, "term.labels")[-unlist(attr(stateForms,
                "specials"))]
            for (i in 1:nbStates) {
                if (!is.null(attr(stateForms, "specials")[[paste0("state",
                  i)]])) {
                  for (j in 1:(nbStates - 1)) {
                    newForm <- c(newForm, gsub(paste0("state",
                      i), paste0("betaCol", (i - 1) * (nbStates -
                      1) + j), attr(stateForms, "term.labels")[attr(stateForms,
                      "specials")[[paste0("state", i)]]]))
                  }
                }
            }
            newformula <- as.formula(paste("~", paste(newForm,
                collapse = "+")))
        }
        formulaStates <- momentuHMM:::stateFormulas(newformula, nbStates *
            (nbStates - 1), spec = "betaCol")
        if (length(unlist(attr(terms(newformula, specials = c(paste0("betaCol",
            1:(nbStates * (nbStates - 1))), "cosinor")), "specials")))) {
            allTerms <- unlist(lapply(formulaStates, function(x) attr(terms(x),
                "term.labels")))
            newformula <- as.formula(paste("~", paste(allTerms,
                collapse = "+")))
            formterms <- attr(terms.formula(newformula), "term.labels")
        }
        else {
            formterms <- attr(terms.formula(newformula), "term.labels")
            newformula <- formula
        }
    }
    tmpCovs <- data.frame(ID = factor(1, levels = 1:nbAnimals))
    if (!is.null(allCovs))
        tmpCovs <- cbind(tmpCovs, allCovs[1, , drop = FALSE])
    if (nbSpatialCovs) {
        for (j in 1:nbSpatialCovs) {
            getCell <- raster::cellFromXY(spatialCovs[[j]], initialPosition[[1]])
            if (is.na(getCell))
                stop("Movement is beyond the spatial extent of the ",
                  spatialcovnames[j], " raster. Try expanding the extent of the raster.")
            tmpCovs[[spatialcovnames[j]]] <- spatialCovs[[j]][getCell]
        }
    }
    if (length(centerInd)) {
        for (j in 1:length(centerInd)) {
            tmpDistAngle <- distAngle(initialPosition[[1]], initialPosition[[1]],
                centers[centerInd[j], ])
            tmpCovs[[centerNames[(j - 1) * 2 + 1]]] <- tmpDistAngle[1]
            tmpCovs[[centerNames[(j - 1) * 2 + 2]]] <- tmpDistAngle[2]
        }
    }
    if (length(centroidInd)) {
        for (j in 1:centroidInd) {
            tmpDistAngle <- distAngle(initialPosition[[1]], initialPosition[[1]],
                as.numeric(centroids[[j]][1, ]))
            tmpCovs[[centroidNames[(j - 1) * 2 + 1]]] <- tmpDistAngle[1]
            tmpCovs[[centroidNames[(j - 1) * 2 + 2]]] <- tmpDistAngle[2]
        }
    }
    nbBetaCovs <- ncol(model.matrix(newformula, tmpCovs))
    if (is.null(beta))
        beta <- matrix(rnorm(nbStates * (nbStates - 1) * nbBetaCovs),
            nrow = nbBetaCovs)
    else {
        if (ncol(beta) != nbStates * (nbStates - 1) | nrow(beta) !=
            nbBetaCovs) {
            error <- paste("beta has wrong dimensions: it should have",
                nbBetaCovs, "rows and", nbStates * (nbStates -
                  1), "columns.")
            stop(error)
        }
    }
    if (nbStates > 1) {
        for (state in 1:(nbStates * (nbStates - 1))) {
            noBeta <- which(match(colnames(model.matrix(newformula,
                tmpCovs)), colnames(model.matrix(formulaStates[[state]],
                tmpCovs)), nomatch = 0) == 0)
            if (length(noBeta))
                beta[noBeta, state] <- 0
        }
    }
    covsDelta <- model.matrix(formulaDelta, tmpCovs)
    nbCovsDelta <- ncol(covsDelta) - 1
    if (!nbCovsDelta) {
        if (length(delta) != (nbCovsDelta + 1) * nbStates)
            stop(paste("delta has the wrong length: it should have",
                nbStates, "elements."))
        deltaB <- log(delta[-1]/delta[1])
    }
    else {
        if (is.null(dim(delta)))
            stop(paste("delta has wrong dimensions: it should have",
                nbCovsDelta + 1, "rows and", nbStates - 1, "columns."))
        if (ncol(delta) != nbStates - 1 | nrow(delta) != nbCovsDelta +
            1)
            stop(paste("delta has wrong dimensions: it should have",
                nbCovsDelta + 1, "rows and", nbStates - 1, "columns."))
        deltaB <- delta
    }
    if (!nbSpatialCovs | !retrySims) {
        for (zoo in 1:nbAnimals) {
            nbObs <- allNbObs[zoo]
            d <- data.frame(ID = factor(rep(zoo, nbObs)))
            subCovs <- data.frame(ID = rep(factor(zoo, levels = 1:nbAnimals),
                nbObs))
            if (nbCovs > 0) {
                if (zoo < 2)
                  ind1 <- 1
                else ind1 <- sum(allNbObs[1:(zoo - 1)]) + 1
                ind2 <- sum(allNbObs[1:zoo])
                subCovs <- cbind(subCovs, data.frame(allCovs[ind1:ind2,
                  , drop = FALSE]))
            }
            if (length(centerInd))
                subCovs <- cbind(subCovs, centerCovs[cumNbObs[zoo] +
                  1:nbObs, ])
            if (length(centroidInd))
                subCovs <- cbind(subCovs, centroidCovs[cumNbObs[zoo] +
                  1:nbObs, ])
            subSpatialcovs <- as.data.frame(matrix(NA, nrow = nbObs,
                ncol = nbSpatialCovs))
            colnames(subSpatialcovs) <- spatialcovnames
            X <- matrix(0, nrow = nbObs, ncol = 2)
            X[1, ] <- initialPosition[[zoo]]
            phi <- 0
            genData <- genArgs <- vector("list", length(distnames))
            names(genData) <- names(genArgs) <- distnames
            for (i in distnames) {
                genData[[i]] <- rep(NA, nbObs)
                genArgs[[i]] <- list(1)
            }
            gamma <- diag(nbStates)
            if (!nbSpatialCovs & !length(centerInd) & !length(centroidInd)) {
                DMcov <- model.matrix(newformula, subCovs)
                gFull <- DMcov %*% beta
                DMinputs <- getDM(subCovs, inputs$DM, dist, nbStates,
                  p$parNames, p$bounds, Par, cons, workcons,
                  zeroInflation, oneInflation, inputs$circularAngleMean)
                fullDM <- DMinputs$fullDM
                DMind <- DMinputs$DMind
                wpar <- n2w(Par, bounds, beta, deltaB, nbStates,
                  inputs$estAngleMean, inputs$DM, DMinputs$cons,
                  DMinputs$workcons, p$Bndind)
                nc <- meanind <- vector("list", length(distnames))
                names(nc) <- names(meanind) <- distnames
                for (i in distnames) {
                  nc[[i]] <- apply(fullDM[[i]], 1:2, function(x) !all(unlist(x) ==
                    0))
                  if (inputs$circularAngleMean[[i]])
                    meanind[[i]] <- which((apply(fullDM[[i]][1:nbStates,
                      , drop = FALSE], 1, function(x) !all(unlist(x) ==
                      0))))
                }
                covsDelta <- model.matrix(formulaDelta, subCovs[1,
                  , drop = FALSE])
                fullsubPar <- w2n(wpar, bounds, parSize, nbStates,
                  length(attr(terms.formula(newformula), "term.labels")),
                  inputs$estAngleMean, inputs$circularAngleMean,
                  stationary = FALSE, DMinputs$cons, fullDM,
                  DMind, DMinputs$workcons, nbObs, dist, p$Bndind,
                  nc, meanind, covsDelta)
                g <- gFull[1, , drop = FALSE]
                delta <- fullsubPar$delta
            }
            else {
                if (nbSpatialCovs) {
                  for (j in 1:nbSpatialCovs) {
                    getCell <- raster::cellFromXY(spatialCovs[[j]],
                      c(X[1, 1], X[1, 2]))
                    if (is.na(getCell))
                      stop("Movement is beyond the spatial extent of the ",
                        spatialcovnames[j], " raster. Try expanding the extent of the raster.")
                    subSpatialcovs[1, j] <- spatialCovs[[j]][getCell]
                  }
                }
                if (length(centerInd)) {
                  for (j in 1:length(centerInd)) {
                    subCovs[1, centerNames[(j - 1) * 2 + 1:2]] <- distAngle(X[1,
                      ], X[1, ], centers[centerInd[j], ])
                  }
                }
                if (length(centroidInd)) {
                  for (j in 1:centroidInd) {
                    subCovs[1, centroidNames[(j - 1) * 2 + 1:2]] <- distAngle(X[1,
                      ], X[1, ], as.numeric(centroids[[j]][1,
                      ]))
                  }
                }
                g <- model.matrix(newformula, cbind(subCovs[1,
                  , drop = FALSE], subSpatialcovs[1, , drop = FALSE])) %*%
                  beta
                covsDelta <- model.matrix(formulaDelta, cbind(subCovs[1,
                  , drop = FALSE], subSpatialcovs[1, , drop = FALSE]))
                delta <- c(rep(0, nbCovsDelta + 1), deltaB)
                deltaXB <- covsDelta %*% matrix(delta, nrow = nbCovsDelta +
                  1)
                expdelta <- exp(deltaXB)
                delta <- expdelta/rowSums(expdelta)
                for (i in which(!is.finite(rowSums(delta)))) {
                  tmp <- exp(Brobdingnag::as.brob(deltaXB[i,
                    ]))
                  delta[i, ] <- as.numeric(tmp/Brobdingnag::sum(tmp))
                }
            }
            gamma[!gamma] <- exp(g)               # This is same as moveHMM
            gamma <- t(gamma)
            gamma <- gamma/apply(gamma, 1, sum)
            if (nbStates > 1) {
                Z <- rep(NA, nbObs)
                Z[1] <- sample(1:nbStates, size = 1, prob = delta %*%
                  gamma)
            }
            else Z <- rep(1, nbObs)
            for (k in 1:(nbObs - 1)) {
                if (nbSpatialCovs | length(centerInd) | length(centroidInd)) {
                  DMinputs <- getDM(cbind(subCovs[k, , drop = FALSE],
                    subSpatialcovs[k, , drop = FALSE]), inputs$DM,
                    dist, nbStates, p$parNames, p$bounds, Par,
                    cons, workcons, zeroInflation, oneInflation,
                    inputs$circularAngleMean)
                  fullDM <- DMinputs$fullDM
                  DMind <- DMinputs$DMind
                  wpar <- n2w(Par, bounds, beta, deltaB, nbStates,
                    inputs$estAngleMean, inputs$DM, DMinputs$cons,
                    DMinputs$workcons, p$Bndind)
                  nc <- meanind <- vector("list", length(distnames))
                  names(nc) <- names(meanind) <- distnames
                  for (i in distnames) {
                    nc[[i]] <- apply(fullDM[[i]], 1:2, function(x) !all(unlist(x) ==
                      0))
                    if (inputs$circularAngleMean[[i]])
                      meanind[[i]] <- which((apply(fullDM[[i]][1:nbStates,
                        , drop = FALSE], 1, function(x) !all(unlist(x) ==
                        0))))
                  }
                  subPar <- w2n(wpar, bounds, parSize, nbStates,
                    length(attr(terms.formula(newformula), "term.labels")),
                    inputs$estAngleMean, inputs$circularAngleMean,
                    stationary = FALSE, DMinputs$cons, fullDM,
                    DMind, DMinputs$workcons, 1, dist, p$Bndind,
                    nc, meanind, covsDelta)
                }
                else {
                  subPar <- lapply(fullsubPar[distnames], function(x) x[,
                    k, drop = FALSE])
                }
                for (i in distnames) {
                  zeroMass[[i]] <- rep(0, nbStates)
                  oneMass[[i]] <- rep(0, nbStates)
                  if (zeroInflation[[i]] | oneInflation[[i]]) {
                    if (zeroInflation[[i]])
                      zeroMass[[i]] <- subPar[[i]][parSize[[i]] *
                        nbStates - nbStates * oneInflation[[i]] -
                        (nbStates - 1):0]
                    if (oneInflation[[i]])
                      oneMass[[i]] <- subPar[[i]][parSize[[i]] *
                        nbStates - (nbStates - 1):0]
                    subPar[[i]] <- subPar[[i]][-(parSize[[i]] *
                      nbStates - (nbStates * oneInflation[[i]] -
                      nbStates * zeroInflation[[i]] - 1):0)]
                  }
                  for (j in 1:(parSize[[i]] - zeroInflation[[i]] -
                    oneInflation[[i]])) genArgs[[i]][[j + 1]] <- subPar[[i]][(j -
                    1) * nbStates + Z[k]]
                  if (dist[[i]] %in% angledists) {
                    genData[[i]][k] <- do.call(Fun[[i]], genArgs[[i]])
                    if (genData[[i]][k] > pi)
                      genData[[i]][k] <- genData[[i]][k] - 2 *
                        pi
                    if (genData[[i]][k] < -pi)
                      genData[[i]][k] <- genData[[i]][k] + 2 *
                        pi
                    if (i == "angle" & ("step" %in% distnames)) {
                      if (dist[["step"]] %in% stepdists) {
                        if (genData$step[k] > 0) {
                          phi <- phi + genData[[i]][k]
                        }
                        m <- genData$step[k] * c(Re(exp((0+1i) *
                          phi)), Im(exp((0+1i) * phi)))
                        X[k + 1, ] <- X[k, ] + m
                      }
                    }
                  }
                  else {
                    if (dist[[i]] == "gamma") {
                      shape <- genArgs[[i]][[2]]^2/genArgs[[i]][[3]]^2
                      scale <- genArgs[[i]][[3]]^2/genArgs[[i]][[2]]
                      genArgs[[i]][[2]] <- shape
                      genArgs[[i]][[3]] <- 1/scale
                    }
                    probs <- c(1 - zeroMass[[i]][Z[k]] - oneMass[[i]][Z[k]],
                      zeroMass[[i]][Z[k]], oneMass[[i]][Z[k]])
                    rU <- which(rmultinom(1, 1, prob = probs) ==
                      1)
                    if (rU == 1)
                      genData[[i]][k] <- do.call(Fun[[i]], genArgs[[i]])
                    else if (rU == 2)
                      genData[[i]][k] <- 0
                    else genData[[i]][k] <- 1
                  }
                  d[[i]] <- genData[[i]]
                }
                gamma <- diag(nbStates)
                if (nbSpatialCovs | length(centerInd) | length(centroidInd)) {
                  if (nbSpatialCovs) {
                    for (j in 1:nbSpatialCovs) {
                      getCell <- raster::cellFromXY(spatialCovs[[j]],
                        c(X[k + 1, 1], X[k + 1, 2]))
                      if (is.na(getCell))
                        stop("Movement is beyond the spatial extent of the ",
                          spatialcovnames[j], " raster. Try expanding the extent of the raster.")
                      subSpatialcovs[k + 1, j] <- spatialCovs[[j]][getCell]
                    }
                  }
                  if (length(centerInd)) {
                    for (j in 1:length(centerInd)) {
                      subCovs[k + 1, centerNames[(j - 1) * 2 +
                        1:2]] <- distAngle(X[k, ], X[k + 1, ],
                        centers[centerInd[j], ])
                    }
                  }
                  if (length(centroidInd)) {
                    for (j in 1:centroidInd) {
                      subCovs[k + 1, centroidNames[(j - 1) *
                        2 + 1:2]] <- distAngle(X[k, ], X[k +
                        1, ], as.numeric(centroids[[j]][k + 1,
                        ]))
                    }
                  }
                  g <- model.matrix(newformula, cbind(subCovs[k +
                    1, , drop = FALSE], subSpatialcovs[k + 1,
                    , drop = FALSE])) %*% beta
                }
                else {
                  g <- gFull[k + 1, , drop = FALSE]
                }
                gamma[!gamma] <- exp(g)
                gamma <- t(gamma)
                gamma <- gamma/apply(gamma, 1, sum)
                Z[k + 1] <- sample(1:nbStates, size = 1, prob = gamma[Z[k],
                  ])
            }
            allStates <- c(allStates, Z)
            if (nbSpatialCovs > 0) {
                allSpatialcovs <- rbind(allSpatialcovs, subSpatialcovs)
            }
            if ("angle" %in% distnames) {
                if (dist[["angle"]] %in% angledists & ("step" %in%
                  distnames))
                  if (dist[["step"]] %in% stepdists) {
                    d$angle[1] <- NA
                    step0 <- which(d$step == 0)
                    d$angle[c(step0, step0 + 1)] <- NA
                    d$x = X[, 1]
                    d$y = X[, 2]
                  }
            }
            else if ("step" %in% distnames) {
                if (dist[["step"]] %in% stepdists) {
                  d$x = c(0, cumsum(d$step)[-nrow(d)])
                  d$y = X[, 2]
                }
            }
            if (length(centerInd))
                centerCovs[cumNbObs[zoo] + 1:nbObs, ] <- subCovs[,
                  centerNames]
            if (length(centroidInd))
                centroidCovs[cumNbObs[zoo] + 1:nbObs, ] <- subCovs[,
                  centroidNames]
            data <- rbind(data, d)
        }
        if (nbSpatialCovs > 0)
            colnames(allSpatialcovs) <- spatialcovnames
        if (nbCovs > 0)
            data <- cbind(data, allCovs)
        if (nbSpatialCovs > 0)
            data <- cbind(data, allSpatialcovs)
        if (length(centerInd)) {
            data <- cbind(data, centerCovs)
            for (j in which(grepl(".angle", names(data)))) {
                if (names(data[j]) %in% centerNames)
                  class(data[[j]]) <- c(class(data[[j]]), "angle")
            }
        }
        if (length(centroidInd)) {
            data <- cbind(data, centroidCovs)
            for (j in which(grepl(".angle", names(data)))) {
                if (names(data[j]) %in% centroidNames)
                  class(data[[j]]) <- c(class(data[[j]]), "angle")
            }
        }
        if (states)
            data <- cbind(data, states = allStates)
        for (i in distnames) {
            if (dist[[i]] %in% angledists)
                class(data[[i]]) <- c(class(data[[i]]), "angle")
        }
        out <- simObsData(momentuHMMData(data), lambda, errorEllipse)
        message("DONE")
        return(out)
    }
    else {
        simCount <- 0
        cat("Attempting to simulate tracks within spatial extent(s) of raster layers(s). Press 'esc' to force exit from 'simData'\n",
            sep = "")
        while (simCount < retrySims) {
            cat("\r    Attempt ", simCount + 1, " of ", retrySims,
                "...", sep = "")
            tmp <- suppressMessages(tryCatch(simData(nbAnimals,
                nbStates, dist, Par, beta, delta, formula, formulaDelta,
                covs, nbCovs, spatialCovs, zeroInflation, oneInflation,
                circularAngleMean, centers, centroids, obsPerAnimal,
                initialPosition, DM, cons, userBounds, workcons,
                stateNames, model, states, retrySims = 0, lambda,
                errorEllipse), error = function(e) e))
            if (inherits(tmp, "error")) {
                if (grepl("Try expanding the extent of the raster",
                  tmp))
                  simCount <- simCount + 1
                else stop(tmp)
            }
            else {
                simCount <- retrySims
                cat("DONE\n")
                return(tmp)
            }
        }
        cat("FAILED\n")
        stop(tmp)
    }
}
<environment: namespace:momentuHMM>

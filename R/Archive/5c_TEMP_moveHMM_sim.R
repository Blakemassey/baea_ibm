function (nbAnimals = 1, nbStates = 2, stepDist = c("gamma",
    "weibull", "lnorm", "exp"), angleDist = c("vm", "wrpcauchy",
    "none"), stepPar = NULL, anglePar = NULL, beta = NULL, covs = NULL,
    nbCovs = 0, zeroInflation = FALSE, obsPerAnimal = c(500,
        1500), model = NULL, states = FALSE)
{
    if (!is.null(model)) {
        nbStates <- ncol(model$mle$stepPar)
        stepDist <- model$stepDist
        angleDist <- model$angleDist
        stepPar <- c(t(model$mle$stepPar))
        anglePar <- c(t(model$mle$anglePar))
        beta <- model$mle$beta
        if (is.null(covs)) {
            covsCol <- which(names(model$data) != "ID" & names(model$data) !=
                "x" & names(model$data) != "y" & names(model$data) !=
                "step" & names(model$data) != "angle")
            covs <- model$data[, covsCol]
            if (length(covsCol) > 1) {
                names <- colnames(covs)
                covs <- data.frame(covs[, -1])
                colnames(covs) <- names[-1]
            }
            else covs <- NULL
        }
        zeroInflation <- model$conditions$zeroInflation
    }
    else {
        if (is.null(stepPar))
            stop("'stepPar' needs to be specified")
    }
    stepDist <- match.arg(stepDist)
    stepFun <- paste("r", stepDist, sep = "")
    angleDist <- match.arg(angleDist)
    angleFun <- paste("r", angleDist, sep = "")
    if (nbAnimals < 1)
        stop("nbAnimals should be at least 1.")
    if (nbStates < 1)
        stop("nbStates should be at least 1.")
    p <- parDef(stepDist, angleDist, nbStates, TRUE, zeroInflation)
    if (length(stepPar) != p$parSize[1] * nbStates | length(anglePar) !=
        p$parSize[2] * nbStates) {
        error <- "Wrong number of parameters: there should be"
        error <- paste(error, p$parSize[1] * nbStates, "step parameters and")
        error <- paste(error, p$parSize[2] * nbStates, "angle parameters")
        stop(error)
    }
    if (zeroInflation) {
        stepBounds <- p$bounds[1:((p$parSize[1] - 1) * nbStates),
            ]
        sp <- stepPar[1:(length(stepPar) - nbStates)]
        zm <- stepPar[(length(stepPar) - nbStates + 1):length(stepPar)]
        if (length(which(zm < 0 | zm > 1)) > 0)
            stop("The zero-mass should be in [0,1].")
    }
    else {
        stepBounds <- p$bounds[1:(p$parSize[1] * nbStates), ]
        sp <- stepPar
    }
    if (length(which(sp <= stepBounds[, 1] | sp >= stepBounds[,
        2])) > 0)
        stop(paste("Check the step parameters bounds (the parameters should be",
            "strictly between the bounds of their parameter space)."))
    if (angleDist != "none") {
        m <- anglePar[1:nbStates]
        k <- anglePar[(nbStates + 1):length(anglePar)]
        if (length(which(m <= (-pi) | m > pi)) > 0)
            stop("Check the angle parameters bounds. The angle mean should be in (-pi,pi].")
        if (length(which(k <= 0)) > 0)
            stop("Check the angle parameters bounds. The concentration should be strictly positive.")
        if (angleDist == "wrpcauchy" & length(which(k >= 1)) >
            0)
            stop("Check the angle parameters bounds. The concentration should be in (0,1).")
    }
    if (length(which(obsPerAnimal < 1)) > 0)
        stop("obsPerAnimal should have positive values.")
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
                for (j in 2:nrow(trackData)) if (is.na(covs[j,
                  i]))
                  covs[j, i] <- covs[j - 1, i]
            }
        }
    }
    if (length(obsPerAnimal) == 1)
        obsPerAnimal <- rep(obsPerAnimal, 2)
    else if (length(obsPerAnimal) != 2)
        stop("obsPerAnimal should be of length 1 or 2.")
    allNbObs <- rep(NA, nbAnimals)
    for (zoo in 1:nbAnimals) {
        if (obsPerAnimal[1] != obsPerAnimal[2])
            allNbObs[zoo] <- sample(obsPerAnimal[1]:obsPerAnimal[2],
                size = 1)
        else allNbObs[zoo] <- obsPerAnimal[1]
    }
    if (!is.null(covs)) {
        covnames <- colnames(covs)
        while (sum(allNbObs) > nrow(covs)) covs <- rbind(covs,
            covs)
        covs <- data.frame(covs[1:sum(allNbObs), ])
        colnames(covs) <- covnames
    }
    if (is.null(beta))
        beta <- matrix(rnorm(nbStates * (nbStates - 1) * (nbCovs +
            1)), nrow = nbCovs + 1)
    else if (nrow(beta) != nbCovs + 1 | ncol(beta) != nbStates *
        (nbStates - 1)) {
        if (nbStates > 1)
            stop(paste("beta should have ", nbCovs + 1, " rows and ",
                nbStates * (nbStates - 1), " columns.", sep = ""))
        else stop("beta should be NULL")
    }
    delta <- rep(1, nbStates)/nbStates
    wpar <- n2w(c(stepPar, anglePar), p$bounds, beta, delta,
        nbStates, estAngleMean = (angleDist != "none"))
    par <- w2n(wpar, p$bounds, p$parSize, nbStates, nbCovs, estAngleMean = (angleDist !=
        "none"), stationary = FALSE)
    if (zeroInflation) {
        zeroMass <- par$stepPar[nrow(par$stepPar), ]
        stepPar <- par$stepPar[-(nrow(par$stepPar)), ]
    }
    else {
        zeroMass <- rep(0, nbStates)
        stepPar <- par$stepPar
    }
    anglePar <- par$anglePar
    trackData <- NULL
    allCovs <- NULL
    allStates <- NULL
    data <- data.frame(ID = character(), step = numeric(), angle = numeric(),
        x = numeric(), y = numeric())
    for (zoo in 1:nbAnimals) {
        nbObs <- allNbObs[zoo]
        if (nbCovs > 0) {
            if (is.null(covs)) {
                subCovs <- data.frame(cov1 = rnorm(nbObs))
                if (nbCovs > 1) {
                  for (j in 2:nbCovs) {
                    c <- data.frame(rnorm(nbObs))
                    colnames(c) <- paste("cov", j, sep = "")
                    subCovs <- cbind(subCovs, c)
                  }
                }
            }
            else {
                if (zoo < 2)
                  ind1 <- 1
                else ind1 <- sum(allNbObs[1:(zoo - 1)]) + 1
                ind2 <- sum(allNbObs[1:zoo])
                subCovs <- data.frame(covs[ind1:ind2, ])
                if (!is.null(covs))
                  colnames(subCovs) <- colnames(covs)
            }
            allCovs <- rbind(allCovs, subCovs)
        }
        if (nbStates > 1) {
            Z <- rep(NA, nbObs)
            Z[1] <- sample(1:nbStates, size = 1, prob = delta)
            for (k in 2:nbObs) {
                gamma <- diag(nbStates)
                g <- beta[1, ]
                if (nbCovs == 1)
                  g <- g + beta[2, ] * subCovs[k, 1]
                if (nbCovs > 1) {
                  for (j in 1:nbCovs) g <- g + beta[j + 1, ] *
                    subCovs[k, j]
                }
                gamma[!gamma] <- exp(g)
                gamma <- t(gamma)
                gamma <- gamma/apply(gamma, 1, sum)
                Z[k] <- sample(1:nbStates, size = 1, prob = gamma[Z[k -
                  1], ])
            }
            allStates <- c(allStates, Z)
        }
        else Z <- rep(1, nbObs)
        X <- matrix(nbObs, nrow = nbObs, ncol = 2)
        X[1, ] <- c(0, 0)
        phi <- 0
        s <- rep(NA, nbObs)
        a <- rep(NA, nbObs)
        for (k in 1:(nbObs - 1)) {
            stepArgs <- list(1)
            angleArgs <- list(1)
            for (j in 1:nrow(stepPar)) stepArgs[[j + 1]] <- stepPar[j,
                Z[k]]
            if (angleDist != "none") {
                for (j in 1:nrow(anglePar)) angleArgs[[j + 1]] <- anglePar[j,
                  Z[k]]
            }
            if (stepDist == "gamma") {
                shape <- stepArgs[[2]]^2/stepArgs[[3]]^2
                scale <- stepArgs[[3]]^2/stepArgs[[2]]
                stepArgs[[2]] <- shape
                stepArgs[[3]] <- 1/scale
            }
            if (runif(1) > zeroMass[Z[k]])
                s[k] <- do.call(stepFun, stepArgs)
            else s[k] <- 0
            if (angleDist != "none" & s[k] > 0) {
                a[k] <- do.call(angleFun, angleArgs)
                if (a[k] > pi)
                  a[k] <- a[k] - 2 * pi
                if (a[k] < -pi)
                  a[k] <- a[k] + 2 * pi
                phi <- phi + a[k]
            }
            else if (s[k] == 0) {
                a[k] <- NA
            }
            m <- s[k] * c(Re(exp((0+1i) * phi)), Im(exp((0+1i) *
                phi)))
            X[k + 1, ] <- X[k, ] + m
        }
        a[1] <- NA
        d <- data.frame(ID = rep(zoo, nbObs), step = s, angle = a,
            x = X[, 1], y = X[, 2])
        data <- rbind(data, d)
    }
    if (!is.null(covs) & is.null(allCovs))
        allCovs <- covs
    if (nbCovs > 0)
        data <- cbind(data, allCovs)
    if (states)
        data <- cbind(data, states = allStates)
    return(moveData(data))
}

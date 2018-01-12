library(circular)

x <- rmixedvonmises(n=5000, mu1=circular(0), mu2=circular(pi/2), kappa1=15,
  kappa2=15, prop=0.5)
plot(x)

mean(as.vector(x))


mvm_pars <- mledist(as.vector(x), "mixedvonmises", silent=TRUE,  fix.arg = list(prop=0.5),
  start = list(mu1=.1, mu2=1.5, kappa1=10, kappa2=10))

mvm_pars$estimate

, fix.arg = list(prop=0.5), optim.method = "default",
  lower = -Inf, upper = Inf, custom.optim = NULL, weights = NULL, silent = TRUE,  gradient = NULL, ...)


































lcm <- function(a, b){
  if(a > b){
    a <- a + b
    b <- a - b
    a <- a - b
  }
  i <- 2
  comb <- 1
  while(i <= a){
    if(a %% i == 0 && b %% i == 0){
      a <- a / i
      b <- b / i
      comb <- comb * i
    } else {
      i <- i + 1
    }
  }
 return (comb * a * b)    # For the non common factors, count all of them
}

lcm1 <- function(list.num){
  LCM_so_far <- 1
  for(i in list.num) {
    LCM_so_far <- lcm(LCM_so_far, i)
    print(LCM_so_far)
  }
  return(LCM_so_far)
}

l <- lcm1(c(3,12,15))

print(l)


library(SPREDA)
install.packages("LearnBayes")
library(LearnBayes)


##RIGHT CENSORED DATA

#function for fitting a mixture distribution to RIGHT censored or complete data
mixcensored <- function (y, d=rep(1, length(y)), wt=rep(1, length(y)),
                         dist="weibull", n, cluster=NULL, classify="EM",
                         maxiter=100, tol=1e-6) {
  #y are the observations
  #d is the censoring indicator (1=failure; 0=censored)
  #wt are the weights for the observations
  #dist: either the "weibull", "lognormal", or "gaussian" distribution
  #n is the number of components
  #cluster: start with random initialization of posterior probabilities (=NULL), or
  # a matrix with n columns of initial posterior probabilities for the observations
  #classify: "EM", "CEM", or "SEM" strategy
  #maxiter is the maximum number of iterations
  #tol is the convergence criterion

  nobs <- sum(wt) #number of observations

  parms <- matrix(NA, ncol=3, nrow=n)
  colnames(parms) <- c("mu", "sigma", "logLik")
  stdErr <- matrix(NA, ncol=2, nrow=n)
  colnames(stdErr) <- c("mu", "log(sigma)")
  posteriorP <- matrix(NA, ncol=n, nrow=length(y))
  P <- matrix(NA, ncol=n, nrow=length(y))

  iteration <- 0 #initialize iteration counter
  loglikInit <- logLikC <- 0 #initialize log-likelihood estimates

  #initialize posterior probabilities
  if (is.null(cluster)) {
    alpha <- rep(.1, n)
    posteriorP <- rdirichlet(length(y), alpha)}
  else {posteriorP <- cluster}

  while (iteration < maxiter) {
    #estimate prior probabilities
    priorP <- apply(wt*posteriorP, 2, sum)/nobs

    #estimate distribution parameters for each component
    if (classify=="EM"){
      for (i in 1:n) {
        wtp <- ifelse(wt*posteriorP[,i]<=0, 1e-15, wt*posteriorP[,i])
        cp <- survreg(Surv(y,d)~1, weights=wtp, dist=dist)
        parms[i,] <- c(coef(cp)[1], cp$scale, logLik(cp)[1])
        stdErr[i,] <- sqrt(diag(vcov(cp)))}
    }

    else if (classify=="CEM"){
      compPost <- apply(posteriorP, 1, which.max)
      for (i in 1:n) {
        cp <- survreg(Surv(y,d)~1, weights=wt, dist=dist, subset=(compPost==i))
        parms[i,] <- c(coef(cp)[1], cp$scale, logLik(cp)[1])
        stdErr[i,] <- sqrt(diag(vcov(cp)))}
    }

    else if (classify=="SEM"){
      compPost <- apply(posteriorP, 1, function (p) sample(x=1:n, size=1, prob=p))
      for (i in 1:n) {
        cp <- survreg(Surv(y,d)~1, weights=wt, dist=dist, subset=(compPost==i))
        parms[i,] <- c(coef(cp)[1], cp$scale, logLik(cp)[1])
        stdErr[i,] <- sqrt(diag(vcov(cp)))}
    }

    #compute the (complete) log-likelihood value
    logLikC <- sum(parms[,3]) + sum(wt*(posteriorP%*%log(priorP)))
    logLikC <- ifelse(is.na(logLikC), 0, logLikC)

    #estimate posterior probabilities
    if (dist=="weibull") {
      for (j in 1:n) {
        mu <- parms[j,1]; sigma <- parms[j,2]
        z <- (log(y)-mu)/sigma
        P[,j] <- priorP[j]*((1/(sigma*y)*dsev(z))^d)*((1-psev(z))^(1-d))}
    }

    else if (dist=="lognormal") {
      for (j in 1:n) {
        mu <- parms[j,1]; sigma <- parms[j,2]
        z <- (log(y)-mu)/sigma
        P[,j] <- priorP[j]*((1/(sigma*y)*dnorm(z))^d)*((1-pnorm(z))^(1-d))}
    }

    else if (dist=="gaussian") {
      for (j in 1:n) {
        mu <- parms[j,1]; sigma <- parms[j,2]
        z <- (y-mu)/sigma
        P[,j] <- priorP[j]*((1/(sigma)*dnorm(z))^d)*((1-pnorm(z))^(1-d))}
    }

    posteriorP <- P/rowSums(P)

    #check convergence criterion
    if (( abs(logLikC-loglikInit) / (abs(loglikInit)+.001*tol) ) < tol) break
    loglikInit <- logLikC #update log-likelihood
    iteration <- iteration + 1 #increment counter
  }

  if (iteration == maxiter) warning("Maximum number of iterations exceeded")
  list(components=parms[,1:2], prior=priorP, loglik=logLikC,
       AIC=-2*logLikC + 2*(n-1+2*n), BIC=-2*logLikC + (n-1+2*n)*log(nobs),
       strategy=classify, distribution=dist, iterations=iteration,
       standardError=stdErr, posterior=posteriorP)
}

##mixture of WEIBULL distributions

#generate mixture of Weibull distributions
N <- 200 #number of observations
probs <- c(.3, .7) #mixing proportions
n <- length(probs) #number of components
component <- sample(1:n, prob=probs, size=N, replace=TRUE)
mus <- c(4, 8)
sigmas <- c(.7, .35)
library(SPREDA)
dataT <- exp(mus[component] + rsev(N)*sigmas[component])
plot(density(dataT)) #plot mixture of distributions

#apply censoring (here: Type I censoring)
#failure=1, censored=0
Cens <- ifelse(dataT>4000, 0, 1)
N-sum(Cens) #number of censored observations
dataT <- ifelse(Cens==0, 4000, dataT)

#fit distribution to components
(comp1 <- survreg(Surv(dataT, Cens)~1, subset=(component==1), dist="weibull"))
(comp2 <- survreg(Surv(dataT, Cens)~1, subset=(component==2), dist="weibull"))



#fit mixture distribution to data
mix <- mixcensored(y=dataT, d=Cens, dist="weibull", n=2)
mix[1:6]


library(circular)
x <- rmixedvonmises(n=100, mu1=circular(0), mu2=circular(pi/2), kappa1=15,
  kappa2=15, prop=0.25)
plot(x)
library(BAMBI)
fit_vmcosmix


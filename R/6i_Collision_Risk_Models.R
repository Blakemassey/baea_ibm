#S1: simFatal function

simFatal <- function(BMin=-1, Fatal=-1, SmpHrKm, ExpFac, aPriExp=1,
    bPriExp=1,aPriCPr=1, bPriCPr=1){

# BMin:     observed number of bird minutes
# Fatal:    annual avian fatalities on an operational wind facility
# SmpHrKm:  total time and area surveyed for bird minutes
# ExpFac:   expansion factor
# aPriExp:  alpha parameter for the prior on lambda
# bPriExp:  beta parameter for the prior on lambda
# aPriCPr:  alpha parameter for the prior on C
# bPriCPr:  beta parameter for the prior on C

# The default of a negative value for BMin or Fatal indicates that no data were collected for those model inputs

require(rv)

# Update the exposure prior
 if(BMin>=0){
   aPostExp <- aPriExp + BMin
   bPostExp <- bPriExp + SmpHrKm
  }else{
   aPostExp <- aPriExp
   bPostExp <- bPriExp}

 Exp <- rvgamma(n=1, aPostExp, bPostExp)

# Update the collisions prior
 if(Fatal>=0){
  aPostCPr <- aPriCPr + Fatal
  bPostCPr <- ((rvmean(Exp) * ExpFac) - Fatal) + bPriCPr
 }else{
  aPostCPr <- aPriCPr
  bPostCPr <- bPriCPr}

 CPr <- rvbeta(n=1, aPostCPr, bPostCPr)

 Fatalities             <- ExpFac * Exp * CPr
 attr(Fatalities,"Exp") <- c(Mean=rvmean(Exp), SD=rvsd(Exp))
 attr(Fatalities,"CPr") <- c(Mean=rvmean(CPr), SD=rvsd(CPr))

 return(Fatalities)}

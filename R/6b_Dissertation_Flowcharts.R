library(DiagrammeR)
library(DiagrammeRsvg)

############################################################################# ##
#### -------------------------- CHAPTER 1 --------------------------------- ####
############################################################################# ##

## MULTISCALE MODEL-FITTING OVERVIEW -------------------------------------------
logistic_fitting_methods <- readLines("R/Graphviz/Logistic_Fitting_Methods.gv")
write(export_svg(grViz(logistic_fitting_methods, engine = 'circo')),
  file="Products/Graphs/Flow_Charts/Logistic_Fitting_Methods.svg")

############################################################################# ##
#### -------------------------- CHAPTER 3 --------------------------------- ####
############################################################################# ##

## IBM BASIC STEPS OVERVIEW ----------------------------------------------------
ibm_step_overview <- readLines("R/Graphviz/IBM_Basic_Step_Overview.gv")
write(export_svg(grViz(ibm_step_overview, engine = 'circo')),
  file="Products/Graphs/Flow_Charts/IBM_Basic_Step_Overview.svg")

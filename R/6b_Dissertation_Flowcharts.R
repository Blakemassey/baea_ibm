library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"

############################################################################# ##
#### -------------------------- CHAPTER 1 --------------------------------- ####
############################################################################# ##

## MULTISCALE MODEL-FITTING OVERVIEW -------------------------------------------
logistic_fitting_methods <- readLines("R/Graphviz/Logistic_Fitting_Methods.gv")
write(export_svg(grViz(logistic_fitting_methods, engine = 'circo')),
  file = "Products/Graphs/Flow_Charts/Logistic_Fitting_Methods.svg")

# Export to Dissertation
file.copy("Products/Graphs/Flow_Charts/Logistic_Fitting_Methods.svg",
  file.path(tex_dir, "Figures/Ch1/Logistic_Fitting_Methods.svg"),
  overwrite = TRUE)

############################################################################# ##
#### -------------------------- CHAPTER 3 --------------------------------- ####
############################################################################# ##

## IBM BASIC STEPS OVERVIEW ----------------------------------------------------
ibm_step_overview <- readLines("R/Graphviz/IBM_Basic_Step_Overview.gv")
write(export_svg(grViz(ibm_step_overview, engine = 'circo')),
  file = "Products/Graphs/Flow_Charts/IBM_Basic_Step_Overview.svg")

# Export to Dissertation
file.copy("Products/Graphs/Flow_Charts/IBM_Basic_Step_Overview.svg",
  file.path(tex_dir, "Figures/Ch3/IBM_Basic_Step_Overview.svg"),
  overwrite = TRUE)

## STEPTYPE OPTIONS ------------------------------------------------------------
steptype_options <- readLines("R/Graphviz/StepType_Options.gv")
write(export_svg(grViz(steptype_options, engine = 'circo')),
  file = "Products/Graphs/Flow_Charts/StepType_Options_TEST.svg")
# NOTE: REMOVE '_TEST' ON FINAL RUN

# Export to Dissertation
file.copy("Products/Graphs/Flow_Charts/StepType_Options.svg",
  file.path(tex_dir, "Figures/Ch3/StepType_Options.svg"),
  overwrite = TRUE)

## STEPTYPE LEGEND OPTIONS -----------------------------------------------------
steptype_options_legend <- readLines("R/Graphviz/StepType_Options_Legend.gv")
write(export_svg(grViz(steptype_options_legend, engine = 'circo')),
  file = "Products/Graphs/Flow_Charts/StepType_Options_Legend_TEST.svg")
# NOTE: REMOVE '_TEST' ON FINAL RUN

# Export to Dissertation
file.copy("Products/Graphs/Flow_Charts/StepType_Options_Legend.svg",
  file.path(tex_dir, "Figures/Ch3/StepType_Options_Legend.svg"),
  overwrite = TRUE)


#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

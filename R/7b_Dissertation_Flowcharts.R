#---------------------- Dissertation Flowcharts -------------------------------#
# Tables: Save as .svg
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------
# Load packages
pacman::p_load(DiagrammeR, DiagrammeRsvg, magick, rsvg, tidyverse)

# Directories
tex_dir <- "C:/Users/Blake/OneDrive/Work/LaTeX/BMassey_Dissertation"

# -------------------------- CHAPTER 1 -----------------------------------------

# Multiscale Model-Fitting Overview --------------------------------------------
logistic_fitting_methods <- readLines(file.path("R/Graphviz",
  "Multiscale_Fitting_Methods.gv"))

write(export_svg(grViz(logistic_fitting_methods, engine = 'circo')),
  file = file.path(tex_dir, "Figures/Ch1/Multiscale_Fitting_Methods.svg"))

# -------------------------- CHAPTER 2 -----------------------------------------

# Behavior Transisitons Model --------------------------------------------------
behavior_transitions <- readLines(file.path("R/Graphviz",
  "Behavior_Transitions.gv"))

write(export_svg(grViz(behavior_transitions, engine = 'circo')),
  file = file.path(tex_dir, "Figures/Ch2/Behavior_Transitions.svg"))

# -------------------------- CHAPTER 3 -----------------------------------------

# IBM Basic Steps Overview -----------------------------------------------------
ibm_step_overview <- readLines("R/Graphviz/IBM_Basic_Step_Overview.gv")
write(export_svg(grViz(ibm_step_overview, engine = 'circo')),
  file = file.path(tex_dir, "Figures/Ch3/IBM_Basic_Step_Overview.svg"))

# Step-type Options ------------------------------------------------------------
steptype_options <- readLines("R/Graphviz/StepType_Options.gv")
write(export_svg(grViz(steptype_options, engine = 'circo')),
  file = "Products/Graphs/Flow_Charts/StepType_Options.svg")

# This operation adjusts the position of the behavior text labels to be in the
# middle of the nodes. If any changes are made to the .gv file, the values
# in this section will need to be adjusted.
step_type_svg  <- readLines("Products/Graphs/Flow_Charts/StepType_Options.svg")
step_type_updated_svg  <- step_type_svg %>%
  str_replace_all(pattern = 'y=\"-24.3\"', replace =  'y=\"-22.3\"') %>% #BotNod
  str_replace_all(pattern = 'y=\"-200.5\"', replace =  'y=\"-198.5\"') %>% #TopN
  str_replace_all(pattern = 'y=\"-207.7\"', replace =  'y=\"-208.7\"') %>% #Strt
  str_replace_all(pattern = 'y=\"-192.1\"', replace =  'y=\"-191.1\"') %>% #Beh
  str_replace_all(pattern = 'y=\"-31.5\"', replace =  'y=\"-32.5\"') %>% #End
  str_replace_all(pattern = 'y=\"-15.9\"', replace =  'y=\"-14.9\"') #Beh

# Export to Dissertation
writeLines(step_type_updated_svg, file.path(tex_dir,
  "Figures/Ch3/StepType_Options.svg"))

# Step-type Legend Options -----------------------------------------------------
steptype_options_legend <- readLines("R/Graphviz/StepType_Options_Legend.gv")
write(export_svg(grViz(steptype_options_legend, engine = 'circo')),
  file = "Products/Graphs/Flow_Charts/StepType_Options_Legend.svg")

# This operation adjusts the position of the text labels to be more spaced out.
# If any changes are made to the .gv file, the values in this section will need
# to be adjusted.
step_type_legend_svg  <- readLines(file.path("Products/Graphs/Flow_Charts",
  "StepType_Options_Legend.svg"))
step_type_legend_updated_svg  <- step_type_legend_svg %>%
  str_replace_all(pattern = 'y=\"-75.7\"', replace =  'y=\"-77.7\"') %>% #TopRow
  str_replace_all(pattern = 'y=\"-49.3\"', replace =  'y=\"-47.3\"') %>% #BotRow
  str_replace_all(pattern = 'y=\"-69.7\"', replace =  'y=\"-70.7\"') %>% #Locat
  str_replace_all(pattern = 'y=\"-54.1\"', replace =  'y=\"-53.1\"') #Changes

# Export to Dissertation
writeLines(step_type_legend_updated_svg,
  file.path(tex_dir, "Figures/Ch3/StepType_Options_Legend.svg"))

#------------------------------------------------------------------------------#
################################ OLD CODE ######################################
#------------------------------------------------------------------------------#

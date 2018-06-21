library(DiagrammeR)
library(DiagrammeRsvg)

# IBM_Basic_Step_Overview
ibm_step_overview <- readLines("R/Graphviz/IBM_Basic_Step_Overview.gv")
write(export_svg(grViz(ibm_step_overview, engine = 'circo')),
  file="Products/Graphs/Flow_Charts/IBM_Basic_Step_Overview.svg")

library(DiagrammeR)
library(DiagrammeRsvg)

graph1 <- readLines("R/Graphviz/IBM_Basic_Step_Overview.gv")
svg1 <- DiagrammeRsvg::export_svg(grViz(graph1, engine = 'circo'))
write(svg1, file="Data/Flow_Charts/IBM_Basic_Step_Overview.svg")

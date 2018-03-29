library(shiny)
library(leaflet)
library(plotly)

library(gisr)

ui <-
  fluidPage(
    wellPanel(
      fluidRow(
        column(width = 7,
            sliderInput(inputId = 'max_r', label = 'Maximum Radius',
              min = 10, max = 300, value = 100, step = 10, round = 0)
        ),
        column(width = 5,
            sliderInput(inputId = 'label_size', label = 'Label Size',
              min = 2, max = 8, value = 4, step = .5, round = 0)
        )
      )
    ),
    hr(),
    fluidRow(
      column(width=11, offset=0,
        plotOutput('circle_plot')
      )
    )
  )

server <- function(input, output) {

  output$circle_plot <- renderPlot({
    p <- PlotRasterCenterCellDistances(cellsize = 30, max_r = input$max_r,
      label_size = input$label_size)
    print(p)
    #ggplotly(p)
  }, height = 800, width = 1000)
}

shinyApp(ui, server)

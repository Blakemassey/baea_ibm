library(shiny)
library(leaflet)
library(plotly)
library(ibmr)

ui <-
  fluidPage(
    titlePanel("Redistribution Kernel"),
      sidebarLayout(
        sidebarPanel(width = 4,
            sliderInput(inputId = 'max_r', label = 'Maximum Radius',
              min = 100, max = 3000, value = 1000, step = 100, round = 0),
            sliderInput(inputId = 'cellsize', label = 'Cell Size',
              min = 10, max = 60, value = 30, step = 10, round = 0),
            sliderInput(inputId = 'mu', label = 'Wrapped Cauchy: Mu (degrees)',
              min = 0, max = 360, value = 0, step = 1, round = 2),
            sliderInput(inputId = 'rho', label = 'Wrapped Cauchy: Rho',
              min = 0, max = .999, value = .2, step = .05, round = 3),
            sliderInput(inputId = 'shape', label = 'Weibull: Shape',
              min = .0001, max = 5, value = .7, step = .1, round = 1),
            sliderInput(inputId = 'scale', label = 'Weibull: Scale',
              min = .0001, max = 5000, value = 100, step = 10, round = 0),
            checkboxGroupInput("ignore_distribution", "Ignore Distribution:",
                     c("Cauchy" = "cauchy",
                       "Weibull" = "weibull"), selected= NULL)
        ),
        mainPanel(width = 4,
          plotlyOutput('redist_kernel', width = "500px", height = "500px")
        )
      )
  )

server <- function(input, output) {
  output$redist_kernel <- renderPlotly({
    ignore_cauchy <- ifelse("cauchy" %in% input$ignore_distribution, TRUE,
      FALSE)
    ignore_weibull <-ifelse("weibull" %in% input$ignore_distribution, TRUE,
      FALSE)
    kernel <- CreateRedistKernelWeibull(max_r = input$max_r,
      cellsize = input$cellsize,
      mu = (input$mu*pi)/180, rho = input$rho, shape = input$shape,
      scale = input$scale, ignore_cauchy = ignore_cauchy,
      ignore_weibull = ignore_weibull)
    r <- (input$cellsize*((nrow(kernel)-1)/2))+(input$cellsize/2)
    kernel_raster <- raster::raster(kernel, xmn=-r, xmx=r, ymn=-r, ymx=r)
    df <- data.frame(raster::rasterToPoints(kernel_raster))
    names(df)[3] <- "Probability"
    ggplotly(ggplot(df, aes(x=x, y=y)) +
      geom_raster(aes(fill=Probability)) +
      coord_fixed(ratio = 1) +
      scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9,"Oranges")) +
      xlab("X") + ylab("Y") +
      coord_fixed(ratio = 1) +
      theme(text=element_text(size=20, colour="black")) +
      theme(axis.text=element_text(colour="black")) +
      theme(axis.title.x = element_text(angle = 0, vjust = 0, hjust=0.5)) +
      theme(axis.title.y = element_text(angle = 0, vjust = 0.5, hjust=0.5)) +
      theme(panel.grid = element_blank()) +
      theme(panel.background = element_rect(fill = "white", color = "black")),
      height = 800, width = 1000)
  },)# height = 800, width = 1000)
}

shinyApp(ui, server)

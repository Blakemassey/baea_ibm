    library(shiny)
    library(dplyr)
    library(leaflet)

    # Fake data
    df <- data.frame(lng = c(-5, -5, -5, -5, -15, -15, -10),
                     lat = c(8, 8, 8, 8, 33, 33, 20),
                     year = c(2018, 2018, 2018, 2017, 2017, 2017, 2016),
                     stringsAsFactors = FALSE)

    ui <- bootstrapPage(
      tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
      leafletOutput("map", width = "100%", height = "100%"),
      absolutePanel(top = 10, right = 10,
                    style="z-index:500;", # legend over my map (map z = 400)
                    tags$h3("map"),
                    sliderInput("periode", "Chronology",
                                min(df$year),
                                max(df$year),
                                value = range(df$year),
                                step = 1,
                                sep = ""
                    )
    )
    )

    server <- function(input, output, session) {

      # reactive filtering data from UI

      reactive_data_chrono <- reactive({
        df %>%
          filter(year >= input$periode[1] & year <= input$periode[2])
      })


      # static backround map
      output$map <- renderLeaflet({
        leaflet(df) %>%
          addTiles() %>%
          fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
      })

      # reactive circles map
      observe({
        leafletProxy("map", data = reactive_data_chrono()) %>%
          clearShapes() %>%
          addMarkers(lng=~lng,
                     lat=~lat,
                     layerId = ~id) # Assigning df id to layerid
      })
    }

    shinyApp(ui, server)


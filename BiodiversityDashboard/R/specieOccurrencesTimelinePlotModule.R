specieOccurrencesTimelinePlotUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("specieOccurrencesTimelineBox"))
}

specieOccurrencesTimelinePlotServer <- function(id, 
                                                subsetSelected,
                                                displayedMapBoundaries) {
  moduleServer(
    id,
    function(input, output, session) {
      targetSubset <- reactive({
        if (is.null(displayedMapBoundaries())) {
          subsetSelected()[0]
        } else if (isTRUE(input$applyMapBoundaries)) {
          subsetSelected()[
            latitudeDecimal <= displayedMapBoundaries()$north &
              latitudeDecimal >= displayedMapBoundaries()$south &
              longitudeDecimal <= displayedMapBoundaries()$east &
              longitudeDecimal >= displayedMapBoundaries()$west
          ]
        } else {
          subsetSelected()
        }
      })
      
      
      output$specieOccurrencesTimelineBox <- renderUI({
        box(
          width = 12,
          title = "Specie Occurrences Timeline",
          if (nrow(subsetSelected()) == 0) {
            HTML("Please select a specie and its' date range in order to display its 
             occurence timeline plot.")
          } else {
            fluidRow(
              column(
                width = 12,
                checkboxInput(
                  inputId = NS(id, "applyMapBoundaries"),
                  label = "Use only observations from within displayed map limits",
                  value = ifelse(is.null(input$applyMapBoundaries), 
                                 FALSE, 
                                 input$applyMapBoundaries)
                ),
                if (nrow(targetSubset()) == 0) {
                  HTML("No data to display for selected area")
                } else {
                  plotlyOutput(NS(id, "specieOccurrencesTimeline"))
                }
              )
            )
          }
        )
      })
      
      output$specieOccurrencesTimeline <- renderPlotly({
        fig <- plot_ly(
          x = targetSubset()$eventDate,
          y = targetSubset()$individualCount,
          name = "ploy",
          type = "bar"
        )  %>% layout(yaxis = list(title = 'Individual Count'),
                      xaxis = list(title = 'Date of Observation'))
      })
    }
  )
}
#' UI Module part for specie occurrences timeline plot
#'
#' @param id Module id
#'
#' @export
#'
#' @importFrom shiny NS uiOutput
specieOccurrencesTimelinePlotUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("specieOccurrencesTimelineBox"))
}

#' Server Module part for species occurrence timeline plot
#'
#' @description
#' Bar plot displaying observed specie individuals over time
#' 
#' @details
#' Bar plot is accompanies by checkbot. When ticked it limits plot data to
#' coordinates span provided via argument \code{displayedMapBoundaries}. Is
#' intended to be used with \code{\link{BiodiversityDashboard::specieOccurrencesMapServer()}}
#' with aforementioned coordinates being modules output.
#' 
#' @param id Module id
#' @param subsetSelected \code{data.frame} containing filtered data, 
#'        as returned by \code{\link{BiodiversityDashboard::dataSubsetSelectionServer()}}
#' @param displayedMapBoundaries named list with coordinates span to whin plot data will be limited to.
#'        By default taken as return from \code{\link{BiodiversityDashboard::specieOccurrencesMapServer()}}
#'
#' @export
#'
#' @importFrom plotly select plotlyOutput renderPlotly plot_ly layout
#' @importFrom shiny moduleServer reactive renderUI HTML fluidRow column checkboxInput NS
#' @importFrom shinydashboard box
specieOccurrencesTimelinePlotServer <- function(id, 
                                                subsetSelected,
                                                displayedMapBoundaries) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      targetSubset <- shiny::reactive({
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
      
      
      output$specieOccurrencesTimelineBox <- shiny::renderUI({
        shinydashboard::box(
          width = 12,
          title = "Specie Occurrences Timeline",
          if (nrow(subsetSelected()) == 0) {
            shiny::HTML("Please plotly::select a specie and its' date range in order to display its 
             occurence timeline plot.")
          } else {
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shiny::checkboxInput(
                  inputId = shiny::NS(id, "applyMapBoundaries"),
                  label = "Use only observations from within displayed map limits",
                  value = ifelse(is.null(input$applyMapBoundaries), 
                                 FALSE, 
                                 input$applyMapBoundaries)
                ),
                if (nrow(targetSubset()) == 0) {
                  shiny::HTML("No data to display for selected area")
                } else {
                  plotly::plotlyOutput(shiny::NS(id, "specieOccurrencesTimeline"))
                }
              )
            )
          }
        )
      })
      
      output$specieOccurrencesTimeline <- plotly::renderPlotly({
        fig <- plotly::plot_ly(
          x = targetSubset()$eventDate,
          y = targetSubset()$individualCount,
          name = "ploy",
          type = "bar"
        )  %>% plotly::layout(yaxis = list(title = 'Individual Count',
                                           tickformat = ',d'),
                      xaxis = list(title = 'Date of Observation'))
      })
    }
  )
}
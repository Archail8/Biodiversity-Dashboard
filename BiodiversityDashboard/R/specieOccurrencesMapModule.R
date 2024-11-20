#' UI Module part for species occurrence map display
#'
#' @param id Module id
#'
#' @export
#'
#' @importFrom shiny NS uiOutput
specieOccurrencesMapUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("specieOccurrencesBox"))
}


#' Server Module part for species occurrence map display
#' 
#' @description
#' Leaflet map with specie occurrences displayed as dots
#' 
#' @details
#' Map is limited to are covered by the dataset. Dots size scales with observed
#' individual count. Count is a sum of all observation at that specific point
#' across whole dataset. If dataset is empty prompt to select proper subset is
#' displayed. Map zoom in level is preserved across specie/date range changes.
#' 
#'
#' @param id Module id
#' @param speciesOccurrences \code{data.frame} containing full data, 
#'        \code{\link{BiodiversityDashboard::occurrence_pl}}
#' @param specieOccurrences \code{data.frame} containing filtered data, 
#'        as returned by \code{\link{BiodiversityDashboard::dataSubsetSelectionServer()}}
#'
#' @return named list with currently displayed map boundaries
#' @export
#'
#' @importFrom data.table setnames
#' @importFrom htmlwidgets onRender
#' @importFrom leaflet leafletOutput renderLeaflet leaflet addTiles fitBounds leafletProxy clearShapes addCircles
#' @importFrom shiny moduleServer reactiveVal observeEvent HTML renderUI tags uiOutput NS textOutput req renderText reactive
#' @importFrom shinydashboard box
#' @importFrom shinyjs runjs
specieOccurrencesMapServer <- function(id, 
                                      speciesOccurrences,
                                      specieOccurrences) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      #' map widget render is reactive to this flag, thus it is to
      #' be updated only when map display state is expected to switch
      isMapToBeShown <- shiny::reactiveVal(FALSE)
      
      shiny::observeEvent(specieOccurrences(), {
        if (nrow(specieOccurrences()) > 0) {
          if (!isMapToBeShown()) {
            isMapToBeShown(TRUE)
          }
        } else {
          if (isMapToBeShown()) {
            isMapToBeShown(FALSE)
          }
        }
      })
      
      #' address leaflet bug where zoom controls cover other elements, 
      #' ex. dropdown menu
      css = shiny::HTML("
        .leaflet-top, .leaflet-bottom {
          z-index: unset !important;
        }
        
        .leaflet-top, .leaflet-bottom {z-index: 95;}
      ")
      
      output$specieOccurrencesBox <- shiny::renderUI({
        shinydashboard::box(
          width = 12,
          title = "Specie Occurences Map",
          shiny::tags$head(shiny::tags$style(css)),
          shiny::uiOutput(shiny::NS(id, "specieOccurrencesMapConditional"))
        )
      })
      
      output$specieOccurrencesMapConditional <- shiny::renderUI({
        if (isMapToBeShown()) {
          leaflet::leafletOutput(shiny::NS(id, "specieOccurrencesMap"))
        } else {
          shinyjs::runjs(
            paste0(
              "Shiny.setInputValue('", 
              shiny::NS(id, "isSpecieOccurencesMapRendered"), 
              "', false);"
            )
          )
          shiny::textOutput(shiny::NS(id, "specieOccurenceSelectionRequest"))
        }
      })
      
      output$specieOccurrencesMap <- leaflet::renderLeaflet({
        shiny::req(isMapToBeShown())
        
        leaflet::leaflet(speciesOccurrences) %>% leaflet::addTiles() %>%
          leaflet::fitBounds(~min(speciesOccurrences$longitudeDecimal), 
                    ~min(speciesOccurrences$latitudeDecimal), 
                    ~max(speciesOccurrences$longitudeDecimal), 
                    ~max(speciesOccurrences$latitudeDecimal)) %>%
          htmlwidgets::onRender(
            paste0("
              function(el, x) {
                Shiny.setInputValue('", 
                shiny::NS(id, "isSpecieOccurencesMapRendered"), 
                "', true);
            }"
            )
          )
      })
      
      output$specieOccurenceSelectionRequest <- shiny::renderText({
        "Please select a specie and its' date range in order to display its 
        occurence on the map."
      })
      
      shiny::observeEvent(c(specieOccurrences(),
                     input$isSpecieOccurencesMapRendered), {
        shiny::req(isMapToBeShown())
        #' value of this flag is controlled by js on client side - it will not
        #' be set to TRUE until leaflet map is fully rendered and can be
        #' altered via proxy               
        shiny::req(input$isSpecieOccurencesMapRendered)
        
        totalOccurrence <- 
          specieOccurrences()[, sum(individualCount), 
                           by = c("longitudeDecimal", "latitudeDecimal")] %>%
          data.table::setnames("V1", "individualCounts")
        
        leaflet::leafletProxy("specieOccurrencesMap", data = specieOccurrences()) %>%
          leaflet::clearShapes() %>%
          leaflet::addCircles(lng = totalOccurrence$longitudeDecimal,
                     lat = totalOccurrence$latitudeDecimal,
                     popup = ~paste(totalOccurrence$individualCounts))
        
      })
      
      displayedMapBoundaries <- shiny::reactive({input$specieOccurrencesMap_bounds})
      
      return(displayedMapBoundaries)
    }
  )
}
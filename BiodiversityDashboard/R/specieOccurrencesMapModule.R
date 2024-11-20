specieOccurrencesMapUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("specieOccurrencesBox"))
}


specieOccurrencesMapServer <- function(id, 
                                      speciesOccurrences,
                                      specieOccurrences) {
  moduleServer(
    id,
    function(input, output, session) {
      isMapToBeShown <- reactiveVal(FALSE)
      
      observeEvent(specieOccurrences(), {
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
      css = HTML("
        .leaflet-top, .leaflet-bottom {
          z-index: unset !important;
        }
        
        .leaflet-top, .leaflet-bottom {z-index: 95;}
      ")
      
      output$specieOccurrencesBox <- renderUI({
        box(
          width = 12,
          title = "Specie Occurences Map",
          tags$head(tags$style(css)),
          uiOutput(NS(id, "specieOccurrencesMapConditional"))
        )
      })
      
      output$specieOccurrencesMapConditional <- renderUI({
        if (isMapToBeShown()) {
          leafletOutput(NS(id, "specieOccurrencesMap"))
        } else {
          shinyjs::runjs(
            paste0(
              "Shiny.setInputValue('", 
              NS(id, "isSpecieOccurencesMapRendered"), 
              "', false);"
            )
          )
          textOutput(NS(id, "specieOccurenceSelectionRequest"))
        }
      })
      
      output$specieOccurrencesMap <- renderLeaflet({
        req(isMapToBeShown())
        
        leaflet(speciesOccurrences) %>% addTiles() %>%
          fitBounds(~min(speciesOccurrences$longitudeDecimal), 
                    ~min(speciesOccurrences$latitudeDecimal), 
                    ~max(speciesOccurrences$longitudeDecimal), 
                    ~max(speciesOccurrences$latitudeDecimal)) %>%
          htmlwidgets::onRender(
            paste0("
              function(el, x) {
                Shiny.setInputValue('", 
                NS(id, "isSpecieOccurencesMapRendered"), 
                "', true);
            }"
            )
          )
      })
      
      output$specieOccurenceSelectionRequest <- renderText({
        "Please select a specie and its' date range in order to display its 
        occurence on the map."
      })
      
      observeEvent(c(specieOccurrences(),
                     input$isSpecieOccurencesMapRendered), {
        req(isMapToBeShown())
        req(input$isSpecieOccurencesMapRendered)
        
        totalOccurrence <- 
          specieOccurrences()[, sum(individualCount), 
                           by = c("longitudeDecimal", "latitudeDecimal")] %>%
          setnames("V1", "individualCounts")
        
        leafletProxy("specieOccurrencesMap", data = specieOccurrences()) %>%
          clearShapes() %>%
          addCircles(lng = totalOccurrence$longitudeDecimal,
                     lat = totalOccurrence$latitudeDecimal,
                     popup = ~paste(totalOccurrence$individualCounts))
        
      })
      
      displayedMapBoundaries <- reactive({input$specieOccurrencesMap_bounds})
      
      return(displayedMapBoundaries)
    }
  )
}
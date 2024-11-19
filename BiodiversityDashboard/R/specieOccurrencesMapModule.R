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
      output$specieOccurrencesBox <- renderUI({
        box(
          width = 12,
          title = "Specie Occurences",
          leafletOutput(NS(id, "specieOccurrencesMap"))
        )
      })
      
      output$specieOccurrencesMap <- renderLeaflet({
        leaflet(speciesOccurrences) %>% addTiles() %>%
          fitBounds(~min(speciesOccurrences$longitudeDecimal), 
                    ~min(speciesOccurrences$latitudeDecimal), 
                    ~max(speciesOccurrences$longitudeDecimal), 
                    ~max(speciesOccurrences$latitudeDecimal))
      })
      
      observeEvent(specieOccurrences(), {
        req(nrow(specieOccurrences()) > 0)
        
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
    }
  )
}
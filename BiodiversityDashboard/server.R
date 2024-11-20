function(input, output, session) {
  occurrence_pl <- readRDS("./data/occurrence_pl.RDS") %>% as.data.table
  
  selectedSubsetParametrization <-
    dataSubsetSelectionServer(
      "polishSpecies",
      occurrence_pl[, .(min(eventDate), max(eventDate)), 
                    by = c("scientificName", "vernacularName")] %>%
        setnames(c("V1", "V2"), c("minEventDate", "maxEventDate"))
      )
  
  displayedMapBoundaries <-
    specieOccurrencesMapServer("polishSpecies",
                               occurrence_pl[0],
                               list(
                                 minLongitude = min(occurrence_pl$longitudeDecimal),
                                 minlatitude = min(occurrence_pl$latitudeDecimal),
                                 maxLongitude = max(occurrence_pl$longitudeDecimal),
                                 maxlatitude = max(occurrence_pl$latitudeDecimal)
                               ),
                               reactive({
                                 occurrence_pl[
                                   scientificName == selectedSubsetParametrization()$scientificName &
                                   eventDate >= selectedSubsetParametrization()$minEventDate &
                                   eventDate <= selectedSubsetParametrization()$maxEventDate
                                 ][, sum(individualCount), 
                                   by = c("longitudeDecimal", "latitudeDecimal")] %>%
                                   data.table::setnames("V1", "individualCounts")
                               }))
  
  specieOccurrencesTimelinePlotServer("polishSpecies",
                                      reactive(occurrence_pl[
                                        scientificName == selectedSubsetParametrization()$scientificName &
                                          eventDate >= selectedSubsetParametrization()$minEventDate &
                                          eventDate <= selectedSubsetParametrization()$maxEventDate,
                                        c("eventDate", "individualCount", 
                                          "latitudeDecimal", "longitudeDecimal")
                                      ]),
                                      reactive(displayedMapBoundaries()))
  

}
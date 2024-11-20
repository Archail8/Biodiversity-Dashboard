function(input, output, session) {
  occurrence_pl <- readRDS("./data/occurrence_pl.RDS") %>% as.data.table
  
  subsetSelected <-
    dataSubsetSelectionServer("polishSpecies",
                              occurrence_pl)
  
  displayedMapBoundaries <- 
    specieOccurrencesMapServer("polishSpecies",
                               occurrence_pl,
                               reactive({subsetSelected()}))
  
  specieOccurrencesTimelinePlotServer("polishSpecies",
                                      reactive(subsetSelected()),
                                      reactive(displayedMapBoundaries()))
  

}
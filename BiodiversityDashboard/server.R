function(input, output, session) {
  occurrence_pl <- readRDS("./data/occurrence_pl.RDS") %>% as.data.table
  
  subsetSelected <-
    dataSubsetSelectionServer("polishSpecies",
                              occurrence_pl)
  
  specieOccurrencesMapServer("polishSpecies",
                            occurrence_pl,
                            reactive({subsetSelected()}))
  
}
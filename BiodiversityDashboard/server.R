function(input, output, session) {
  occurrence_pl <- readRDS("./data/occurrence_pl.rds")
  
  selectedSpecie <- specieSelectionServer("polishSpecies",
                                          unique(occurrence_pl$vernacularName),
                                          unique(occurrence_pl$scientificName))
  
}
occurrence_pl_elk <- readRDS("../../data/occurrence_pl.RDS") %>%
  as.data.table %>%
  .[vernacularName == "Elk"]

testServer(specieOccurrencesTimelinePlotServer,
           args = list(subsetSelected = reactiveVal(occurrence_pl_elk),
                       displayedMapBoundaries = reactiveVal(NULL)
                       ), {
  # check if targetSubset was initialized as empty subset
  expect_equal(nrow(targetSubset()), 0)

  # check if targetSubset updates properly when map boundaries are set and used
  session$setInputs(applyMapBoundaries = TRUE)
  displayedMapBoundaries(list(west = 20, east = 25, north = 55, south = 40))
  expect_equal(nrow(targetSubset()), 591)
})

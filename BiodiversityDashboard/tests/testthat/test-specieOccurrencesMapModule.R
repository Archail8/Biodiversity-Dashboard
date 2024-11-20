occurrence_pl <- readRDS("../../data/occurrence_pl.RDS") %>% as.data.table

testServer(specieOccurrencesMapServer,
           args = list(speciesOccurrences = reactiveVal(occurrence_pl),
                       specieOccurrences = reactiveVal(occurrence_pl[0])), {

   # check that isMapToBeShown flag is initialized as FALSE
   expect_equal(isMapToBeShown(), FALSE)

   # update data subset and check that isMapToBeShown is now TRUE
   specieOccurrences(speciesOccurrences()[vernacularName == "Elk"])
   session$flushReact()
   expect_equal(isMapToBeShown(), TRUE)
})

occurrence_pl <- readRDS("../../data/occurrence_pl.RDS") %>% as.data.table

testServer(specieOccurrencesMapServer,
           args = list(leafletDataSkeleton = occurrence_pl[0],
                       leafletMapBounderies = list(
                         minLongitude = min(occurrence_pl$longitudeDecimal),
                         minlatitude = min(occurrence_pl$latitudeDecimal),
                         maxLongitude = max(occurrence_pl$longitudeDecimal),
                         maxlatitude = max(occurrence_pl$latitudeDecimal)
                       ),
                       specieOccurrences = reactiveVal(
                         occurrence_pl[vernacularName == "Elk"]
                         [, sum(individualCount),
                          by = c("longitudeDecimal",
                          "latitudeDecimal")] %>%
                         data.table::setnames("V1", "individualCounts")
                       )
                         ), {

   # check that isMapToBeShown flag is initialized as FALSE
   expect_equal(isMapToBeShown(), FALSE)

   # force observeEvent() execution an check that isMapToBeShown flag
   # updates properly
   session$flushReact()
   expect_equal(isMapToBeShown(), TRUE)
})

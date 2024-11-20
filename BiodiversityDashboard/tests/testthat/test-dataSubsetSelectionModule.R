#' selectizeInput is not found on output list, when run with testServer.
#' Thus testing is very limited
testServer(dataSubsetSelectionServer,
           args = list(speciesObservationDates = readRDS("../../data/occurrence_pl.RDS") %>%
                         as.data.table %>%
                         .[, .(min(eventDate), max(eventDate)),
                           by = c("scientificName", "vernacularName")] %>%
                         setnames(c("V1", "V2"), c("minEventDate", "maxEventDate"))), {
  session$setInputs(vernacularNameSelectize = "Elk")
  session$setInputs(scientificNameSelectize = "Alces alces")

  # check that dateRangeConditional rendered
  expect_equal(!is.null(output$dateRangeConditional), TRUE)

  # check that module output is a list
  expect_equal(class(session$returned()), c("list"))
})

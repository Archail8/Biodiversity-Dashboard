occurrence_pl <- readRDS("../../data/occurrence_pl.RDS") %>% as.data.table

#' selectizeInput is not found on output list, when run with testServer.
#' Thus testing is very limited
testServer(dataSubsetSelectionServer,
           args = list(specieOccurrences = occurrence_pl), {
  session$setInputs(vernacularNameSelectize = "Elk")
  session$setInputs(scientificNameSelectize = "Alces alces")

  # check that dateRangeConditional rendered
  expect_equal(!is.null(output$dateRangeConditional), TRUE)

  # check that module output is a data.table
  expect_equal(class(session$returned()), c("data.table", "data.frame"))
})

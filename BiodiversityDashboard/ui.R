library(shinydashboard)
library(shinyjs)
library(leaflet)
library(RColorBrewer)
library(magrittr)
library(data.table)

source("./R/dataSubsetSelectionModule.R")
source("./R/specieOccurrencesMapModule.R")

header <- dashboardHeader(
  title = "Biodiversity for Poland"
)

body <- dashboardBody(
  useShinyjs(),
  dataSubsetSelectionUI("polishSpecies"),
  specieOccurrencesMapUI("polishSpecies")
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
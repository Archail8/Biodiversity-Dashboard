library(shinydashboard)
library(shinyjs)
library(leaflet)
library(RColorBrewer)
library(magrittr)
library(data.table)
library(plotly)

source("./R/dataSubsetSelectionModule.R")
source("./R/specieOccurrencesMapModule.R")
source("./R/specieOccurrencesTimelinePlotModule.R")

header <- dashboardHeader(
  title = "Biodiversity for Poland"
)

body <- dashboardBody(
  useShinyjs(),
  dataSubsetSelectionUI("polishSpecies"),
  specieOccurrencesMapUI("polishSpecies"),
  specieOccurrencesTimelinePlotUI("polishSpecies")
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
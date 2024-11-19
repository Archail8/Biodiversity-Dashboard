library(shinydashboard)
library(shinyjs)

source("./R/specieSelectionModule.R")

header <- dashboardHeader(
  title = "Biodiversity for Poland"
)

body <- dashboardBody(
  useShinyjs(),
  specieSelectionUI("polishSpecies"),
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
library(shinydashboard)

header <- dashboardHeader(
  title = "Biodiversity for Poland"
)

body <- dashboardBody()

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
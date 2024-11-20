#' UI Module part for Biodiversity data subset selection
#'
#' @param id Module id
#'
#' @export
#'
#' @importFrom shiny NS uiOutput
dataSubsetSelectionUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("dataSubsetSelection"))
}

#' Server Module part for Biodiversity data subset selection
#'
#' @description
#' Box with a couple of widgets for narrowing down underlying data set
#' 
#' @details
#' Widgets are:
#'  - Use scientific name checkbox
#'  - Specie scientific/vernacular single item selectize. Of those two only one
#'    will be displayed at any given time, which one depends on checkbox state.
#'    Mind that both will always be rendered and updated so that names match.
#'  - dateRange widget limited to max/min observation dates for selected specie.
#'    Widget will be replaced with 'missing data' info if no specie is selected.
#' 
#' @param id Module id
#' @param specieOccurrences data.table of the following structure 
#'        \code{\link{BiodiversityDashboard::occurrence_pl}}
#'
#' @return reactive \code{data.table}, subset of specieOccurrences input data
#' @export
#'
#' @importFrom lubridate is.Date
#' @importFrom shiny moduleServer renderUI fluidRow column selectizeInput NS checkboxInput uiOutput HTML dateRangeInput observeEvent updateSelectInput reactive
#' @importFrom shinydashboard box
#' @importFrom shinyjs hidden hide show
dataSubsetSelectionServer <- function(id, specieOccurrences) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$dataSubsetSelection <- shiny::renderUI({
        shinydashboard::box(
          width = 12,
          title = "Data subset selection",
          shiny::fluidRow(
            shiny::column(
              width = 5,
              shinyjs::hidden(
                shiny::selectizeInput(
                  inputId = shiny::NS(id, "scientificNameSelectize"),
                  label = NULL,
                  choices = c("", unique(specieOccurrences$scientificName)),
                  selected = NULL,
                  multiple = FALSE,
                  options = list(placeholder = "scientific name of the specie")
                )
              ),
              shiny::selectizeInput(
                inputId = shiny::NS(id, "vernacularNameSelectize"),
                label = NULL,
                choices = c("", unique(specieOccurrences$vernacularName)),
                selected = NULL,
                multiple = FALSE,
                options = list(placeholder = "vernacular name of the specie")
              )
            ),
            shiny::column(
              width = 5,
              shiny::checkboxInput(
                inputId = shiny::NS(id, "useScientificNamesCheckbox"),
                label = "Use scientific name of the specie",
                value = FALSE
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::uiOutput(shiny::NS(id, "dateRangeConditional"))
            )
          )
        )
      })
      
      output$dateRangeConditional <- shiny::renderUI({
        if (input$scientificNameSelectize == "" ||
            input$vernacularNameSelectize == "") {
          shiny::HTML("Please select a specie prior to establishing date range")
        } else {
          earliestEventDate <- 
            min(specieOccurrences[
                  scientificName == input$scientificNameSelectize,
                  eventDate])
          latestEventDate <- 
            max(specieOccurrences[
                  scientificName == input$scientificNameSelectize,
                  eventDate])
          shiny::dateRangeInput(shiny::NS(id, "dateRange"), "Date Range", 
                         start = earliestEventDate,
                         end = latestEventDate,
                         min = earliestEventDate,
                         max = latestEventDate
          )
        }
      })
      
      shiny::observeEvent(input$useScientificNamesCheckbox, {
        if (input$useScientificNamesCheckbox) {
          shinyjs::hide("vernacularNameSelectize")
          shinyjs::show("scientificNameSelectize")
        } else {
          shinyjs::show("vernacularNameSelectize")
          shinyjs::hide("scientificNameSelectize")
        }
      })
      
      shiny::observeEvent(input$vernacularNameSelectize, {
        shiny::updateSelectInput(
          inputId = "scientificNameSelectize",
          selected = specieOccurrences[
            vernacularName == input$vernacularNameSelectize,
            scientificName]
          )
      })
      
      shiny::observeEvent(input$scientificNameSelectize, {
        shiny::updateSelectInput(
          inputId = "vernacularNameSelectize",
          selected = specieOccurrences[
            scientificName == input$scientificNameSelectize,
            vernacularName]
        )
      })
      
      #' recalculates twice due to dateRangeInput rendering with end==start on 
      #' specie change
      subsetSelected <- shiny::reactive({
        areDatesValid <- 
          lubridate::is.Date(input$dateRange[1]) &
          lubridate::is.Date(input$dateRange[2])
        if (areDatesValid) {
          specieOccurrences[scientificName == input$scientificNameSelectize &
                            eventDate >= input$dateRange[1] &
                            eventDate <= input$dateRange[2]]
        } else {
          specieOccurrences[0]
        }
      })
      
      return(subsetSelected)
    }
  )
}
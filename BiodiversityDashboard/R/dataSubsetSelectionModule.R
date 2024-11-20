dataSubsetSelectionUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dataSubsetSelection"))
}

dataSubsetSelectionServer <- function(id, specieOccurrences) {
  moduleServer(
    id,
    function(input, output, session) {
      output$dataSubsetSelection <- renderUI({
        box(
          width = 12,
          title = "Data subset selection",
          fluidRow(
            column(
              width = 5,
              shinyjs::hidden(
                selectizeInput(
                  inputId = NS(id, "scientificNameSelectize"),
                  label = NULL,
                  choices = c("", unique(specieOccurrences$scientificName)),
                  selected = NULL,
                  multiple = FALSE,
                  options = list(placeholder = "scientific name of the specie")
                )
              ),
              selectizeInput(
                inputId = NS(id, "vernacularNameSelectize"),
                label = NULL,
                choices = c("", unique(specieOccurrences$vernacularName)),
                selected = NULL,
                multiple = FALSE,
                options = list(placeholder = "vernacular name of the specie")
              )
            ),
            column(
              width = 5,
              checkboxInput(
                inputId = NS(id, "useScientificNamesCheckbox"),
                label = "Use scientific name of the specie",
                value = FALSE
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              uiOutput(NS(id, "dateRangeConditional"))
            )
          )
        )
      })
      
      output$dateRangeConditional <- renderUI({
        if (input$scientificNameSelectize == "" ||
            input$vernacularNameSelectize == "") {
          HTML("Please select a specie prior to establishing date range")
        } else {
          earliestEventDate <- 
            min(specieOccurrences[
                  scientificName == input$scientificNameSelectize,
                  eventDate])
          latestEventDate <- 
            max(specieOccurrences[
                  scientificName == input$scientificNameSelectize,
                  eventDate])
          dateRangeInput(NS(id, "dateRange"), "Date Range", 
                         start = earliestEventDate,
                         end = latestEventDate,
                         min = earliestEventDate,
                         max = latestEventDate
          )
        }
      })
      
      observeEvent(input$useScientificNamesCheckbox, {
        if (input$useScientificNamesCheckbox) {
          shinyjs::hide("vernacularNameSelectize")
          shinyjs::show("scientificNameSelectize")
        } else {
          shinyjs::show("vernacularNameSelectize")
          shinyjs::hide("scientificNameSelectize")
        }
      })
      
      observeEvent(input$vernacularNameSelectize, {
        updateSelectInput(
          inputId = "scientificNameSelectize",
          selected = specieOccurrences[
            vernacularName == input$vernacularNameSelectize,
            scientificName]
          )
      })
      
      observeEvent(input$scientificNameSelectize, {
        updateSelectInput(
          inputId = "vernacularNameSelectize",
          selected = specieOccurrences[
            scientificName == input$scientificNameSelectize,
            vernacularName]
        )
      })
      
      #' recalculates twice due to dateRangeInput rendering with end==start on 
      #' specie change
      subsetSelected <- reactive({
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
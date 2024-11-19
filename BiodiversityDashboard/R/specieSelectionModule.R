specieSelectionUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("specieSelection"))
}

specieSelectionServer <- function(id, vernacularNames, scientificNames) {
  moduleServer(
    id,
    function(input, output, session) {
      output$specieSelection <- renderUI({
        box(
          width = 12,
          title = "Specie selection",
          fluidRow(
            column(
              width = 5,
              shinyjs::hidden(
                selectizeInput(
                  inputId = NS(id, "scientificNameSelectize"),
                  label = NULL,
                  choices = c("", scientificNames),
                  selected = NULL,
                  multiple = FALSE,
                  options = list(placeholder = "scientific name of the specie")
                )
              ),
              selectizeInput(
                inputId = NS(id, "vernacularNameSelectize"),
                label = NULL,
                choices = c("", vernacularNames),
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
          )
        )
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
      
      specieSelected <- reactive({
        validate(need(!is.null(input$useScientificNamesCheckbox), message = FALSE))
        
        if (input$useScientificNamesCheckbox) {
          input$scientificNameSelectize
        } else {
          input$vernacularNameSelectize
        }
      })
      
      return(specieSelected)
    }
  )
}
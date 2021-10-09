### Corticosterone Plate Analysis Module

# https://shiny.rstudio.com/articles/modules.html


uploadCortEIAUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      div(
        class = "col-xs-6",
        fileInput(
          ns("dataFile"),
          label = "Select CSV File",
          accept = ".csv"
        ),
        textOutput(ns("fileStatus")),
      ),
      div(
        class = "col-xs-6",
        a("Example CSV File", href = "exampleCortEIAPlate.csv"),
        br(),
        a("Template CSV File", href = "templateCortEIAPlate.csv")
      )
    ),
    cortEIAUI(ns("cortEIA"))
  )
}


uploadCortEIAServer <- function(
  id,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        req(input$dataFile$datapath %>% path_ext() == "csv")
        cortEIAServer(
          "cortEIA",
          input$dataFile$datapath,
          input$dataFile$name,
          compType
        )
      })
      
      output$fileStatus <- renderText({
        req(input$dataFile)
        ext <- input$dataFile$datapath %>% path_ext()
        validate(
          need(ext == "csv", "Please upload a csv file")
        )
        
      })
      
    }
  )
}
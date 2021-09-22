### Cycles App Module

# https://shiny.rstudio.com/articles/modules.html


# https://gist.github.com/wch/5436415/#gistcomment-1646351 - some initial inspiration for loading imgs


LBNCyclesUI <- function(id, off_data){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        4,
        selectInput(
          ns("cohort"),
          "Which cohorts?",
          choices = unique(off_data$cohort),
          multiple = FALSE,
          selected = unique(off_data$cohort),
        )
      )
    ),
    dataTableOutput(ns("testTable")),
    cyclesUI(ns("cycles"))
  )
}


LBNCyclesServer <- function(
  id,
  damInfo,
  offspringInfo,
  Cycles_off,
  CohortCyclingFolder
){
  moduleServer(
    id,
    function(input, output, session) {
      folderImgsServer("folderImgs")
      
      # Prepare Dataframes ------------------------------------------------------
      cycleDir <- reactive({
        req(input$cohort)
        cycleDir <- CohortCyclingFolder$cyclingFolderPath[which(CohortCyclingFolder$cohort == input$cohort)]
        return(cycleDir)
      })
      
      cycles_react <- reactive({
        req(input$cohort)
        df <- Cycles_off %>%
          filter(
            cohort %in% input$cohort
          ) %>%
          mutate(
            cycleStartDate = DOB + 70,
            cycleID = num_ID
          )
        return(df)
      })
      
      output$testTable <- renderDataTable({
        cycles_react()
      })
      
      observe({
        req(input$cohort)
        cyclesServer(
          "cycles",
          cycleDir(),
          damInfo,
          offspringInfo,
          cycles_react()
        )
      })
      
    }
  )
}


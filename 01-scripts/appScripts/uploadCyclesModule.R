### Cycles App Module

# https://shiny.rstudio.com/articles/modules.html


# https://gist.github.com/wch/5436415/#gistcomment-1646351 - some initial inspiration for loading imgs

uploadCyclesUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Load Excel",
        fluidRow(
          column(
            6,
            fileInput(
              ns("dataFile"),
              label = "Select Excel File",
              accept = ".xlsx"
            )
          ),
          column(
            6,
            a("Example Excel File", href = "cycleFileExample.xlsx")
          )
        ),
        
        cyclesUI(ns("cycles"))
      ),
      tabPanel(
        "View Images",
        folderImgsUI(ns("folderImgs"))
      )
    )
  )
}


uploadCyclesServer <- function(
  id
){
  moduleServer(
    id,
    function(input, output, session) {
      folderImgsServer("folderImgs")
      
      # Prepare Dataframes ------------------------------------------------------
      cycleDir <- reactive({
        req(input$dataFile)
        fileInfo <- loadExcelSheet_fromFile(input$dataFile$datapath, "fileInfo")
        cycleDir <- fileInfo$cycleImgFolder[1]
        cycleDir <- normalizePath(cycleDir)
        return(cycleDir)
      })
      
      damInfo <- reactive({
        req(input$dataFile)
        loadExcelSheet_fromFile(input$dataFile$datapath, "DamInfo")
      })
      offspringInfo <- reactive({
        req(input$dataFile)
        loadExcelSheet_fromFile(input$dataFile$datapath, "OffspringInfo")
      })
      cycles <- reactive({
        req(input$dataFile)
        loadExcelSheet_fromFile(input$dataFile$datapath, "Cycles_off")
      })
      
      cycles_react <- reactive({
        req(input$dataFile)
        df <- cycles() %>%
          left_join(
            offspringInfo(),
            by = "mouseID"
          ) %>%
          left_join(
            damInfo(),
            by = "damID"
          )
        return(df)
      })
      
      observe({
        req(input$dataFile)
        cyclesServer(
          "cycles",
          cycleDir(),
          damInfo(),
          offspringInfo(),
          cycles_react()
        )
      })
      
    }
  )
}


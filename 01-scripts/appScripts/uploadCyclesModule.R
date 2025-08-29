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
          div(
            class = "col-xs-6",
            shinyDirButton(ns("cycleDir"), "Select Cycle Image Folder", "Please select a folder"),
            fileInput(
              ns("dataFile"),
              label = "Select Excel File",
              accept = ".xlsx"
            ),
            textOutput(ns("fileStatus"))
          ),
          div(
            class = "col-xs-6",
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
  id,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      folderImgsServer("folderImgs")
      
      # Folder Picker -----------------------------------------------------------
      shinyDirChoose(input, "cycleDir", roots = c(home = "~"), session = session)
      cycleDir <- reactive({
        req(input$cycleDir)
        parseDirPath(c(home = "~"), input$cycleDir)
      })

      # Prepare Dataframes ------------------------------------------------------
      damInfo <- reactive({
        req(input$dataFile$datapath %>% path_ext() == "xlsx")
        loadExcelSheet_fromFile(input$dataFile$datapath, "DamInfo")
      })
      offspringInfo <- reactive({
        req(input$dataFile$datapath %>% path_ext() == "xlsx")
        loadExcelSheet_fromFile(input$dataFile$datapath, "OffspringInfo")
      })
      cycles <- reactive({
        req(input$dataFile$datapath %>% path_ext() == "xlsx")
        loadExcelSheet_fromFile(input$dataFile$datapath, "Cycles_off")
      })

      cycles_react <- reactive({
        req(input$dataFile$datapath %>% path_ext() == "xlsx")
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
        req(input$dataFile$datapath %>% path_ext() == "xlsx")
        req(cycleDir())
        cyclesServer(
          "cycles",
          cycleDir(),
          damInfo(),
          offspringInfo(),
          cycles_react(),
          compType
        )
      })

      output$fileStatus <- renderText({
        req(input$dataFile)
        ext <- input$dataFile$datapath %>% path_ext()
        validate(
          need(ext == "xlsx", "Please upload an xlsx file")
        )
      })
    }
  )
}
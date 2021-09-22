### Cycles App Module

# https://shiny.rstudio.com/articles/modules.html


# https://gist.github.com/wch/5436415/#gistcomment-1646351 
get_plot_bootstrapjs_div <- function(plot_object_list, id_prefix) {
  #### local_function
  get_col_div <- function(plot_object_list, id_prefix, index, css_class = 'col-xs-12 col-sm-6')  {
    col_div <- div(class = css_class)
    
    if(length(plot_object_list) >= index) {
      plot_name <- paste0(id_prefix, '_', index)
      plot_output_object <- plotOutput(plot_name)
      plot_output_object <- renderPlot(plot_object_list[[index]])
      col_div <- tagAppendChild(col_div, plot_output_object)
    }
    return(col_div)
  }
  #
  get_plot_div <- function(plot_object_list, id_prefix) {
    result_div <- div(class = 'container-fluid')
    
    for(i in 1:length(plot_object_list)) {
      row_div <- div(class = 'row')
      row_div <- tagAppendChild(row_div, get_col_div(plot_object_list, id_prefix, i))
      row_div <- tagAppendChild(row_div, get_col_div(plot_object_list, id_prefix, i+1))    
      result_div <- tagAppendChild(result_div, row_div)
    }
    return(result_div)
  }
  ####
  plot_output_list_div <- get_plot_div(plot_object_list, id_prefix)
  
  return(plot_output_list_div)
}

get_plot_object_list <- function(max_plots, input_n) {
  result_plot_list <- lapply(1:input_n, function(i) {
    plot(1:i, 1:i,
         xlim = c(1, max_plots), ylim = c(1, max_plots),
         main = paste("1:", i, ".  n is ", input_n, sep = "")
    )
  })
  return(result_plot_list)
}

get_plot_output_list_div <- function(max_plots, input_n) {
  plot_object_list <- get_plot_object_list(max_plots, input_n)
  plot_output_div <- get_plot_bootstrapjs_div(plot_object_list, 'ui_plot')
  return(plot_output_div)
}

# https://stackoverflow.com/questions/38822863/shiny-dynamic-colour-fill-input-for-ggplot 
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

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
        
        fluidRow(
          column(
            4,
            radioButtons(
              ns("xVar"),
              "Plot by:",
              c("Cycle Day" = "day", "PND" = "PND")
            )
          ),
          column(
            4,
            checkboxInput(
              ns("useLineColor"),
              "Use line color",
              value = FALSE
            )
          ),
          column(
            4,
            varSelectInput(
              ns("lineColorVar"),
              "Line Color Variable",
              NULL
            )
          )
        ),
        fluidRow(
          uiOutput(ns("colorUI"))
        ),
        
        actionButton(ns("resetColors"), "Default Colors", icon = icon("undo")),
        
        tabsetPanel(
          tabPanel(
            "Cycle Plots",
            plotOutput(ns("cyclesPlot"), height = "600px")
          ),
          tabPanel(
            "by mouse",
            fluidRow(
              column(
                4,
                selectInput(
                  ns("selectedMouse"),
                  label = "Select Mouse:",
                  choices = character()
                )
              )
            ),
            plotOutput(
              ns("selCyclePlot"),
              height = "200px"
            ),
            dataTableOutput(
              ns("fileTable")
            ),
            uiOutput(
              ns("selImages")
            )
          )
        )
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
      
      cycles_long <- reactive({
        req(input$dataFile)
        df <- cycles_react() %>%
          makeCyclesLong(afterVar = cycleStartDate) %>%
          mutate(
            cycleDate = cycleStartDate + (day - 1),
            PND = cycleDate - DOB,
            .after = stage
          )
        return(df)
      })

      # Cycle Plots Panel -------------------------------------------------------

      ## Color Variable and Levels ------------------
      observeEvent(cycles_react(), {
        updateVarSelectInput(
          session,
          "lineColorVar",
          data = cycles_react() %>% 
              select(where(~ is.character(.x) | is.factor(.x)))
        )
      })
      
      lineColorLevels <- reactive({
        req(input$dataFile, input$lineColorVar)
        unique(cycles_long()[[input$lineColorVar]])
      })
      
      output$colorUI <- renderUI({
        req(input$dataFile, input$lineColorVar)
        
        if(input$useLineColor){
          lev <- lineColorLevels()
          cols <- gg_fill_hue(length(lev))
          
          # New IDs "col+level"
          lapply(seq_along(lev), function(i) {
            column(
              4,
              colourInput(inputId = session$ns(paste0("col", lev[i])),
                          label = paste0("Choose colour for ", lev[i]), 
                          value = cols[i]
              )
            )
          })
        }
      })
      
      observeEvent(input$resetColors, {
        # Problem: dynamic number of widgets
        # - lapply, do.call
        
        lev <- lineColorLevels()
        cols <- gg_fill_hue(length(lev))
        
        lapply(seq_along(lev), function(i) {
          do.call(what = "updateColourInput",
                  args = list(
                    session = session,
                    inputId = paste0("col", lev[i]),
                    value = cols[i]
                  )
          )
        })
      })
      
      ## Plot --------------------------
      output$cyclesPlot <- renderPlot({
        req(input$dataFile, input$lineColorVar)
        
        
        if(input$useLineColor){
          lineColor = expr(.data[[input$lineColorVar]])
          noLegends = FALSE
          lev <- lineColorLevels()
          cols <- paste0("c(", paste0("input$col", lev, collapse = ", "), ")")
          # print(cols)
          cols <- eval(parse(text = cols))
          # print(cols)
          
          req(length(lev) == length(cols))
        } else{
          lineColor = NULL
          noLegends = TRUE
          lev = NULL
          cols = NULL
        }
        
        cycles_long() %>%
          plotCycleTraces(
            day = .data[[input$xVar]],
            lineColorVar = !! lineColor,
            colorLimits = lev,
            colorValues = cols,
            removeFacets = FALSE,
            removeLegend = noLegends,
            scales = "free_x"
          )
      })
      

      # By Mouse ----------------------------------------------------------------
      observeEvent(cycles_react(), {
        updateSelectInput(
          session,
          "selectedMouse",
          choices = cycles_react()$mouseID
        )
      })
      
      selectedMouseFiles <- reactive({
        req(input$dataFile)
        
        cycleID <- cycles_react()$cycleID[which(cycles_react()$mouseID == input$selectedMouse)]
        
        idText <- sprintf("%04d", as.integer(cycleID))
        
        fileEnding <- paste0("*", idText, ".jpg")
        
        files <- dir_ls(
          cycleDir(),
          all = TRUE,
          recurse = TRUE,
          type = "any",
          glob = fileEnding
        )
      })
      
      output$selCyclePlot <- renderPlot({
        req(input$dataFile, input$lineColorVar)
        
        if(input$useLineColor){
          lineColor = expr(.data[[input$lineColorVar]])
          noLegends = FALSE
          lev <- lineColorLevels()
          cols <- paste0("c(", paste0("input$col", lev, collapse = ", "), ")")
          # print(cols)
          cols <- eval(parse(text = cols))
          # print(cols)
          
          req(length(lev) == length(cols))
        } else{
          lineColor = NULL
          noLegends = TRUE
          lev = NULL
          cols = NULL
        }
        
        cycles_long() %>%
          filter(
            mouseID == input$selectedMouse
          ) %>%
          plotCycleTraces_single(
            day = .data[[input$xVar]],
            lineColorVar = !! lineColor,
            colorLimits = lev,
            colorValues = cols,
            removeFacets = FALSE,
            removeLegend = noLegends
          )
      })
      
      output$selImages <- renderUI({
        req(input$dataFile, input$selectedMouse)

        image_output_list <-
          # lapply(1:nrow(selectedMouseFiles()),
          lapply(1:length(selectedMouseFiles()),
                 function(i)
                 {
                   fileName <- paste0("name", i)
                   stageName <- paste0("stage", i)
                   imagename <- paste0("image", i)
                   tags$div(
                     class = "col-sm-3",
                     textOutput(session$ns(fileName), container = h4),
                     textOutput(session$ns(stageName), container = p),
                     imageOutput(session$ns(imagename), height = "auto") # auto fixes the overlap
                   )
                 })


        div(
          class = "container-fluid",
          div(
            class = "row",
            do.call(tagList, image_output_list)
          )
        )
      })
      
      output$fileTable <- renderDataTable({
        # selectedMouseFiles()
      })
      
      observe({
        req(input$dataFile, input$selectedMouse)
        # if(is.null(selectedMouseFiles())) return(NULL)
        for (i in 1:length(selectedMouseFiles()))
        {
          # print(i)
          local({
            my_i <- i
            imagename <- paste0("image", my_i)
            stageName <- paste0("stage", my_i)
            fileName <- paste0("name", my_i)
            outputWidth <- paste0("output_", imagename, "_width")
            outputHeight <- paste0("output_", imagename, "_height")
            print(imagename)
            
            thisFileName <- selectedMouseFiles()[my_i] %>%
              path_file() %>%
              path_ext_remove()
            
            output[[fileName]] <- renderText({
              thisFileName
            })
            
            # fileDate <- gsub("^20[0-2][0-9]-((0[1-9])|(1[0-2]))-([0-2][1-9]|3[0-1])", "", thisFileName)
            fileDate <- str_extract(thisFileName, "^20[0-2][0-9]-((0[1-9])|(1[0-2]))-(0[1-9]|[1-2][0-9]|3[0-1])")
            
            output[[stageName]] <- renderText({
              df <- cycles_long() %>%
                filter(
                  mouseID == input$selectedMouse,
                  cycleDate == as_date(fileDate)
                )
              thisStage <- df$stage[1]
              thisStageName <- case_when(
                thisStage == 1 ~ "estrus",
                thisStage == 2 ~ "diestrus",
                thisStage == 3 ~ "proestrus",
                TRUE ~ "no stage scored"
                
              )
              return(thisStageName)
            })
            
            output[[imagename]] <-
              renderImage({
                list(src = selectedMouseFiles()[my_i],
                     width = "100%",
                     height = "auto",
                     alt = "Image failed to render")
              }, deleteFile = FALSE)
          })
        }
      })
      
    }
  )
}


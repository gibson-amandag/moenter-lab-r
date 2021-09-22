### Cycles App Module

# https://shiny.rstudio.com/articles/modules.html


# https://gist.github.com/wch/5436415/#gistcomment-1646351 

# https://stackoverflow.com/questions/38822863/shiny-dynamic-colour-fill-input-for-ggplot 
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cyclesUI <- function(id){
  ns <- NS(id)
  tagList(
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
        uiOutput(
          ns("selImages")
        )
      )
    )
  )
}


cyclesServer <- function(
  id,
  cycleDir,
  damInfo,
  offspringInfo,
  Cycles_off
){
  moduleServer(
    id,
    function(input, output, session) {
      print(cycleDir)
      
      cycles_long <- reactive({
        req(Cycles_off)
        df <- Cycles_off %>%
          makeCyclesLong(afterVar = cycleStartDate) %>%
          addPNDForCyles()
          # mutate(
          #   cycleDate = cycleStartDate + (day - 1),
          #   PND = cycleDate - DOB,
          #   .after = stage
          # )
        return(df)
      })
      
      # Cycle Plots Panel -------------------------------------------------------
      
      ## Color Variable and Levels ------------------
      observeEvent(Cycles_off, {
        updateVarSelectInput(
          session,
          "lineColorVar",
          data = Cycles_off %>% 
            select(where(~ is.character(.x) | is.factor(.x)))
        )
      })
      
      lineColorLevels <- reactive({
        req(Cycles_off, input$lineColorVar)
        unique(cycles_long()[[input$lineColorVar]])
      })
      
      output$colorUI <- renderUI({
        req(Cycles_off, input$lineColorVar)
        
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
        req(Cycles_off, input$lineColorVar)
        
        
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
      observeEvent(Cycles_off, {
        updateSelectInput(
          session,
          "selectedMouse",
          choices = Cycles_off$mouseID
        )
      })
      
      selectedMouseFiles <- eventReactive(input$selectedMouse, {
        req(Cycles_off, input$selectedMouse)
        
        
        cycleID <- Cycles_off$cycleID[which(Cycles_off$mouseID == input$selectedMouse)]
        print(cycleID)
        
        if(! length(cycleID) == 0){
          idText <- sprintf("%04d", as.integer(cycleID))
          
          fileEnding <- paste0("*", idText, ".jpg")
          
          files <- dir_ls(
            normalizePath(cycleDir),
            all = TRUE,
            recurse = TRUE,
            type = "any",
            glob = fileEnding
          ) 
          sortOrder <- files %>% path_file() %>% order()
          files <- files[sortOrder]
          print(files)
          return(
            files
          )
        } else{
          return(
            NULL
          )
        }
      })
      
      output$selCyclePlot <- renderPlot({
        req(Cycles_off, input$lineColorVar)
        
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
        req(Cycles_off, input$selectedMouse, selectedMouseFiles())
        
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
      
      observe({
        req(Cycles_off, input$selectedMouse, selectedMouseFiles())
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
            # print(imagename)
            
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


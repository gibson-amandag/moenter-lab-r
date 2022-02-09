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
    # dataTableOutput(ns("test")),
    fluidRow(
      div(
        class = "col-xs-4",
        radioButtons(
          ns("xVar"),
          "Plot by:",
          c("Cycle Day" = "day", "PND" = "PND")
        )
      ),
      div(
        class = "col-xs-4",
        varSelectInput(
          ns("groupingVar"),
          "Grouping Variable",
          NULL
        )
      ),
      div(
        class = "col-xs-4",
        checkboxInput(
          ns("useLineColor"),
          "Use line color",
          value = FALSE
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
        fluidRow(
          div(
            class = "col-xs-4",
            sliderInput(
              ns("cycleDays"),
              "Select Days",
              min = 1,
              max = 100,
              step = 1,
              value = c(1, 100)
            )
          ),
          div(
            class = "col-xs-4",
            selectInput(
              ns("includeLevels"),
              "Include levels",
              multiple = TRUE,
              choices = c()
            )
          ),
          div(
            class = "col-xs-4",
            radioButtons(
              ns("imgType"),
              "Select File Type",
              choices = c("png", "pdf")
            ),
            downloadButton(
              ns("downloadCyclePlot"),
              "Download current plot"
            )
          )
        ),
        plotOutput(ns("cyclesPlot"))
      ),
      
      # by mouse -------------
      tabPanel(
        "by mouse",
        fluidRow(
          div(
            class = "col-xs-4",
            selectInput(
              ns("selectedMouse"),
              label = "Select Mouse:",
              choices = character()
            ),
            actionButton(
              ns("makeImgDiv"),
              label = "Show Images",
              icon = icon("redo")
            )
          ),
          div(
            class = "col-xs-4",
            radioButtons(
              ns("imgsPerRow"),
              label = "# imgs per row",
              choices = c(1, 2, 3, 4, 6),
              selected = 3
            )
          ),
          div(
            class = "col-xs-4",
            radioButtons(
              ns("imgsPerSlide"),
              label = "# imgs per slide",
              choices = c(4, 9, 12),
              selected = 12
            ),
            downloadButton(
              ns("downloadMousePPT"),
              label = "Download Mouse's PPT"
            )
          )
        ),
        plotOutput(
          ns("selCyclePlot"),
          height = "200px"
        ),
        tags$div(id = "placeholder"),
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
  cyclesDF,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        cycleDir
        damInfo
        offspringInfo
        cyclesDF
        compType
      })
      
      # Reactive wide cycles table
      # Filter by grouping variable levels
      cycles_react <- reactive({
        # req(cyclesDF, input$groupingVar, input$includeLevels)
        if(!is.null(input$groupingVar)){
          df <- cyclesDF %>%
            filter(
              !! input$groupingVar %in% as.character(input$includeLevels)
            )
        } else (df <- cyclesDF)
        return(df)
      })
      
      # Update days slider
      observe({
        req(cyclesDF, input$groupingVar)
        df <- cyclesDF %>%
          makeCyclesLong(afterVar = cycleStartDate) %>%
          addPNDForCyles()
        if(length(df$day)>0){
          minDay <- min(df$day, na.rm = TRUE)
          maxDay <- max(df$day, na.rm = TRUE)
          updateSliderInput(
            session = session,
            "cycleDays",
            min = minDay,
            max = maxDay,
            value = c(minDay, maxDay)
          )
        }
      })
      
      # Long-form cycles dataframe. Filters by day selection + groupingVar levels
      cycles_long <- reactive({
        req(cyclesDF, input$groupingVar)
        # print(input$cycleDays[1])
        df <- cycles_react() %>%
          makeCyclesLong(afterVar = cycleStartDate) %>%
          addPNDForCyles()%>%
          filter(
            day >= input$cycleDays[1] & day <= input$cycleDays[2]
          )
        return(df)
      })
      
      # Long-form cycles dataframe. Does not filter by day/groupingVar levels
      cycles_long_indiv <- reactive({
        req(cyclesDF)
        df <- cyclesDF %>%
          makeCyclesLong(afterVar = cycleStartDate) %>%
          addPNDForCyles()
        return(df)
      })
      
      # Cycle Plots Panel -------------------------------------------------------
      
      ## Color Variable and Levels ------------------
      # Update groupingVar options
      observe({
        updateVarSelectInput(
          session,
          "groupingVar",
          data = cyclesDF %>%
            mutate(
              mouseID = as.factor(mouseID)
            ) %>%
            select(where(~ is.character(.x) | is.factor(.x))) %>%
            relocate(
              mouseID,
              .after = last_col()
            )
        )
      })
      
      # Update possible groupingVar levels
      groupingVarLevels <- reactive({
        req(cyclesDF, input$groupingVar)
        unique(cyclesDF[[input$groupingVar]])
      })
      
      # Update levels selection options when groupingVar changes
      observeEvent(input$groupingVar,{
        updateSelectInput(
          session = session,
          inputId = "includeLevels",
          choices = groupingVarLevels(),
          selected = groupingVarLevels()
        )
      })
      
      # Create a colorUI if useLineColor is selected
      output$colorUI <- renderUI({
        req(cyclesDF, input$groupingVar)
        
        if(input$useLineColor){
          lev <- groupingVarLevels()
          cols <- gg_fill_hue(length(lev))
          
          # New IDs "col+level"
          lapply(seq_along(lev), function(i) {
            div(
              class = "col-xs-4",
              colourpicker::colourInput(inputId = session$ns(paste0("col", i)),
                                        label = paste0("Choose color for ", lev[i]),
                                        value = cols[i]
              )
            )
          })
        }
      })
      
      # Reset to default colors when resetColors is pressed
      observeEvent(input$resetColors, {
        lev <- groupingVarLevels()
        cols <- gg_fill_hue(length(lev))
        
        lapply(seq_along(lev), function(i) {
          do.call(what = "updateColourInput",
                  args = list(
                    session = session,
                    inputId = paste0("col", i),
                    value = cols[i]
                  )
          )
        })
      })
      
      ## Plot --------------------------
      # Number of rows in the cycles plot. Used to determine size of container on page
      nrowsPlot <- reactive({
        numMice <- nrow(cycles_react())
        nrowsPlot <- ceiling(numMice/4)
        nrowsPlot <- ifelse(nrowsPlot > 0, nrowsPlot, 1)
        return(nrowsPlot)
      })
      
      # Make the groupCycles plot. Need at least one row and the groupingVar input ready
      groupCyclesPlot <- reactive({
        req(nrow(cycles_long()) > 0, input$groupingVar)
        
        
        if(input$useLineColor){
          lineColor = expr(.data[[input$groupingVar]])
          noLegends = FALSE
          lev <- groupingVarLevels()
          cols <- paste0("c(", paste0("input$col", 1:length(lev), collapse = ", "), ")")
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
        
        viz <- cycles_long() %>%
          plotCycleTraces(
            day = .data[[input$xVar]],
            lineColorVar = !! lineColor,
            colorLimits = lev,
            colorValues = cols,
            removeFacets = FALSE,
            removeLegend = noLegends,
            scales = "free_x",
            ncol = 4
          )
        return(viz)
      })
      
      # Add the plot to the page and use nrowsPlot to make it the right size
      output$cyclesPlot <- renderPlot(
        groupCyclesPlot(),
        height = function() {nrowsPlot() * 150} #function prevents it from resetting each time
      )
      
      # Download the group cycles plot
      output$downloadCyclePlot <- downloadHandler(
        filename = function() {
          paste0("cyclePlots-", Sys.Date(), ".", input$imgType)
        },
        content = function(file) {
          flexSave(
            baseName = file,
            thisFilePrefix = NULL,
            plot = last_plot(),
            fileType = input$imgType,
            filePath = NULL,
            width = 8,
            height = ifelse(nrowsPlot() * 2 < 10, nrowsPlot() * 2, 10),
            units = "in",
            compType = currentCompType,
            shinySettings = TRUE,
          )
        }
      )
      
      # By Mouse ----------------------------------------------------------------
      # Update the selected mouse options. Don't make reactive to other filters
      observeEvent(cyclesDF, {
        updateSelectInput(
          session,
          "selectedMouse",
          choices = cyclesDF$mouseID
        )
      })
      
      ## Individual Mouse Plot -------------------
      output$selCyclePlot <- renderPlot({
        req(cyclesDF, input$groupingVar)
        
        if(input$useLineColor){
          lineColor = expr(.data[[input$groupingVar]])
          noLegends = FALSE
          lev <- groupingVarLevels()
          cols <- paste0("c(", paste0("input$col", 1:length(lev), collapse = ", "), ")")
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
        
        cycles_long_indiv() %>%
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
      
      ## Images UI -------------------------------
      
      ### Build UI -------------------------------
      # Clear the image UI when a new mouse is selected
      observeEvent(
        c(
          input$selectedMouse
        ),
        {
          output$selImages <- renderUI({p("Press the button above to show the cycling images")})
        })
      
      # Update the selectedMouseFiles, only when button is pressed
      selectedMouseFiles <-
        eventReactive(
          input$makeImgDiv
          ,
          {
            # reactive({
            req(cyclesDF, input$selectedMouse)
            
            cycleID <- cyclesDF$cycleID[which(cyclesDF$mouseID == input$selectedMouse)]
            
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
              return(
                files
              )
            } else{
              return(
                NULL
              )
            }
          })
      
      # When the button is pressed, make the new UI for the images
      observeEvent(
        input$makeImgDiv,
        {
          output$selImages <- renderUI({
            req(cycles_long(), input$selectedMouse, selectedMouseFiles())
            
            imgsPerRow <- input$imgsPerRow
            divCols <- case_when(
              imgsPerRow == 1 ~ "12",
              imgsPerRow == 2 ~ "6",
              imgsPerRow == 3 ~ "4",
              imgsPerRow == 4 ~ "3",
              imgsPerRow == 6 ~ "2"
            )
            
            if(!is.null(input$selectedMouse)){
              image_output_list <-
                lapply(
                  1:length(selectedMouseFiles()),
                  function(i)
                  {
                    imagename <- paste0("image", i)
                    
                    thisFileName <- selectedMouseFiles()[i] %>%
                      path_file() %>%
                      path_ext_remove()
                    
                    fileDate <- str_extract(thisFileName, "^20[0-2][0-9][-_]((0[1-9])|(1[0-2]))[-_](0[1-9]|[1-2][0-9]|3[0-1])")
                    
                    if(!is.na(fileDate)){
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
                    } else {
                      thisStageName <- "no date found in title"
                    }
                    
                    tags$div(
                      class = paste0("col-xs-", divCols),# "col-xs-6 col-sm-4 col-md-3", # "col-sm-3",
                      h4(thisFileName),
                      p(thisStageName),
                      imageOutput(session$ns(imagename), height = "auto") %>% # auto fixes the overlap
                        tagAppendAttributes(class = 'myImages')
                    )
                  })
              
              thisUI <- div(
                class = "container-fluid",
                div(
                  class = "row",
                  p("The images take longer to load than the text. Please be patient"),
                  do.call(tagList, image_output_list)
                )
              )
            }
            return(thisUI)
          })
        })
      
      
      ### Fill UI -------------
      observeEvent(
        input$makeImgDiv,
        {
          req(cyclesDF, input$selectedMouse, selectedMouseFiles())
          
          # Number of images within directory
          numImgs <- length(selectedMouseFiles())
          
          for (i in 1:numImgs)
          {
            local({
              my_i <- i
              imagename <- paste0("image", my_i)
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
      
      ## Powerpoint Output --------------------
      individualPPT <- reactive({
        req(input$imgsPerSlide) # To get to reset when this changes
        ppt <- read_pptx("estrousCycleTemplate.pptx")
        ppt <- add_slide(ppt, layout = "Title Slide")
        ppt <- ph_with(x = ppt, value = input$selectedMouse, location = ph_location_label("Title 1"))
      })
      
      output$downloadMousePPT <- downloadHandler(
        filename = function() {
          paste0("cycleImgs_", input$selectedMouse, "-", Sys.Date(), ".pptx")
        },
        content = function(file) {
          
          # Redo selected files here, because not updating appropriately
          cycleID <- cyclesDF$cycleID[which(cyclesDF$mouseID == input$selectedMouse)]
          
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
          } else{
            files <- NULL
          }
          
          # Number of images within directory
          numImgs <- length(files)
          
          # First index
          iImg <- 1
          # print("reset iImg")
          
          cyclePPT <- individualPPT()
          
          for (i in 1:numImgs)
          {
            thisFileName <- files[i] %>%
              path_file() %>%
              path_ext_remove()
            
            fileDate <- str_extract(thisFileName, "^20[0-2][0-9]-((0[1-9])|(1[0-2]))-(0[1-9]|[1-2][0-9]|3[0-1])")
            
            if(!is.na(fileDate)){
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
            } else {
              thisStageName <- "no date found in title"
            }
            
            img <- external_img(files[i], width = 2.68, heigh = 2.14) # get the image
            textID <- paste0("text", iImg)
            stageID <- paste0("stage", iImg)
            imgID <- paste0("img", iImg)
            
            # If img index is 1, add a new slide
            if(iImg == 1){
              slideLayout <- paste0("estrousCycle", input$imgsPerSlide)
              cyclePPT <- add_slide(x = cyclePPT, layout = slideLayout)
            }
            
            # add the title
            cyclePPT <- ph_with(
              x = cyclePPT, value = thisFileName,
              location = ph_location_label(
                textID
              ),
              use_loc_size = TRUE)
            
            # add the title
            cyclePPT <- ph_with(
              x = cyclePPT, value = thisStageName,
              location = ph_location_label(
                stageID
              ),
              use_loc_size = TRUE)
            
            # add the image
            cyclePPT <- ph_with(
              x = cyclePPT, value = img,
              location = ph_location_label(
                imgID
              ),
              use_loc_size = TRUE)
            
            
            # if index is less than the number per slide, add one, otherwise, restart at 1
            if(iImg < as.numeric(input$imgsPerSlide)) {
              iImg <- iImg + 1
            } else {
              iImg <- 1
            }
          }
          
          print(individualPPT(), target = file)
        }
      )
      
    }
  )
}


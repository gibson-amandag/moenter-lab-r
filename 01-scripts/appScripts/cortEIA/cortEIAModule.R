### Corticosterone Plate Analysis Module

# https://shiny.rstudio.com/articles/modules.html


cortEIAUI <- function(id){
  ns <- NS(id)
  tagList(
    tags$body(
      tags$style( 
        HTML("
            .plateTable, .plateTable th, .plateTable td {
              border: 1px solid black;
            }
            
            .plateTable {border-collapse: collapse}
            
            .plateWell.NSB{color:purple}
            .plateWell.STD{color:blue}
            .plateWell.QC{color:green}
            .plateWell.bufferCtrl{color:red}
            .plateWell.sample{color:black}
            
            .plateTable tr:nth-child(4n + 3) td,
            .plateTable tr:nth-child(4n + 4) td {
                background-color: #E8E8E8;
            }
            "
        )
      )
    ),
    
    useShinyjs(),
    
    fluidRow(
      div(
        class = "col-sm-4",
        downloadButton(ns("downloadResults"), "Download Results in Excel"),
      ),
      div(
        class = "col-xs-6 col-sm-2",
        radioButtons(
          ns("imgType"),
          "Select File Type",
          choices = c("png", "pdf")
        )
      ),
      div(
        class = "col-xs-6",
        downloadButton(ns("downloadStdCurve"), "Download Std Curve Plot"),
        downloadButton(ns("downloadSamplesOnCurve"), "Download Std Curve with Samples Plot"),
      )
    ),
    
    tabsetPanel(

      # Calculation -------------------------------------------------------------

      
      tabPanel(
        "Calculation",
        
        tabsetPanel(
          ## Buffer Control ----------------------------------------------------------
          tabPanel(
            "Buffer Control",
            h3("Buffer Control"),
            shiny::tableOutput(ns("bufferCtrlTable")),
            uiOutput(ns("bufferCtrlText"))
          ),
          ## Percent Binding -------------------------------------------------------
          tabPanel(
            "Percent Binding",
            h3("Percent Binding"),
            p("We will use the buffer control value to calculate",
              "the percentage of maximal binding displayed by each well, %B/B", tags$sub("0")),
            
            actionButton(ns("togglePercBindingDisplay"), "Toggle Plate Display"),
            
            uiOutput(ns("percBindingDisplay"))
          ),
          ## Standard Curve -------------------------------------------------------
          tabPanel(
            "Standard Curve",
            h3("Standard Curve"),
            p("Now, we'll use the standards to calculate the relationship between percent binding and corticosterone per well"),
            p("Click on a row in the table to exclude the standard from the curve generation"),
            DTOutput(ns("standardsTable")),
            plotOutput(ns("standardCurvePlot"), click = ns("standardCurvePlot_click")),
            tableOutput(ns("standardCurvePlotInfo")),
            verbatimTextOutput(ns("stdCurveModel")),
            p("%B/B", tags$sub("0"), " = c + (d - c) / (1 + exp(b(log(ng/well) - log(e))))")
          ),

          ## Samples Estimates -------------------------------------------------------
          tabPanel(
            "Samples Estimates",
            h3("Samples Estimates"),
            p("Using the curve we calculated above, we can now estimate the amount of corticosterone that was in each well"),
            p("From the pg/well, we can calculate the concentration (in pg/mL) of the sample + assay buffer dilution if we know the volume added to the well", em("pg/well * volPerWell")),
            p("From the pg/mL concentration, we can convert to ng/mL", em("pg/mL / 1000")),
            p("Finally, using the dilution factor of the original serum sample in the assay buffer, we can calculate the corticosterone concentration of the origianl serum sample", 
              em("ng/mL * dilutionFactor")),
            tabsetPanel(
              ### Plate ----------
              tabPanel(
                "plate",
                fluidRow(
                  div(
                    class = "col-xs-3",
                    checkboxInput(
                      ns("viewCalcPgPerWell"),
                      "View pg/well",
                      value = TRUE
                    )
                  ),
                  div(
                    class = "col-xs-3",
                    checkboxInput(
                      ns("viewCalcPgPer_mL"),
                      "View pg/mL",
                      value = TRUE
                    ),
                  ),
                  div(
                    class = "col-xs-3",
                    checkboxInput(
                      ns("viewCalcNgPer_mL"),
                      "View ng/mL",
                      value = TRUE
                    )
                  ),
                  div(
                    class = "col-xs-3",
                    checkboxInput(
                      ns("viewCalcSampleConc"),
                      "View sample conc",
                      value = TRUE
                    )
                  ),
                ),
                uiOutput(ns("calcPlate"))
              ),
              ### All on Curve -------------
              tabPanel(
                "All on STD Curve",
                plotOutput(ns("samplesOnStdCurvePlot"), click = ns("samplesOnStdCurvePlot_click")),
                tableOutput(ns("samplesPgInfo"))
              ),
              ### Mouse on Curve -----------
              tabPanel(
                "By Mouse on STD Curve",
                selectInput(
                  ns("selectedMouse"),
                  label = "Select Mouse:",
                  choices = character()
                ),
                plotOutput(ns("mouseOnStdCurvePlot")),
                tableOutput(ns("mouseOnStdCurvePlotInfo"))
              ),
              ### Quality Control ----------
              tabPanel(
                "Quality Control",
                plotOutput(ns("QConStdCurvePlot")),
                tableOutput(ns("QConStdCurvePlotInfo"))
              )
            )
          )
        ),
      ),
      # Plate View ------------------------------
      tabPanel(
        "Plate View",
        fluidRow(
          div(
            class = "col-xs-3",
            checkboxInput(
              ns("viewPlateID"),
              "View PlateID",
              value = TRUE
            )
          ),
          div(
            class = "col-xs-3",
            checkboxInput(
              ns("viewMouseID"),
              "View MouseID",
              value = TRUE
            ),
            checkboxInput(
              ns("viewTime"),
              "View Time",
              value = TRUE
            )
          ),
          div(
            class = "col-xs-3",
            checkboxInput(
              ns("viewNetOD"),
              "View NetOD",
              value = TRUE
            )
          ),
          div(
            class = "col-xs-3",
            checkboxInput(
              ns("viewConc"),
              "View concentration",
              value = TRUE
            )
          ),
        ),
        
        
        uiOutput(ns("comboPlate")),
      ),
      # Table View -----------
      tabPanel(
        "Table View",
        dataTableOutput(ns("calculationTable"))
      ),
      # Results ----------------
      tabPanel(
        "Sample Concentrations",
        dataTableOutput(ns("assayResults"))
      )
    )
  )
}


cortEIAServer <- function(
  id,
  filePath,
  fileName,
  compType
){
  moduleServer(
    id,
    function(input, output, session) {
      library(plater)
      library(shinyjs)
      
      getMeanCVfromReplicates <- function(assayPlate, colToSum = netOD) {
        summarizedPlate <- assayPlate %>%
          group_by(plateID) %>%
          summarise(
            "mean{{ colToSum }}" := mean({{ colToSum }}, na.rm = TRUE),
            "CV{{ colToSum }}" := sd({{ colToSum }}, na.rm = TRUE)/mean({{ colToSum }}, na.rm = TRUE) * 100
          )
        return(summarizedPlate)
      }
      
      assayPlate <- reactive({
        assayPlate <- read_plate(filePath, "wells") %>%
          mutate(
            type = factor(type, c("NSB", "bufferCtrl", "STD", "QC", "sample"))
          )%>%
          arrange(type, plateID)
        return(assayPlate)
      })
      
      assayPlate_netOD_meanCV <- reactive({
        sumPlate <- assayPlate() %>%
          getMeanCVfromReplicates()
        return(sumPlate)
      })
      
      assayPlate_indPlusMean <- reactive({
        comboPlate <- assayPlate() %>%
          left_join(
            assayPlate_netOD_meanCV(),
            by = "plateID"
          )
      })
      
      bufferCtrlOD <- reactive({
        df <- assayPlate_netOD_meanCV() %>%
          filter(
            plateID == "bufferCtrl"
          )
        return(df$meannetOD[1])
      })
      
      # Percent binding relative to buffer control
      assayPlate_percBinding <- reactive({
        df <- assayPlate() %>%
          mutate(
            percBinding = netOD / bufferCtrlOD() * 100
          )
        return(df)
      })
      
      # Standard Values
      standards <- reactive({
        stds <- assayPlate_percBinding() %>%
          filter(
            type == "STD"
          )
        return(stds)
      })
      
      ## Standards table - select to exclude
      output$standardsTable <- renderDT({
        standards()
      })
      
      standards_included <- reactive({
        standards() %>%
          filter(! (row_number() %in% input$standardsTable_rows_selected))
      })
      
      # Standard Curve Model
      stdCurve <- reactive({
        stdCurve <- drm(
          percBinding ~ stdPgPerWell, 
          data = standards_included(), 
          fct = LL.4()
        )
        return(stdCurve)
      })
      
      # Sample Estimates
      assayPlate_concEstimates <- reactive({
        df <- assayPlate_percBinding() %>%
          rowwise()%>%
          mutate(
            pgPerWell = ifelse(
              type == "STD",
              stdPgPerWell,
              ifelse(
                type == "QC" | type == "sample",
                ED(stdCurve(), percBinding, type="absolute", display=F)[1,1],
                NA
              )
            ),
            pgPer_mL = ifelse(is.na(pgPerWell), NA, pgPerWell / volPerWell),
            ngPer_mL = ifelse(is.na(pgPer_mL), NA, pgPer_mL / 1000),
            sampleConc_ngPer_mL = ifelse(is.na(ngPer_mL), NA, ngPer_mL * dilutionFactor)
          )
        return(df)
      })
      
      assayPlate_concEstimates_meanCV <- reactive({
        df <- assayPlate_concEstimates()
        
        df_mean <- df %>%
          group_by(plateID) %>%
          summarise(
            cortMean = mean(sampleConc_ngPer_mL, na.rm = TRUE),
            cortCV = sd(sampleConc_ngPer_mL, na.rm = TRUE) / cortMean * 100 
          )
        
        df_addMean <- df %>%
          left_join(
            df_mean,
            by = "plateID"
          )
        return(df_addMean)
      })
      
      samplesEst <- reactive({
        assayPlate_concEstimates() %>%
          filter(
            type == "QC" | type == "sample"
          ) %>%
          arrange(
            type,
            mouseID,
            time
          )
      })
      
      # Mean estimates for samples
      meanSampleResults <- reactive({
        df <- assayPlate_concEstimates() %>%
          filter(
            type == "sample"
          ) %>%
          group_by(
            mouseID, time
          ) %>%
          summarize(
            cort = mean(sampleConc_ngPer_mL, na.rm = TRUE),
            cortCV = sd(sampleConc_ngPer_mL, na.rm = TRUE)/cort * 100,
            .groups = "drop"
          )
        return(df)
      })
      

      # Table Elements ----------------------------------------------------------
      
      makeWellDivConcCalcs <- function(
        percBinding,
        pgPerWell,
        pgPer_mL,
        ngPer_mL,
        sampleConc,
        type
      ){
        thisPercBinding <- p(ifelse(
          is.na(percBinding),
          "",
          paste0(round(percBinding, 2), "%")
          )
        ) %>% tagAppendAttributes(class = "calcPercB")
        
        thisPgPerWell <- p(ifelse(
          is.na(pgPerWell),
          "",
          paste0(round(pgPerWell, 2), "pg/well")
        ))%>% tagAppendAttributes(class = "calcPgPerWell")
        
        thisPgPer_mL <- p(
          ifelse(
            is.na(pgPer_mL),
            "",
            paste0(round(pgPer_mL, 2), "pg/mL")
          )
        )%>% tagAppendAttributes(class = "calcPgPer_mL")
        
        thisNgPer_mL <- p(
          ifelse(
            is.na(ngPer_mL),
            "",
            paste0(round(ngPer_mL, 2), "ng/mL")
          )
        )%>% tagAppendAttributes(class = "calcNgPer_mL")
        
        thisSampleConc <- p(
          ifelse(
            is.na(sampleConc),
            "",
            paste0(round(sampleConc, 2), "ng/mL")
          )
        )%>% tagAppendAttributes(class = "calcSampleConc")
        
        thisClass <- paste0("plateWell ", type)
        
        thisDiv <- tags$div(
          thisPercBinding,
          thisPgPerWell,
          thisPgPer_mL,
          thisNgPer_mL,
          thisSampleConc,
          class = thisClass
        )
        return(thisDiv)
      }
      
      makeWellDivPercBinding <- function(
        netOD,
        percBinding,
        type
      ){
        thisNetOD <- p(ifelse(is.na(netOD), "", netOD)) %>% tagAppendAttributes(class = "inProgNetOD")
        thisPercBinding <- p(ifelse(
          is.na(percBinding),
          "",
          paste0(round(percBinding, 2), "%")
          )
        ) %>% tagAppendAttributes(class = "inProgPercB")
        
        thisClass <- paste0("plateWell ", type)
        
        thisDiv <- tags$div(
          thisNetOD,
          thisPercBinding,
          class = thisClass
        )
        return(thisDiv)
      }

      makeWellDiv <- function(
        plateID,
        mouseID,
        time,
        netOD,
        sampleConc,
        type
      ){
        thisID <- p(plateID) %>% tagAppendAttributes(class = "plateID")
        thisMouse <- p(mouseID) %>% tagAppendAttributes(class = "mouseID")
        thisTime <- p(paste("Time: "), time) %>% tagAppendAttributes(class = "time")
        thisNetOD <- h4(netOD) %>% tagAppendAttributes(class = "netOD")
        thisSampleConc <- p(
          ifelse(
            is.na(sampleConc),
            "",
            paste0(round(sampleConc, 2), "ng/mL")
          )
        ) %>% tagAppendAttributes(class="conc")
        if(is.na(type)){
          thisDiv <- tags$div()
        } else if(type == "NSB"){
          thisDiv <- tags$div(
            thisNetOD,
            thisID,
            class = "plateWell NSB"
          )
        } else if (type == "STD"){
          thisDiv <- tags$div(
            thisNetOD,
            thisID,
            thisSampleConc,
            class = "plateWell STD",
          )
        } else if (type == "bufferCtrl"){
          thisDiv <- tags$div(
            thisNetOD,
            thisID,
            class = "plateWell bufferCtrl",
          )
        } else if(type == "sample"){
          thisDiv <- tags$div(
            thisNetOD,
            thisID,
            thisMouse,
            thisTime,
            thisSampleConc,
            class = "plateWell sample",
          )
        } else if (type == "QC"){
          thisDiv <- tags$div(
            thisNetOD,
            thisID,
            thisSampleConc,
            class = "plateWell QC",
          )
        }else{
          thisDiv <- tags$div()
        }
        return(thisDiv)
      }
      
      fillTableRowWellsConcCalcs <- function(
        rowName,
        assayPlate
      ){
        bodyRow <- lapply(
          1:12, function(i){
            wellID <- paste0(rowName, sprintf("%02d", as.integer(i)))
            thisWell <- assayPlate %>%
              filter(
                wells == wellID
              )
            
            thisPercBinding <- thisWell$percBinding[1]
            thisPgPerWell <- thisWell$pgPerWell[1]
            thisPgPer_mL <- thisWell$pgPer_mL[1]
            thisNgPer_mL <- thisWell$ngPer_mL[1]
            thisSampleConc <- thisWell$sampleConc_ngPer_mL[1]
            thisType <- thisWell$type[1]
            tags$td(
              style="vertical-align: top",
              makeWellDivConcCalcs(
                thisPercBinding,
                thisPgPerWell,
                thisPgPer_mL,
                thisNgPer_mL,
                thisSampleConc,
                thisType
              )
            )
          }
        )
        return(bodyRow)
      }
      fillTableRowWellsPercBinding <- function(
        rowName,
        assayPlate
      ){
        bodyRow <- lapply(
          1:12, function(i){
            wellID <- paste0(rowName, sprintf("%02d", as.integer(i)))
            thisWell <- assayPlate %>%
              filter(
                wells == wellID
              )
            thisNetOD <- thisWell$netOD[1]
            thisPercBinding <- thisWell$percBinding[1]
            thisType <- thisWell$type[1]
            tags$td(
              style="vertical-align: top",
              makeWellDivPercBinding(
                thisNetOD,
                thisPercBinding,
                thisType
              )
            )
          }
        )
        return(bodyRow)
      }
      
      fillTableRowWells <- function(
        rowName,
        assayPlate
      ){
        bodyRow <- lapply(
          1:12, function(i){
            wellID <- paste0(rowName, sprintf("%02d", as.integer(i)))
            thisWell <- assayPlate %>%
              filter(
                wells == wellID
              )
            thisPlateID <- thisWell$plateID[1]
            thisMouseID <- thisWell$mouseID[1]
            thisTime <- thisWell$time[1]
            thisNetOD <- thisWell$netOD[1]
            thisConc <- thisWell$sampleConc_ngPer_mL[1]
            thisType <- thisWell$type[1]
            tags$td(
              style="vertical-align: top",
              makeWellDiv(
                thisPlateID,
                thisMouseID,
                thisTime,
                thisNetOD,
                thisConc,
                thisType
              )
            )
          }
        )
        return(bodyRow)
      }
      
      makeTableBodyConcCalcs <- function(
        assayPlate
      ){
        rows <- lapply(
          c("A", "B", "C", "D", "E", "F", "G", "H"), function(rowName){
            tags$tr(
              tags$th(
                rowName
              ),
              do.call(tagList, fillTableRowWellsConcCalcs(rowName, assayPlate))
            )
          }
        )
        return(rows)
      }
      
      makeTableBodyPercBinding <- function(
        assayPlate
      ){
        rows <- lapply(
          c("A", "B", "C", "D", "E", "F", "G", "H"), function(rowName){
            tags$tr(
              tags$th(
                rowName
              ),
              do.call(tagList, fillTableRowWellsPercBinding(rowName, assayPlate))
            )
          }
        )
        return(rows)
      }
      
      makeTableBody <- function(
        assayPlate
      ){
        rows <- lapply(
          c("A", "B", "C", "D", "E", "F", "G", "H"), function(rowName){
            tags$tr(
              tags$th(
                rowName
              ),
              do.call(tagList, fillTableRowWells(rowName, assayPlate))
            )
          }
        )
        return(rows)
      }
      

      # Show or Hide components of plate ----------------------------------------
      showIfChecked <- function(inputEl, selector){
        if(inputEl == TRUE){
          runjs(
            paste0('$("', selector, '").show()')
          )
        }else {
          runjs(
            paste0('$("', selector, '").hide()')
          )
        }
      }
      
      observeEvent(input$viewMouseID, {
          showIfChecked(input$viewMouseID, ".mouseID")
        }
      )
      
      observeEvent(input$viewPlateID, {
          showIfChecked(input$viewPlateID, ".plateID")
        }
      )
      
      observeEvent(input$viewTime, {
          showIfChecked(input$viewTime, ".time")
        }
      )
      
      observeEvent(input$viewNetOD, {
          showIfChecked(input$viewNetOD, ".netOD")
        }
      )
      observeEvent(input$viewConc, {
          showIfChecked(input$viewConc, ".conc")
        }
      )
      
      observeEvent(input$viewCalcPgPerWell, {
          showIfChecked(input$viewCalcPgPerWell, ".calcPgPerWell")
        }
      )
      observeEvent(input$viewCalcPgPer_mL, {
          showIfChecked(input$viewCalcPgPer_mL, ".calcPgPer_mL")
        }
      )
      observeEvent(input$viewCalcNgPer_mL, {
          showIfChecked(input$viewCalcNgPer_mL, ".calcNgPer_mL")
        }
      )
      observeEvent(input$viewCalcSampleConc, {
          showIfChecked(input$viewCalcSampleConc, ".calcSampleConc")
        }
      )
      
      
      

      # Plate Output ------------------------------------------------------------

      
      output$comboPlate <- renderUI({
        headerRow <- lapply(
          1:12, function (i){
            tags$th(
              i
            )
          }
        )
        
        tableDiv <- tagList(
          tags$div(
            class = "container",
            tags$table(
              class="myComboPlateTable plateTable",
              tags$thead(
                tags$tr(
                  tags$th(),
                  do.call(tagList, headerRow)
                )
              ),
              tags$tbody(
                do.call(tagList, makeTableBody(assayPlate_concEstimates()))
              )
            )
          )
        )
        
        return(tableDiv)
      })


      # Buffer Control ----------------------------------------------------------

      output$bufferCtrlTable <- shiny::renderTable({
        assayPlate() %>%
          filter(
            type == "bufferCtrl"
          ) %>%
          select(
            wells, plateID, netOD
          )
      }, digits = 3)
      
      output$bufferCtrlText <- renderUI({
        tagList(
          h4("Mean buffer control OD: ", 
             span(bufferCtrlOD()) %>%
              tagAppendAttributes(style = 'color:red')
          )
        )
      })
      
      output$percBindingDisplay <- renderUI({
        headerRow <- lapply(
          1:12, function (i){
            tags$th(
              i
            )
          }
        )
        
        tableDiv <- tagList(
          tags$div(
            class = "container percBindingDisplay",
            tags$table(
              class="percBindingTable plateTable",
              tags$thead(
                tags$tr(
                  tags$th(),
                  do.call(tagList, headerRow)
                )
              ),
              tags$tbody(
                do.call(tagList, makeTableBodyPercBinding(assayPlate_percBinding()))
              )
            )
          )
        )
        
        return(tableDiv)
      })
      
      observeEvent(
        input$togglePercBindingDisplay,
        {
          runjs('$(".percBindingDisplay").toggle()')
        }
      )
      

      # Standard Curve ----------------------------------------------------------

      stdsNoZero <- reactive({
        noZero <- standards_included() %>% 
          mutate(pgPerWell = 
                   ifelse(stdPgPerWell == 0, 
                          yes = (sort(stds$stdPgPerWell[which.min(sort(stds$stdPgPerWell)) + 1]/100)),
                          no = stdPgPerWell
                   )
          )
        return(noZero)
      })
      
      stdCurvePlot <- reactive({
        viz <- ggplot(
            data = stdsNoZero(),
            aes(x = pgPerWell, y = percBinding)
          ) + 
          geom_point(color = "blue") + 
          stat_summary(fun = mean, na.rm = TRUE, color = "red") +
          geom_smooth(method = drm, formula = y ~ x, method.args = list(fct = L.4()), se = FALSE, color = "darkgrey") +
          scale_x_log10() +
          labs(
            x = "cort (pg/well)",
            y = "% B/B0"
          ) +
          textTheme(size = 16) +
          boxTheme()
        
        return(viz)
      })
      
      output$stdCurveModel <- renderPrint(stdCurve()$coefficients)
      
      output$standardCurvePlot <- renderPlot(stdCurvePlot())
      output$standardCurvePlotInfo <- renderTable(
        nearPoints(
          stdsNoZero() %>%
            select(
              wells,
              plateID,
              netOD,
              pgPerWell,
              percBinding
            ),
          input$standardCurvePlot_click
        )
      )
      

      # Samples -----------------------------------------------------------------
      
      ## Plate Display ---------------------------
      output$calcPlate <- renderUI({
        headerRow <- lapply(
          1:12, function (i){
            tags$th(
              i
            )
          }
        )
        
        tableDiv <- tagList(
          tags$div(
            class = "container percBindingDisplay",
            tags$table(
              class="percBindingTable plateTable",
              tags$thead(
                tags$tr(
                  tags$th(),
                  do.call(tagList, headerRow)
                )
              ),
              tags$tbody(
                do.call(tagList, makeTableBodyConcCalcs(assayPlate_concEstimates()))
              )
            )
          )
        )
        
        return(tableDiv)
      })
      
      ## Plots ------------------
      samplesOnStdCurvePlot <- reactive({
        viz <- stdCurvePlot() +
          geom_point(data = samplesEst())
        # stat_summary(fun = mean, na.rm = TRUE, data = samplesEst(), color = "purple", size = 0.4)
        return(viz)
      })
      
      output$samplesOnStdCurvePlot <- renderPlot({
        samplesOnStdCurvePlot()
      })
      
      output$samplesPgInfo <- renderTable({
        nearPoints(
          samplesEst() %>% select(
            mouseID,
            time,
            plateID,
            percBinding,
            pgPerWell,
            sampleConc_ngPer_mL
          ), 
          input$samplesOnStdCurvePlot_click)
      })
      
      observeEvent(assayPlate(), {
        updateSelectInput(
          session,
          "selectedMouse",
          choices = assayPlate()$mouseID
        )
      })
      
      output$mouseOnStdCurvePlot <- renderPlot({
        viz <- stdCurvePlot() +
          geom_point(data = samplesEst() %>% filter(mouseID == input$selectedMouse), size = 2.5)
        return(viz)
      })
      
      output$mouseOnStdCurvePlotInfo <- renderTable({
        samplesEst() %>% 
          filter(mouseID == input$selectedMouse)%>% 
          select(
            mouseID,
            time,
            plateID,
            percBinding,
            pgPerWell,
            sampleConc_ngPer_mL
          )
      })
      output$QConStdCurvePlot <- renderPlot({
        viz <- stdCurvePlot() +
          geom_point(data = samplesEst() %>% filter(type == "QC"), size = 2.5)
        return(viz)
      })
      
      output$QConStdCurvePlotInfo <- renderTable({
        samplesEst() %>% 
          filter(type == "QC")%>% 
          select(
            wells,
            plateID,
            percBinding,
            pgPerWell,
            sampleConc_ngPer_mL
          )
      })
      
      output$calculationTable <- renderDataTable({
        assayPlate_concEstimates_meanCV() %>%
          relocate(
            netOD,
            .after = time
          ) %>%
          datatable(
            extensions = "FixedColumns",
            options = list(
              paging = FALSE, searching = TRUE, info = FALSE,
              fixedHeader = list(header= TRUE, footer = TRUE), scrollY = "400px",
              sort = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 3)
            )
          ) %>%
          formatRound(
            columns = c(
              "cortMean", 
              "cortCV",
              "percBinding",
              "pgPerWell",
              "pgPer_mL",
              "ngPer_mL",
              "sampleConc_ngPer_mL"
              ), 
            digits = 3
          )
      })
      
      output$assayResults <- renderDataTable({
         meanSampleResults()%>%
          datatable() %>%
          formatRound(
            columns = c("cort", "cortCV"), 
            digits = 3
          )
      })
      
      # Download the results
      output$downloadResults <- downloadHandler(
        filename = function() {
          paste0(fileName %>% path_ext_remove(), "_results_", Sys.Date(), ".xlsx")
        },
        content = function(file) {
          saveDFsToExcel_shiny(
            file,
            cortResults = meanSampleResults(),
            calcs = assayPlate_concEstimates_meanCV()
          )
        }
      )
      
      # Download the standard curve
      output$downloadStdCurve <- downloadHandler(
        filename = function() {
          paste0(fileName %>% path_ext_remove(), "_stdCurve_", Sys.Date(), ".", input$imgType)
        },
        content = function(file) {
          flexSave(
            baseName = file,
            thisFilePrefix = NULL,
            plot = stdCurvePlot(),
            fileType = input$imgType,
            filePath = NULL,
            units = "in",
            compType = currentCompType,
            shinySettings = TRUE
          )
        }
      )
      # Download the standard curve with samples
      output$downloadSamplesOnCurve <- downloadHandler(
        filename = function() {
          paste0(fileName %>% path_ext_remove(), "_samplesOnStdCurve_", Sys.Date(), ".", input$imgType)
        },
        content = function(file) {
          flexSave(
            baseName = file,
            thisFilePrefix = NULL,
            plot = samplesOnStdCurvePlot(),
            fileType = input$imgType,
            filePath = NULL,
            units = "in",
            compType = currentCompType,
            shinySettings = TRUE
          )
        }
      )

      
      
    }
  )
}
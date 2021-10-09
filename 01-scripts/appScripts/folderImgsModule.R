### Show all images in folder

# https://shiny.rstudio.com/articles/modules.html

folderImgsUI <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(inputId = ns('files'), 
              label = 'Select images',
              multiple = TRUE,
              accept=c('image/png', 'image/jpeg', 'image/tif')),
    radioButtons(
      ns("imgsPerRow"),
      label = "# imgs per row",
      choices = c(1, 2, 3, 4, 6),
      selected = 3
    ),
    uiOutput(ns('images'))
  )
}


folderImgsServer <- function(
  id
){
  moduleServer(
    id,
    function(input, output, session) {
      
      
      files <- reactive({
        files <- input$files
        files$datapath <- gsub("\\\\", "/", files$datapath)
        files
      })
      
      
      output$images <- renderUI({
        if(is.null(input$files)) return(NULL)
        
        imgsPerRow <- input$imgsPerRow
        divCols <- case_when(
          imgsPerRow == 1 ~ "12",
          imgsPerRow == 2 ~ "6",
          imgsPerRow == 3 ~ "4",
          imgsPerRow == 4 ~ "3",
          imgsPerRow == 6 ~ "2"
        )
        
        image_output_list <- 
          lapply(
            1:nrow(files()),
            function(i)
            {
              imagename <- paste0("image", i)
              
              thisFileName <- files()$name[i] %>%
                path_file() %>%
                path_ext_remove()
              
              tags$div(
                class = paste0("col-xs-", divCols),# "col-xs-6 col-sm-4 col-md-3", # "col-sm-3",
                h4(thisFileName),
                imageOutput(session$ns(imagename), height = "auto") %>% # auto fixes the overlap
                  tagAppendAttributes(class = 'myImages')
              )
            })
        
        thisUI <- div(
          class = "container-fluid",
          div(
            class = "row",
            do.call(tagList, image_output_list)
          )
        )
        
        return(thisUI)
      })
      
      observe({
        if(is.null(input$files)) return(NULL)
        for (i in 1:nrow(files()))
        {
          # print(i)
          local({
            my_i <- i
            imagename = paste0("image", my_i)
            output[[imagename]] <- 
              renderImage({
                list(src = files()$datapath[my_i],
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


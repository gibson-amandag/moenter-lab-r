### Show all images in folder

# https://shiny.rstudio.com/articles/modules.html

folderImgsUI <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(inputId = ns('files'), 
              label = 'Select images',
              multiple = TRUE,
              accept=c('image/png', 'image/jpeg', 'image/tif')),
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
        image_output_list <- 
          lapply(1:nrow(files()),
                 function(i)
                 {
                   fileName <- paste0("name", i)
                   imagename <- paste0("image", i)
                   tags$div(
                     class = "col-sm-3",
                     textOutput(session$ns(fileName), container = h4),
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
        if(is.null(input$files)) return(NULL)
        for (i in 1:nrow(files()))
        {
          print(i)
          local({
            my_i <- i
            imagename = paste0("image", my_i)
            fileName <- paste0("name", my_i)
            outputWidth <- paste0("output_", imagename, "_width")
            outputHeight <- paste0("output_", imagename, "_height")
            print(imagename)
            output[[fileName]] <- renderText({
              files()$name[my_i]
            })
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


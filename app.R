#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("./01-scripts/01-set-up.R")
source(file.path(scriptsFolder, "02-get-datasets.R"))
library(shinyFiles)

moduleFiles <- list.files(
    appScriptsFolder, 
    full.names = TRUE,
    recursive = TRUE, 
    pattern = "*.R"
)

sapply(moduleFiles, source)

# Define UI for application 
ui <- navbarPage(
    "Moenter Lab",
    tabPanel(
        "Cycles",
        uploadCyclesUI("uploadCycles")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    uploadCyclesServer("uploadCycles")
}

# Run the application 
shinyApp(ui = ui, server = server)

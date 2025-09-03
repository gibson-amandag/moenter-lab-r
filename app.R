#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if(!require(shiny)) install.packages('shiny')
if(!require(shinyFiles)) install.packages('shinyFiles')
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
        tags$body(
            # Note the wrapping of the string in HTML()
            tags$style( # This keeps the nav bar as a single line instead of wasting space with each panel as a line on small screen
                HTML("
                    .navbar-header, .navbar-nav, .navbar-nav>li {
                        float: left;
                    }
                    
                    .navbar-nav{
                        margin: 0px
                    }
                    
                    .navbar-nav>li>a {
                        padding-top: 15px;
                        padding-bottom: 15px;
                    }
                    
                    .container-fluid>.navbar-collapse, .container-fluid>.navbar-header, .container>.navbar-collapse, .container>.navbar-header{
                        margin-right: 0;
                        margin-left: 0;
                    }
                    
                     .navbar>.container .navbar-brand, .navbar>.container-fluid .navbar-brand {
                        margin-left: -15px;
                    }
                     ")
            )
        ),
        uploadCyclesUI("uploadCycles")
    ),
    tabPanel(
        "Cort EIA",
        uploadCortEIAUI(
            "cortEIA"
        )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    uploadCyclesServer("uploadCycles", compType = currentCompType)
    uploadCortEIAServer("cortEIA", compType = currentCompType)
}

# Run the application 
shinyApp(ui = ui, server = server)

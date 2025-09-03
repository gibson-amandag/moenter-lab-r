if (!require(tidyverse)) install.packages('tidyverse')
if (!require(rlang)) install.packages('rlang')
if (!require(scales)) install.packages('scales') # with tidyverse?
if (!require(knitr)) install.packages('knitr')
if (!require(officer)) install.packages('officer')
if (!require(GGally)) install.packages('GGally')
if (!require(ggfortify)) install.packages('ggfortify')
if (!require(openxlsx)) install.packages('openxlsx')
if (!require(lubridate)) install.packages('lubridate') # with tidyverse?
if (!require(shiny)) install.packages('shiny')
if (!require(ggrepel)) install.packages('ggrepel')
if (!require(ggpubr)) install.packages('ggpubr')
if (!require(rstatix)) install.packages('rstatix')
if (!require(cowplot)) install.packages('cowplot')
if (!require(extrafont)) install.packages('extrafont')
if (!require(flextable)) install.packages('flextable')
if(!require(remotes)) install.packages('remotes')
if(!require(fs)) install.packages('fs') # with tidyverse?
if(!require(DT)) install.packages('DT')
# if(!require(Cairo)) install.packages('Cairo')
if (!require(MASS)) install.packages('MASS')
if (!require(drc)) install.packages('drc')
if(!require(shinyjs)) install.packages('shinyjs')
if(!require(colourpicker)) install.packages('colourpicker')
if(!require(plater)) install.packages('plater')


#### Load Libraries ##############################
library(MASS)
library(drc)
library(tidyverse)
library(rlang)
library(scales)
library(knitr)
library(officer)
library(GGally)
library(ggfortify)
library(openxlsx)
library(lubridate)
library(shiny)
library(ggrepel)
library(ggpubr)
library(rstatix)
library(cowplot)
library(extrafont)
library(flextable)
library(fs)
library(DT)
# library(Cairo)
library(shinyjs)
library(colourpicker)
library(plater)
select <- dplyr::select
# Save a PDF - in ggsave, device = cairo_pdf on Windows
# https://r-graphics.org/recipe-output-fonts-pdf - even when following these
# steps (downloaded Ghostscript, added to environment, embedded fonts), the
# pdf text is still overlapping on windows


## 2021-08-17 - had to install older version of Rttf2pt1 for the font_import from extrafont to work appropriately
## https://github.com/wch/extrafont/issues/88
## Download Rtools for Windows: https://cran.r-project.org/bin/windows/Rtools/

## Run these lines once on the computer
# remotes::install_version("Rttf2pt1", version = "1.3.8")
# font_import()

if(! length(fonts()) > 0){
  # have to add fonts to be able to load them into pdfs
  # https://fromthebottomoftheheap.net/2013/09/09/preparing-figures-for-plos-one-with-r/
  print("Loading fonts")
  loadfonts(dev="pdf")
}


dateToday <- Sys.Date()

# READ ENVIRONMENT --------------------------------------------------------
if(file_access("./.Renviron")){ # if .Renviron exists
  readRenviron("./.Renviron") #read the .Renviron document from the project root folder
  
  # DEFINE OUTPUT OPTIONS ---------------------------------------------------
  
  # This gets prepended to file names when saving
  filePrefix <- Sys.getenv("FILE_PREFIX")
  
  # type of images to save for plots. 
  # .png, .svg, or .pdf, for example
  imgTypePlots <- Sys.getenv("PLOT_OUTPUT_FORMAT")
  
  # save individual plot files?
  savePlots <- Sys.getenv("SAVE_PLOTS")
  
  # format specTreatment and treatment for PNA groups
  usePNAgroups <- Sys.getenv("USE_PNA_GROUPS")
  
  ## Data Paths ------------------------------------------------------------
  
  # Define in your own .Renviron file
  dataFolder <- Sys.getenv("DATA_FOLDER")
  
  ## Output Paths ----------------------------------------------------------
  
  # Define in your own .Renviron file
  outputFolder <- Sys.getenv("OUTPUT_FOLDER")
  
  # EXCEL FILE -------------------------------------------------------
  
  excelFileName <- Sys.getenv("EXCEL_FILE_NAME")
  
  currentCompType <- Sys.getenv("COMP_TYPE")
} else {
  # print("Note: No .Renviron found. If you are reading or saving files outside of RShiny file pickers, copy and edit the Renviron.example file")
  
  # Give defaults if no .Renviron
  
  filePrefix <- NULL
  
  # type of images to save for plots. 
  # .png, .svg, or .pdf, for example
  imgTypePlots <- ".pdf"
  
  # save individual plot files?
  savePlots <- FALSE
  
  # format specTreatment and treatment for PNA groups
  usePNAgroups <- FALSE
  
  ## Data Paths ------------------------------------------------------------
  
  # Define in your own .Renviron file
  dataFolder <- "./data"
  
  ## Output Paths ----------------------------------------------------------
  
  # Define in your own .Renviron file
  outputFolder <- "./output"
  
  # EXCEL FILE -------------------------------------------------------
  
  excelFileName <- "excel-demo.xlsx"
  
  currentCompType <- "windows"
}

burstOutputFolder <- file.path(dataFolder, "burst-output")
dataOutputFolder <- file.path(outputFolder, "data")
plotOutputFolder <- file.path(outputFolder, "plots")
reportOutputFolder <- file.path(outputFolder, "reports")


#Where R script files are saved
scriptsFolder <- file.path("01-scripts")
appScriptsFolder <- file.path(scriptsFolder, "appScripts")


# SOURCE FUNCTION SCRIPTS ---------------------------------------------------

functionFolder <- "./02-functions"

functionFiles <- list.files(
  functionFolder, 
  full.names = TRUE,
  recursive = TRUE, 
  pattern = "*.R"
)
sapply(functionFiles, source)

excelFilePath <- file.path(dataFolder, excelFileName)


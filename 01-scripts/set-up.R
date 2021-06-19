# INSTALL PACKAGES --------------------------------------------------------
# You should only have to do this once on your computer
# install.packages("DT"")
# install.packages("shiny")
# install.packages("tidyverse")
# install.packages("ggpubr")
# install.packages("readr")
# install.packages("rlang")
# install.packages("purrr")
# install.packages("scales")
# install.packages("knitr")
# install.packages("officer")
# install.packages("GGally")
# install.packages("dplyr")
# install.packages("openxlsx")
# install.packages("car")
# install.packages("kableExtra")
# install.packages("tools")
# install.packages("cowplot")
# install.packages("rstatix")
# install.packages("extrafont")

# LOAD LIBRARIES ----------------------------------------------------------
library(DT)
library(shiny)
library(tidyverse)
library(ggpubr)
library(readr)
library(rlang)
library(purrr)
library(scales)
library(knitr)
library(officer)
library(GGally)
library(dplyr)
library(openxlsx)
library(car)
library(kableExtra)
library(tools)
library(cowplot)
library(rstatix)
library(extrafont)
# font_import()
if(! length(fonts()) > 0){
  # have to add fonts to be able to load them into pdfs
  # https://fromthebottomoftheheap.net/2013/09/09/preparing-figures-for-plos-one-with-r/
  loadfonts(dev="pdf")
}
#https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html#Overview

# READ ENVIRONMENT --------------------------------------------------------

readRenviron("./.Renviron") #read the .Renviron document from the project root folder
dateToday <- Sys.Date()

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
burstOutputFolder <- file.path(dataFolder, "burst-output")

## Output Paths ----------------------------------------------------------

# Define in your own .Renviron file
outputFolder <- Sys.getenv("OUTPUT_FOLDER")

dataOutputFolder <- file.path(outputFolder, "data")
plotOutputFolder <- file.path(outputFolder, "plots")
reportOutputFolder <- file.path(outputFolder, "reports")

# SOURCE FUNCTION SCRIPTS ---------------------------------------------------

functionFolder <- "./02-functions"

functionFiles <- list.files(
  functionFolder, 
  full.names = TRUE,
  recursive = TRUE, 
  pattern = "*.R"
)
sapply(functionFiles, source)

# EXCEL FILE -------------------------------------------------------

excelFileName <- Sys.getenv("EXCEL_FILE_NAME")
excelFilePath <- file.path(dataFolder, excelFileName)


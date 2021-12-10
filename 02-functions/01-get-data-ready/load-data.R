### Loading Data Sets-----------------------------------------------------------------------------------------
#' Load an excel sheet as a dataframe
#' 
#' Uses the openxlsx package function read.xlsx to read the excel sheet
#' The first row is used as column names. These should not have spaces
#' NA cells in excel should be "NA" string
#' Dates are detected, empty rows and columns are skipped
#'
#' @param folderPath folder path where excel file is saved
#' @param fileName excel file name - include extension
#' @param sheetName name of sheet you want to load
#'
#' @return a dataframe with the data from the sheet
#' @export
#'
#' @examples
loadExcelSheet = function(folderPath, fileName, sheetName){
  read.xlsx(
    file.path(folderPath, fileName),
    sheet = sheetName,
    colNames = TRUE,
    rowNames = FALSE,
    detectDates = TRUE,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    na.strings = "NA"
  )
}

loadExcelSheet_fromFile = function(filePath, sheetName){
  read.xlsx(
    filePath,
    sheet = sheetName,
    colNames = TRUE,
    rowNames = FALSE,
    detectDates = TRUE,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    na.strings = "NA"
  )
}


loadBurstData_addToDF <- function(
  burstAnalysis_fileName, 
  BurstOutputsFolder, 
  demoDF,
  columnLabel = NA
){
  fileName = paste0(burstAnalysis_fileName, ".txt")
  
  bParamsInit <- read.csv(file.path(BurstOutputsFolder, fileName), sep = "\t")
  
  # Add the demographic info to the burst params dataframe
  bParamsOut <- combineBurstData(demoDF, bParamsInit, columnLabel)
  bParamsDF <- bParamsOut$bParamsDF
  
  return(bParamsDF)
}

combineBurstData = function(fullDF, bParamsDF, columnLabel = NA){
  bParamsDF <- as.data.frame(bParamsDF)
  
  if("cellName" %in% colnames(bParamsDF)){
    bParamsDF <- bParamsDF %>% rename(cellID = cellName)
  }
  
  if(!is.na(columnLabel)){
    bParamsDF <- bParamsDF %>%
      rename_with(
        ~ paste0((.), "_", columnLabel),
        .cols = -cellID
      )
  }
  
  bParamsDF <- bParamsDF %>%
    left_join(
      fullDF,
      by = "cellID"
    ) 
  
  
  my_list = list(
    "bParamsDF" = bParamsDF
  )
  
  return(my_list)
}


getBurstDF <- function(
  burstAnalysis_fileName, 
  BurstOutputsFolder,
  addLabelToColNames = NA
){
  fileName = paste0(burstAnalysis_fileName, ".txt")
  
  bParamsInit <- read.csv(file.path(BurstOutputsFolder, fileName), sep = "\t")
  
  bParamsDF <- as.data.frame(bParamsInit)
  
  if("cellName" %in% colnames(bParamsDF)){
    bParamsDF <- bParamsDF %>% rename(cellID = cellName)
  }
  
  if(!is.na(addLabelToColNames)){
    bParamsDF <- bParamsDF %>%
      rename_with(
        ~ paste0((.), "_", addLabelToColNames),
        .cols = -cellID
      )
  }
  
  return(bParamsDF)
}

readBurstDFs <- function(
  treatmentInfo, # a list containing the fileName and name for the treatment epoch
  burstOutputsFolder, # folder where igor txt output files are saved
  addLabelToColNames = FALSE # if want to append _name to end of each column, for making wide
){
  fileName = treatmentInfo[["fileName"]]
  name = treatmentInfo[["name"]]
  
  if(addLabelToColNames){
    addLabelToColNames = name
  } else {
    addLabelToColNames = NA
  }
  
  
  bParamsDF = getBurstDF(
    fileName,
    burstOutputsFolder,
    addLabelToColNames
  )
  
  return(bParamsDF)
}




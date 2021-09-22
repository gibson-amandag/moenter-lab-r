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
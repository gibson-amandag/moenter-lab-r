
# LOAD FROM EXCEL ---------------------------------------------------------

loadExcelSheet <- function(filePath, sheetName){
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

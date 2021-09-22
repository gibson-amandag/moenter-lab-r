

# WRITE TO WORKBOOK -------------------------------------------------------

writeToWorkbook <- function(sheetName, df, wb){
  addWorksheet(wb, sheetName)
  writeDataTable(wb, sheetName, df, tableStyle = "TableStyleMedium2")
}


saveProjectDatasetsToExcel <- function(
  fileBaseName = "projectData",
  prefix = filePrefix,
  addDate = dateToday,
  saveFolder = dataOutputFolder,
  mouseInfoDF = mouseInfo,
  mouseRecordingDF = mouseRecording,
  cellInfoDF = cellInfo,
  cellBinnedFiringDF = cellBinnedFiring,
  cyclesDataDF = cyclesData,
  agdDataDF = agdData,
  maturationDataDF = maturationData
){
  saveDFsToExcel(
    fileBaseName = fileBaseName,
    prefix = prefix,
    addDate = addDate,
    saveFolder = saveFolder,
    mouseInfo = mouseInfoDF,
    mouseRecording = mouseRecordingDF,
    cellInfo = cellInfoDF,
    cellBinnedFiring = cellBinnedFiringDF,
    cycles = cyclesDataDF,
    AGD = agdDataDF,
    maturation = maturationDataDF
  )
}

saveDFsToExcel <- function(
  fileBaseName,
  prefix = filePrefix,
  addDate = dateToday,
  saveFolder = dataOutputFolder,
  ... # use sheetName = df, sheetName2 = df2, sheetName3 = df3 for each df you want to add to a new sheet
){
  dfList <- list(...)
  
  wb <- createWorkbook()
  
  for(sheetName in names(dfList)){
    writeToWorkbook(sheetName, dfList[[sheetName]], wb)
  }
  
  fileName <- paste0(prefix, fileBaseName, "_", addDate, ".xlsx")
  filePath <- file.path(saveFolder, fileName)
  print(filePath)
  
  saveWorkbook(wb, filePath, overwrite = TRUE)
}

greenFill <- function(){
  createStyle(bgFill = "#C6EFCE")
}

yellowFill <- function(){
  createStyle(bgFill = "#ffff00")
}

redFill <- function(){
  createStyle(bgFill = "#FFC7CE")
}

greyFill <- function(){
  createStyle(bgFill = 	"#D3D3D3")
}

blueFill <- function(){
  createStyle(bgFill = "#9edaff")
}

pinkFill <- function(){
  createStyle(bgFill = "#f3b4f6")
}

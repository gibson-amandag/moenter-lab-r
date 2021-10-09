if(file_access(excelFilePath) & path_ext(excelFileName) == "xlsx"){
# Load Excel Sheets -------------------------------------------------------

mouseInfo <- loadExcelSheet(dataFolder, excelFileName, "mouseInfo")
mouseRecording <- loadExcelSheet(dataFolder, excelFileName, "mouseRecording")
cellInfo <- loadExcelSheet(dataFolder, excelFileName, "cellInfo")
cellTiming <- loadExcelSheet(dataFolder, excelFileName, "cellTiming")
cellBinnedFiring <- loadExcelSheet(dataFolder, excelFileName, "cellBinnedFiring")
cyclesData <- loadExcelSheet(dataFolder, excelFileName, "cycles")
agdData <- loadExcelSheet(dataFolder, excelFileName, "AGD")
maturationData <- loadExcelSheet(dataFolder, excelFileName, "maturation")


# FORMAT DATASETS ---------------------------------------------------------

mouseInfo <- mouseInfo %>%
  formatMouseInfo()

mouseRecording <- mouseRecording %>%
  formatMouseRecording() %>%
  addMouseInfoToDataset() %>%
  addAgeColumn(ageAtCol = recordingDate) %>%
  addTreatByAgeCol()

cellTiming <- cellTiming %>%
  formatCellID()

cellInfo <- cellInfo %>%
  formatCellInfo() %>%
  addMouseRecordingToCells() %>%
  addCellTimingToDataset() %>%
  addRecTimeSinceSlice()

cellBinnedFiring <- cellBinnedFiring %>%
  formatCellID() %>%
  addCellInfoToDataset()

cyclesData <- cyclesData %>%
  formatMouseID() %>%
  addMouseInfoToDataset() %>%
  countEstrousStageDays()

agdData <- agdData %>%
  formatMouseID() %>%
  addMouseInfoToDataset() %>%
  setUpAGD()

maturationData <- maturationData %>%
  formatMouseID() %>%
  addMouseInfoToDataset() %>%
  setUpMaturation()
}

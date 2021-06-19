# Load Excel Sheets -------------------------------------------------------

mouseInfo <- loadExcelSheet(excelFilePath, "mouseInfo")
mouseRecording <- loadExcelSheet(excelFilePath, "mouseRecording")
cellInfo <- loadExcelSheet(excelFilePath, "cellInfo")
cellTiming <- loadExcelSheet(excelFilePath, "cellTiming")
cellBinnedFiring <- loadExcelSheet(excelFilePath, "cellBinnedFiring")
cyclesData <- loadExcelSheet(excelFilePath, "cycles")
agdData <- loadExcelSheet(excelFilePath, "AGD")
maturationData <- loadExcelSheet(excelFilePath, "maturation")


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


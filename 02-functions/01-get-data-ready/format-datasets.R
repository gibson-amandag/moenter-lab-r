# FORMAT AND COMBINE DATASETS
# Use mouse or cell demographic sheets to add the necessary information to the
# datasets on other excel sheets


# FORMAT ------------------------------------------------------------------

#' make factors
#'
#' @param df a dataframe
#' @param cols the columns that you wish to make factors; if using multiple use c(...)
#'
#' @return a dataframe with the indicated columns made factors
#' @export
#'
#' @examples
#' Demo_dam %>%
#' makeFactors(c(
#'  damID,
#'  dam,
#'  ParaType,
#'  litterNum,
#'  cohort
#' ))
#' 
#' makeFactors(Off_ID, c(mouseID))
#' makeFactors(EndPara_off, mouseID)
makeFactors <- function(df, cols){
  df <- df %>%
    mutate(
      across(
        {{ cols }},
        as.factor
      )
    )
  return(df)
}

changeYNtoTF <- function(x){
  val <- case_when(
    x == "Y" ~ TRUE,
    x == "N" ~ FALSE,
    x == TRUE ~ TRUE,
    x == FALSE ~ FALSE,
    TRUE ~ as.logical(x)
  )
  return(val)
}

removeSpaces <- function(x){
  val <- gsub('\\s+', '', x)
  return(val)
}

formatCellID <- function(df){
  df <- df %>%
    mutate(
      cellID = removeSpaces(cellID)
    ) %>%
    mutate(
      cellID = as.factor(cellID)
    )
  return(df)
}

formatMouseID <- function(df){
  df <- df %>%
    mutate(
      mouseID = removeSpaces(mouseID)
    ) %>%
    mutate(
      mouseID = as.factor(mouseID)
    )
  return(df)
}

formatMouseInfo <- function (mouseInfo, formatPNA_treat = usePNAgroups){
  mouseInfo <- mouseInfo %>%
    mutate(
      across(
        c(
          mouseID,
          cage,
          generation,
          damID,
          sireID,
          specTreatment,
          treatment,
          sex,
          zygosity,
          strain,
          gonadStatus,
          implantType
        ),
        as.factor
      )
    ) %>%
    mutate(
      treatment = case_when(
        treatment == "Control" ~ "control", # make lowercase
        treatment == "Vehicle" ~ "vehicle",
        TRUE ~ as.character(treatment) # everything else as is
      )
    ) %>%
    mutate(
      across(where(is.factor), removeSpaces)
    )
  
  if(formatPNA_treat == TRUE){
    mouseInfo <- mouseInfo %>%
      formatPNAgroups()
  }
  return(mouseInfo)
}

addMouseNumber <- function(
  df
){
  df <- df %>% 
    arrange(
      recordingDate
    )%>%
    group_by(
      damID,
      dateOfBirth,
      sex
    ) %>%
    mutate(
      mouseNum = as_factor(row_number())
    ) %>%
    ungroup()
  return(df)
}

addCellNumber <- function(
  df
){
  df <- df %>% 
    arrange(
      recordStart
    )%>%
    group_by(
      mouseID
    ) %>%
    mutate(
      cellNum = as_factor(row_number())
    ) %>%
    ungroup()
  return(df)
}

formatPNAgroups <- function(
  df,
  specTreatmentCol = specTreatment,
  genTreatmentCol = treatment
){
  df <- df %>%
    mutate(
      {{ specTreatmentCol }} := factor(
        {{ specTreatmentCol }},
        levels = c("CON_main", "CON", "VEH", "DHT")
        # labels = c(...) # if you want to change these labels
      ),
      {{ genTreatmentCol }} := factor(
        {{ genTreatmentCol }},
        levels = c("control", "PNA"),
        labels = c("CON", "PNA")
      )
    )
  
  return(df)
}

formatMouseRecording <- function(mouseRecording) {
  mouseRecording <- mouseRecording %>%
    mutate(
      across(
        c(
          mouseID,
          whoSliced,
          cycleStage
        ),
        as.factor
      )
    ) %>%
    mutate(
      across(where(is.factor), removeSpaces)
    ) %>%
    mutate(
      across(
        c(
          savedPit
        ),
        ~ changeYNtoTF(.)
      )
    ) %>%
    select(
      -c(daylightSavings
         , timeSac
         # , ageInDays
         )
    )
  
  
  return(mouseRecording)
}

formatCellInfo <- function(cellInfo){
  cellInfo <- cellInfo %>%
    mutate(
      across(
        c(
          cellID,
          mouseID,
          whoRecorded,
          status
        ),
        as.factor
      )
    ) %>%
    mutate(
      across(where(is.factor), removeSpaces)
    ) %>%
    addCellNumber() %>%
    addRecTimeHr() %>%
    select( # Calculation done in excel, since it's stupid complicated here in R
      -c(daylightSavings, recordStart, recordEnd)
    )
  
  return(cellInfo)
}


# ADD MOUSE INFO ----------------------------------------------------------

addMouseInfoToDataset <- function(
  df,
  mouseInfoDF = mouseInfo
){
  df <- df %>%
    left_join(
      mouseInfoDF,
      by = "mouseID"
    ) %>%
    relocate(
      treatment,
      sex,
      .after = mouseID
    )
  return(df)
}

addMouseRecordingToCells <- function(
  df,
  mouseRecDF = mouseRecording
) {
  df <- df %>%
    left_join(
      mouseRecDF,
      by = "mouseID"
    ) %>%
    relocate(
      cellID,
      .before = mouseID
    ) %>%
    relocate(
      treatment,
      sex,
      ageGroup,
      mouseNum,
      cellNum,
      .after = mouseID
    )
  
  return(df)
}


# COMBINE CELL DATASETS ---------------------------------------------------

# This adds the cell timing dataframe to the df. Keeps all rows of DF, and only
# adds cell timing rows present in df
addCellTimingToDataset <- function(
  df,
  cellTimingDF = cellTiming
){
  df <- df %>%
    left_join(
      cellTimingDF,
      by = "cellID"
    )
  return(df)
}

# This adds the cell info dataframe to the df. Keeps all rows of DF, and only
# add cell info rows present in df
addCellInfoToDataset <- function(
  df,
  cellInfoDF = cellInfo
){
  df <- df %>%
    left_join(
      cellInfoDF,
      by = "cellID"
    ) %>%
    relocate(
      starts_with("FreqHz"),
      .after = last_col()
    )
  return(df)
}


# CALCULATIONS ------------------------------------------------------------

#' addAgeColumn
#'
#' @param df dataframe with the two date columns from which you want to
#'   calculate an age
#' @param ageAtCol the column name, unquoted, at which you want to calculate the
#'   age in days
#' @param dobCol the column name, unquoted, of the date of birth column
#' @param dobAsDay enter either 0 or 1 if the date of birth should be considered
#'   postnatal day 0 or 1, respecively
#'
#' @return dataframe with column "age" added
addAgeColumn <- function(
  df, 
  ageAtCol,
  ageName = "age",
  addAgeGroup = TRUE,
  dobCol = dateOfBirth,
  dobAsDay = 0 # this still works to provide a single number, though
){
  df <- df %>%
    mutate(
      age := as.numeric(
        difftime({{ ageAtCol }}, {{ dobCol }}, 
                 units = c("days"))
        ) + {{ dobAsDay }} # added to be able to specify with a column, 2021-12-06
    )
  
  if(addAgeGroup == TRUE){
    df <- df %>%
      mutate(
        ageGroup = case_when(
          (age >= 18 & age <= 22) ~ "3wk",
          age >= 60 ~ "adult"
        ) 
      ) %>%
      mutate(
        across(ageGroup, as.factor)
      )
  }
  df <- df %>%
    rename(
      {{ ageName }} := age
    )
  return(df)
}

addTreatByAgeCol <- function(df) {
  df <- df %>%
    mutate(
      TreatxAge = case_when(
        (!is.na(treatment) & !is.na(ageGroup)) ~ paste(treatment, ageGroup, sep="_")
      )
    )
  
  return(df)
}

addRecTimeHr <- function(df){
  df <- df %>%
    mutate(
      lightsOn = case_when(
        daylightSavings == "N" ~ 3/24,
        daylightSavings == "Y" ~ 4/24,
      ),
      recordStart_hr = (recordStart - lightsOn) * 24,
      recordEnd_hr = (recordEnd - lightsOn) * 24,
      .after = recordStart
    ) %>%
    select(-lightsOn)
  return(df)
}

addRecTimeSinceSlice <- function(df){
  df <- df %>%
    mutate(
      recTime_hrPostSac = recordStart_hr - sacHrs,
      .before = recordStart_hr
    ) %>%
    relocate(
      recordStart_hr, recordEnd_hr,
      .after = last_col()
    )
  return(df)
}
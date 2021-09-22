#' Run a mean summary on a dataframe
#'
#' Uses rstatix::get_summary_stats() to return the number, mean, standard error,
#' and standard deviation for selected variables
#'
#' @param df a dataframe to be summarized. Provide a grouped df if desired
#' @param col The column(s) to be summarized. Provide as colName, col1:col5, c(col1, col3), etc
#'
#' @return a dataframe with n, mean, sem, sd for each selected column
#' @export
#'
#' @examples
meanSummary <- function(df, col){ # colName, col1:col5, c(col1, col3) etc 
  sumDF <- df %>%
    get_summary_stats(
      {{ col }},
      type = "full",
      show = c("n", "mean", "se", "sd")
    ) %>%
    rename(
      "sem" = "se"
    )
  
  return(sumDF)
}

#' Do a quartile summary for selected variables
#' 
#' Uses rstatix::get_summary_stats() five number summary
#'
#' @param df a dataframe to be summarized. Provide as grouped df if desired
#' @param col The column(s) to be summarized. Provide as colName, col1:col5, c(col1, col3), etc
#'
#' @return a dataframe with min, q1, median, q3, max summarized for each variable
#' @export
#'
#' @examples
quartilesSummary <- function(df, col){
  sumDF <- df %>%
    get_summary_stats(
      {{ col }},
      type = "five_number"
    )
  return(sumDF)
}


# SUMMARIZE BY DAM ------------------------------------------------------------

#' group a dataframe by dam ID
#'
#' @param df a dataframe with the column damID
#'
#' @return a dataframe grouped by damID
#' @export
#'
#' @examples
groupByDam <- function(df){
  df_byDam <- df %>%
    group_by(damID)
  return(df_byDam)
}

#' Get the average of numeric variables for a litter
#' 
#' Summarizes all numeric variables for the provided dataframe by damID
#' Adds back in the non-numeric variables from the damDemo_forOff dataframe
#'
#' @param df a dataframe of offspring data. Must contain damID column
#' @param damDemo_forOff a dataframe with the dam demographic information that is re-added to the offspring dataframe
#'
#' @return a dataframe with the offspring numeric data averaged for each dam
#' @export
#'
#' @examples
getAvgByDam <- function(df, damDemo_forOff = Demo_dam_for_offspring){
  avgDF <- df %>%
    groupByDam() %>%
    summarise(
      across(
        where(is.numeric),
        ~ mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    addDamDemoData(
      damDemo_forOff = Demo_dam_for_offspring %>%
        select(!where(is.numeric))
    )
  return(avgDF)
}


# Get maximum -------------------------------------------------------------

#' Get the maximum value from repeated measures
#' 
#' This function first groups the dataframe by the grouping variable, for example a mouse ID,
#' and finds the maximum of the selected column for each group
#' 
#' The result is placed in a new named column, defined by maxColName
#'
#' @param df a dataframe with long-form repeated measures data
#' @param col the column containing the repeated measures values
#' @param maxColName the name of the new column that should contain the maximum value
#' @param groupingVar the variable on which to group and find the maximum for
#'
#' @return a dataframe where the maximum col value for each groupingVar value is placed in the maxColName columns
#' @export
#'
#' @examples
getMaxFromRepMeasures <- function(df, col, maxColName, groupingVar){
  df_max <- df %>%
    group_by( {{ groupingVar }} ) %>%
    summarize(
      {{ maxColName }} := max({{ col }}, na.rm = TRUE),
      .groups = "drop"
    )
  return(df_max)
}

# AgeInDays -------------------------------------------------------------

#' Calculate age in days at a given date
#' 
#' Uses the date of birth (DOBVar) to determine the age in days at another date
#' (ageAtDateVar).
#'
#' @param df a dataframe
#' @param DOBVar the column containing the date of birth. Default is DOB
#' @param ageAtDateVar the column containing the date that you wish to calculate the age for. Default is Sac_date
#' @param DOBisDay a number. Age on date of birth. Should be 0 or 1. Default is 0
#' @param ageColName the name for the column where the age should be stored. Default is AgeInDays
#'
#' @return a dataframe with the ageColName column added that contains the difference between the DOBVar and ageAtDateVar plus age at DOB (DOBisDay)
#' @export
#'
#' @examples
calcAgeInDays <- function(
  df,
  DOBVar = DOB,
  ageAtDateVar = Sac_date,
  DOBisDay = 0,
  ageColName = AgeInDays
){
  df <- df %>%
    mutate(
      {{ ageColName }} := ifelse(
        !is.na({{ageAtDateVar}}) & !is.na({{DOBVar}}),
        as.numeric({{ageAtDateVar}} - {{DOBVar}}) + DOBisDay,
        NA
      ),
      .after = {{ageAtDateVar}}
    )
  return(df)
}


# Organ mass by body mass -------------------------------------------------

#' Calculate the ratio of organ mass to body mass (g)
#'
#' @param df a dataframe
#' @param organMassVar name of the column containing the organ mass data
#' @param bodyMassVar name of the column containing the body mass data
#'
#' @return a dataframe with a new column added (name of OrganMassVar with _perBody_g appended) that 
#' contains the organMassVar divided by the bodyMassVar
#' @export
#'
#' @examples
calcOrganMassByBodyMass <- function(
  df,
  organMassVar,
  bodyMassVar = Body_mass_sac
){
  df <- df %>%
    mutate(
      "{{ organMassVar }}_perBody_g" := {{ organMassVar }} / {{ bodyMassVar }},
      .after = {{ organMassVar }}
    )
  return(df)
}

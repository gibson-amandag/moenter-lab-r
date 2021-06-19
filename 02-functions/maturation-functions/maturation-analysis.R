addVOAge <- function(df, dobAsDay = 0){
  df <- df %>%
    mutate(
      VO_age = ifelse(
        is.na(VO_age) & sex == "F",
        as.numeric(VO_day - dateOfBirth) + dobAsDay,
        VO_age
      )
    )
  return(df)
}

addEstrusAge <- function(df, dobAsDay = 0){
  df <- df %>%
    mutate(
      Estrus_age = ifelse(
        is.na(Estrus_age) & sex == "F",
        as.numeric(Estrus_day - dateOfBirth) + dobAsDay,
        Estrus_age
      )
    )
  return(df)
}

addPreputialSepAge <- function(df, dobAsDay = 0){
  df <- df %>%
    mutate(
      PreputialSep_age = ifelse(
        is.na(PreputialSep_age) & sex == "M",
        as.numeric(PreputialSep_day - dateOfBirth) + dobAsDay,
        PreputialSep_age
      )
    )
  return(df)
}

getMeanAcrossDays <- function(
  df,
  colsToAvg, # tidy select - almost any format (c(x, y), x:y, "x")
  avgColName # can be quoted or unquoted
){
  df <- df %>%
    mutate(
      {{ avgColName }} := rowMeans(select(., {{ colsToAvg }}), na.rm = FALSE),
      .before = {{ colsToAvg }}
    )
  
  return(df)
}

setUpMaturation <-  function(df) {
  df <- df %>%
    addVOAge() %>%
    addEstrusAge() %>%
    addPreputialSepAge() %>%
    getMeanAcrossDays(colsToAvg = c(AGD_P22, AGD_P23, AGD_P24), avgColName = AGD_wean) %>%
    getMeanAcrossDays(colsToAvg = c(AGD_P70, AGD_P71, AGD_P72), avgColName = AGD_adult)
  return(df)
}

setUpAGD <- function(df, normToMass = FALSE){
  df <- df %>%
    relocate(
      starts_with("AGD"),
      starts_with("mass"),
      .after = mouseID
    ) %>%
    getMeanAcrossDays(starts_with("AGD"), "AGD_avg") %>%
    getMeanAcrossDays(starts_with("mass"), "mass_avg") %>%
    relocate(
      starts_with("AGD_day"),
      starts_with("mass_day"),
      .after = last_col()
    )
  
  if(normToMass == TRUE){
    df <- df %>%
      mutate(
        AGD_normToMass = AGD_avg / mass_avg
      )
  }
  
  return(df)
}

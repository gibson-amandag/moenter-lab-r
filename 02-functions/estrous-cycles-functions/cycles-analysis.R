countEstrousStageDays <- function(df, minDaysToSum = 10){
  df <- df %>%
    mutate(
      cycleDays = rowSums(
        !is.na(
          select(., starts_with("day"))
        )
      ),
      numE = rowSums(
        select(., starts_with("day")) == 1,
        na.rm = TRUE
      ),
      numD = rowSums(
        select(., starts_with("day")) == 2,
        na.rm = TRUE
      ),
      numP = rowSums(
        select(., starts_with("day")) == 3,
        na.rm = TRUE
      ),
      percE = ifelse(cycleDays >= minDaysToSum, numE / cycleDays * 100, NA),
      percD = ifelse(cycleDays >= minDaysToSum, numD / cycleDays * 100, NA),
      percP = ifelse(cycleDays >= minDaysToSum, numP / cycleDays * 100, NA),
      .after = sex
    )
  
  return(df)
}
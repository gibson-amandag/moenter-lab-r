makeDamMassLong <- function(df){
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("Dam_Mass_P"),
      names_to = "day",
      names_prefix = "Dam_Mass_P",
      values_to = "mass",
      values_drop_na = TRUE,
      names_transform = list("day" = as.integer)
    )
  return(df_long)
}

makeOffMassLong <- function(df){
  df_long <- df %>%
    pivot_longer(
      cols = starts_with("Mass_P"),
      names_to = "day",
      names_prefix = "Mass_P",
      values_to = "mass",
      values_drop_na = TRUE,
      names_transform = list("day" = as.integer)
    )
  return(df_long)
}


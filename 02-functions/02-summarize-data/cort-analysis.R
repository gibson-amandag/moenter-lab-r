#' Analyze two-way anova for early-life and adult treatment effects on corticosterone
#'
#' Runs the two-way anova with rstatix::anova_test(). 
#' The dependent variable is cort
#' Within animal id is mouseID
#' Between animal variables are early-life and adult treatment (earlyLifeTrt, adultTrt)
#' The within animal variable is time
#' 
#' Formats the output as a flextable using formatAnova()
#' 
#' @param df a long-form data frame with the columns cort, mouseID, earlyLifeTrt, adultTrt, and time
#'
#' @return a flextable with the formatted anova results
#' @export
#'
#' @examples
cortAnova <- function(
  df,
  betweenVars = c(adultTrt)
){
  anovaRes <- df %>%
    anova_test(
      dv = cort,
      wid = mouseID,
      between = betweenVars,
      within = time
    )
  flxTbl <- formatAnova(anovaRes)
  return(flxTbl)
  # return(anovaRes)
}

#' Format stress anova output
#' 
#' takes the rstatix::anova_test() output and makes it a dataframe, then mutates to make the following changes:
#' If p is less than 0.001, it is reported as "<0.001"
#' Makes it a flextable
#' Bolds rows where p is < 0.05
#' Font size is 11
#'
#' @param anovaDF 
#'
#' @return
#' @export
#'
#' @examples
formatAnova <- function(anovaDF){
  flxTbl <- anovaDF %>%
    as_tibble() %>% # replaced as_data_frame()
    mutate(
      p = case_when(
        p < 0.001 ~ as.character("<0.001"),
        TRUE ~ as.character(p)
        # ),
        # Effect = case_when(
        #   Effect == "earlyLifeTrt" ~ "early life trt",
        #   Effect == "adultTrt" ~ "adult trt",
        #   Effect == "earlyLifeTrt:adultTrt" ~ "early life x adult trt",
        #   Effect == "earlyLifeTrt:time" ~ "early life trt x time",
        #   Effect == "adultTrt:time" ~ "adult trt x time",
        #   Effect == "earlyLifeTrt:adultTrt:time" ~ "early life x adult trt x time",
        #   TRUE ~ Effect
      )
    ) %>%
    flextable() %>%
    bold(
      i = ~ `p<.05` == "*"
    ) %>%
    fontsize(
      size = 11
    )
  return(flxTbl)
}

scatterPlot_twoGroups <- function(
  df,
  xGroupVar,
  yVar,
  yLab
){
  viz <- df %>%
    ggplot(
      aes(
        x = {{ xGroupVar }},
        y = {{ yVar }},
        fill = {{ xGroupVar }}
      )
    ) +
    jitterGeom() +
    addMeanHorizontalBar() +
    addMeanSE_vertBar()+
    labs(y = yLab)+
    whiteBlackFill() +
    theme_pubr()+
    expand_limits(y=0)+
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )+
    textTheme()+
    boxTheme()
  
  return(viz)
}

scatterPlotLBN <- function(
  df,
  yVar,
  yLab
){
  viz <- df %>%
    ggplot(
      aes(
        x = earlyLifeTrt,
        y = {{ yVar }},
        fill = earlyLifeTrt
      )
    ) +
    jitterGeom() +
    addMeanHorizontalBar() +
    addMeanSE_vertBar()+
    labs(y = yLab)+
    earlyLifeFill() +
    theme_pubr()+
    expand_limits(y=0)+
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )+
    textTheme()+
    boxTheme()
  
  return(viz)
}

scatterPlotTwoVars_fillTwoGroups <- function(
  df,
  yVar,
  yLab,
  xVar,
  xLab,
  fillVar
){
  viz <- df %>%
    ggplot(
      aes(
        x = {{ xVar }},
        y = {{ yVar }},
        fill = {{ fillVar }}
      )
    ) +
    jitterGeom() +
    labs(y = yLab, x = xLab)+
    expand_limits(x = 0, y = 0)+
    whiteBlackFill() +
    textTheme()+
    boxTheme()
  
  return(viz)
}

scatterPlotTwoVars_byLBN <- function(
  df,
  yVar,
  yLab,
  xVar,
  xLab
){
  viz <- df %>%
    ggplot(
      aes(
        x = {{ xVar }},
        y = {{ yVar }},
        fill = earlyLifeTrt
      )
    ) +
    jitterGeom() +
    labs(y = yLab, x = xLab)+
    expand_limits(x = 0, y = 0)+
    earlyLifeFill() +
    textTheme()+
    boxTheme()
  
  return(viz)
}

scatterPlotComboTrt <- function(
  df,
  yVar,
  yLab,
  dotSize = 1.2,
  fontSize = 11
){
  viz <- df %>%
    filter(
      !is.na({{ yVar }})
    ) %>%
    ggplot(
      aes(
        x = comboTrt,
        y = {{ yVar }},
        fill = comboTrt,
        shape = comboTrt
      )
    ) +
    jitterGeom(size = dotSize) +
    addMeanHorizontalBar() +
    addMeanSE_vertBar()+
    labs(y = yLab)+
    comboTrtFillShape() +
    theme_pubr()+
    expand_limits(y=0)+
    theme(
      axis.title.x = element_blank(),
      legend.position = "none"
    )+
    textTheme(size = fontSize)+
    boxTheme()
  
  return(viz)
}
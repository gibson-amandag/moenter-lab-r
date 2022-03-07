cortPlot <- function(
  df_long,
  pointSize = 1.2,
  fontSize = 11,
  zoom_x = FALSE, #Zoom to a part of x axis
  xmin = NULL,
  xmax = NULL,
  zoom_y = FALSE, #Zoom to a part of y axis
  ymin = NULL,
  ymax = NULL,
  plotMean = TRUE,
  plotSE = TRUE,
  xBreaks = c(0, 5),
  xLabels = c("pre", "post"),
  lineTypeGuide = c("dotted", "dotted", "solid", "solid"),
  positionDodge = 1.2,
  groupVar = comboTrt
){
  viz <- ggplot(
    df_long,
    aes(
      x = time,
      y = cort,
      group =  {{ groupVar }}
    )
  ) +
    geom_line(
      alpha = 0.4,
      # color = "black",
      aes(group = mouseID, linetype =  {{ groupVar }}, color =  {{ groupVar }}),
      position = position_dodge(positionDodge)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill= {{ groupVar }},group=mouseID, shape= {{ groupVar }}, color= {{ groupVar }}), 
      position = position_dodge(positionDodge), 
      size = pointSize
    ) +
    theme_pubr() +
    expand_limits(y = 0) +
    coord_cartesian(if(zoom_x){xlim = c(xmin, xmax)}, if(zoom_y){ylim = c(ymin, ymax)}) +
    # rremove("xlab") + ## seems like can't add back after if do this
    labs(
      y = "corticosterone (ng/mL)",
      x = NULL
    ) +
    scale_x_continuous(
      breaks = xBreaks, #c(0, 5),
      labels = xLabels, #c("pre", "post")
    ) +
    textTheme(size = fontSize)+
    boxTheme()+
    guides()#linetype = "none")
  
  if(plotMean){
    viz <- viz + addMeanHorizontalBar(
      width = 1.4, 
      addLineType = TRUE,
      lineTypeName = "treatment",
      lineTypeGuide = c("dotted", "dotted", "solid", "solid"),
      typeVar= {{ groupVar }},
      color= {{ groupVar }}
    )
  } else {
    viz <- viz + labs(linetype = "treatment")
  }
  
  if(plotSE){
    viz <- viz + addMeanSE_vertBar(color= {{ groupVar }})
  }
  
  if(deparse(substitute(groupVar)) == "comboTrt"){
    viz <- viz + comboTrtFillShape()
  } else {
    viz <- viz + labs(color = "treatment", shape = "treatment", fill = "treatment")
  }
  return(viz)
}
baseCortPlot <- function(
  df_long,
  dotSize = 1.2,
  groupByVar = comboTrt
){
  ggplot(
    df_long,
    aes(
      x = time,
      y = cort,
      group = {{ groupByVar }}
    )
  ) +
    geom_line(
      alpha = 0.4,
      color = "black",
      aes(group = mouseID),
      position = position_dodge(0.4)
    ) +
    geom_point(
      alpha = 1, 
      aes(fill={{ groupByVar }},group=mouseID, shape={{ groupByVar }}, color={{ groupByVar }}), 
      position = position_dodge(0.4), 
      size = dotSize
      ) +
    addMeanHorizontalBar(
      width = 0.85, 
      addLineType = TRUE,
      lineTypeName = "treatment",
      lineTypeGuide = c("dotted", "dotted", "solid", "solid"),
      typeVar={{ groupByVar }},
      color={{ groupByVar }}
    )+
    addMeanSE_vertBar(color={{ groupByVar }})+
    comboTrtFillShape()+ ### IMPORTANT - Change based on needs
    theme_pubr() +
    rremove("xlab") +
    labs(
      y = "corticosterone (ng/mL)",
      fill = "treatment"
    ) +
    scale_x_continuous(
      breaks = c(0, 5),
      labels = c("pre", "post")
    )
}

longCortPlot <- function(
  basePlot,
  fontSize = 11,
  groupByVar = comboTrt
){
  longPlot <- basePlot +
    facet_wrap(
      ~ {{ groupByVar }},
      strip.position = "bottom",
      ncol = 4,
      nrow = 1
    ) +
    rremove(
      "legend"
    ) + 
    textTheme(size = fontSize)+
    boxTheme()
  return(longPlot)
}

plotByUterineMass <- function(
  df,
  yVar,
  yLab,
  fontSize = 11,
  dotSize = 1.2,
  groupByVar = comboTrt
){
  plot <- df %>%
    ggplot(
      aes(
        x = ReproTract_mass,
        y = {{ yVar }},
        fill = {{ groupByVar }},
        shape = {{ groupByVar }},
        color = {{ groupByVar }},
      )
    ) +
    geom_jitter(size = dotSize) +
    expand_limits(x = 0, y = 0) +
    labs(x = "uterine mass (mg)", y = yLab)+
    comboTrtFillShape()+ ### IMPORTANT - Change based on needs
    theme_pubr()+
    textTheme(size = fontSize)+
    boxTheme()
  return(plot)
}

plotUterineMassByGroup <- function(
  df,
  showHline = TRUE,
  hLineVal = 140,
  xGroupVar = comboTrt,
  fontSize = 11,
  dotSize = 1.2,
  fillVar = comboTrt,
  shapeVar = comboTrt
){
  viz <- df %>%
    ggplot(
      aes(
        y = ReproTract_mass,
        x = {{ xGroupVar }},
        fill = {{ fillVar }},
        shape = {{ shpaeVar }}
      )
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill={{ fillVar }},group=mouseID, shape= {{ shapeVar }}), 
      position = position_dodge(0.4), 
      size = dotSize
    ) +
    comboTrtFillShape()+ ### IMPORTANT - Change based on needs
    theme_pubr() +
    rremove("xlab") +
    labs(
      y = "uterine mass (mg)"
    )+
    expand_limits(y = 0)+
    textTheme(size = fontSize)+
    boxTheme()
  
  if(showHline){
    viz <- viz + geom_hline(yintercept = hLineVal, color = "red")
  }
  return(viz)
}

LHPlot <- function(
  df_long,
  fontSize = 11,
  dotSize = 1.2,
  groupByVar = comboTrt
){
  ggplot(
    df_long,
    aes(
      x = time,
      y = LH,
      group = comboTrt
    )
  ) +
    geom_line(
      alpha = 0.4,
      color = "black",
      aes(group = mouseID),
      position = position_dodge(0.4)
    ) +
    geom_point(
      # shape = 21, 
      alpha = 1, 
      aes(fill={{ groupByVar }},group=mouseID, shape={{ groupByVar }}), 
      position = position_dodge(0.4), 
      size = dotSize
    ) +
    addMeanHorizontalBar(width = 0.85, addLineType = TRUE)+
    addMeanSE_vertBar()+
    comboTrtFillShape()+ ### IMPORTANT - Change based on needs
    theme_pubr() +
    labs(
      y = "LH (ng/mL)"
    ) +
    textTheme(size = fontSize)+
    boxTheme()+
    guides(linetype = "none")
}

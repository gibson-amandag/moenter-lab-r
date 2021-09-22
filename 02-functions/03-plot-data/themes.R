textTheme <- function(size = 11){
  theme = theme(
    text = element_text(size = size, family = "Arial", color = "black"),
    axis.text = element_text(size = size, family = "Arial", color = "black"), # for some reason, not acquiring consistently
    strip.text = element_text(face = "bold", size = size),
    axis.title = element_text(face = "bold")
  )
  return(theme)  
}

boxTheme <- function(axisSize = 0.5){
  theme = theme(
    panel.border = element_blank(),
    axis.line = element_line(color = "black", size = axisSize),
    axis.ticks = element_line(color = "black"),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.background = element_blank(), # testing, unsure if allowed
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank()
  )
}

whiteBlackFill <- function(
  scaleName = "trt"
){
  fill <- scale_fill_manual(scaleName, values = c("white", "black"))
}

earlyLifeFill <- function(
  STD = "STD",
  LBN = "LBN"
){
  fill <- scale_fill_manual("early life trt", values = c(STD="white", LBN="black"))
  return(fill)
}

comboTrtFillShape <- function(){
  layers <- list(
    scale_color_manual("treatment", values = c("black", "darkcyan", "black", "darkcyan")),
    scale_fill_manual("treatment", values = c("white", "lightblue1", "black", "darkcyan")),
    scale_shape_manual("treatment", values = c(21, 23, 21, 23))
  )
  return(layers)
}
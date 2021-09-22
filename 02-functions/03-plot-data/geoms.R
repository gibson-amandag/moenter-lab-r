#' Add a jitter geom to a ggplot
#' 
#' Defaults to a circle with fill and outline (shape 21). Provides defaults for
#' size, alpha (fill density), jitter width, and jitter height
#'
#' @param size a number. Size of the shape. Default is 1.5
#' @param alpha a number. density of fill. Default is 1
#' @param width a number. jitter width - spread of the data horizontally. Default is 0.35
#' @param height a number. jitter height - spread of the data vertically. Default is 0
#'
#' @return a geom_jitter with selected values
#' @export
#'
#' @examples
jitterGeom <- function(
  size = 1.5,
  alpha = 1,
  width = 0.35,
  height = 0
){
  geom_jitter(
    shape = 21,
    size = size,
    alpha = alpha,
    width = width,
    height = height
  ) 
}

addMeanHorizontalBar <- function(
  width = 0.7,
  size = 0.4,
  addLineType = FALSE,
  lineTypeName = "early life trt",
  lineTypeGuide = c("STD" = "dotted", "LBN" = "solid"),
  typeVar = earlyLifeTrt,
  ... # Into aes
){
  if(!addLineType){
    geom <- stat_summary(
        geom = "errorbar", 
        fun.min = mean, 
        fun = mean, 
        fun.max = mean, 
        width = width,
        size = size,
        aes(...)
      )
  } else{
    geom <- list(
      stat_summary(
        geom = "errorbar", 
        fun.min = mean, 
        fun = mean, 
        fun.max = mean, 
        width = width,
        size = size,
        aes(linetype = {{ typeVar }}, ...)
      ),
      scale_linetype_manual(lineTypeName, values = lineTypeGuide)
    )
  }
}

addMeanSE_vertBar <- function(
  size = 0.4,
  ... # into aes
){
  stat_summary(
    geom = "linerange", 
    fun.data = mean_se,
    size = size,
    aes(...),
    show.legend = FALSE
  )
}

addMedianHorizontalBar <- function(
  width = 0.7,
  size = 0.4,
  color = "red",
  alpha = 0.7
){
  stat_summary(
    geom = "errorbar", 
    fun.min = median, 
    fun = median, 
    fun.max = median, 
    width = width,
    size = size,
    color = color,
    alpha = alpha
  )
}
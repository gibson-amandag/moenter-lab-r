flexSave <- function(
  baseName,
  thisFilePrefix = filePrefix,
  plot = last_plot(),
  fileType = "pdf",
  filePath = plotOutputFolder,
  width = 7,
  height = 7,
  units = "in",
  compType = currentCompType,
  shinySettings = FALSE,
  ...
){
  if(shinySettings){
    thisFileName <- baseName
    thisPath <- NULL
  } else {
    thisFileName <- paste0(filePrefix, baseName, ".", fileType)
    thisPath <- filePath
  }
  
  if(fileType == "pdf" && compType == "mac"){
    ggsave(
      plot = plot,
      filename = thisFileName,
      device = fileType,
      path = thisPath,
      width = width,
      height = height,
      units = units,
      useDingbats = FALSE,
      ...
    )
  } else if(fileType == "pdf" && compType == "windows"){
    ggsave(
      plot = plot,
      filename = thisFileName,
      device = cairo_pdf,
      path = thisPath,
      width = width,
      height = height,
      units = units,
      ...
    )
  } else if(fileType == "png" && compType == "mac"){
    ggsave(
      plot = plot,
      filename = thisFileName,
      device = fileType,
      path = thisPath,
      width = width,
      height = height,
      units = units,
      ...
    )
  } else if(fileType == "png" && compType == "windows"){
    ggsave(
      plot = plot,
      filename = thisFileName,
      device = fileType,
      # type = "cairo", # may choose not to include this and just deal with crappy figure
      path = thisPath,
      width = width,
      height = height,
      units = units,
      ...
    )
  }
}
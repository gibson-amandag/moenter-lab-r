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
  ...
){
  if(fileType == "pdf" && compType == "mac"){
    ggsave(
      plot = plot,
      filename = paste0(thisFilePrefix, baseName, ".", fileType),
      device = fileType,
      path = filePath,
      width = width,
      height = height,
      units = units,
      useDingbats = FALSE,
      ...
    )
  } else if(fileType == "pdf" && compType == "windows"){
    ggsave(
      plot = plot,
      filename = paste0(filePrefix, baseName, ".", fileType),
      device = cairo_pdf,
      path = filePath,
      width = width,
      height = height,
      units = units,
      ...
    )
  } else if(fileType == "png" && compType == "mac"){
    ggsave(
      plot = plot,
      filename = paste0(filePrefix, baseName, ".", fileType),
      device = fileType,
      path = filePath,
      width = width,
      height = height,
      units = units,
      ...
    )
  } else if(fileType == "png" && compType == "windows"){
    ggsave(
      plot = plot,
      filename = paste0(filePrefix, baseName, ".", fileType),
      device = fileType,
      # type = "cairo", # may choose not to include this and just deal with crappy figure
      path = filePath,
      width = width,
      height = height,
      units = units,
      ...
    )
  }
}
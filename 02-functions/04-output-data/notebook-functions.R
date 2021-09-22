myDisplay <- function(df, docType = doc.type){
  if(!is.null(docType) && docType == "docx"){
    return(flextable(df))
  } else {
    df_print <- df
    return(df)
  }
}
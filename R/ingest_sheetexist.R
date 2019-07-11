#' Confirm that sheet exists
#'
#' @param filepath filepath to MMD Collection Tool
#' @param sheetname name of the sheet to check
#'
#' @export

ingest_sheetexist <- function(filepath, sheetname){

  sheets <- readxl::excel_sheets(filepath)

  sheetname %in% sheets
}

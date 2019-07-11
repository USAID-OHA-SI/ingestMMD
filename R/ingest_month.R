#' Identify Month
#'
#' @param filepath filepath to MMD Collection Tool
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #extract current month from tool
#'   path <- "~/MMDData/MMD National Data Collection Tool PEPFARlandia.xlsx"
#'   ingest_month(path) }

ingest_month <- function(filepath){


  if(ingest_sheetexist(filepath, '6. Monthly Reporting')) {

    m <- readxl::read_xlsx(filepath, sheet = "6. Monthly Reporting",
                           range = "B3", col_names = "month")
    if(length(m) == 0) {
      as.Date("2019-06-01")
    } else {
      m %>%
        dplyr::pull() %>%
        as.Date() #%>%
      #format("%B %Y")
    }

  } else {
    as.Date("2019-06-01")
  }


}

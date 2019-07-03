#' Identify Month
#'
#' @param path filepath to MMD Collection Tool
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #extract current month from tool
#'   path <- "~/MMDData/MMD National Data Collection Tool PEPFARlandia.xlsx"
#'   ingest_month(path) }

ingest_month <- function(path){

  m <- readxl::read_xlsx(path, sheet = "6. Monthly Reporting",
                    range = "B3", col_names = "month")
  if(length(m) == 0) {
    as.Date("2019-06-01")
  } else {
    m %>%
      dplyr::pull() %>%
      as.Date() #%>%
      #format("%B %Y")
  }


}

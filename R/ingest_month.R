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

  readxl::read_xlsx(path, sheet = "6. Monthly Reporting",
                    range = "B3", col_names = "month") %>%
    dplyr::pull() %>%
    as.Date() %>%
    format("%B %Y")
}

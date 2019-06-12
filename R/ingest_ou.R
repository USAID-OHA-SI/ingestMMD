#' Identify OU name
#'
#' @param path filepath to MMD Collection Tool
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #pull ou name from tool
#'   path <- "~/MMDData/MMD National Data Collection Tool PEPFARlandia.xlsx"
#'   ingest_ou(path) }

ingest_ou <- function(path){

    readxl::read_xlsx(path,
                      sheet = "2. Select Operating Unit",
                      n_max = 1) %>%
    dplyr::pull()
}


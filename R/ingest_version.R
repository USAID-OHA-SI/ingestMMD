#' Identify template version
#'
#' @param path filepath to MMD Collection Tool
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #extract tool version
#'   path <- "~/MMDData/MMD National Data Collection Tool PEPFARlandia.xlsx"
#'   ingest_version(path) }

ingest_version <- function(path){

  readxl::read_xlsx(path,
                    sheet = "1. Overview and Instructions",
                    n_max = 1) %>%
    dplyr::pull()
}

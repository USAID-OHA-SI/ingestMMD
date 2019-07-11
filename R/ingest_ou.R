#' Identify OU name
#'
#' @param filepath filepath to MMD Collection Tool
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #pull ou name from tool
#'   filepath <- "~/MMDData/MMD National Data Collection Tool PEPFARlandia.xlsx"
#'   ingest_ou(filepath) }

ingest_ou <- function(filepath){

  if(ingest_sheetexist(filepath, "2. Select Operating Unit")){
    readxl::read_xlsx(filepath,
                      sheet = "2. Select Operating Unit",
                      n_max = 1) %>%
      dplyr::pull()
  } else {
    print("Not Specified")
  }

}


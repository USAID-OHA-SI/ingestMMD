#' Import Regimes Tab
#'
#' @param filepath filepath to MMD Collection Tool
#'
#' @export

ingest_regimens <- function(filepath){

  month <- ingest_month(filepath)

  df <- readxl::read_xlsx(filepath, skip = 2, sheet = "4. Current ART Regimens")

  df <- df %>%
    dplyr::filter(!is.na(regimen_type)) %>%
    dplyr::mutate(month = month,
                  category = "Regimen",
                  regimen_mmd_enrolled =
                    ifelse(is.na(regimen_mmd_enrolled) & regimen_mmd_enrolled_share > 0, "Yes", regimen_mmd_enrolled)) %>%
    dplyr::select(month, dplyr::everything())

  return(df)
}

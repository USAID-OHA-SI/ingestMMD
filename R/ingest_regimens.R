#' Import Regimes Tab
#'
#' @param filepath filepath to MMD Collection Tool
#'
#' @export

ingest_regimens <- function(filepath){

  month <- ingest_month(filepath)

  if(ingest_sheetexist(filepath, "4. Current ART Regimens")){

    df <- readxl::read_xlsx(filepath, skip = 2, sheet = "4. Current ART Regimens", col_types = "text")

    df <- dplyr::mutate_at(df, dplyr::vars(dplyr::matches("count|share")), as.numeric)

    df <- df %>%
      dplyr::filter(!is.na(regimen_type)) %>%
      dplyr::mutate(month = month,
                    category = "Regimen",
                    regimen_mmd_enrolled =
                      ifelse(is.na(regimen_mmd_enrolled) & regimen_mmd_enrolled_share > 0, "Yes", regimen_mmd_enrolled),
                    regimen_mmd_eligible_count = regimen_mmd_eligible_share * regimen_count,
                    regimen_mmd_enrolled_count = regimen_mmd_enrolled_share * regimen_mmd_eligible_count) %>%
      dplyr::select(month, dplyr::everything())

    return(df)

  } else {

    return(NULL)

  }
}

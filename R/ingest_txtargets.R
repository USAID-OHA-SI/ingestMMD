#' Import TX_CURR Targets
#'
#' @param filepath filepath to MMD Collection Tool
#'
#' @export

ingest_txtargets <- function(filepath){

  ou <- ingest_ou(filepath)

  df <- readxl::read_xlsx(filepath, skip = 2, sheet = "TX_CURR Targets")

  df <- df %>%
    tidyr::gather(pd, tx_curr_target, -operatingunit) %>%
    dplyr::filter(operatingunit == ou) %>%
    dplyr::mutate(month = paste0(stringr::str_sub(pd, 3), "-09-01") %>%
                    lubridate::as_date(),
                  category = "Tx Target") %>%
    dplyr::select(-operatingunit, -pd)

  return(df)
}

#' Import all data from the MMD Tool
#'
#' @param filepath filepath to MMD Collection Tool
#'
#' @export

ingest_data <- function(filepath){

  ou <- ingest_ou(filepath)

  print(paste("Ingest:", ou))

  df_reg <- ingest_regimens(filepath)

  df_plan <- ingest_plan(filepath)

  df_tx <- ingest_txtargets(filepath)

  df_combo <- dplyr::bind_rows(df_reg, df_plan, df_tx)

  df_combo <- df_combo %>%
    dplyr::mutate(operatingunit = ou) %>%
    dplyr::select(operatingunit, category, month, dplyr::everything()) %>%
    dplyr::select(-regimen_notes, -mmdtarget_notes, dplyr::everything())

  invisible(df_combo)

}



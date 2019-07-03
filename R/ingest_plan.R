#' import MMD Plan tab
#'
#' @param filepath filepath to MMD Collection Tool
#'
#' @export

ingest_plan <- function(filepath){

  df <- readxl::read_xlsx(filepath, skip = 3, sheet = "5. Transition Plan")

  df <- df %>%
    tidyr::gather(ind, value, -month, -mmdtarget_notes) %>%
    dplyr::mutate(month = lubridate::as_date(month),
                  type = ifelse(stringr::str_detect(ind, "_pct"), "mmdtarget_percent", "mmdtarget_target"),
                  ind = stringr::str_remove(ind, "_pct")) %>%
    tidyr::spread(type, value) %>%
    tidyr::separate(ind, c(NA, NA, "mmdtarget_duration"), sep = "\\.") %>%
    dplyr::filter(mmdtarget_duration != "total") %>%
    dplyr::mutate(mmdtarget_percent = ifelse(is.na(mmdtarget_duration), NA, mmdtarget_percent),
                  mmdtarget_duration = dplyr::recode(mmdtarget_duration,
                                                     mo1 = "1 month",
                                                     mo2 = "2 months",
                                                     mo3 = "3 months",
                                                     mo45 = "4-5 months",
                                                     mo6 = "6 months or more"),
                  category = "Plan")

  return(df)

}

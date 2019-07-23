library(devtools)
library(usethis)

files <- list.files("C:/Users/achafetz/Downloads/June 2019",
                    "MMD",
                    full.names = TRUE)

df <- purrr::map_dfr(files, ingest_data)


readr::write_csv(df, paste0("C:/Users/achafetz/Downloads/MMD_National_Data_June_",format(Sys.Date(),"%Y%m%d"),".csv"), na = "")


# REVIEW ------------------------------------------------------------------

#how many OUs submit templates?
  submit <- df %>%
    dplyr::distinct(operatingunit) %>%
    dplyr::mutate(submit = "X")

#how many OUs reported on eligible
eligible <- df %>%
    dplyr::filter(regimen_mmd_eligible_share > 0) %>%
    dplyr::count(operatingunit, wt = regimen_mmd_eligible_share) %>%
    dplyr::select(-n) %>%
    dplyr::mutate(eligible = "X")

#how many OUs reported a transition plan?
  plan <- df %>%
    dplyr::filter(category == "Plan",
                  month > "2019-07-01",
                  !is.na(mmdtarget_target)) %>%
    dplyr::distinct(operatingunit) %>%
    dplyr::mutate(plan = "X")

  submit %>%
    dplyr::left_join(eligible, by = "operatingunit") %>%
    dplyr::left_join(plan, by  = "operatingunit") %>%
    dplyr::arrange(operatingunit)

#what share of eligible patients are enrolled on MMD?
  df %>%
    summarize_at(vars(regimen_mmd_eligible_count, regimen_mmd_enrolled_count), sum, na.rm = TRUE) %>%
    mutate(pct = regimen_mmd_enrolled_count/regimen_mmd_eligible_count)

  df %>%
    dplyr::filter(category == "Plan",
                  month > "2019-07-01",
                  !is.na(mmdtarget_target)) %>%
    dplyr::count(operatingunit, month, wt = mmdtarget_target) %>%
    tidyr::spread(operatingunit, n)

  df %>%
    dplyr::filter(category == "Tx Target") %>%
    dplyr::count(operatingunit, wt = tx_curr_target)

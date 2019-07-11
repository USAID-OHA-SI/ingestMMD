library(tidyverse)
library(ggplot2)
library(scales)
library(extrafont)


df_mer <- list.files("~/ICPI/Data", full.names = TRUE)[1] %>% ICPIutilities::read_msd(remove_txt = TRUE)

#how many OUs reported on eligible
ous <- df %>%
  filter(regimen_mmd_eligible_share > 0) %>%
  count(operatingunit, wt = regimen_mmd_eligible_share) %>%
  pull(operatingunit)

df_tx <- df_mer %>%
  filter(operatingunit %in% ous,
         indicator == "TX_CURR",
         standardizeddisaggregate == "Total Numerator",
         fiscal_year == 2019) %>%
  group_by(operatingunit) %>%
  summarise(tx_curr_cum = sum(cumulative, na.rm = TRUE)) %>%
  ungroup() %>%
  rename()

df %>%
  group_by(operatingunit) %>%
  summarise_at(vars(regimen_count, regimen_mmd_eligible_count, regimen_mmd_enrolled_count),
               sum, na.rm = TRUE) %>%
  ungroup() %>%
  filter(regimen_mmd_eligible_count > 0) %>%
  left_join(df_tx, ., by = "operatingunit") %>% 1
  mutate(tx_curr_adj = ifelse(regimen_count > tx_curr_cum, regimen_count, tx_curr_cum)) %>%
  summarise_at(vars(tx_curr_cum, regimen_count, regimen_mmd_eligible_count, regimen_mmd_enrolled_count),
               sum, na.rm = TRUE) %>%
  gather(ind, val)


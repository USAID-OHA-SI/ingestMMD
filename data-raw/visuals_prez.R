library(tidyverse)
library(ggplot2)
library(scales)
library(extrafont)

df  <- read_csv("C:/Users/achafetz/Downloads/MMD_National_Data_June_20190711.csv")
df_mer <- read_rds("~/ICPI/Data/MER_Structured_Datasets_OU_IM_FY17-19_20190621_v2_1.rds")


#colors
 dblue   <- "#4a66ac"
 dblue50 <- "#90a2cf"
 lblue   <- "#629dd1"
 lblue50 <- "#a0c4e3"
 teal    <- "#5aa2ae"
 teal50  <- "#9bc7ce"

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
  left_join(df_tx, ., by = "operatingunit") %>%
  mutate(tx_curr_adj = ifelse(regimen_count > tx_curr_cum, regimen_count, tx_curr_cum)) %>%
  summarise_at(vars(tx_curr_cum, regimen_count, regimen_mmd_eligible_count, regimen_mmd_enrolled_count),
               sum, na.rm = TRUE) %>%
  gather(ind, val)

df_ou_breakdown <- df %>%
  group_by(operatingunit) %>%
  summarise_at(vars(regimen_count, regimen_mmd_eligible_count, regimen_mmd_enrolled_count),
               sum, na.rm = TRUE) %>%
  ungroup() %>%
  filter(regimen_mmd_eligible_count > 0) %>%
  mutate(Enrolled = regimen_mmd_enrolled_count,
         `Eligible, Not Enrolled` = regimen_mmd_eligible_count - regimen_mmd_enrolled_count,
         `Not Eligible` = regimen_count - regimen_mmd_eligible_count) %>%
  select(-ends_with("count")) %>%
  gather(type, val, -operatingunit) %>%
  mutate(type = factor(type, c("Enrolled", "Eligible, Not Enrolled", "Not Eligible"))) %>%
  group_by(operatingunit) %>%
  mutate(share = val /sum(val)) %>%
  ungroup()

ou_order <- df_ou_breakdown %>%
  select(-val) %>%
  spread(type, share) %>%
  arrange(Enrolled) %>%
  pull(operatingunit)

df_ou_breakdown <- mutate(df_ou_breakdown, operatingunit = factor(operatingunit, ou_order))


df_ou_breakdown %>%
  ggplot(aes(operatingunit, share, fill = type)) +
  geom_col() +
  geom_text(aes(label = percent(share)), hjust = -.2,
            color = "#595959", family = "Gill Sans MT") +
  scale_y_continuous(limits = c(0, 1.1), breaks = c(0, .25, .5, .75, 1),
                     label = percent) +
  scale_fill_manual(values = c(dblue, teal, lblue)) +
  labs(x = "", y = "") +
  coord_flip() +
  facet_wrap(. ~ type) +
  theme_light() +
  theme(legend.position = "none",
        text = element_text(family  = "Gill Sans MT", size = 14),
        strip.text = element_text(size = 20),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank())

ggsave("C:/Users/achafetz/Downloads/MMD_enrollment.png", dpi = 300,
       units = c("in"), width = 8.6, height = 4)



filter_ous <- df %>%
  dplyr::filter(category == "Plan",
                month > "2019-07-01",
                !is.na(mmdtarget_target)) %>%
  dplyr::distinct(operatingunit) %>%
  dplyr::mutate(plan = "X")

mmd_distro <- df %>%
  filter(!is.na(mmdtarget_target),
         month == "2019-06-01") %>%
  # group_by(operatingunit, month, mmdtarget_duration) %>%
  # summarise_at(vars(mmdtarget_target), sum, na.rm = TRUE) %>%
  # ungroup() %>%
  mutate(mmd = mmdtarget_duration!= "1 month")

mmd_distro %>%
  filter(operatingunit == "Eswatini") %>%
  ggplot(aes(mmdtarget_duration, mmdtarget_target, fill = mmd)) +
  geom_col() +
  labs(y = "patients", x = "month of Rx") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c(lblue, dblue)) +
  #facet_wrap(. ~ operatingunit) +
  theme_light() +
  theme(legend.position = "none",
        text = element_text(family  = "Gill Sans MT", size = 14),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


ou_sel <- df %>%
  filter(category == "Plan",
                month > "2019-07-01",
                !is.na(mmdtarget_target)) %>%
  distinct(operatingunit) %>%
  pull()

plan <- df %>%
  filter(category == "Plan",
         operatingunit %in% ou_sel,
         ) %>%
  mutate(mmd = ifelse(mmdtarget_duration!= "1 month", "MMD", "1 month")) %>%
  group_by(operatingunit, month, mmd) %>%
  summarise_at(vars(mmdtarget_target), sum, na.rm = TRUE) %>%
  group_by(operatingunit, month) %>%
  mutate(mmd_share = mmdtarget_target/sum(mmdtarget_target)) %>%
  ungroup() %>%
  mutate(label = case_when(month == max(month) ~ operatingunit))
  # mutate(label = case_when(operatingunit == "Namibia" & month == min(month) ~ operatingunit,
  #                          operatingunit != "Namibia" & month == max(month) ~ operatingunit))


plan %>%
  filter(mmd == "MMD",
         is.finite(mmd_share)) %>%
  arrange(operatingunit, month) %>%
  ggplot(aes(month, mmd_share, group = operatingunit, color = operatingunit)) +
  geom_hline(yintercept = .8, linetype = "dotted", size = 1) +
  geom_point(size = 4) +
  geom_line(size = 1) +
  geom_text(aes(label = label), vjust = -1, na.rm = TRUE, size = 5) +
  labs(x = "", y = "MMD Share") +
  #expand_limits(y = 0) + #x = as.Date("2020-10-01")) +
  expand_limits(y = c(0, 1.05)) +
  scale_y_continuous(labels = percent) +
  scale_color_manual(values = c(lblue, teal, dblue, lblue50)) +
  #scale_x_continuous(labels = date_format(monht,format = "%m%Y")) +
  theme_light() +
  theme(legend.position = "none",
        text = element_text(family  = "Gill Sans MT", size = 14),
        axis.ticks = element_blank())

ggsave("C:/Users/achafetz/Downloads/MMD_trend.png", dpi = 300,
       units = c("in"), width = 8.6, height = 4)

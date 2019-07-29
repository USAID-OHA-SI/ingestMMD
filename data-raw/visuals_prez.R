library(tidyverse)
library(ggplot2)
library(scales)
library(extrafont)
library(gridExtra)

df  <- read_csv("out/MMD_National_Data_June_20190723.csv")

df_mer <- read_rds("~/ICPI/Data/MER_Structured_Datasets_OU_IM_FY17-19_20190621_v2_2.rds")


#colors
 dblue   <- "#4a66ac"
 dblue50 <- "#90a2cf"
 lblue   <- "#629dd1"
 lblue50 <- "#a0c4e3"
 teal    <- "#5aa2ae"
 teal50  <- "#9bc7ce"
 bgray   <- "#7f8fa9"

#rename DRC
 df <- mutate(df, operatingunit = ifelse(operatingunit == "Democratic Republic of the Congo", "DRC", operatingunit))
 df_mer <- mutate(df_mer, operatingunit = ifelse(operatingunit == "Democratic Republic of the Congo", "DRC", operatingunit))

#how many OUs reported on eligible
ous <- df %>%
  filter(regimen_mmd_eligible_share > 0) %>%
  count(operatingunit, wt = regimen_mmd_eligible_share) %>%
  pull(operatingunit)

df_tx <- df_mer %>%
  filter(operatingunit %in% ous,
         indicator %in%  c("TX_CURR", "TX_PVLS"),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
         fiscal_year >= 2018) %>%
  mutate(indicator = ifelse(numeratordenom == "D", paste0(indicator, "_D"), indicator)) %>%
  group_by(operatingunit, indicator, fiscal_year) %>%
  summarise(cumulative = sum(cumulative, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(indicator, cumulative) %>%
  group_by(operatingunit) %>%
  mutate(`VL Coverage` = TX_PVLS_D / lag(TX_CURR),
         `VL Suppression` = TX_PVLS / TX_PVLS_D) %>%
  filter(fiscal_year == 2019) %>%
  select(-fiscal_year)

df_comp <- df %>%
  group_by(operatingunit) %>%
  summarise_at(vars(regimen_count, regimen_mmd_eligible_count, regimen_mmd_enrolled_count),
               sum, na.rm = TRUE) %>%
  ungroup() %>%
  filter(regimen_mmd_eligible_count > 0) %>%
  left_join(df_tx, ., by = "operatingunit") %>%
  #mutate(tx_curr_adj = ifelse(regimen_count > tx_curr_cum, regimen_count, tx_curr_cum)) %>%
  summarise_at(vars(TX_CURR, regimen_count, regimen_mmd_eligible_count, regimen_mmd_enrolled_count),
               sum, na.rm = TRUE) %>%
  ungroup() %>%
  rename(`TX_CURR [MER]` = TX_CURR,
         `Tx regimens reported June` = regimen_count,
         `MMD Eligible` = regimen_mmd_eligible_count,
         `MMD Enrolled` = regimen_mmd_enrolled_count) %>%
  gather(ind, val, -operatingunit) %>%
  mutate(ind = factor(ind, c("TX_CURR [MER]", "Tx regimens reported June",
                             "MMD Eligible", "MMD Enrolled")))

ou_order <- df_comp %>%
  spread(ind, val) %>%
  mutate(mmd_share = `MMD Enrolled`/`TX_CURR [MER]`) %>%
  arrange(mmd_share) %>%
  pull(operatingunit)

df_comp %>%
  mutate(operatingunit = factor(operatingunit, ou_order),
         val = val/1000) %>%
  ggplot(aes(ind, val, fill = ind)) +
  geom_col(position = position_dodge()) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c(bgray, lblue, teal, dblue)) +
  labs(y = "patients (thousands)", x = "") +
  #coord_flip() +
  facet_wrap(. ~ operatingunit, scale = "free") +
  theme_light() +
  theme(legend.position = "top",
        legend.text = element_text(size = 11, color = "#7f7f7f"),
        legend.title = element_blank(),
        text = element_text(family  = "Gill Sans MT", size = 12),
        #strip.text = element_text(size = 20),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank())

ggsave("out/MMD_cascade.png", dpi = 300,
       units = c("in"), width = 8.6, height = 4)

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

ggsave("out/MMD_enrollment.png", dpi = 300,
       units = c("in"), width = 8.6, height = 4)



mmd_distro <- df %>%
  mutate(mmdtarget_target = ifelse(mmdtarget_duration == "1 month" & operatingunit == "Ghana", 0, mmdtarget_target)) %>%
  filter(!is.na(mmdtarget_target),
         month == "2019-06-01") %>%
  mutate(mmd = mmdtarget_duration!= "1 month")

ou_order <- mmd_distro %>%
  filter(mmdtarget_duration == "1 month") %>%
  select(operatingunit, mmdtarget_percent) %>%
  arrange(desc(mmdtarget_percent)) %>%
  distinct(operatingunit) %>%
  pull(operatingunit)


mmd_distro %>%
  mutate(mmdtarget_duration = str_remove(mmdtarget_duration, " mo.*"),
         mmdtarget_duration = str_replace(mmdtarget_duration, "6", "6+"),
         operatingunit = factor(operatingunit, ou_order),
         mmdtarget_target = mmdtarget_target/1000) %>%
  ggplot(aes(mmdtarget_duration, mmdtarget_target, fill = mmd)) +
  geom_col() +
  labs(y = "patients (thousands)", x = "month of Rx dispensed") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c(lblue, dblue)) +
  facet_wrap(. ~ operatingunit, scales = "free_y") +
  theme_light() +
  theme(legend.position = "none",
        axis.title = element_text(color = "#595959"),
        text = element_text(family  = "Gill Sans MT", size = 13),
        axis.text.x = element_text(color = "#595959"),
        strip.text = element_text(size = 14),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave("out/MMD_distro.png", dpi = 300,
       units = c("in"), width = 8.6, height = 4)


ou_sel <- df %>%
  filter(category == "Plan",
                month > "2019-07-01",
                !is.na(mmdtarget_target)) %>%
  distinct(operatingunit) %>%
  pull()

plan <- df %>%
  filter(category == "Plan",
         operatingunit %in% ou_sel,
         !is.na(mmdtarget_target)) %>%
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
  labs(x = "", y = "share of patients on MMD") +
  #expand_limits(y = 0) + #x = as.Date("2020-10-01")) +
  expand_limits(y = 1.05) +
  scale_y_continuous(labels = percent_format(1)) +
  scale_color_manual(values = c(lblue, teal, lblue50, bgray, dblue, dblue50)) +
  #scale_x_continuous(labels = date_format(monht,format = "%m%Y")) +
  theme_light() +
  theme(legend.position = "none",
        axis.title.y = element_text(color = "#595959", size = 13),
        text = element_text(family  = "Gill Sans MT", size = 14),
        axis.ticks = element_blank())

ggsave("out/MMD_trend.png", dpi = 300,
       units = c("in"), width = 8.6, height = 4)



# df_vl <- mmd_distro %>%
#   filter(!(operatingunit == "Kenya" & is.na(mmdtarget_notes))) %>% #remove kenya peds
#   group_by(operatingunit, mmd) %>%
#   summarise_at(vars(mmdtarget_target, mmdtarget_percent), sum, na.rm = TRUE) %>%
#   ungroup() %>%
#   filter(mmd == TRUE) %>%
#   left_join(df_tx, by = "operatingunit") %>%
#   filter(!is.na(TX_CURR)) %>%
#   select(operatingunit, `MMD Share` = mmdtarget_percent, `VL Coverage`, `VL Suppression`) %>%
#   gather(ind, val, -operatingunit) %>%
#   mutate(operatingunit = factor(operatingunit, ou_order),
#          lab_type = case_when(operatingunit == "Ghana" ~ ind))
#
# df_vl %>%
#   ggplot(aes(val, operatingunit, color = ind)) +
#   geom_point(size = 14,
#              #ifelse(df_vl$ind == "MMD_Share", 16, 14),
#              na.rm = TRUE) +
#   geom_text(aes(label = lab_type), vjust = -2.3,
#             family = "Gill Sans MT", na.rm = TRUE) +
#   geom_text(aes(label = percent(val, 1)), color = ifelse(df_vl$ind == "MMD Share", "black", "white"),
#             family = "Gill Sans MT",
#             check_overlap = TRUE, na.rm = TRUE) +
#   expand_limits(x = c(0, 1.05), y = 9) +
#   scale_x_continuous(labels = percent) +
#   scale_color_manual(values = c(teal, lblue, dblue)) +
#   labs(x = "", y = "") +
#   theme_light() +
#   theme(legend.position = "none",
#         text = element_text(family  = "Gill Sans MT", size = 14),
#         panel.border = element_blank(),
#         # panel.grid.major.x = element_blank(),
#         # panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_line(color = "gray", size = 1),
#         axis.text.x = element_blank(),
#         axis.ticks.y = element_blank())
#
# ggsave("out/MMD_vl.png", dpi = 300,
#        units = c("in"), width = 8.6, height = 4)
#
# df_vl2 <- mmd_distro %>%
#   filter(!(operatingunit == "Kenya" & is.na(mmdtarget_notes))) %>% #remove kenya peds
#   group_by(operatingunit, mmd) %>%
#   summarise_at(vars(mmdtarget_target, mmdtarget_percent), sum, na.rm = TRUE) %>%
#   ungroup() %>%
#   filter(mmd == TRUE) %>%
#   left_join(df_tx, by = "operatingunit") %>%
#   filter(!is.na(TX_CURR)) %>%
#   select(operatingunit, `MMD Share` = mmdtarget_percent, `VL Coverage`, `VL Suppression`) %>%
#   mutate(`VL Suppression` = `VL Coverage` * `VL Suppression`
#          #`MMD Share` = `VL Suppression` * `MMD Share`
#          ) %>%
#   arrange(`VL Coverage`) %>%
#   mutate(operatingunit = as_factor(operatingunit)) %>%
#   gather(ind, val, -operatingunit) %>%
#   mutate(lab_type = case_when(operatingunit == "Kenya" ~ ind))
#
# df_vl2 %>%
#   ggplot(aes(val, operatingunit, color = ind)) +
#   geom_point(size = 14,
#              #ifelse(df_vl$ind == "MMD_Share", 16, 14),
#              na.rm = TRUE) +
#   geom_text(aes(label = lab_type), vjust = -2.3,
#             hjust = ifelse(df_vl2$lab_type == "VL Coverage", .1, .5),
#             family = "Gill Sans MT", na.rm = TRUE) +
#   geom_text(aes(label = percent(val, 1)), color = ifelse(df_vl$ind == "MMD Share", "black", "white"),
#             family = "Gill Sans MT",
#             check_overlap = TRUE, na.rm = TRUE) +
#   expand_limits(x = c(0, 1), y = 9) +
#   scale_x_continuous(labels = percent) +
#   scale_color_manual(values = c(teal50, lblue, dblue)) +
#   labs(x = "", y = "") +
#   theme_light() +
#   theme(legend.position = "none",
#         text = element_text(family  = "Gill Sans MT", size = 14),
#         panel.border = element_blank(),
#         # panel.grid.major.x = element_blank(),
#         # panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_line(color = "gray", size = 1),
#         axis.text.x = element_blank(),
#         axis.ticks.y = element_blank())
#
# ggsave("out/MMD_vl2.png", dpi = 300,
#        units = c("in"), width = 8.6, height = 4)

df_vl2 <- mmd_distro %>%
  filter(!(operatingunit == "Kenya" & is.na(mmdtarget_notes))) %>% #remove kenya peds
  group_by(operatingunit, mmd) %>%
  summarise_at(vars(mmdtarget_target, mmdtarget_percent), sum, na.rm = TRUE) %>%
  ungroup() %>%
  filter(mmd == TRUE) %>%
  left_join(df_tx, by = "operatingunit") %>%
  filter(!is.na(TX_CURR)) %>%
  select(operatingunit, `MMD Share` = mmdtarget_percent, `VL Coverage`, `Documented VL Suppression` = `VL Suppression`) %>%
  mutate(`Documented VL Suppression` = `VL Coverage` * `Documented VL Suppression`) %>%
  arrange(`VL Coverage`) %>%
  mutate(operatingunit = as_factor(operatingunit)) %>%
  gather(ind, val, -operatingunit) %>%
  mutate(lab_type = case_when(operatingunit == "Kenya" ~ ind))

df_vl3  <- filter(df_vl2, ind != "MMD Share")
v1 <- df_vl3 %>%
  ggplot(aes(val, operatingunit, color = ind)) +
  geom_point(size = 11,
             #ifelse(df_vl$ind == "MMD_Share", 16, 14),
             na.rm = TRUE) +
  geom_text(aes(label = lab_type), vjust = -2.3,
            hjust = ifelse(df_vl3$lab_type == "VL Coverage", .1, 1),
            family = "Gill Sans MT", na.rm = TRUE) +
  geom_text(aes(label = percent(val, 1)), hjust = ifelse(df_vl3$ind == "Documented VL Suppression", 1.8, .5),
            color = ifelse(df_vl3$ind == "Documented VL Suppression", dblue, "white"),
            family = "Gill Sans MT",
            check_overlap = TRUE, na.rm = TRUE) +
  expand_limits(x = c(0, 1.05), y = 13.5) +
  scale_x_continuous(labels = percent) +
  scale_color_manual(values = c(lblue, dblue)) +
  labs(x = "", y = "") +
  theme_light() +
  theme(legend.position = "none",
        text = element_text(family  = "Gill Sans MT", size = 14),
        panel.border = element_blank(),
        # panel.grid.major.x = element_blank(),
        # panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 1),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())


df_vl4  <-filter(df_vl2, ind == "MMD Share")
v2 <- df_vl4 %>%
  ggplot(aes(val, operatingunit, color = ind)) +
  geom_point(size = 11,
             #ifelse(df_vl$ind == "MMD_Share", 16, 14),
             na.rm = TRUE) +
  geom_text(aes(label = lab_type), vjust = -2.3,
            hjust = ifelse(df_vl4$lab_type == "VL Coverage", .1, .5),
            family = "Gill Sans MT", na.rm = TRUE) +
  geom_text(aes(label = percent(val, 1)), color = "black",
            family = "Gill Sans MT",
            check_overlap = TRUE, na.rm = TRUE) +
  expand_limits(x = c(0, 1.05), y = 13.5) +
  scale_x_continuous(labels = percent) +
  scale_color_manual(values = c(teal50)) +
  labs(x = "", y = "") +
  theme_light() +
  theme(legend.position = "none",
        text = element_text(family  = "Gill Sans MT", size = 14),
        panel.border = element_blank(),
        # panel.grid.major.x = element_blank(),
        # panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 1),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_text(color = "white"))

# ggsave("C:/Users/achafetz/Downloads/MMD_vl4.png", dpi = 300,
#        units = c("in"), width = 4.3, height = 4)


grid.arrange(v1, v2, ncol = 2) %>%
  ggsave("out/MMD_vl6.png", ., dpi = 300,
         units = c("in"), width = 8.6, height = 4)

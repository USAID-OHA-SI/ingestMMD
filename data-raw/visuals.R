library(tidyverse)
library(ggplot2)
library(scales)
library(extrafont)

df %>%
  glimpse()

df_reg <- df %>%
  filter(category == "Regimen") %>%
  mutate(regimen_type = ifelse(str_length(regimen_type) < 20,
                               regimen_type, paste0(str_sub(regimen_type, 1, 20), "...")))

viz_reg <- df_reg %>%
  filter(operatingunit == "Eswatini") %>%
  ggplot(aes(reorder(regimen_type, regimen_count), regimen_count)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(label = comma) +
  labs(x = "", y = "enrolled on regimen") +
  #facet_wrap(. ~ operatingunit, scales = "free") +
  theme_light() +
  theme(axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank())


viz_elig <- df_reg %>%
  filter(operatingunit == "Eswatini") %>%
  ggplot(aes(regimen_mmd_eligible_share, reorder(regimen_type, regimen_count))) +
  geom_segment(aes(x = 0, y = regimen_type, xend = regimen_mmd_eligible_share, yend = regimen_type), size = 1.5) +
  geom_point(size = 6) +
  geom_text(aes(label = percent(regimen_mmd_eligible_share, accuracy = 1)), hjust = -.5) +
  scale_x_continuous(limits = c(0, 1.05), label = percent) +
  labs(y = "", x = "% eligible for MMD") +
  #facet_wrap(. ~ operatingunit, scales = "free") +
  theme_light() +
  theme(axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank())

viz_enroll <- df_reg %>%
  filter(operatingunit == "Eswatini") %>%
  ggplot(aes(regimen_mmd_enrolled_share, reorder(regimen_type, regimen_count))) +
  geom_segment(aes(x = 0, y = regimen_type, xend = regimen_mmd_enrolled_share, yend = regimen_type), size = 1.5) +
  geom_point(size = 6) +
  geom_text(aes(label = percent(regimen_mmd_enrolled_share, accuracy = 1)), hjust = -.5) +
  scale_x_continuous(limits = c(0, 1.05), label = percent) +
  labs(y = "", x = "% enrolled on MMD") +
  #facet_wrap(. ~ operatingunit, scales = "free") +
  theme_light() +
  theme(axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank())


df_plan <- df %>%
  filter(category == "Plan")

df_plan %>%
  filter(operatingunit == "Eswatini") %>%
  mutate(lab = case_when(month == max(month, na.rm = TRUE) ~ mmdtarget_duration)) %>%
  ggplot(aes(month, mmdtarget_target, group = mmdtarget_duration)) +
  geom_point(size = 2) +
  geom_line(na.rm = TRUE) +
  geom_text(aes(label = lab)) +
  scale_y_continuous(label = comma) +
  labs(y = "", y = "# enrolled on MMD") +
  theme_light() +
  theme(axis.ticks = element_blank(),
        panel.border = element_blank())

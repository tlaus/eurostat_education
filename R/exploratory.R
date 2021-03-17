## Exploratory analysis of Eurostat data on education
## Tereza Lausová
## 30.11.2020

## loading libraries
library(tidyverse)
library(here)
library(readxl)

## read in one table
table_22 <- readxl::read_xls(here("source/22_Distribution_of_pupils_percentages_general_vs_vocational_different_levels_regions.xls"), 
                             skip = 8, n_max = 464, na = ":")
View(table_22)

countries <- c("Belgium", "Bulgaria", "Czechia", "Denmark",
               "Germany (until 1990 former territory of the FRG)", "Estonia", "Ireland", "Greece",
               "Spain", "France", "Croatia", "Italy",
               "Cyprus", "Luxembourg", "Hungary", "Malta",
               "Netherlands", "Austria", "Poland", "Portugal",
               "Romania", "Slovenia", "Slovakia", "Finland",
               "Sweden", "United Kingdom", "Iceland", "Liechtenstein",
               "Norway", "Switzerland", "Montenegro", "North Macedonia",
               "Serbia", "Turkey")

europe_parts <- factor(x = c("western", "eastern", "eastern", "northern",
                             "western", "eastern", "western", "southern",
                             "western", "western", "southern", "southern",
                             "southern", "western", "eastern", "southern",
                             "western", "western", "eastern", "western",
                             "eastern", "southern", "eastern", "northern",
                             "northern", "western", "northern", "western",
                             "northern", "western", "western", "southern",
                             "southern", "southern"),
                       levels = c("western", "eastern", "southern", "northern"))

table_22_clean <- table_22 %>%
  rename("country" = `GEO/TIME`) %>%
  filter(country %in% countries) %>%
  mutate("country" = if_else(country == "Germany (until 1990 former territory of the FRG)", "Germany", country)) %>%
  distinct(country, .keep_all = TRUE) %>%
  mutate("europe_part" = europe_parts) %>%
  pivot_longer(cols = c(`2013`, `2014`, `2015`, `2016`, `2017`, `2018`), names_to = "year", values_to = "percent") #%>%
  # mutate("year" = as.numeric(year))

# ggplot(data = table_22_clean %>% filter(year == 2018)) +
#   geom_bar(aes(x = percent, y = country), stat = "identity", position = "dodge")

ggplot(data = table_22_clean) +
  theme_minimal() +
  geom_boxplot(aes(y = percent, x = year, fill = europe_part)) +
  # geom_text(data = table_22_clean %>% group_by(year) %>% mutate("min" = min(percent, na.rm = TRUE)) %>% ungroup %>% filter(percent == min), aes(x = year, y = percent, label = country), nudge_y = -0.35) +
  # geom_label(data = table_22_clean %>% filter(country == "Czechia"), aes(x = year, y = percent, label = country), nudge_y = -3, nudge_x = 0.2, color = "red") +
  # geom_point(data = table_22_clean %>% filter(country == "Czechia"), aes(x = year, y = percent, color = country), position = position_nudge(x = -0.1), fill = "red", color = "black", shape = 21, size = 2) +
  labs(title = "Distribution of pupils and students enrolled in general and vocational programmes by education level and NUTS2 regions") #+
  # guides(color=guide_legend(title = "Czechia", list(shape=21, color = "black", fill = "red")))


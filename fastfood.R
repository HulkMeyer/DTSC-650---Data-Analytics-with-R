suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))

Q1 <- fastfood %>%
  select(restaurant, item, calories) %>%
  filter(restaurant == 'Burger King' | restaurant == 'Chick Fil-A') %>%
  arrange(desc(calories)) %>%
  select(item) %>%
  head(1)

Q2 <- fastfood %>%
  select(restaurant, sugar) %>%
  filter(restaurant == 'Subway') %>%
  summarise(mean_sugar = round(mean(sugar), 2)) %>%
  as.data.frame()

Q3 <- fastfood %>%
  select(restaurant, calories) %>%
  filter(restaurant == 'Taco Bell') %>%
  summarise(mean_calories = round(mean(calories), 2)) %>%
  as.data.frame()

Q4 <- fastfood %>%
  mutate(fatXsugar = total_fat * sugar) %>%
  select(restaurant, item, fatXsugar) %>%
  arrange(desc(fatXsugar)) %>%
  head(3)

Q5 <- fastfood %>%
  group_by(restaurant) %>%
  summarise(avg_satfat = round(mean(sat_fat),2)) %>%
  filter(avg_satfat > 10) %>%
  nrow()











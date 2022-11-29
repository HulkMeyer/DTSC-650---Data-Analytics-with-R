suppressPackageStartupMessages(library(tidyverse))

Q1 <- msleep %>%
  filter(vore == 'carni' & conservation == 'lc' ) %>%
  summarise(var = round(var(sleep_total),2)) %>%
  as.data.frame()

Q2 <- msleep %>%
  filter(order == 'Rodentia') %>%
  mutate(ratio = round(sleep_total/sleep_rem, 2)) %>%
  arrange(desc(ratio)) %>%
  select(name) %>%
  head(1)

Q3 <- msleep %>%
  filter(order == 'Primates') %>%
  mutate(ratio = bodywt/brainwt) %>%
  filter(ratio >= 100) %>%
  count(ratio >= 100)

Q4 <- msleep %>%
  group_by(conservation) %>%
  mutate(mean_sleep = round(mean(sleep_total,na.rm = TRUE), 2)) %>%
  mutate(var_sleep = round(var(sleep_total,na.rm = TRUE), 2)) %>%
  distinct(conservation, mean_sleep, var_sleep) %>%
  arrange(conservation) %>%
  head(6) %>%
  as.data.frame()

Q5 <- msleep %>%
  filter(conservation == 'domesticated' & vore == 'herbi' & sleep_total >= 12) %>%
  select(name)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nycflights13))


Q1 <- flights %>%
  group_by(carrier) %>%
  filter(carrier == 'AA' | carrier == 'EV' | carrier == 'FL') %>%
  summarise(avg_dis = round(mean(distance),2)) %>%
  as.data.frame()

Q2 <- flights %>%
  group_by(month) %>%
  summarise(NumFlights = n()) %>%
  arrange(desc(NumFlights)) %>%
  head(1)

Q3 <- flights %>%
  mutate(min_dist = distance) %>%
  select(origin, dest, min_dist) %>%
  arrange(min_dist) %>%
  distinct(origin, dest, min_dist) %>%
  head(5)

Q4 <- flights %>%
  filter(origin == 'JFK') %>%
  group_by(month, day) %>%
  mutate(mean_distance = round(mean(distance,na.rm = TRUE),2)) %>%
  select(month, day, mean_distance) %>%
  distinct(month, day, mean_distance) %>%
  arrange(desc(mean_distance)) %>%
  head(5) %>%
  as.data.frame()

Q5 <- flights %>%
  group_by(carrier) %>%
  filter(dest == 'ATL' | dest == 'BOS') %>%
  group_by(dest) %>%
  summarise(max(arr_delay, na.rm = TRUE))
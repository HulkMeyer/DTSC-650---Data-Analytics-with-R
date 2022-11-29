suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(nycflights13))
suppressPackageStartupMessages(library(lm.beta))


#Problem 1
flights_df <- as.data.frame(flights)
flights_upper <- quantile(flights_df$dep_delay, .9985, na.rm = TRUE)
flights_lower <- quantile(flights_df$dep_delay, .0015, na.rm = TRUE)
flights_out <- which(flights_df$dep_delay > flights_upper | flights_df$dep_delay < flights_lower)
flights_df_no_out <- flights_df[-flights_out,]
Q1 <- round((nrow(flights_df) - length(flights_out))/ nrow(flights_df)*100,2)

#Problem 2
Q2 <- cor.test(flights_df_no_out$dep_delay, flights_df_no_out$distance)

#Problem 3
reg_flights <- lm(flights_df_no_out$dep_delay ~ flights_df_no_out$distance, data = flights_df_no_out)
Q3 <- summary(reg_flights)

#Problem 4
Q4 <- lm.beta(reg_flights)

#Problem 5
reg_flights2 <- lm(flights_df_no_out$dep_delay ~ flights_df_no_out$distance + flights_df_no_out$carrier, data = flights_df_no_out)
Q5 <- summary(reg_flights2)
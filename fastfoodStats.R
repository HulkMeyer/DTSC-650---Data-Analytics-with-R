suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(openintro))
suppressPackageStartupMessages(library(lm.beta))

#Problem 1
fastfood_sub1 <- fastfood %>%
  select(restaurant, calories, total_fat, sugar, calcium) %>%
  filter(restaurant == 'Sonic' | restaurant == 'Subway' | restaurant == 'Taco Bell') %>%
  na.omit() %>%
  select(calories, total_fat, sugar, calcium)
Q1 <- round(cor(fastfood_sub1),2)

#Problem 2
fastfood_sub2 <- fastfood %>%
  select(restaurant, calories, sodium, protein) %>%
  filter(restaurant == 'Mcdonalds' | restaurant == 'Subway') %>%
  mutate(restaurant = ifelse(restaurant == 'Subway',0,1)) %>%
  na.omit()
sub2_log_reg <- glm(restaurant ~ calories + sodium + protein, binomial(), fastfood_sub2)
Q2 <- round(sub2_log_reg$coefficients,2)

#Problem 3
fastfood_sub3 <- fastfood %>%
  select(restaurant, calories, protein) %>%
  filter(restaurant == 'Mcdonalds' | restaurant == 'Subway') %>%
  mutate(restaurant = ifelse(restaurant == 'Subway',0,1)) %>%
  na.omit()
sub3_log_reg <- glm(restaurant ~ calories + protein, binomial(), fastfood_sub3)
Q3 <- round(AIC(sub2_log_reg),2)
round(AIC(sub3_log_reg),2)

#Problem 4
fastfood_sub4 <- fastfood %>%
  select(calories, sat_fat, fiber, sugar) %>%
  na.omit()
sub4_multi_reg <- lm(calories ~ sat_fat + fiber + sugar, fastfood_sub4)
sub4_std_reg <- lm.beta(sub4_multi_reg)
Q4 <- round(sub4_multi_reg$coefficients[2],2)


#Problem 5
fastfood_sub5 <- fastfood %>%
  select(total_fat, cholesterol, total_carb, vit_a, restaurant) %>%
  group_by(restaurant) %>%
  filter(restaurant == 'Mcdonalds' | restaurant == 'Arbys' | restaurant == 'Sonic')

  # Multiple Regression   
fastfood_sub5_reg <- lm(total_fat ~ cholesterol + total_carb + restaurant, fastfood_sub5)

  # Standardized Regression
stdreg_fastfood_sub5_reg <- lm.beta(fastfood_sub5_reg)
  #Final Answer
Q5 <- round(stdreg_fastfood_sub5_reg$standardized.coefficients[2],2)






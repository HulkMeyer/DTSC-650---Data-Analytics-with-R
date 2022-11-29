suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lm.beta))
pizza <- read_csv('pizza.csv')

#Problem 1
pizza_sub1 <- pizza %>%
  select(temperature, bill, pizzas, got_wine)
#create correlation matrix
Q1 <- round(cor(pizza_sub1),2)

# Problem 2
pizza_sub2 <- pizza %>%
  filter(operator == 'Laura' & branch == 'East') %>%
  select(time, temperature, bill, pizzas)
Q2 <- round(cor(pizza_sub2),2)


# Problem 3
pizza_sub3 <- pizza %>%
  select(got_wine, temperature, bill, pizzas)
  # run regression 
pizza_sub3_reg <- glm(got_wine ~ temperature + bill + pizzas, binomial(), pizza_sub3)
sum_pizza_sub3_reg <- summary(pizza_sub3_reg)
  #assign coefficients
Q3 <- round(sum_pizza_sub3_reg$coefficients,2)


# Problem 4
pizza_sub4 <- pizza %>%
  select(bill, temperature, pizzas, got_wine)
  # multi regression
pizza_sub4_reg <- lm(bill ~ temperature + pizzas + got_wine, pizza_sub4)
  # standardize
pizza_sub4_stdreg <- lm.beta(pizza_sub4_reg)
Q4 <- pizza_sub4_stdreg


#Problem 5
#build data set
pizza_sub5 <- pizza %>%
  select(bill, temperature, pizzas, got_wine, operator)
# multi regression
pizza_sub5_reg <- lm(bill ~ temperature + pizzas + got_wine + operator, pizza_sub5)
sub5_AIC <- AIC(pizza_sub5_reg, k=2)
sub4_AIC <- AIC(pizza_sub4_reg, k=2)
Q5 <- sub4_AIC




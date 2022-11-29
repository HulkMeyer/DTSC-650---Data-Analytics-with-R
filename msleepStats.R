suppressPackageStartupMessages(library(lm.beta))
suppressPackageStartupMessages(library(tidyverse))

# Problem 1
Q1 <- cor.test(msleep$sleep_total, msleep$bodywt)

# Problem 2
sleep_sub <- msleep %>%
  select(sleep_total, sleep_rem, brainwt, bodywt) %>%
  drop_na()
Q2 <- round(cor(sleep_sub),2)

# Problem 3
Q3 <- reg_body.vore <- lm(bodywt ~ vore , msleep)

# Problem 4
reg_body.vore.rem <- lm(bodywt ~ vore + sleep_rem , msleep)
AIC(reg_body.vore, k=1)
Q4 <- AIC(reg_body.vore.rem, k=2)

# Problem 5
sleep_log_reg <- msleep %>%
  select(vore, sleep_total) %>%
  filter(vore != 'omni' & vore != 'insecti') %>%
  mutate(vorebin = ifelse(vore == 'carni',0,1)) %>%
  drop_na()
log_reg1 <- glm(vorebin ~ sleep_total, binomial(), sleep_log_reg)
Q5 <- log_reg1
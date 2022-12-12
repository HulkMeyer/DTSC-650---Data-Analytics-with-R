# Load libraries into system
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(carat))
suppressPackageStartupMessages(library(Hmisc))
suppressPackageStartupMessages(library(olsrr))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(lsr))


# Load data set 
data <- read.csv('BRFSS2015_650.csv')


#Taking a look at the data
  #head(data)
  #describe(data)
  #summary(data) 
    #Program was built in a Markdown file then transferred to Script

#Problem 1
Q1 <- sum(data$GENHLTH == 1, na.rm = TRUE)
  #PDF Page 13 states question and shows results as well as the scale where 1 is equated with excellent health  

#Problem 2
max.numwomen <- data %>%
  select(NUMWOMEN, CVDSTRK3) %>% #pulling the number of women in any given household and whether or not someone was told they had a stroke
  arrange(CVDSTRK3, desc(round(NUMWOMEN))) %>% # Put data in desc order and rounding them to the nearest whole number (some and decimals to the -79 power)
  filter(CVDSTRK3 == 1 & NUMWOMEN >= 0) %>% # Filtering out those who had not had strokes and women in household less than 0
  summarise(max_numwomen = max(NUMWOMEN)) # finding the max number of women in a household where someone had cancer 
Q2 <- max.numwomen # value is max_numwomen in the max.numwomen dataframe

#Problem 3
mean_std_caregivers <- data %>%
  filter(CRGVPERS <= 2) %>% #filters out non-caregivers and those that did not preform identified tasks
  mutate(MENTHLTH = replace(MENTHLTH, MENTHLTH == 88, 0)) %>% #replace 88 (no bad days) with 0
  filter(MENTHLTH <= 30) %>% #filtering out all scores above the max number of days of poor mental health
  select(CRGVPERS, MENTHLTH) %>% #selecting just the variables that I wish to work with
  group_by(CRGVPERS) %>% #grouping by Caregiver groups
  summarise(mean_health = mean(MENTHLTH), sd_health = sd(MENTHLTH)) #getting desired calculations
Q3 <- round(as.data.frame(mean_std_caregivers,2)) #getting 2 decimal places where needed as outlined by format, and from tibble to dataframe

#Problem 4
med_diab_age <- data %>%
  select(X_STATE, DIABAGE2) %>%  # selecting data wanted to work with
  filter(X_STATE == 42 & DIABAGE2 <= 97) %>% # PA designated as 42 and DIABAGE2 has range 1-97, 98 meaning they did not know or remember
  summarise(med.diab.age = median(DIABAGE2)) #compute the median
Q4 <- med_diab_age

#Problem 5
Menhlth_marital <- data %>%
  select(MENTHLTH, MARITAL) %>% #Selecting just Marital status and MenHealth
  mutate(MENTHLTH = replace(MENTHLTH, MENTHLTH == 88, 0)) %>% #replace 88 (no bad days) with 0
  filter(MENTHLTH <= 30 & MARITAL <= 6) #pulling out the refused to answer Martial responses and those that answered Not Sure(77) and Refused(99) treating them as outliers
Menhlth_marital_reg <- lm(MENTHLTH ~ MARITAL, Menhlth_marital) #running the regression
Q5 <- summary(Menhlth_marital_reg) #assign the summary

#Problem 6
Menhlth_marital2 <- data %>%
  select(MENTHLTH, MARITAL) %>%
  mutate(MENTHLTH = replace(MENTHLTH, MENTHLTH == 88, 0)) %>% #replace 88 (no bad days) with 0
  filter(MENTHLTH <= 30 & MARITAL <= 6) %>% #Refinement of variables for same reasons as stated in Problem 5
  group_by(MARITAL) %>% # grouping by martial status
  summarise(mean_mental = mean(MENTHLTH)) #compute means for each of the martial status
Q6 <- round(as.data.frame(Menhlth_marital2),2)

#Problem 7
no_hlthins_stroke_mental <- data %>%
  filter(HLTHPLN1 == 2 & CVDSTRK3 <= 2) %>% # getting only those without health insurance and those that declared they either did or did not have a stroke
  mutate(MENTHLTH = replace(MENTHLTH, MENTHLTH == 88, 0)) %>% #replace 88 (no bad days) with 0
  filter(MENTHLTH <= 30) %>% #filtering out all scores above the max number of days of poor mental health
  select(CVDSTRK3, MENTHLTH) %>% #pulling just the columns I want to work with
  group_by(CVDSTRK3) %>% #group by stroke or no stroke
  summarise(mean_mental = mean(MENTHLTH), sd_mental = sd(MENTHLTH)) #compute mean and standard deviation
Q7 <- round(as.data.frame(no_hlthins_stroke_mental),2)

#Problem 8
exerperweek_marital <- data %>%
  filter(MARITAL <= 6 & EXEROFT1 <= 199) %>% # Pulling data for Marital for those that have a defined stats only, also pulling those that put they worked out weekly (0-199)
  select(EXEROFT1, MARITAL) # Pulling just the items I am concerned about for this ANOVA and Tukey
exerperweek_marital.ANOVA <- aov(EXEROFT1 ~ factor(MARITAL), data = exerperweek_marital) #Run ANOVA - Factor on Marital was added due to error: No Factor returning from Tukey - got from Stackoverflow
Q8 <- TukeyHSD(exerperweek_marital.ANOVA) # Run Tukey and assigned to Q8

#Problem 9
drink_exercise_var <- data %>%
  filter(SEX == 1) %>% # Pulling just Males
  mutate(ALCDAY5 = replace(ALCDAY5, ALCDAY5 == 888, 100)) %>% #replacing 888 (drink 0 days) with 100 
  filter(between(ALCDAY5,100,199)) %>% # removing all responses less than 100 and removing all responses greater than 199
  select(ALCDAY5, EXRACT11) %>% # pulling drinks per day and most done exercise
  na.omit() %>%
  group_by(EXRACT11) %>% # Group by exercise
  summarise(var.drinks = var(ALCDAY5)) %>% # Calculating the drink variance by exercise
  arrange(desc(var.drinks)) %>% # putting in desc order
  head(6) #6 highest variances
Q9 <- round(as.data.frame(drink_exercise_var),2) #changing from tibble to data frame and assigning to Q9

#Part 2 - variables chosen are - VETERAN3, MENTHLTH, MARITAL, EXRACT11

#Problem 10
final_data <- data %>%
  select(VETERAN3, MENTHLTH, MARITAL, EXRACT11) %>%
  na.omit(EXRACT11) %>% #Removing none or NA responses
  mutate(MENTHLTH = replace(MENTHLTH, MENTHLTH == 88, 0)) %>% #replace 88 (no bad days) with 0
  filter(MARITAL != 9) #Removing None and Refused Responses
Q10 <- final_data

#Problem 11
final_data_no_out <- final_data %>%
  filter(VETERAN3 <=2) %>%
    #Removing all responses greater than 2, From visualizations there looks to be an outlier at 7 which is the I don't know response.
  filter(MENTHLTH <= 30) %>%
    #Removing Refused(99), Don't know/ Not sure(77) responses from data set to allow for more reliable data to be collected since highest number of days a subjects mental health could be not good is 30
  filter(MARITAL <= 6) %>%
    #Removing all values except the 1-6 as those are the relationship classification that were measured
  filter(EXRACT11 != 99 | EXRACT11 != 77) %>%
   #Removing values for Don't know and Refused from the exercise variable
  filter(EXRACT11 <= 98)
    #Came back and added this as still had some 99 values showing up when I did the visualizations
      #This took care of the issue. Did not do it initially as the 98 Value is for Other forms of exercise that are not listed
Q11 <- final_data_no_out

#Problem 12
# Visualizations for VETERN3
ggplot(final_data_no_out) +
  geom_boxplot(mapping = aes(VETERAN3))
ggplot(final_data_no_out) +
  geom_histogram(mapping = aes(x=VETERAN3), binwidth = 1)
# Visualizations for MENTHLTH
ggplot(final_data_no_out) +
  geom_boxplot(mapping = aes(MENTHLTH)) #Shows all values are between 0 and 30. Which are the expected values.  Shows 0 bad mental health days is easily the MODE for the set
ggplot(final_data_no_out) +
  geom_histogram(mapping = aes(x=MENTHLTH), binwidth = 1)
# Visualizations for MARITAL
ggplot(final_data_no_out) +
  geom_boxplot(mapping = aes(MARITAL))
ggplot(final_data_no_out) +
  geom_histogram(mapping = aes(x=MARITAL), binwidth = 1)
# Visualizations for EXRACT11
ggplot(final_data_no_out) +
  geom_boxplot(mapping = aes(EXRACT11))
ggplot(final_data_no_out) +
  geom_histogram(mapping = aes(x=EXRACT11), binwidth = 1)

# All Visualizations confirmed that all outliers had been removed from the data set

#Problem 13
head(final_data_no_out,10)
  #get to look at the raw data, but honestly would not have run this due to how I went about refining the data set
summary(final_data_no_out)
  # Like this to get a quick view of the data
describe(final_data_no_out)
  #More indepth view of the data and I like this because of how much I get from it.  Does get overwhelming with the large data set we worked with starting out.

#Problem 14
final_data_no_out_bi <- final_data_no_out %>%
  mutate(VETERAN3 = ifelse(VETERAN3 == 2 ,0,1)) #made the Veteran parameter binomial with 0,1.  Now 0 is NOT VETERAN and 1 is VETERAN
#Create Linear Regression
final_data_multi_reg <- lm( MENTHLTH ~ VETERAN3 + MARITAL + EXRACT11, data = final_data_no_out_bi)
#summary(final_data_multi_reg)
    # Shows that all predictors are significant
    # Look at the correlations between the variables
#cor(final_data_no_out_bi)
    #No huge positive or negative correlations.  Highest correlation is with Marital and Menthlth ~ .10798
all_final_data_models <- ols_step_all_possible(final_data_multi_reg)
    #all_final_data_models shoes that final_data_multi_reg is best by R-Square and Adj.R-Square
AIC(final_data_multi_reg)
    #631304.1
final_data_multi_reg2 <- lm( MENTHLTH ~ MARITAL + EXRACT11, data = final_data_no_out_bi) # second highest R and Adj. R from the all_final_data_model
    #summary(final_data_multi_reg2)
AIC(final_data_multi_reg2)
    #631549.5
Q14 <- final_data_multi_reg


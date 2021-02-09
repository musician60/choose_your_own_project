#########################################################################
##                                                                     ##
##                 CHOOSE YOUR OWN PROJECT                             ##
##                                                                     ##
## This program predicts whether an income exceeds $50K per year       ##
## based on census data extracted from the 1994 Census Bureau database ## 
##                                                                     ##
##        https://www.kaggle.com/uciml/adult-census-income             ##
##                                                                     ##
##                                                                     ##
## The three project files and the data files in .csv and .xlsx format ##
##      are available in the following GitHub repository:              ##
##                                                                     ##
##      https://github.com/musician60/choose_your_own_project          ##
##                                                                     ##
##                                                                     ##
#########################################################################

#suppress warning messages
options(warn = -1)

#install missing packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# load the libraries that are needed
library(tidyverse)

# read in the data
savedwd <- getwd()
setwd("C:/Users/ryoung/Desktop")
adult <- read.csv("adult.csv")
setwd(savedwd)

# clean the data
# some of the variables in the dataset contain a "?" for missing data
# replace the question marks with NAs
adult[adult == "?"] <- NA

# select the variables that will be needed for the program
# then replace the NAs in the occupation variable with the word "Other"
# then select only the observations for the United States
adult <- adult %>%
  select(age, education, marital.status, occupation, race,
         sex, hours.per.week, native.country, income) %>%
  mutate(occupation = ifelse(is.na(occupation), "Other", occupation)) %>%
  filter(native.country == "United-States")

######################
## analyze the data ##
######################
#display the structure of dataset adult
str(adult)

#let's examine our possible predictors and how they are related to the income
#variables age and hours.per.week are type integer
#and the remaining variables are type character

#first we examine age and income
ggplot(adult, aes(income, age, fill = income))+
  geom_boxplot()

adult %>% group_by(income) %>%
          summarize(min_age = min(age),
                    Q1_age = quantile(age, 0.25),
                    median_age = median(age),
                    mean_age = mean(age),
                    Q3_age = quantile(age, 0.75),
                    max_age = max(age), .groups = "drop")

#Next we will look at education and income
ggplot(adult, aes(education, fill = income))+
  geom_bar(position = "stack") +
  scale_y_continuous(trans = "log2") +
  ylab("count (scale = log2)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(adult$education, adult$income)


#next we examine marital status and income
ggplot(adult, aes(marital.status, fill = income))+
  geom_bar(position = "stack") +
  scale_y_continuous(trans = "log2") +
  ylab("count (scale = log2)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(adult$marital.status, adult$income)


#next we examine occupation and income
ggplot(adult, aes(occupation, fill = income))+
  geom_bar(position = "stack") +
  scale_y_continuous(trans = "log2") +
  ylab("count (scale = log2)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(adult$occupation, adult$income)


#next we examine race and income
ggplot(adult, aes(race, fill = income))+
  geom_bar(position = "stack") +
  scale_y_continuous(trans = "log2") +
  ylab("count (scale = log2)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(adult$race, adult$income)


#next we examine sex and income
ggplot(adult, aes(sex, fill = income))+
  geom_bar(position = "stack")

table(adult$sex, adult$income)

#finally we examine hours per week and income
ggplot(adult, aes(income, hours.per.week, fill = income))+
  geom_boxplot()

adult %>% 
  group_by(income) %>%
  summarize(min_hours = min(hours.per.week),
            Q1_hour = quantile(hours.per.week, 0.25),
            median_hours = median(hours.per.week),
            mean_hours = mean(hours.per.week),
            Q3_hours = quantile(hours.per.week, 0.75),
            max_hours = max(hours.per.week), .groups = "drop")


####################################
## develop and compare the models ##
####################################


#model 1 - using age as a predictor of income
ages <- adult$age[adult$income == ">50K"]
ages_summary <- summary(ages)
ages_cutoff <- ages_summary["1st Qu."]
predicted_values <- ifelse(adult$age >= ages_cutoff, 1, 0)
true_values <- as.numeric(as.factor(adult$income)) - 1
accuracy <- mean(predicted_values == true_values)

#create a results table
results <- tibble(method = "model 1", accuracy = accuracy)
results


#model 2 - using education as a predictor of income
education_table <- table(adult$education, adult$income)
education_table
predicted_values <- ifelse(adult$education %in% c("Doctorate", "Masters", "Prof-school"), 1, 0)
accuracy <- mean(predicted_values == true_values)

#update the results table
results <- bind_rows(results, tibble(method = "model 2", accuracy = accuracy))
results


#model 3 - using hours per week as a predictor of income
hours <- adult$hours.per.week[adult$income == ">50K"]
hours_summary <- summary(hours)
hours_cutoff <- hours_summary["1st Qu."]
predicted_values <- ifelse(adult$hours.per.week >= hours_cutoff, 1, 0)
accuracy <- mean(predicted_values == true_values)

#update the results table
results <- bind_rows(results, tibble(method = "model 3", accuracy = accuracy))
results

#model 4 - using age and education as predictors of income
#notice that column "<=50K" is less than than column ">50K" for Doctorate, Masters, and
#Prof-school
predicted_values <- ifelse(adult$age >= ages_cutoff &
                           adult$education %in% c("Doctorate", "Masters", "Prof-school"), 1, 0)
accuracy <- mean(predicted_values == true_values)

#update the results table
results <- bind_rows(results, tibble(method = "model 4", accuracy = accuracy))
results


#model 5 - using age and hours per week as predictors of income
predicted_values <- ifelse(adult$age >= ages_cutoff & adult$hours.per.week >= hours_cutoff, 1, 0)
accuracy <- mean(predicted_values == true_values)

#update the results table
results <- bind_rows(results, tibble(method = "model 5", accuracy = accuracy))
results


#model 6 - using hours per week and education as predictors of income
predicted_values <- ifelse(adult$hours.per.week >= hours_cutoff &
                           adult$education %in% c("Doctorate", "Masters", "Prof-school"), 1, 0)
accuracy <- mean(predicted_values == true_values)

#update the results table
results <- bind_rows(results, tibble(method = "model 6", accuracy = accuracy))
results


#model 7 - age and hours worked and education as predictors of income
predicted_values <- ifelse(adult$age >= ages_cutoff & adult$hours.per.week >= hours_cutoff &
                           adult$education %in% c("Doctorate", "Masters", "Prof-school"), 1, 0)
accuracy <- mean(predicted_values == true_values)

#update the results table
results <- bind_rows(results, tibble(method = "model 7", accuracy = accuracy))
results


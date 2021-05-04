#### Tryout code to merge all the data
## Jonne Kamphorst
# or may 2021

#load libraries and data
library(tidyverse)


covid_arm <- read.csv("Data/pilot/covid_arm.csv")
immi_arm <- read.csv("Data/pilot/immi_arm.csv")

data <- full_join(covid_arm, immi_arm)
rm(covid_arm, immi_arm)



#### try to change wide to long for cjoint ####

### WORKS
#  Reshape the candidate attributes so each candidate gets its own row.
data_cand = data %>% select(ResponseId, starts_with("choice"))
data_cand = data_cand %>%
  gather(variable, value, -ResponseId) %>%
  mutate(
    choiceNum = gsub("[A-Za-z]|_.+", "", variable),
    candNum   = gsub(".+(.$)", "\\1", variable),
    attribute = gsub(".+_|.$", "", variable)
  ) %>%
  select(-variable) %>%
  spread(attribute, value)



#WORKS BUT ONLY FOR FIRST FOUR 
#  Reshape the respondent's preferences so each choice gets its own row.
d_pref = data %>% select(ResponseId, ends_with("pref"))
d_pref = d_pref %>%
  gather(variable, preference, -ResponseId) %>%
  mutate(
    choiceNum  = gsub("_pref", "", variable),
    preference = as.character(gsub("Candidate ", "", preference)) #had to change to character
  ) %>%  select(-variable) %>%
  mutate(
    choiceNum = str_remove_all(choiceNum, "X") #had to remove the X because R loads the csv with the X before the question number. Try different cvs package?
  )
  

#  Merge the attributes and preferences.
d_stack = left_join(data_cand, d_pref)
d_stack = d_stack %>%
  mutate(
    Y = as.numeric(candNum == preference)
  )

#  Check that you did not create any extra rows.
nrow(d_stack) == (nrow(data) * as.numeric(max(d_stack$candNum)) * as.numeric(max(d_stack$choiceNum)))



#### Merge with other data ####
## TO DO:

#merge with other data

# recode party as copartisan or or not

# Recode Tweets based on emotional score







####  check estimation ####
library(estimatr)
colnames(d_stack)

d_stack$preference[d_stack$preference == "A"] <- 1
d_stack$preference[d_stack$preference == "B"] <- 0
d_stack$preference <- as.numeric(d_stack$preference)
d_stack <- d_stack %>% drop_na(preference)


summary(lm_robust(preference~as.factor(edu) + as.factor(gender) + as.factor(party) + as.factor(race) + as.factor(tweet), data=d_stack))












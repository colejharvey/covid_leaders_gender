######
######## Cleaning the data from the survey
########
######## Jonne Kamphorst
######## 14/10/2021



library(qualtRics)
library(dplyr)
library(tidyverse)


## Load data
immi <- read_survey("survey/data/immitrationarm_choicetext.csv")
covid <- read_survey("survey/data/covidarm_choicetext.csv")

immi$arm <- "immi"
covid$arm <- "covid"

data <- union(immi, covid)


## Reshape the data
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





### WORKING STARTS HERE

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


d_stack$preference[d_stack$preference == "A"] <- 1
d_stack$preference[d_stack$preference == "B"] <- 2
d_stack$preference <- as.numeric(d_stack$preference)
d_stack <- d_stack %>% drop_na(preference)

d_stack$chosen <- NA
d_stack$chosen[d_stack$candNum==1 & d_stack$preference==1] <- 1
d_stack$chosen[d_stack$candNum==2 & d_stack$preference==2] <- 1
d_stack$chosen[d_stack$candNum==1 & d_stack$preference==2] <- 0
d_stack$chosen[d_stack$candNum==2 & d_stack$preference==1] <- 0


data_bla <- data %>% select(arm, ResponseId,Q2.8_2, Q2.3)
d_stack<- full_join(data_bla, d_stack)


####  check estimation ####
library(estimatr)
colnames(d_stack)

length(unique(d_stack$tweet))



## Add in the Tweet information
library(readxl)
tweet_scores <- read_excel("survey/design survey/urls and tweet names.xlsx")
tweet_scores <- tweet_scores  %>% select(Fear, Trust, Happiness, Anger, URL)



summary(lm_robust(preference~as.factor(edu) + as.factor(gender) + as.factor(party) + as.factor(race) , data=d_stack))

summary(lm(preference~as.factor(edu)*arm + as.factor(gender)*arm + as.factor(party)*arm + as.factor(exp)*arm + as.factor(race)*arm, data=d_stack))

summary(lm(chosen~as.factor(edu)*arm + as.factor(gender)*arm + as.factor(party)*arm + as.factor(exp)*arm + as.factor(race) + as.factor(tweet)*as.factor(gender), data=d_stack))

summary(lm(chosen~as.factor(edu) + as.factor(gender) + as.factor(party) + as.factor(exp) + as.factor(race) + as.factor(tweet)*as.factor(gender), data=d_stack))






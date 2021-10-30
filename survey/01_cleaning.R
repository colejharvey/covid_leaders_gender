######
######## Cleaning the data from the survey
########
######## Jonne Kamphorst
######## 14/10/2021



library(qualtRics)
library(dplyr)
library(tidyverse)
library(readxl)


## Manual cleaning done in excel:
# Delete everyone who does not give consent (Q1.1)
# delete start dates from before 04/05/2021 08:00 (these are tryouts and the pilot)
# delete people who did not answer the question whether they paid attention
    # these people did not finish the survey. Note that some people finished it
    # but didn't click 'next' so Qualtrics' did someone finish thing doesn't work
# Delete everyone who took more than 30000 seconds or less than 100 seconds
# NOTE: the number of people in the immigration and covid arms are NOT the same
#       respondents were piped through based on a randomized number, yet this was not 
#       randomized in such a way that there are the same number of respondents in each
#       group. This is thus another small coding error (that does not influence inference)


## Load data
immi <- read_survey("survey/data/immitrationarm_choicetext.csv")
covid <- read_survey("survey/data/covidarm_choicetext.csv")

immi$arm <- "immi"
covid$arm <- "covid"

data <- union(immi, covid)
rm(immi, covid)



tweet_scores <- read_excel("survey/design survey/selected_tweets_withscores.xlsx")


############################## CLEAN CONJOINT DATA ############################

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



#  Reshape the respondent's preferences so each choice gets its own row for the first couple profiles
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


#  Reshape the respondent's preferences so each choice gets its own row for the other profiles and extra Qs
d_pref_rest_5 = data %>% select(ResponseId, starts_with("5_full_pref")) %>%
  mutate(choiceNum = "5") %>%
  rename(preference = `5_full_pref_1`, thingsdone = `5_full_pref_2`,
         commited = `5_full_pref_3`, pressure = `5_full_pref_4`) %>%
  mutate(preference = as.character(gsub("Candidate ", "", preference)),
         thingsdone = as.character(gsub("Candidate ", "", thingsdone)),
         commited = as.character(gsub("Candidate ", "", commited)),
         pressure = as.character(gsub("Candidate ", "", pressure)))

d_pref_rest_6 = data %>% select(ResponseId, starts_with("6_full_pref")) %>%
  mutate(choiceNum = "6") %>%
  rename(preference = `6_full_pref_1`, thingsdone = `6_full_pref_2`,
         commited = `6_full_pref_3`, pressure = `6_full_pref_4`) %>%
  mutate(preference = as.character(gsub("Candidate ", "", preference)),
         thingsdone = as.character(gsub("Candidate ", "", thingsdone)),
         commited = as.character(gsub("Candidate ", "", commited)),
         pressure = as.character(gsub("Candidate ", "", pressure)))

d_pref_rest_7 = data %>% select(ResponseId, starts_with("7_full_pref")) %>%
  mutate(choiceNum = "7") %>%
  rename(preference = `7_full_pref_1`, thingsdone = `7_full_pref_2`,
         commited = `7_full_pref_3`, pressure = `7_full_pref_4`) %>%
  mutate(preference = as.character(gsub("Candidate ", "", preference)),
         thingsdone = as.character(gsub("Candidate ", "", thingsdone)),
         commited = as.character(gsub("Candidate ", "", commited)),
         pressure = as.character(gsub("Candidate ", "", pressure)))

d_pref_rest_8 = data %>% select(ResponseId, starts_with("8_full_pref")) %>%
  mutate(choiceNum = "8") %>%
  rename(preference = `8_full_pref_1`, thingsdone = `8_full_pref_2`,
         commited = `8_full_pref_3`, pressure = `8_full_pref_4`) %>%
  mutate(preference = as.character(gsub("Candidate ", "", preference)),
         thingsdone = as.character(gsub("Candidate ", "", thingsdone)),
         commited = as.character(gsub("Candidate ", "", commited)),
         pressure = as.character(gsub("Candidate ", "", pressure)))


d_pref_rest <- union(d_pref_rest_5, d_pref_rest_6)
d_pref_rest <- union(d_pref_rest, d_pref_rest_7)
d_pref_rest <- union(d_pref_rest, d_pref_rest_8)
rm(d_pref_rest_5, d_pref_rest_6, d_pref_rest_7, d_pref_rest_8)


d_pref<- full_join(d_pref_rest, d_pref)
rm(d_pref_rest)



#  Merge the attributes and preferences.
d_stack = left_join(data_cand, d_pref)
rm(d_pref, data_cand)

#  Check that you did not create any extra rows.
nrow(d_stack) == (nrow(data) * as.numeric(max(d_stack$candNum)) * as.numeric(max(d_stack$choiceNum)))



#add a variable for whether a profile is chosen or not or capable on one of the three measures
d_stack$candNum[d_stack$candNum=="1"] <- "A"
d_stack$candNum[d_stack$candNum=="2"] <- "B"

d_stack$chosen <- NA
d_stack$chosen <- ifelse(d_stack$candNum == d_stack$preference, 1, 0)

d_stack$thingsdone_chosen <- NA
d_stack$thingsdone_chosen <- ifelse(d_stack$candNum == d_stack$thingsdone, 1, 0)

d_stack$commited_chosen <- NA
d_stack$commited_chosen <- ifelse(d_stack$candNum == d_stack$commited, 1, 0)

d_stack$pressure_chosen <- NA
d_stack$pressure_chosen <- ifelse(d_stack$candNum == d_stack$pressure, 1, 0)


# Only keep columns needed for the regressions
d_stack <- d_stack %>% select(-preference, -thingsdone, -commited, -pressure)



##### Add in the Tweet information and the rest of the survey
# first quickly add in the arm
x <- data %>% select(arm, ResponseId)
d_stack<- full_join(x, d_stack)
rm(x)

# add in the Tweet scores. Select useful ones first
tweet_scores <- tweet_scores  %>% select(Fear, Trust, Happiness, Anger, URL) %>%
  rename(tweet=URL)

d_stack <- left_join(d_stack, tweet_scores)


#### Add sensible baselines for the conjoint levels and slice up Tweet data by scores
d_stack <- d_stack %>% group_by(arm) %>%
  mutate(anger_cut = as.character(ntile(Anger, 3)),
         fear_cut = as.character(ntile(Fear, 3)),
         trust_cut = as.character(ntile(Trust, 3))) %>%
  ungroup()

lowmedhi <- function(variable){
  variable <- ifelse(variable == "1", "Low",
                     ifelse(variable == "2", "Medium", 
                            ifelse(variable == "3", "High", "ERROR")))
  variable <- factor(variable, levels=c("Low", "Medium", "High"))
}

d_stack$anger_cut <- lowmedhi(d_stack$anger_cut)
d_stack$fear_cut <- lowmedhi(d_stack$fear_cut)
d_stack$trust_cut <- lowmedhi(d_stack$trust_cut)


## Add baselines for other variables
d_stack$edu <- factor(d_stack$edu, levels=c("High School", "College", "Graduate degree"))

# Experience coding (we had none and 0 years in there. Put these together, was a coding error in the survey)
d_stack$exp[d_stack$exp=="None"] <- "0 years"
d_stack$exp <- factor(d_stack$exp, levels=c("0 years", "5 years", "10 years", "15 years"))

d_stack$race <- factor(d_stack$race, levels=c("Caucasian", "African American", "Asian", "Hispanic"))





##### Clean the main data ########
#Only keep the useful coumns
data <- data %>% select("ResponseId", "EndDate", "Duration (in seconds)", contains("Q"), )


## Drop respondent who didn't pass the attention check or indicated that they did not pay attention
data <- subset(data, Q2.17== "Social Media" & Q7.2 == "I paid close attention to the candidate profiles.")



## Save the datasets
saveRDS(data, "survey/data/survey_data.rds")





### Add the normal dataset to the conjoint dataset and safe it 
conjoint_data <- left_join(data, d_stack) #left join so the respondnts who didn't pass the checks are thrown out

saveRDS(conjoint_data, "survey/data/conjoint_data.rds")



















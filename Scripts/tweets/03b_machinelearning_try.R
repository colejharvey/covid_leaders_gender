#### Machine learning try-out
#### JK  15/02/2021
#### This script tries to see if machine learning can teach us anything here



#### Cleaning the data as in the other scripts ####


library(tidyverse)
library(stargazer)
library(sjPlot)
library(lubridate)
library(glmnet)

#tweet_data <- read.csv("C:/Users/colej/OneDrive/Documents/Research projects/covid_leaders_gender/Read only (do not track)/tweets_with_covariates_fixed.csv") 
tweet_data <- read_csv("Data/20210128_tweets_with_covariates_fixed.csv") #not sure if this is the right data, it should load on all our machines now though

##Created at as date
tweet_data <- tweet_data %>% mutate(created_at = ymd_hms(created_at))

##Number of words per string
tweet_data <- tweet_data %>% mutate(n.words = str_count(text, boundary("word")))

##Emotion words divided by word length
tweet_data <- tweet_data %>% mutate(anger.prop = NRC.10.anger / n.words)
tweet_data <- tweet_data %>% mutate(trust.prop = NRC.10.trust / n.words)
tweet_data <- tweet_data %>% mutate(anticip.prop = NRC.10.anticipation / n.words)
tweet_data <- tweet_data %>% mutate(disgust.prop = NRC.10.disgust / n.words)
tweet_data <- tweet_data %>% mutate(fear.prop = NRC.10.fear / n.words)
tweet_data <- tweet_data %>% mutate(joy.prop = NRC.10.joy / n.words)
tweet_data <- tweet_data %>% mutate(sad.prop = NRC.10.sadness / n.words)
tweet_data <- tweet_data %>% mutate(surprise.prop = NRC.10.surprise / n.words)


##Economy tweets

tweet_data <- tweet_data %>% mutate(economy_tweet = ifelse(grepl("business* | jobs | economy | reopen* | opening | relaunch | restart | workforce | economic | easing restrictions | operate | owners | recover*", tweet_data$text) == TRUE, 1, 0))

##Gun tweets (because 'gun violence epidemic' is common phrase)

tweet_data <- tweet_data %>% mutate(gun_tweet = ifelse(grepl("gun", tweet_data$text) == TRUE, 1, 0))
tweet_data <- tweet_data %>% filter(gun_tweet == 0)

tweet_data <- tweet_data %>% mutate(epidemic_tweet = ifelse(grepl("epidemic", tweet_data$text) == TRUE, 1, 0))
tweet_data <- tweet_data %>% filter(epidemic_tweet == 0)

##Cases and deaths per capita

tweet_data <- tweet_data %>% mutate(cases_pc = as.numeric(cases) / as.numeric(factor(pop2019)))
tweet_data <- tweet_data %>% mutate(deaths_pc = as.numeric(deaths) / as.numeric(factor(pop2019)))




#### Try machine learning model ####
## Using Lasso as it's not a classification task

# Select emtional columns
positions <- c(33:62)
tweet_data_ml <- tweet_data %>% select(favorite_count, gender,  positions)


# prepare the model
x_vars <- model.matrix(favorite_count~. , tweet_data_ml)[,-1]
y_var <- tweet_data_ml$favorite_count
lambda_seq <- 10^seq(2, -2, by = -.1)


# Splitting the data into test and train
set.seed(666)
train = sample(1:nrow(x_vars), nrow(x_vars)/2)
x_test = (-train)
y_test = y_var[x_test]



cv_output <- cv.glmnet(x_vars[train,], y_var[train],
                       alpha = 1, lambda = lambda_seq, 
                       nfolds = 5)


# identifying best lamda
best_lam <- cv_output$lambda.min
best_lam


# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(x_vars[train,], y_var[train], alpha = 1, lambda = best_lam)
pred <- predict(lasso_best, s = best_lam, newx = x_vars[x_test,])


final <- cbind(y_var[train], pred)
# Checking the first six obs
head(final)


# Inspecting beta coefficients
coef(lasso_best)


######
######## Doing some preliminary analysis for the survey
########
######## Jonne Kamphorst
######## 29/10/2021



library(qualtRics)
library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(extrafont)
library(interplot)
library(sjPlot)
library(viridis)
library(estimatr)
library(ggeffects)
library(cregg)




## Load data
#conjoint_data <- readRDS("survey/data/conjoint_data.rds")
conjoint_data <- readRDS("C:/github projects/polpan/covid_leaders_gender/survey/data/conjoint_data.rds")



#font_import()
loadfonts(quiet = T, device = "pdf")
windowsFonts(Georgia = windowsFont("Georgia")) #load fonts for windows machines


## Set ggplot theme with the Georgia font
theme_set(theme_light(base_family = "Georgia"))

## Basic plot
x <- lm_robust(chosen ~ edu + exp + party + #use robust to cluster
                 race + trust_cut + gender, 
               data=conjoint_data, cluster=ResponseId) 

plot_model(x, SE=T, show.values = TRUE,
           show.p = TRUE, value.size = 3, line.size = 0.5, dot.size = 1, 
           digits = 3) +
  ylim(-.15, .15) + 
  theme_minimal() + ggtitle("Basic Model")



# basic plot with gender interaction
x <- lm_robust(chosen ~ edu + exp + party + #use robust to cluster
                 race + trust_cut + gender * arm, 
               data=conjoint_data, cluster=ResponseId) 

plot_model(x, SE=T, show.values = TRUE,
           show.p = TRUE, value.size = 3, line.size = 0.5, dot.size = 1, 
           digits = 3) +
  ylim(-.15, .15) + 
  theme_minimal() + ggtitle("Policy Domain and Gender")

  
#plot manually with marginal means 
# the right hand side should be factors. left hand side numeric
conjoint_data$arm_f <- as.factor(conjoint_data$arm)
conjoint_data$edu_f <- factor(conjoint_data$edu)
conjoint_data$exp_f <- factor(conjoint_data$exp)
conjoint_data$party_f <- factor(conjoint_data$party)
conjoint_data$race_f <- factor(conjoint_data$race)
conjoint_data$trust_cut_f <- factor(conjoint_data$trust_cut)
conjoint_data$gender_f <- factor(conjoint_data$gender)

f <- chosen ~ edu_f + exp_f + party_f + race_f + gender_f
marginalmeans <- cj(conjoint_data, f, id = ~ResponseId, estimate = "mm", by = ~arm_f)# for the left-right bins
marginalmeans$group <- marginalmeans$BY #add a grouping variable for the plot function


plot(marginalmeans, group = "group") + theme_minimal() +
  labs(x="Marginal Means for the probability to choose a profile")  + ggplot2::facet_wrap(~BY, ncol = 4L) + 
  geom_point(aes(y=level, x=estimate),size = 2.2) + geom_linerange(aes(xmin=lower, xmax=upper), size=1.2) +
  theme_set(theme_light(base_family = "Georgia")) +
  geom_vline(xintercept = 0.5, linetype="dashed")



conjoint_data_mech <- conjoint_data %>% drop_na(thingsdone_chosen_f)
f2 <- thingsdone_chosen ~ edu_f + exp_f + party_f + race_f + gender_f
marginalmeans <- cj(conjoint_data_mech, f2, id = ~ResponseId, estimate = "mm", by = ~arm_f)# for the left-right bins
marginalmeans$group <- marginalmeans$BY #add a grouping variable for the plot function


plot(marginalmeans, group = "group") + theme_minimal() +
  labs(x="Marginal Means for the probability to say a candidate gets things done")  + ggplot2::facet_wrap(~BY, ncol = 4L) + 
  geom_point(aes(y=level, x=estimate),size = 2.2) + geom_linerange(aes(xmin=lower, xmax=upper), size=1.2) +
  theme_set(theme_light(base_family = "Georgia")) +
  geom_vline(xintercept = 0.5, linetype="dashed")














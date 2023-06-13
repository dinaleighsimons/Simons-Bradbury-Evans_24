#Simons, Bradbury, and Evans (2022) extra script

#Setting environment####
rm(list = ls())

#load packages
library(ggpubr)
library(ggplot2)
library(dplyr)
library(car)
library(survey)
library(Hmisc)
library(corrplot)
library(ggfortify)

#read in data
combined_data <- read.csv("Data/combined_data_PCA_new.csv")
str(combined_data)

combined_data <- combined_data %>% select(-c("X.1", "X"))
combined_data<- rename(combined_data, meanflood = newflood)
combined_data$message_framing = as.factor(combined_data$message_framing)
combined_data$nudge = as.factor(combined_data$nudge)

#make positive for models
combined_data$behaviour= combined_data$behaviour - min(combined_data$behaviour)
combined_data$sympathy= combined_data$sympathy - min(combined_data$sympathy)
combined_data$climate_scores= combined_data$climate_scores - min(combined_data$climate_scores)
combined_data$experience= combined_data$experience - min(combined_data$experience)
combined_data$social_norm= combined_data$social_norm - min(combined_data$social_norm)

#Behaviour extra mods----
mod_behaviour_int<- lm(behaviour ~ message_framing*ego +
                         nudge + 
                         efficacy + 
                         connectedness + 
                         social_norm +
                         finance_security + 
                         age +
                         gender +
                         ethnicity +
                         education_rank +
                         experience + 
                         climate_scores + 
                         meanflood+
                         MD_index, data = combined_data)

autoplot(mod_behaviour_int) #ok
summary(mod_behaviour_int)
Anova(mod_behaviour_int)

#Don't use - message framing * ego not significant

mod_behaviour_int1<- lm(behaviour ~ message_framing*nudge + 
                          efficacy + 
                          connectedness + 
                          social_norm +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          experience + 
                          ego + 
                          climate_scores + 
                          meanflood +
                          MD_index, data = combined_data)
autoplot(mod_behaviour_int1) #ok
summary(mod_behaviour_int1)
Anova(mod_behaviour_int1) 

#Don't use - message framing * nudge not significant

mod_behaviour_int2<- lm(behaviour ~ message_framing*experience +
                          nudge + 
                          efficacy + 
                          connectedness + 
                          social_norm +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          ego + 
                          climate_scores + 
                          meanflood +
                          MD_index, data = combined_data)

autoplot(mod_behaviour_int2) #ok
summary(mod_behaviour_int2)
Anova(mod_behaviour_int2) 

#Don't use - message framing * experience not significant

mod_behaviour_int3<- lm(behaviour ~ message_framing*climate_scores +
                          nudge + 
                          efficacy + 
                          connectedness + 
                          social_norm +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          ego + 
                          experience + 
                          meanflood +
                          MD_index, data = combined_data)

autoplot(mod_behaviour_int3) #ok
summary(mod_behaviour_int3)
Anova(mod_behaviour_int3) 

#Don't use - message framing * climate change not significant

mod_behaviour_int4<- lm(behaviour ~ message_framing*meanflood +
                          nudge + 
                          efficacy + 
                          connectedness + 
                          social_norm +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          climate_scores +
                          education_rank +
                          ego + 
                          experience + 
                          MD_index, data =combined_data)

autoplot(mod_behaviour_int4) #ok
summary(mod_behaviour_int4)
Anova(mod_behaviour_int4)

mod_behaviour_int5<- lm(behaviour ~ message_framing*MD_index +
                          nudge + 
                          efficacy + 
                          connectedness + 
                          social_norm +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          experience + 
                          ego + 
                          climate_scores + 
                          meanflood, data = combined_data)

autoplot(mod_behaviour_int5) #ok
summary(mod_behaviour_int5)
Anova(mod_behaviour_int5)

#Don't use - message framing * MD_index not significant

mod_behaviour_int6<- lm(behaviour ~ nudge*social_norm+
                          message_framing+
                          MD_index +
                          efficacy + 
                          connectedness + 
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          experience + 
                          ego + 
                          climate_scores + 
                          meanflood, data = combined_data)

autoplot(mod_behaviour_int6) #ok
summary(mod_behaviour_int6)
Anova(mod_behaviour_int6)

#Don't use - nudge*social_norm not significant


#Sympathy extra models ----

mod_sympathy_int<- lm(sympathy ~ message_framing*ego +
                        nudge + 
                        efficacy + 
                        connectedness + 
                        social_norm +
                        finance_security + 
                        age +
                        gender +
                        ethnicity +
                        education_rank +
                        experience + 
                        climate_scores + 
                        meanflood +
                        MD_index, data = combined_data)

autoplot(mod_sympathy_int)
summary(mod_sympathy_int)
Anova(mod_sympathy_int)

#Don't use - message framing * ego not significant

mod_sympathy_int1<- lm(sympathy ~ message_framing*nudge + 
                         efficacy + 
                         connectedness + 
                         social_norm +
                         finance_security + 
                         age +
                         gender +
                         ethnicity +
                         education_rank +
                         experience + 
                         ego + 
                         climate_scores + 
                         meanflood +
                         MD_index, data = combined_data)

autoplot(mod_sympathy_int1)
summary(mod_sympathy_int1)
Anova(mod_sympathy_int1)

#Don't use - message framing * nudge not significant


mod_sympathy_int2<- lm(sympathy ~ message_framing*experience+
                         nudge + 
                         efficacy + 
                         connectedness + 
                         social_norm +
                         finance_security + 
                         age +
                         gender +
                         ethnicity +
                         education_rank +
                         ego + 
                         climate_scores + 
                         meanflood +
                         MD_index, data = combined_data)

autoplot(mod_sympathy_int2)
summary(mod_sympathy_int2)
Anova(mod_sympathy_int2)

#Don't use - message framing * experience not significant

mod_sympathy_int4<- lm(sympathy ~ message_framing*meanflood +
                         nudge + 
                         efficacy + 
                         connectedness + 
                         social_norm +
                         finance_security + 
                         age +
                         gender +
                         ethnicity +
                         education_rank +
                         climate_scores+
                         experience +
                         ego +
                         MD_index, data = combined_data)

autoplot(mod_sympathy_int4)
summary(mod_sympathy_int4)
Anova(mod_sympathy_int4)

#Don't use - message framing * flood not significant

mod_sympathy_int5<- lm(sympathy ~ nudge*social_norm +
                         message_framing + 
                         meanflood +
                         efficacy + 
                         connectedness + 
                         finance_security + 
                         age +
                         gender +
                         ethnicity +
                         education_rank +
                         climate_scores+
                         experience +
                         ego +
                         MD_index, data = combined_data)

autoplot(mod_sympathy_int5)
summary(mod_sympathy_int5)
Anova(mod_sympathy_int5)

#Don't use - nudge*social_norm not significant


#Financial extra models----

mod_financial_int<- glm(financial ~ message_framing*ego+
                          nudge + 
                          efficacy + 
                          connectedness + 
                          log(1 + social_norm_donation) +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          experience + 
                          climate_scores + 
                          meanflood+
                          MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int)
summary(mod_financial_int)
Anova(mod_financial_int, test = "F")

#Don't use - message framing * ego not significant

mod_financial_int1<- glm(financial ~ message_framing*nudge + 
                           efficacy + 
                           connectedness + 
                           log(1 + social_norm_donation) +
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           experience + 
                           ego + 
                           climate_scores + 
                           meanflood+
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int1)
summary(mod_financial_int1)
Anova(mod_financial_int1, test = "F")

#Don't use - message framing * nudge not significant

mod_financial_int2<- glm(financial ~ message_framing*experience +
                           nudge + 
                           efficacy + 
                           connectedness + 
                           climate_scores+
                           log(1 + social_norm_donation) +
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           ego + 
                           meanflood+
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int2)
summary(mod_financial_int2)
Anova(mod_financial_int2, test = "F")

#Don't use - message framing * experience not significant

mod_financial_int3<- glm(financial ~ message_framing*climate_scores +
                           nudge + 
                           efficacy + 
                           connectedness + 
                           log(1 + social_norm_donation) +
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           meanflood+
                           experience + 
                           ego +
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int3)
summary(mod_financial_int3)
Anova(mod_financial_int3, test = "F")

#Don't use - message framing * climate change not significant

mod_financial_int4<- glm(financial ~ message_framing*meanflood +
                           nudge + 
                           efficacy + 
                           connectedness + 
                           log(1 + social_norm_donation) +
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           climate_scores +
                           education_rank +
                           experience + 
                           ego +
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int4)
summary(mod_financial_int4)
Anova(mod_financial_int4, test = "F")

#Don't use - message framing * flood not significant

mod_financial_int5<- glm(financial ~  nudge*log(1 + social_norm_donation) +
                           message_framing + 
                           meanflood +
                           efficacy + 
                           connectedness +
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           climate_scores +
                           education_rank +
                           experience + 
                           ego +
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int5)
summary(mod_financial_int5)
Anova(mod_financial_int5, test = "F")

#Don't use - nudge*social_norm not significant

#Sufficiency extra models----

mod_sufficieny_int1<- lm(sufficiency ~ message_framing*nudge + 
                           efficacy + 
                           connectedness + 
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           experience + 
                           ego + 
                           climate_scores + 
                           meanflood +
                           MD_index, data = combined_data)
autoplot(mod_sufficieny_int1)
summary(mod_sufficieny_int1)
Anova(mod_sufficieny_int1)

#Don't use - message framing * nudge not significant

mod_sufficieny_int2<- lm(sufficiency ~ message_framing*experience+
                           nudge + 
                           efficacy + 
                           connectedness + 
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           ego + 
                           climate_scores + 
                           meanflood +
                           MD_index, data = combined_data)
autoplot(mod_sufficieny_int2)
summary(mod_sufficieny_int2)
Anova(mod_sufficieny_int2)

#Don't use - message framing * experience not significant

mod_sufficieny_int3<- lm(sufficiency ~ message_framing*climate_scores+
                           nudge + 
                           efficacy + 
                           connectedness + 
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           experience + 
                           ego + 
                           meanflood +
                           MD_index, data = combined_data)
autoplot(mod_sufficieny_int3)
summary(mod_sufficieny_int3)
Anova(mod_sufficieny_int3)

#Don't use - message framing * climate change not significant

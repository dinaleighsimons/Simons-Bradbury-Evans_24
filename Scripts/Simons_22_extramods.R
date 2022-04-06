#Simons, Evans and Bradbury script extra script

#Setting environment####
rm(list = ls())


###FACTS###

#DINA IS VERY COOL AND SMART#

#load packages
library(ggpubr)
library(ggplot2)
library(dplyr)
library(car)
library(survey)
library(Hmisc)
library(corrplot)
library(ggfortify)

#Behaviour extra mods----
mod_behaviour_int<- lm(behaviour ~ message_framing*ego +
                         nudge + 
                         efficacy + 
                         connectedness + 
                         allocated_wild_300 +
                         social_norm +
                         finance_security + 
                         age +
                         gender +
                         ethnicity +
                         education_rank +
                         experience + 
                         sqrt(1+climate_change) + 
                         flood+
                         MD_index, data = combined_data)

autoplot(mod_behaviour_int) #ok
summary(mod_behaviour_int)
Anova(mod_behaviour_int)

#Don't use - message framing * ego not significant

mod_behaviour_int1<- lm(behaviour ~ message_framing*nudge + 
                          efficacy + 
                          connectedness + 
                          allocated_wild_300 +
                          social_norm +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          experience + 
                          ego + 
                          sqrt(1+climate_change) + 
                          flood +
                          MD_index, data = combined_data)
autoplot(mod_behaviour_int1) #ok
summary(mod_behaviour_int1)
Anova(mod_behaviour_int1) 

#Don't use - message framing * nudge not significant

mod_behaviour_int2<- lm(behaviour ~ message_framing*experience +
                          nudge + 
                          efficacy + 
                          connectedness + 
                          allocated_wild_300 +
                          social_norm +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          ego + 
                          sqrt(1+climate_change) + 
                          flood +
                          MD_index, data = combined_data)

autoplot(mod_behaviour_int2) #ok
summary(mod_behaviour_int2)
Anova(mod_behaviour_int2) 

#Don't use - message framing * experience not significant

mod_behaviour_int3<- lm(behaviour ~ message_framing*sqrt(1+ climate_change) +
                          nudge + 
                          efficacy + 
                          connectedness + 
                          allocated_wild_300 +
                          social_norm +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          ego + 
                          experience + 
                          flood +
                          MD_index, data = combined_data)

autoplot(mod_behaviour_int3) #ok
summary(mod_behaviour_int3)
Anova(mod_behaviour_int3) 

#Don't use - message framing * climate change not significant

mod_behaviour_int4<- lm(behaviour ~ message_framing*flood +
                          nudge + 
                          efficacy + 
                          connectedness + 
                          allocated_wild_300 +
                          social_norm +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          sqrt(1+ climate_change) +
                          education_rank +
                          ego + 
                          experience + 
                          MD_index, data = join_quant_survey_data)

autoplot(mod_behaviour_int4) #ok
summary(mod_behaviour_int4)
Anova(mod_behaviour_int4)

mod_behaviour_int5<- lm(behaviour ~ message_framing*MD_index +
                          nudge + 
                          efficacy + 
                          connectedness + 
                          allocated_wild_300 +
                          social_norm +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          experience + 
                          ego + 
                          sqrt(1+climate_change) + 
                          flood, data = combined_data)

autoplot(mod_behaviour_int5) #ok
summary(mod_behaviour_int5)
Anova(mod_behaviour_int5)

#Don't use - message framing * MD_index not significant

#Sympathy models ----

mod_sympathy_int<- lm(sympathy^(2) ~ message_framing*ego +
                        nudge + 
                        efficacy + 
                        connectedness + 
                        allocated_wild_300 +
                        social_norm +
                        finance_security + 
                        age +
                        gender +
                        ethnicity +
                        education_rank +
                        experience + 
                        sqrt(1+climate_change) + 
                        flood +
                        MD_index, data = combined_data)

#Don't use - message framing * ego not significant

mod_sympathy_int1<- lm(log(1+sympathy)~ message_framing*nudge + 
                         efficacy + 
                         connectedness + 
                         allocated_wild_300 +
                         social_norm +
                         finance_security + 
                         age +
                         gender +
                         ethnicity +
                         education_rank +
                         experience + 
                         ego + 
                         sqrt(1+climate_change) + 
                         flood +
                         MD_index, data = combined_data)

#Don't use - message framing * nudge not significant


mod_sympathy_int2<- lm(log(1+sympathy) ~ message_framing*experience+
                         nudge + 
                         efficacy + 
                         connectedness + 
                         allocated_wild_300 +
                         social_norm +
                         finance_security + 
                         age +
                         gender +
                         ethnicity +
                         education_rank +
                         ego + 
                         sqrt(1+ climate_change) + 
                         flood +
                         MD_index, data = combined_data)

#Don't use - message framing * experience not significant

mod_sympathy_int4<- lm(log(1+sympathy) ~ message_framing*flood +
                         nudge + 
                         efficacy + 
                         connectedness + 
                         allocated_wild_300 +
                         social_norm +
                         finance_security + 
                         age +
                         gender +
                         ethnicity +
                         education_rank +
                         sqrt(1+climate_change)+
                         experience +
                         ego +
                         MD_index, data = combined_data)

#Don't use - message framing * flood not significant

##Financial models----

mod_financial_int<- glm(financial ~ message_framing*ego+
                          nudge + 
                          efficacy + 
                          connectedness + 
                          allocated_wild_300 +
                          log(1 + social_norm_donation) +
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          experience + 
                          sqrt(1+ climate_change) + 
                          flood+
                          MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int)
summary(mod_financial_int)
Anova(mod_financial_int, test = "F")

#Don't use - message framing * ego not significant

mod_financial_int1<- glm(financial ~ message_framing*nudge + 
                           efficacy + 
                           connectedness + 
                           allocated_wild_300 +
                           log(1 + social_norm_donation) +
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           experience + 
                           ego + 
                           sqrt(1+ climate_change) + 
                           flood+
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int1)
summary(mod_financial_int1)

#Don't use - message framing * nudge not significant

mod_financial_int2<- glm(financial ~ message_framing*experience +
                           nudge + 
                           efficacy + 
                           connectedness + 
                           sqrt(1+ climate_change)+
                           allocated_wild_300 +
                           log(1 + social_norm_donation) +
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           ego + 
                           flood+
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int2)
summary(mod_financial_int2)
Anova(mod_financial_int2, test = "F")

#Don't use - message framing * experience not significant

mod_financial_int3<- glm(financial ~ message_framing*sqrt(1+climate_change) +
                           nudge + 
                           efficacy + 
                           connectedness + 
                           allocated_wild_300 +
                           log(1 + social_norm_donation) +
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           flood+
                           experience + 
                           ego +
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int3)
summary(mod_financial_int3)
Anova(mod_financial_int3, test = "F")

#Don't use - message framing * climate change not significant

mod_financial_int4<- glm(financial ~ message_framing*flood +
                           nudge + 
                           efficacy + 
                           connectedness + 
                           allocated_wild_300 +
                           log(1 + social_norm_donation) +
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           sqrt(1+ climate_change) +
                           education_rank +
                           experience + 
                           ego +
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int4)
summary(mod_financial_int4)
Anova(mod_financial_int4, test = "F")


#Don't use - message framing * flood not significant



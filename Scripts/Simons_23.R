#Simons, Bradbury and Evans (2024) - Models and figures script

#Setting environment------------------------------------------------------------

##Clear global environment----
rm(list = ls())

##Load packages----
library(ggpubr)
library(ggplot2)
library(dplyr)
library(car)
library(survey)
library(Hmisc)
library(corrplot)
library(ggfortify)
library(performance) #for colinearity checks
library(jtools) #for model plots
library(ggstance) #for model plots
library(broom.mixed) #for model plots
library(sjPlot) #https://strengejacke.github.io/sjPlot/reference/plot_models.html
library(sjlabelled)
library(sjmisc)

##Read in data----
combined_data <- read.csv("Data/combined_data_PCA_new.csv")
str(combined_data)

##Data tidying----
combined_data <- combined_data %>% select(-c("X.1", "X"))
combined_data<- rename(combined_data, meanflood = newflood)
combined_data$message_framing = as.factor(combined_data$message_framing)
combined_data$nudge = as.factor(combined_data$nudge)

#scaling PCA values for model functionality
combined_data$behaviour= combined_data$behaviour - min(combined_data$behaviour)
combined_data$sympathy= combined_data$sympathy - min(combined_data$sympathy)
combined_data$climate_scores= combined_data$climate_scores - min(combined_data$climate_scores)
combined_data$experience= combined_data$experience - min(combined_data$experience)
combined_data$social_norm= combined_data$social_norm - min(combined_data$social_norm)

#undo reverse code for egoism (now psychological benefits)
combined_data$ego = -(combined_data$ego)

#Regression models--------------------------------------------------------------

##Manipulation tests----

###Effect of nudge on perceived social norm----
combined_data %>% 
  group_by(nudge) %>% 
  summarise(mean = mean(social_norm), 
            sd = sd(social_norm),
            samp_size = n())

ggplot(combined_data, aes(x = social_norm)) + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~nudge, ncol = 1)

t.test(social_norm ~ nudge, combined_data)

normsocialmod<- lm(social_norm ~ nudge + 
                    finance_security + 
                    age +
                    gender +
                    ethnicity +
                    education_rank +
                    MD_index, data = combined_data)
autoplot(normsocialmod) #ok
summary(normsocialmod)
Anova(normsocialmod)

confint(normsocialmod)

###Effect of nudge on perceived social norm (finance)----
combined_data %>% 
  group_by(nudge) %>% 
  summarise(mean = mean(social_norm_donation), 
            sd = sd(social_norm_donation),
            samp_size = n())

ggplot(combined_data, aes(x = log(1 + social_norm_donation))) + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~nudge, ncol = 1)

t.test(log(1 + social_norm_donation) ~ nudge, combined_data)

normsocialmod2<- lm(log(1 + social_norm_donation) ~ nudge + 
                      finance_security + 
                      age +
                      gender +
                      ethnicity +
                      education_rank +
                      MD_index, data = combined_data)
autoplot(normsocialmod2) #ok
summary(normsocialmod2)
Anova(normsocialmod2)

confint(normsocialmod2)

###Effect of nudge on self-efficacy----
combined_data %>% 
  group_by(nudge) %>% 
  summarise(mean = mean(efficacy), 
            sd = sd(efficacy),
            samp_size = n())

ggplot(combined_data, aes(x = efficacy)) + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~nudge, ncol = 1)

t.test(efficacy ~ nudge, combined_data)

normefficacymod<- lm(efficacy ~ nudge + 
                      finance_security + 
                      age +
                      gender +
                      ethnicity +
                      education_rank +
                      MD_index, data = combined_data)
autoplot(normefficacymod) #ok
summary(normefficacymod)
Anova(normefficacymod)

confint(normefficacymod)

###Create output table for manuscript----
# Extract Parameter Estimates, Standard Errors, and P-Values and Create a Table with the Results
estimates_1 <- sprintf("%.3f", summary(normsocialmod)$coefficients[-1,1])
std_errors_1 <- sprintf("(%.3f)", summary(normsocialmod)$coefficients[-1,2])
p_values_1 <- sprintf("p = %.3f%s", summary(normsocialmod)$coefficients[-1,4], ifelse(summary(normsocialmod)$coefficients[-1,4] < 0.05, "*", ""))

results_table_1 <- data.frame(
  Predictor = rownames(summary(normsocialmod)$coefficients)[-1],
  `Parameter Estimate (Std. Error)` = paste(estimates_1, std_errors_1, p_values_1, sep = " "),
  check.names = FALSE)

# Extract Parameter Estimates, Standard Errors, and P-Values and Create a Table with the Results
estimates_2 <- sprintf("%.3f", summary(normsocialmod2)$coefficients[-1,1])
std_errors_2 <- sprintf("(%.3f)", summary(normsocialmod2)$coefficients[-1,2])
p_values_2 <- sprintf("p = %.3f%s", summary(normsocialmod2)$coefficients[-1,4], ifelse(summary(normsocialmod2)$coefficients[-1,4] < 0.05, "*", ""))

results_table_2 <- data.frame(
  Predictor = rownames(summary(normsocialmod2)$coefficients)[-1],
  `Parameter Estimate (Std. Error)` = paste(estimates_2, std_errors_2, p_values_2, sep = " "),
  check.names = FALSE)

# Extract Parameter Estimates, Standard Errors, and P-Values and Create a Table with the Results
estimates_3 <- sprintf("%.3f", summary(normefficacymod)$coefficients[-1,1])
std_errors_3 <- sprintf("(%.3f)", summary(normefficacymod)$coefficients[-1,2])
p_values_3 <- sprintf("p = %.3f%s", summary(normefficacymod)$coefficients[-1,4], ifelse(summary(normefficacymod)$coefficients[-1,4] < 0.05, "*", ""))

results_table_3 <- data.frame(
  Predictor = rownames(summary(normefficacymod)$coefficients)[-1],
  `Parameter Estimate (Std. Error)` = paste(estimates_3, std_errors_3, p_values_3, sep = " "),
  check.names = FALSE)

# Merge All Tables
results_table_final <- merge(results_table_1, results_table_2, by = "Predictor", all = TRUE)
results_table_final <- merge(results_table_final, results_table_3, by = "Predictor", all = TRUE)

# Add Column Names
colnames(results_table_final) <- c("Predictor", "Model 1 Estimate (Std. Error, p-value)", "Model 2 Estimate (Std. Error, p-value)", "Model 3 Estimate (Std. Error, p-value)")

# Add R Squared, F-Value, and Degrees of Freedom for Each Model
results_table_final[9,] <- c("R-Squared", sprintf("%.3f", summary(normsocialmod)$r.squared), sprintf("%.3f", summary(normsocialmod2)$r.squared), sprintf("%.3f", summary(normefficacymod)$r.squared))
results_table_final[10,] <- c("F-Value", sprintf("%.3f", summary(normsocialmod)$fstatistic[1]), sprintf("%.3f", summary(normsocialmod2)$fstatistic[1]), sprintf("%.3f", summary(normefficacymod)$fstatistic[1]))
results_table_final[11,] <- c("DF", paste(summary(normsocialmod)$fstatistic[2], summary(normsocialmod)$fstatistic[3], sep = "/"), paste(summary(normsocialmod2)$fstatistic[2], summary(normefficacymod)$fstatistic[3], sep = "/"), paste(summary(normefficacymod)$fstatistic[2], summary(normefficacymod)$fstatistic[3], sep = "/"))

# Output the Table in a Text Format
cat(paste(capture.output(results_table_final), collapse = "\\\\\\n"))

##Sufficiency models----
###Main ----
mod_sufficieny_main<- lm(sufficiency ~ message_framing +
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
                           climate_scores + 
                           meanflood +
                           MD_index, data = combined_data)
autoplot(mod_sufficieny_main)
summary(mod_sufficieny_main)
output <- Anova(mod_sufficieny_main)
output

adjustedpsuf <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf

confint(mod_sufficieny_main)

###Interactions with MF----

####Interaction 1: MF and nudge----
mod_sufficieny_int_1<- lm(sufficiency ~ message_framing *
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
                           climate_scores + 
                           meanflood +
                           MD_index, data = combined_data)
autoplot(mod_sufficieny_int_1)
summary(mod_sufficieny_int_1)
output <- Anova(mod_sufficieny_int_1)
output

confint(mod_sufficieny_int_1)

adjustedpsuf_int_1 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_1

####Interaction 2: MF and self efficacy----
mod_sufficieny_int_2<- lm(sufficiency ~ message_framing *
                            efficacy +
                            nudge + 
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
autoplot(mod_sufficieny_int_2)
summary(mod_sufficieny_int_2)
output <- Anova(mod_sufficieny_int_2)
output

confint(mod_sufficieny_int_2)

adjustedpsuf_int_2 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_2

####Interacion 3: MF * connection ----
mod_sufficieny_int_3<- lm(sufficiency ~ message_framing *
                            connectedness +
                            efficacy +
                            nudge + 
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
autoplot(mod_sufficieny_int_3)
summary(mod_sufficieny_int_3)
output <- Anova(mod_sufficieny_int_3)
output

confint(mod_sufficieny_int_3)

adjustedpsuf_int_3 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_3

####Interacion 4: MF * experience of developing countries ----
mod_sufficieny_int_4<- lm(sufficiency ~ message_framing *
                            experience + 
                            connectedness +
                            efficacy +
                            nudge + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            ego + 
                            climate_scores + 
                            meanflood +
                            MD_index, data = combined_data)
autoplot(mod_sufficieny_int_4)
summary(mod_sufficieny_int_4)
output <- Anova(mod_sufficieny_int_4)
output

confint(mod_sufficieny_int_4)

adjustedpsuf_int_4 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_4

####Interacion 5: MF * benefits ----
mod_sufficieny_int_5<- lm(sufficiency ~ message_framing *
                            ego + 
                            experience + 
                            connectedness +
                            efficacy +
                            nudge + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            climate_scores + 
                            meanflood +
                            MD_index, data = combined_data)
autoplot(mod_sufficieny_int_5)
summary(mod_sufficieny_int_5)
output <- Anova(mod_sufficieny_int_5)
output

confint(mod_sufficieny_int_5)

adjustedpsuf_int_5 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_5

####Interacion 6: MF * climate scores ----
mod_sufficieny_int_6<- lm(sufficiency ~ message_framing *
                            climate_scores +
                            ego + 
                            experience + 
                            connectedness +
                            efficacy +
                            nudge + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            meanflood +
                            MD_index, data = combined_data)
autoplot(mod_sufficieny_int_6)
summary(mod_sufficieny_int_6)
output <- Anova(mod_sufficieny_int_6)
output

confint(mod_sufficieny_int_6)

adjustedpsuf_int_6 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_6

####Interacion 7: MF * flood ----
mod_sufficieny_int_7<- lm(sufficiency ~ message_framing *
                            meanflood +
                            climate_scores +
                            ego + 
                            experience + 
                            connectedness +
                            efficacy +
                            nudge + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            MD_index, data = combined_data)
autoplot(mod_sufficieny_int_7)
summary(mod_sufficieny_int_7)
output <- Anova(mod_sufficieny_int_7)
output

confint(mod_sufficieny_int_7)

adjustedpsuf_int_7 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_7

### Interactions with nudge ----
####Interaction 8: nudge and self efficacy----
mod_sufficieny_int_8<- lm(sufficiency ~ nudge*
                            efficacy +
                            message_framing + 
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
autoplot(mod_sufficieny_int_8)
summary(mod_sufficieny_int_8)
output <- Anova(mod_sufficieny_int_8)
output

confint(mod_sufficieny_int_8)

adjustedpsuf_int_8 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_8

####Interacion 9: Nudge * connection ----
mod_sufficieny_int_9 <- lm(sufficiency ~ nudge *
                            connectedness +
                            efficacy +
                            message_framing + 
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
autoplot(mod_sufficieny_int_9)
summary(mod_sufficieny_int_9)
output <- Anova(mod_sufficieny_int_9)
output

confint(mod_sufficieny_int_9)

adjustedpsuf_int_9 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_9

####Interacion 10: nudge * experience of developing countries ----
mod_sufficieny_int_10<- lm(sufficiency ~ nudge *
                            experience + 
                            connectedness +
                            efficacy +
                            message_framing  + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            ego + 
                            climate_scores + 
                            meanflood +
                            MD_index, data = combined_data)
autoplot(mod_sufficieny_int_10)
summary(mod_sufficieny_int_10)
output <- Anova(mod_sufficieny_int_10)
output

confint(mod_sufficieny_int_10)

adjustedpsuf_int_10 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_10

####Interacion 11: nudge * benefits ----
mod_sufficieny_int_11<- lm(sufficiency ~  nudge *
                            ego + 
                            experience + 
                            connectedness +
                            efficacy +
                            message_framing + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            climate_scores + 
                            meanflood +
                            MD_index, data = combined_data)
autoplot(mod_sufficieny_int_11)
summary(mod_sufficieny_int_11)
output <- Anova(mod_sufficieny_int_11)
output

confint(mod_sufficieny_int_11)

adjustedpsuf_int_11 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_11

####Interacion 12: nudge * climate scores ----
mod_sufficieny_int_12<- lm(sufficiency ~  nudge *
                            climate_scores +
                            ego + 
                            experience + 
                            connectedness +
                            efficacy +
                            message_framing + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            meanflood +
                            MD_index, data = combined_data)
autoplot(mod_sufficieny_int_12)
summary(mod_sufficieny_int_12)
output <- Anova(mod_sufficieny_int_12)
output

confint(mod_sufficieny_int_12)

adjustedpsuf_int_12 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_12

####Interacion 13: nudge * flood ----
mod_sufficieny_int_13<- lm(sufficiency ~ nudge *
                            meanflood +
                            climate_scores +
                            ego + 
                            experience + 
                            connectedness +
                            efficacy +
                            message_framing + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            MD_index, data = combined_data)
autoplot(mod_sufficieny_int_13)
summary(mod_sufficieny_int_13)
output <- Anova(mod_sufficieny_int_13)
output

confint(mod_sufficieny_int_13)

adjustedpsuf_int_13 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsuf_int_13


##Sympathy models----
###Main ----
mod_sympathy_main<- lm(sympathy ~ message_framing+
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
                         meanflood +
                         MD_index, data = combined_data)
autoplot(mod_sympathy_main)
summary(mod_sympathy_main)
output <- Anova(mod_sympathy_main)
output

adjustedpsymp <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 15)
adjustedpsymp

list.pvalues.symp.int <- c("0.742",
                           "0.793",
                           "0.378",
                           "0.620",
                           "0.664",
                           "0.976",
                           "0.001",
                           "0.425",
                           "0.848",
                           "0.696",
                           "0.134",
                           "0.508",
                           "0.750",
                           "0.906",
                           "0.588",
                           output$`Pr(>F)`)

list.pvalues.symp.int

adjustedpsymp.int <- round(p.adjust(list.pvalues.symp.int, method="fdr"), digits = 4)
adjustedpsymp.int


###Interactions with MF----

####Interaction 1: MF and nudge----
mod_sympathy_int_1<- lm(sympathy ~ message_framing *
                            nudge + 
                            efficacy + 
                            social_norm +
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
autoplot(mod_sympathy_int_1)
summary(mod_sympathy_int_1)
output <- Anova(mod_sympathy_int_1)
output

confint(mod_sympathy_int_1)

adjustedpsymp_int_1 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_1

####Interaction 2: MF and self efficacy----
mod_sympathy_int_2<- lm(sympathy ~ message_framing *
                            efficacy +
                            nudge + 
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
autoplot(mod_sympathy_int_2)
summary(mod_sympathy_int_2)
output <- Anova(mod_sympathy_int_2)
output

confint(mod_sympathy_int_2)

adjustedpsymp_int_2 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_2

####Interacion 3: MF * connection ----
mod_sympathy_int_3<- lm(sympathy ~ message_framing *
                            connectedness +
                            efficacy +
                            nudge + 
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
autoplot(mod_sympathy_int_3)
summary(mod_sympathy_int_3)
output <- Anova(mod_sympathy_int_3)
output

confint(mod_sympathy_int_3)

adjustedpsymp_int_3 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_3

####Interacion 4: MF * experience of developing countries ----
mod_sympathy_int_4<- lm(sympathy ~ message_framing *
                            experience + 
                            connectedness +
                            efficacy +
                            nudge + 
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
autoplot(mod_sympathy_int_4)
summary(mod_sympathy_int_4)
output <- Anova(mod_sympathy_int_4)
output

confint(mod_sympathy_int_4)

adjustedpsymp_int_4 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_4

####Interacion 5: MF * benefits ----
mod_sympathy_int_5<- lm(sympathy ~ message_framing *
                            ego + 
                            experience + 
                            connectedness +
                            efficacy +
                            nudge + 
                            social_norm +
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            climate_scores + 
                            meanflood +
                            MD_index, data = combined_data)
autoplot(mod_sympathy_int_5)
summary(mod_sympathy_int_5)
output <- Anova(mod_sympathy_int_5)
output

confint(mod_sympathy_int_5)

adjustedpsymp_int_5 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_5

####Interacion 6: MF * climate scores ----
mod_sympathy_int_6<- lm(sympathy ~ message_framing *
                            climate_scores +
                            ego + 
                            experience + 
                            connectedness +
                            social_norm +
                            efficacy +
                            nudge + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            meanflood +
                            MD_index, data = combined_data)
autoplot(mod_sympathy_int_6)
summary(mod_sympathy_int_6)
output <- Anova(mod_sympathy_int_6)
output

confint(mod_sympathy_int_6)

adjustedpsymp_int_6 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_6

####Interacion 7: MF * flood ----
mod_sympathy_int_7<- lm(sympathy ~ message_framing *
                            meanflood +
                            climate_scores +
                            ego + 
                            experience + 
                            social_norm + 
                            connectedness +
                            efficacy +
                            nudge + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            MD_index, data = combined_data)
autoplot(mod_sympathy_int_7)
summary(mod_sympathy_int_7)
output <- Anova(mod_sympathy_int_7)
output

confint(mod_sympathy_int_7)

adjustedpsymp_int_7 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_7

#Interaction 7.5 - MF * social norm
mod_sympathy_int_7.5<- lm(sympathy ~ message_framing *
                          social_norm +
                          meanflood +
                          climate_scores +
                          ego + 
                          experience +
                          connectedness +
                          efficacy +
                          nudge + 
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          MD_index, data = combined_data)
autoplot(mod_sympathy_int_7.5)
summary(mod_sympathy_int_7.5)
output <- Anova(mod_sympathy_int_7.5)
output

confint(mod_sympathy_int_7.5)

adjustedpsymp_int_7.5 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_7.5

### Interactions with nudge ----
####Interaction 8: nudge and self efficacy----
mod_sympathy_int_8<- lm(sympathy ~ nudge*
                            efficacy +
                            message_framing + 
                            connectedness + 
                            finance_security +
                            social_norm +
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            experience + 
                            ego + 
                            climate_scores + 
                            meanflood +
                            MD_index, data = combined_data)
autoplot(mod_sympathy_int_8)
summary(mod_sympathy_int_8)
output <- Anova(mod_sympathy_int_8)
output

confint(mod_sympathy_int_8)

adjustedpsymp_int_8 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_8

####Interacion 9: Nudge * connection ----
mod_sympathy_int_9 <- lm(sympathy ~ nudge *
                             connectedness +
                             efficacy +
                             message_framing + 
                             finance_security + 
                             social_norm +
                             age +
                             gender +
                             ethnicity +
                             education_rank +
                             experience + 
                             ego + 
                             climate_scores + 
                             meanflood +
                             MD_index, data = combined_data)
autoplot(mod_sympathy_int_9)
summary(mod_sympathy_int_9)
output <- Anova(mod_sympathy_int_9)
output

confint(mod_sympathy_int_9)

adjustedpsymp_int_9 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_9

####Interacion 10: nudge * experience of developing countries ----
mod_sympathy_int_10<- lm(sympathy ~ nudge *
                             experience + 
                             connectedness +
                             efficacy +
                             social_norm +
                             message_framing  + 
                             finance_security + 
                             age +
                             gender +
                             ethnicity +
                             education_rank +
                             ego + 
                             climate_scores + 
                             meanflood +
                             MD_index, data = combined_data)
autoplot(mod_sympathy_int_10)
summary(mod_sympathy_int_10)
output <- Anova(mod_sympathy_int_10)
output

confint(mod_sympathy_int_10)

adjustedpsymp_int_10 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_10

####Interacion 11: nudge * benefits ----
mod_sympathy_int_11<- lm(sympathy ~  nudge *
                             ego + 
                             experience + 
                             connectedness +
                             efficacy +
                             social_norm +
                             message_framing + 
                             finance_security + 
                             age +
                             gender +
                             ethnicity +
                             education_rank +
                             climate_scores + 
                             meanflood +
                             MD_index, data = combined_data)
autoplot(mod_sympathy_int_11)
summary(mod_sympathy_int_11)
output <- Anova(mod_sympathy_int_11)
output

confint(mod_sympathy_int_11)

adjustedpsymp_int_11 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_11

####Interacion 12: nudge * climate scores ----
mod_sympathy_int_12<- lm(sympathy ~  nudge *
                             climate_scores +
                             ego + 
                             experience + 
                             connectedness +
                             efficacy +
                             social_norm +
                             message_framing + 
                             finance_security + 
                             age +
                             gender +
                             ethnicity +
                             education_rank +
                             meanflood +
                             MD_index, data = combined_data)
autoplot(mod_sympathy_int_12)
summary(mod_sympathy_int_12)
output <- Anova(mod_sympathy_int_12)
output

confint(mod_sympathy_int_12)

adjustedpsymp_int_12 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_12

####Interacion 13: nudge * flood ----
mod_sympathy_int_13<- lm(sympathy ~ nudge *
                             meanflood +
                             climate_scores +
                             ego + 
                             experience + 
                             social_norm +
                             connectedness +
                             efficacy +
                             message_framing + 
                             finance_security + 
                             age +
                             gender +
                             ethnicity +
                             education_rank +
                             MD_index, data = combined_data)
autoplot(mod_sympathy_int_13)
summary(mod_sympathy_int_13)
output <- Anova(mod_sympathy_int_13)
output

confint(mod_sympathy_int_13)

adjustedpsymp_int_13 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_13

####Interacion 14: nudge * social norm ----
mod_sympathy_int_14<- lm(sympathy ~ nudge *
                           social_norm +
                           meanflood +
                           climate_scores +
                           ego + 
                           experience + 
                           connectedness +
                           efficacy +
                           message_framing + 
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           MD_index, data = combined_data)
autoplot(mod_sympathy_int_14)
summary(mod_sympathy_int_14)
output <- Anova(mod_sympathy_int_14)
output

confint(mod_sympathy_int_14)

adjustedpsymp_int_14 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpsymp_int_14


##Behaviour models----

###Main ----
mod_behaviour_main<- lm(behaviour ~ message_framing +
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
                          climate_scores+ 
                          meanflood+
                          MD_index, data = combined_data)
autoplot(mod_behaviour_main) #ok
summary(mod_behaviour_main)
output <- Anova(mod_behaviour_main)
output

confint(mod_behaviour_main)

adjustedpbehav <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 12)
adjustedpbehav #check FDR adjusted

list_pvalues_behav_int <- c("0.500",
                            "0.092",
                            "0.593",
                            "0.845",
                            "0.426",
                            "0.063",
                            "0.900",
                            "0.593",
                            "0.524",
                            "0.809",
                            "0.013",
                            "0.736",
                            "0.683",
                            "0.128",
                            "0.909",
                            output$`Pr(>F)`)

adjustedpbehav.int <- round(p.adjust(list_pvalues_behav_int,method="fdr"), digits = 4)
adjustedpbehav.int #check FDR adjusted

###Interactions with MF----

####Interaction 1: MF and nudge----
mod_behaviour_int_1<- lm(behaviour ~ message_framing *
                          nudge + 
                          efficacy + 
                          social_norm + 
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
autoplot(mod_behaviour_int_1)
summary(mod_behaviour_int_1)
output <- Anova(mod_behaviour_int_1)
output

confint(mod_behaviour_int_1)

adjustedpbhav_int_1 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_1

####Interaction 2: MF and self efficacy----
mod_behaviour_int_2<- lm(behaviour ~ message_framing *
                          efficacy +
                          social_norm + 
                          nudge + 
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
autoplot(mod_behaviour_int_2)
summary(mod_behaviour_int_2)
output <- Anova(mod_behaviour_int_2)
output

confint(mod_behaviour_int_2)

adjustedpbhav_int_2 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_2

####Interacion 3: MF * connection ----
mod_behaviour_int_3<- lm(behaviour ~ message_framing *
                          connectedness +
                          efficacy +
                          social_norm + 
                          nudge + 
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
autoplot(mod_behaviour_int_3)
summary(mod_behaviour_int_3)
output <- Anova(mod_behaviour_int_3)
output

confint(mod_behaviour_int_3)

adjustedpbhav_int_3 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_3

####Interacion 4: MF * experience of developing countries ----
mod_behaviour_int_4<- lm(behaviour ~ message_framing *
                          experience + 
                          connectedness +
                          efficacy +
                          social_norm + 
                          nudge + 
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          ego + 
                          climate_scores + 
                          meanflood +
                          MD_index, data = combined_data)
autoplot(mod_behaviour_int_4)
summary(mod_behaviour_int_4)
output <- Anova(mod_behaviour_int_4)
output

confint(mod_behaviour_int_4)

adjustedpbhav_int_4 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_4

####Interacion 5: MF * benefits ----
mod_behaviour_int_5<- lm(behaviour ~ message_framing *
                          ego + 
                          experience + 
                          connectedness +
                          efficacy +
                          social_norm + 
                          nudge + 
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          climate_scores + 
                          meanflood +
                          MD_index, data = combined_data)
autoplot(mod_behaviour_int_5)
summary(mod_behaviour_int_5)
output <- Anova(mod_behaviour_int_5)
output

confint(mod_behaviour_int_5)

adjustedpbhav_int_5 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_5

####Interacion 6: MF * climate scores ----
mod_behaviour_int_6<- lm(behaviour ~ message_framing *
                          climate_scores +
                          ego + 
                          experience + 
                          connectedness +
                          efficacy +
                          social_norm + 
                          nudge + 
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          meanflood +
                          MD_index, data = combined_data)
autoplot(mod_behaviour_int_6)
summary(mod_behaviour_int_6)
output <- Anova(mod_behaviour_int_6)
output

confint(mod_behaviour_int_6)

adjustedpbhav_int_6 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_6

####Interacion 7: MF * flood ----
mod_behaviour_int_7<- lm(behaviour ~ message_framing *
                          meanflood +
                          climate_scores +
                          ego + 
                          experience + 
                          connectedness +
                          efficacy +
                          social_norm + 
                          nudge + 
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          MD_index, data = combined_data)
autoplot(mod_behaviour_int_7)
summary(mod_behaviour_int_7)
output <- Anova(mod_behaviour_int_7)
output

confint(mod_behaviour_int_7)

adjustedpbhav_int_7 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_7

####Interacion 7: MF * social norm ----
mod_behaviour_int_7.5<- lm(behaviour ~ message_framing *
                           social_norm +
                           meanflood +
                           climate_scores +
                           ego + 
                           experience + 
                           connectedness +
                           efficacy +
                           nudge + 
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           MD_index, data = combined_data)
autoplot(mod_behaviour_int_7.5)
summary(mod_behaviour_int_7.5)
output <- Anova(mod_behaviour_int_7.5)
output

confint(mod_behaviour_int_7.5)

adjustedpbhav_int_7.5 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_7.5

### Interactions with nudge ----
####Interaction 8: nudge and self efficacy----
mod_behaviour_int_8<- lm(behaviour ~ nudge*
                          efficacy +
                          message_framing + 
                          connectedness + 
                          finance_security + 
                          age +
                          gender +
                          ethnicity +
                          education_rank +
                          experience + 
                          social_norm + 
                          ego + 
                          climate_scores + 
                          meanflood +
                          MD_index, data = combined_data)
autoplot(mod_behaviour_int_8)
summary(mod_behaviour_int_8)
output <- Anova(mod_behaviour_int_8)
output

confint(mod_behaviour_int_8)

adjustedpbhav_int_8 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_8

####Interacion 9: Nudge * connection ----
mod_behaviour_int_9 <- lm(behaviour ~ nudge *
                           connectedness +
                           efficacy +
                           message_framing + 
                           finance_security +
                           social_norm + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           experience + 
                           ego + 
                           climate_scores + 
                           meanflood +
                           MD_index, data = combined_data)
autoplot(mod_behaviour_int_9)
summary(mod_behaviour_int_9)
output <- Anova(mod_behaviour_int_9)
output

confint(mod_behaviour_int_9)

adjustedpbhav_int_9 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_9

####Interacion 10: nudge * experience of developing countries ----
mod_behaviour_int_10<- lm(behaviour ~ nudge *
                           experience + 
                           connectedness +
                           efficacy +
                           social_norm + 
                           message_framing  + 
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           ego + 
                           climate_scores + 
                           meanflood +
                           MD_index, data = combined_data)
autoplot(mod_behaviour_int_10)
summary(mod_behaviour_int_10)
output <- Anova(mod_behaviour_int_10)
output

confint(mod_behaviour_int_10)

adjustedpbhav_int_10 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_10

####Interacion 11: nudge * benefits ----
mod_behaviour_int_11<- lm(behaviour ~  nudge *
                           ego + 
                           experience + 
                           connectedness +
                           efficacy +
                           social_norm + 
                           message_framing + 
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           climate_scores + 
                           meanflood +
                           MD_index, data = combined_data)
autoplot(mod_behaviour_int_11)
summary(mod_behaviour_int_11)
output <- Anova(mod_behaviour_int_11)
output

confint(mod_behaviour_int_11)

adjustedpbhav_int_11 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_11

####Interacion 12: nudge * climate scores ----
mod_behaviour_int_12<- lm(behaviour ~  nudge *
                           climate_scores +
                           ego + 
                           experience + 
                           connectedness +
                           efficacy +
                           social_norm + 
                           message_framing + 
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           meanflood +
                           MD_index, data = combined_data)
autoplot(mod_behaviour_int_12)
summary(mod_behaviour_int_12)
output <- Anova(mod_behaviour_int_12)
output

confint(mod_behaviour_int_12)

adjustedpbhav_int_12 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_12

####Interacion 13: nudge * flood ----
mod_behaviour_int_13<- lm(behaviour ~ nudge *
                           meanflood +
                           climate_scores +
                           ego + 
                           experience + 
                           connectedness +
                           efficacy +
                           social_norm + 
                           message_framing + 
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           MD_index, data = combined_data)
autoplot(mod_behaviour_int_13)
summary(mod_behaviour_int_13)
output <- Anova(mod_behaviour_int_13)
output

confint(mod_behaviour_int_13)

adjustedpbhav_int_13 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_13

####Interacion 14: nudge * social norm ----
mod_behaviour_int_14<- lm(behaviour ~ nudge *
                            social_norm +
                            meanflood +
                            climate_scores +
                            ego + 
                            experience + 
                            connectedness +
                            efficacy +
                            message_framing + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            MD_index, data = combined_data)
autoplot(mod_behaviour_int_14)
summary(mod_behaviour_int_14)
output <- Anova(mod_behaviour_int_14)
output

confint(mod_behaviour_int_14)

adjustedpbhav_int_14 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpbhav_int_14

##Financial models----

###Main ----
mod_financial_main<- glm(financial ~ message_framing+
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
                           ego + 
                           climate_scores + 
                           meanflood+
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_main)
summary(mod_financial_main)
output <- Anova(mod_financial_main, test = "F")
output

with(summary(mod_financial_main), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_main, mod_0, test="F") #F stats and p-value

confint(mod_financial_main)

adjustedpfin <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 11)
adjustedpfin

###Interactions with MF ----

#### Interaction 1: MF + nudge ----
mod_financial_int_1<- glm(financial ~ message_framing*
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
                           ego + 
                           climate_scores + 
                           meanflood+
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int_1)
summary(mod_financial_int_1)
output <- Anova(mod_financial_int_1, test = "F")
output

with(summary(mod_financial_int_1), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_1, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_1)

adjustedpfin_int_1 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_1

####Interaction 2: MF + efficacy ----
mod_financial_int_2<- glm(financial ~ message_framing*
                            efficacy + 
                            nudge + 
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
plot(mod_financial_int_2)
summary(mod_financial_int_2)
output <- Anova(mod_financial_int_2, test = "F")
output

with(summary(mod_financial_int_2), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_2, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_2)

adjustedpfin_int_2 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_2

#### Interaction 3: MF + connection ----
mod_financial_int_3<- glm(financial ~ message_framing*
                            connectedness +
                            efficacy + 
                            nudge + 
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
plot(mod_financial_int_3)
summary(mod_financial_int_3)
output <- Anova(mod_financial_int_3, test = "F")
output

with(summary(mod_financial_int_3), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_3, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_3)

adjustedpfin_int_3 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_3

####Interaction 4: MF + experience -----
mod_financial_int_4<- glm(financial ~ message_framing*
                            experience +
                            connectedness +
                            efficacy + 
                            nudge + 
                            log(1 + social_norm_donation) +
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            ego + 
                            climate_scores + 
                            meanflood+
                            MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int_4)
summary(mod_financial_int_4)
output <- Anova(mod_financial_int_4, test = "F")
output

with(summary(mod_financial_int_4), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_4, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_4)

adjustedpfin_int_4 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_4

####Interaction 5: benefits ----
mod_financial_int_5<- glm(financial ~ message_framing*
                            ego + 
                            experience +
                            connectedness +
                            efficacy + 
                            nudge + 
                            log(1 + social_norm_donation) +
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            climate_scores + 
                            meanflood+
                            MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int_5)
summary(mod_financial_int_5)
output <- Anova(mod_financial_int_5, test = "F")
output

with(summary(mod_financial_int_5), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_5, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_5)

adjustedpfin_int_5 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_5

####interaction 6 - MF + social norm ----
mod_financial_int_6<- glm(financial ~ message_framing*
                            log(1 + social_norm_donation) +
                            ego + 
                            experience +
                            connectedness +
                            efficacy + 
                            nudge + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            climate_scores + 
                            meanflood+
                            MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int_6)
summary(mod_financial_int_6)
output <- Anova(mod_financial_int_6, test = "F")
output

with(summary(mod_financial_int_6), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_6, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_6)

adjustedpfin_int_6 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_6

####interaction 7 - MF + climate scores ----
mod_financial_int_7<- glm(financial ~ message_framing*
                            climate_scores + 
                            log(1 + social_norm_donation) +
                            ego + 
                            experience +
                            connectedness +
                            efficacy + 
                            nudge + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            meanflood+
                            MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int_7)
summary(mod_financial_int_7)
output <- Anova(mod_financial_int_7, test = "F")
output

with(summary(mod_financial_int_7), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_7, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_7)

adjustedpfin_int_7 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_7

####interaction 8 - MF + flood ----
mod_financial_int_8<- glm(financial ~ message_framing*
                            meanflood+
                            climate_scores + 
                            log(1 + social_norm_donation) +
                            ego + 
                            experience +
                            connectedness +
                            efficacy + 
                            nudge + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int_8)
summary(mod_financial_int_8)
output <- Anova(mod_financial_int_8, test = "F")
output

with(summary(mod_financial_int_8), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_8, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_8)

adjustedpfin_int_8 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_8

### Interactions with nudge ----
####Interaction 9: nudge + efficacy ----
mod_financial_int_9<- glm(financial ~ nudge*
                            efficacy + 
                            message_framing + 
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
plot(mod_financial_int_9)
summary(mod_financial_int_9)
output <- Anova(mod_financial_int_9, test = "F")
output

with(summary(mod_financial_int_9), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_9, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_9)

adjustedpfin_int_9 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_9

#### Interaction 10: nudge + connection ----
mod_financial_int_10<- glm(financial ~ nudge*
                            connectedness +
                            efficacy + 
                            message_framing + 
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
plot(mod_financial_int_10)
summary(mod_financial_int_10)
output <- Anova(mod_financial_int_10, test = "F")
output

with(summary(mod_financial_int_10), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_10, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_10)

adjustedpfin_int_10 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_10

####Interaction 11: nudge + experience -----
mod_financial_int_11<- glm(financial ~ nudge*
                            experience +
                            connectedness +
                            efficacy + 
                            message_framing + 
                            log(1 + social_norm_donation) +
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            ego + 
                            climate_scores + 
                            meanflood+
                            MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int_11)
summary(mod_financial_int_11)
output <- Anova(mod_financial_int_11, test = "F")
output

with(summary(mod_financial_int_11), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_11, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_11)

adjustedpfin_int_11 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_11

####Interaction 12: nudge + benefits ----
mod_financial_int_12<- glm(financial ~ nudge*
                            ego + 
                            experience +
                            connectedness +
                            efficacy + 
                            message_framing + 
                            log(1 + social_norm_donation) +
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            climate_scores + 
                            meanflood+
                            MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int_12)
summary(mod_financial_int_12)
output <- Anova(mod_financial_int_12, test = "F")
output

with(summary(mod_financial_int_12), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_12, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_12)

adjustedpfin_int_12 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_12

####interaction 13 - nudge + social norm ----
mod_financial_int_13<- glm(financial ~ nudge*
                            log(1 + social_norm_donation) +
                            ego + 
                            experience +
                            connectedness +
                            efficacy + 
                            message_framing+ 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            climate_scores + 
                            meanflood+
                            MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int_13)
summary(mod_financial_int_13)
output <- Anova(mod_financial_int_13, test = "F")
output

with(summary(mod_financial_int_13), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_13, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_13)

adjustedpfin_int_13 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_13

####interaction 14 - nudge + climate scores ----
mod_financial_int_14<- glm(financial ~ nudge*
                            climate_scores + 
                            log(1 + social_norm_donation) +
                            ego + 
                            experience +
                            connectedness +
                            efficacy + 
                            message_framing + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            meanflood+
                            MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int_14)
summary(mod_financial_int_14)
output <- Anova(mod_financial_int_14, test = "F")
output

with(summary(mod_financial_int_14), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_14, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_14)

adjustedpfin_int_14 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_14

####interaction 15 - nudge + flood ----
mod_financial_int_15<- glm(financial ~ nudge*
                            meanflood+
                            climate_scores + 
                            log(1 + social_norm_donation) +
                            ego + 
                            experience +
                            connectedness +
                            efficacy + 
                            message_framing + 
                            finance_security + 
                            age +
                            gender +
                            ethnicity +
                            education_rank +
                            MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_int_15)
summary(mod_financial_int_15)
output <- Anova(mod_financial_int_15, test = "F")
output

with(summary(mod_financial_int_15), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_int_15, mod_0, test="F") #F stats and p-value

confint(mod_financial_int_15)

adjustedpfin_int_15 <- round(p.adjust(output$`Pr(>F)`,method="fdr"), digits = 4)
adjustedpfin_int_15

#check for overdispersion
#summary(mod_financial_int1)$deviance/summary(mod_financial)$df.residual
#Anova(mod_financial_int1, test = "F"

#Create output table for manuscript---------------------------------------------

# Extract Parameter Estimates, Standard Errors, and P-Values and Create a Table with the Results
estimates_1 <- sprintf("%.3f", summary(mod_behaviour_main)$coefficients[-1,1])
std_errors_1 <- sprintf("(%.3f)", summary(mod_behaviour_main)$coefficients[-1,2])
p_values_1 <- sprintf("p = %.3f%s", summary(mod_behaviour_main)$coefficients[-1,4], ifelse(summary(mod_behaviour_main)$coefficients[-1,4] < 0.05, "*", ""))

results_table_1 <- data.frame(
  Predictor = rownames(summary(mod_behaviour_main)$coefficients)[-1],
  `Parameter Estimate (Std. Error)` = paste(estimates_1, std_errors_1, p_values_1, sep = " "),
  check.names = FALSE)

# Extract Parameter Estimates, Standard Errors, and P-Values and Create a Table with the Results
estimates_2 <- sprintf("%.3f", summary(mod_sympathy_main)$coefficients[-1,1])
std_errors_2 <- sprintf("(%.3f)", summary(mod_sympathy_main)$coefficients[-1,2])
p_values_2 <- sprintf("p = %.3f%s", summary(mod_sympathy_main)$coefficients[-1,4], ifelse(summary(mod_sympathy_main)$coefficients[-1,4] < 0.05, "*", ""))

results_table_2 <- data.frame(
  Predictor = rownames(summary(mod_sympathy_main)$coefficients)[-1],
  `Parameter Estimate (Std. Error)` = paste(estimates_2, std_errors_2, p_values_2, sep = " "),
  check.names = FALSE)

# Extract Parameter Estimates, Standard Errors, and P-Values and Create a Table with the Results
estimates_3 <- sprintf("%.3f", summary(mod_financial_main)$coefficients[-1,1])
std_errors_3 <- sprintf("(%.3f)", summary(mod_financial_main)$coefficients[-1,2])
p_values_3 <- sprintf("p = %.3f%s", summary(mod_financial_main)$coefficients[-1,4], ifelse(summary(mod_financial_main)$coefficients[-1,4] < 0.05, "*", ""))

results_table_3 <- data.frame(
  Predictor = rownames(summary(mod_financial_main)$coefficients)[-1],
  `Parameter Estimate (Std. Error)` = paste(estimates_3, std_errors_3, p_values_3, sep = " "),
  check.names = FALSE)

# Extract Parameter Estimates, Standard Errors, and P-Values and Create a Table with the Results
estimates_4 <- sprintf("%.3f", summary(mod_sufficieny_main)$coefficients[-1,1])
std_errors_4 <- sprintf("(%.3f)", summary(mod_sufficieny_main)$coefficients[-1,2])
p_values_4 <- sprintf("p = %.3f%s", summary(mod_sufficieny_main)$coefficients[-1,4], ifelse(summary(mod_sufficieny_main)$coefficients[-1,4] < 0.05, "*", ""))

results_table_4 <- data.frame(
  Predictor = rownames(summary(mod_sufficieny_main)$coefficients)[-1],
  `Parameter Estimate (Std. Error)` = paste(estimates_4, std_errors_4, p_values_4, sep = " "),
  check.names = FALSE)

# Merge All Tables
results_table_final <- merge(results_table_1, results_table_2, by = "Predictor", all = TRUE)
suppressWarnings(results_table_final <- merge(results_table_final, results_table_3, by = "Predictor", all = TRUE))
suppressWarnings(results_table_final <- merge(results_table_final, results_table_4, by = "Predictor", all = TRUE))

# Add Column Names
colnames(results_table_final) <- c("Predictor", "Behavioural", "Sympathetic", "Financial", "Sufficiency")

# Add R Squared, F-Value, and Degrees of Freedom for Each Model
#results_table_final[21,] <- c("R-Squared", sprintf("%.3f", summary(mod_behaviour_main)$r.squared), sprintf("%.3f", summary(mod_sympathy_main)$r.squared), sprintf("%.3f", summary(mod_financial_main)$r.squared), sprintf("%.3f", summary(mod_sufficieny_main)$r.squared))
#results_table_final[22,] <- c("F-Value", sprintf("%.3f", summary(mod_behaviour_main)$fstatistic[1]), sprintf("%.3f", summary(mod_sympathy_main)$fstatistic[1]), sprintf("%.3f", summary(mod_financial_main)$fstatistic[1]), sprintf("%.3f", summary(normefficacymod)$fstatistic[1]))
#results_table_final[23,] <- c("DF", paste(summary(mod_behaviour_main)$fstatistic[2], summary(mod_behaviour_main)$fstatistic[3], sep = "/"), 
                              #paste(summary(mod_sympathy_main)$fstatistic[2], summary(mod_sympathy_main)$fstatistic[3], sep = "/"), 
                              #paste(summary(mod_financial_main)$fstatistic[2], summary(mod_financial_main)$fstatistic[3], sep = "/"),
                              #paste(summary(mod_sufficieny_main)$fstatistic[2], summary(mod_sufficieny_main)$fstatistic[3], sep = "/"))

# With confidence intervals
# Output the Table in a Text Format
cat(paste(capture.output(results_table_final), collapse = "\\\\\\n"))


# Extract Parameter Estimates, Standard Errors, and P-Values and Create a Table with the Results
betas_1 <- summary(mod_behaviour_main)$coefficients[-1,1]
se_1 <- summary(mod_behaviour_main)$coefficients[-1,2]
ci_1 <- paste0("[", round(betas_1 - 1.96*se_1, 3), ", ", round(betas_1 + 1.96*se_1, 3), "]")
p_values_1 <- sprintf("p = %.3f%s", summary(mod_behaviour_main)$coefficients[-1,4], ifelse(summary(mod_behaviour_main)$coefficients[-1,4] < 0.05, "*", ""))

results_table_1 <- data.frame(
  Predictor = rownames(summary(mod_behaviour_main)$coefficients)[-1],
  `Parameter Estimate` = sprintf("%.3f", betas_1),
  `95% CI` = ci_1,
  `P-Value` = p_values_1,
  check.names = FALSE)

# Extract Parameter Estimates, Standard Errors, and P-Values and Create a Table with the Results
betas_2 <- summary(mod_sympathy_main)$coefficients[-1,1]
se_2 <- summary(mod_sympathy_main)$coefficients[-1,2]
ci_2 <- paste0("[", round(betas_2 - 1.96*se_2, 3), ", ", round(betas_2 + 1.96*se_2, 3), "]")
p_values_2 <- sprintf("p = %.3f%s", summary(mod_sympathy_main)$coefficients[-1,4], ifelse(summary(mod_sympathy_main)$coefficients[-1,4] < 0.05, "*", ""))

results_table_2 <- data.frame(
  Predictor = rownames(summary(mod_sympathy_main)$coefficients)[-1],
  `Parameter Estimate` = sprintf("%.3f", betas_2),
  `95% CI` = ci_2,
  `P-Value` = p_values_2,
  check.names = FALSE)

# Extract Parameter Estimates, Standard Errors, and P-Values and Create a Table with the Results
betas_3 <- summary(mod_financial_main)$coefficients[-1,1]
se_3 <- summary(mod_financial_main)$coefficients[-1,2]
ci_3 <- paste0("[", round(betas_3 - 1.96*se_3, 3), ", ", round(betas_3 + 1.96*se_3, 3), "]")
p_values_3 <- sprintf("p = %.3f%s", summary(mod_financial_main)$coefficients[-1,4], ifelse(summary(mod_financial_main)$coefficients[-1,4] < 0.05, "*", ""))

results_table_3 <- data.frame(
  Predictor = rownames(summary(mod_financial_main)$coefficients)[-1],
  `Parameter Estimate` = sprintf("%.3f", betas_3),
  `95% CI` = ci_3,
  `P-Value` = p_values_3,
  check.names = FALSE)

# Extract Parameter Estimates, Standard Errors, and P-Values and Create a Table with the Results
betas_4 <- summary(mod_sufficieny_main)$coefficients[-1,1]
se_4 <- summary(mod_sufficieny_main)$coefficients[-1,2]
ci_4 <- paste0("[", round(betas_4 - 1.96*se_4, 3), ", ", round(betas_4 + 1.96*se_4, 3), "]")
p_values_4 <- sprintf("p = %.3f%s", summary(mod_sufficieny_main)$coefficients[-1,4], ifelse(summary(mod_sufficieny_main)$coefficients[-1,4] < 0.05, "*", ""))

results_table_4 <- data.frame(
  Predictor = rownames(summary(mod_sufficieny_main)$coefficients)[-1],
  `Parameter Estimate` = sprintf("%.3f", betas_4),
  `95% CI` = ci_4,
  `P-Value` = p_values_4,
  check.names = FALSE)

# Merge All Tables
results_table_final <- merge(results_table_1, results_table_2, by = "Predictor", all = TRUE)
results_table_final <- merge(results_table_final, results_table_3, by = "Predictor", all = TRUE)
results_table_final <- merge(results_table_final, results_table_4, by = "Predictor", all = TRUE)

# Add Column Names
colnames(results_table_final) <- c("Predictor", "Behavioural_E", "Behv_CI", "Behv_p", "Sympathetic", "Sympathetic", "Sympathetic", "Financial","Financial","Financial", "Sufficiency","Sufficiency","Sufficiency")


#Plots--------------------------------------------------------------------------

##Mean and SE summaries for nudge----

nudge.finance <- combined_data %>% 
  group_by(nudge) %>% 
  summarise(mean = mean(financial), 
            sd = sd(financial),
            se = sd(financial)/sqrt(n()),
            samp_size = n())

nudge.behaviour <- combined_data %>% 
  group_by(nudge) %>% 
  summarise(mean = mean(behaviour), 
            sd = sd(behaviour),
            se = sd(behaviour)/sqrt(n()),
            samp_size = n())

nudge.sufficiency <- combined_data %>% 
  group_by(nudge) %>% 
  summarise(mean = mean(sufficiency), 
            sd = sd(sufficiency),
            se = sd(sufficiency)/sqrt(n()),
            samp_size = n())

nudge.sympathy <- combined_data %>% 
  group_by(nudge) %>% 
  summarise(mean = mean(sympathy), 
            sd = sd(sympathy),
            se = sd(sympathy)/sqrt(n()),
            samp_size = n())

##Nudge plot----

nf <- ggplot(nudge.finance, aes(x=nudge, y=mean, colour = nudge))+
  scale_color_manual(values=c("grey40", "sky blue"), 
                     name="Treatment",
                     breaks=c("absent", "present"),
                     labels=c("Absent", "Present"))+
  geom_pointrange(ymin=nudge.finance$mean-nudge.finance$se, 
                  ymax=nudge.finance$mean+nudge.finance$se,
                  size = 1.5)+
  #coord_cartesian(ylim = c(0, 2))+
  theme_classic()+
  theme(text = element_text(size = 13),
        legend.position = "none")+
  xlab("Nudge") + ylab("Financial support")+
  #ylim(0.95,1.26)+
  scale_y_continuous(breaks = seq(0, 1.27, by=0.05), limits=c(0.95,1.27))+
                     #sec.axis = sec_axis(~ . * 72.78892, name = "Equivalent donation (£)\n ", breaks = seq(0, 93, by=3.5)))+
  scale_x_discrete(labels = c("Absent","Present"))+
  geom_signif(comparisons = list(c("absent", "present")), 
              map_signif_level=TRUE,
              y_position = 1.25,
              colour = "black",
              annotations = "*",
              textsize = 8, 
              size = 0.7)

nf

ns <- ggplot(nudge.sufficiency, aes(x=nudge, y=mean, colour = nudge))+
  scale_color_manual(values=c("grey40", "sky blue"), 
                     name="Treatment",
                     breaks=c("absent", "present"),
                     labels=c("Absent", "Present"))+
  geom_pointrange(ymin=nudge.sufficiency$mean-nudge.sufficiency$se, 
                  ymax=nudge.sufficiency$mean+nudge.sufficiency$se,
                  size = 1.5)+
  #coord_cartesian(ylim = c(0, 2))+
  theme_classic()+
  theme(text = element_text(size = 13),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())+
  xlab("Nudge") + ylab("Advert sufficiency")+
 #ylim(11.3,13)+
  scale_y_continuous(breaks = seq(0, 13, by=0.25), limits=c(11.35,13))+
                     #sec.axis = sec_axis(~ ., name = "Equivalent score\n ", breaks = seq(0, 13, by=0.25)))+
  scale_x_discrete(labels = c("Absent","Present"))+
  geom_signif(comparisons = list(c("absent", "present")), 
              map_signif_level=TRUE,
              y_position = 12.9,
              colour = "black",
              annotations = "*",
              textsize = 8, 
              size = 0.7)
ns

nsym <- ggplot(nudge.sympathy, aes(x=nudge, y=mean, colour = nudge))+
  scale_color_manual(values=c("grey40", "sky blue"), 
                     name="Treatment",
                     breaks=c("absent", "present"),
                     labels=c("Absent", "Present"))+
  geom_pointrange(ymin=nudge.sympathy$mean-nudge.sympathy$se, 
                  ymax=nudge.sympathy$mean+nudge.sympathy$se,
                  size = 1.5)+
  #coord_cartesian(ylim = c(0, 2))+
  theme_classic()+
  theme(text = element_text(size = 13),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())+
  xlab("Nudge") + ylab("Sympathetic attitudes")+
  #ylim(31.2,32.6)+
  scale_y_continuous(breaks = seq(0, 8, by=0.05), limits=c(6.23,6.5))+
                     #sec.axis = sec_axis(~ . * 2.541596, name = "Equivalent score\n ", breaks = seq(0, 20, by=0.15)))+
  scale_x_discrete(labels = c("Absent","Present"))

nsym


nb <- ggplot(nudge.behaviour, aes(x=nudge, y=mean, colour = nudge))+
  scale_color_manual(values=c("grey40", "sky blue"), 
                     name="Treatment",
                     breaks=c("absent", "present"),
                     labels=c("Absent", "Present"))+
  geom_pointrange(ymin=nudge.behaviour$mean-nudge.behaviour$se, 
                  ymax=nudge.behaviour$mean+nudge.behaviour$se,
                  size = 1.5)+
  #coord_cartesian(ylim = c(0, 2))+
  theme_classic()+
  theme(text = element_text(size = 13),
        legend.position = "none")+
  xlab("Nudge") + ylab("Behavioural support")+
  #ylim(51,56.2)+
  scale_y_continuous(breaks = seq(0, 8, by=0.08), limits=c(3.4,3.9))+
                     #sec.axis = sec_axis(~ . * 2.776812, name = "Equivalent score\n ", breaks = seq(0, 20, by=0.3)))+
  scale_x_discrete(labels = c("Absent","Present"))+
  geom_signif(comparisons = list(c("absent", "present")), 
              map_signif_level=TRUE,
              y_position = 3.85,
              colour = "black",
              annotations = "*",
              textsize = 8, 
              size = 0.7)

nb

nudge_plot <- ggarrange(ns, nsym, nf, nb,
                        labels = c("a", "b", "c", "d"),
                        ncol = 2, nrow = 2,
                        align = 'v')
nudge_plot
ggsave(path = "Figures", filename = "nudge.png", nudge_plot, height =8, width = 6)


##Mean and SE summary for message framing----

message.finance <- combined_data %>% 
  group_by(message_framing) %>% 
  summarise(mean = mean(financial), 
            sd = sd(financial),
            se = sd(financial)/sqrt(n()),
            samp_size = n())

message.behaviour <- combined_data %>% 
  group_by(message_framing) %>% 
  summarise(mean = mean(behaviour), 
            sd = sd(behaviour),
            se = sd(behaviour)/sqrt(n()),
            samp_size = n())

message.sufficiency <- combined_data %>% 
  group_by(message_framing) %>% 
  summarise(mean = mean(sufficiency), 
            sd = sd(sufficiency),
            se = sd(sufficiency)/sqrt(n()),
            samp_size = n())

message.sympathy <- combined_data %>% 
  group_by(message_framing) %>% 
  summarise(mean = mean(sympathy), 
            sd = sd(sympathy),
            se = sd(sympathy)/sqrt(n()),
            samp_size = n())

message.sympathy$type <- "sympathy"
message.sufficiency$type <- "sufficiency"
message.behaviour$type <- "behaviour"
message.finance$type <- "finance"


message.summary<- rbind(message.sympathy,message.sufficiency,message.behaviour,message.finance)
message.summary$type = as.factor(message.summary$type)
message.summary$message_framing = as.factor(message.summary$message_framing)

##Message framing plot----

#ggplot(message.summary, aes(x=message_framing, y = mean, ymin = mean - se, ymax = mean + se)) +
  #geom_linerange(aes(color = type), size = 1, alpha = 0.5, position = position_dodge(width = 1)) +
  #geom_point(aes(color = type, shape = type), size = 2, position = position_dodge(width = 1))+
  #scale_x_discrete("Message framing", labels=c("Biodiveristy","ES-global", "ES-local")) + # here you define coordinates for A and B 
  #theme_classic()+
  #theme(text = element_text(size = 13))


#ggplot(message.summary, aes(x=type, y = log(mean), ymin = log(mean) - log(sd/2), ymax = log(mean) + log(sd/2))) +
  #geom_linerange(aes(color = message_framing), size = 1, alpha = 0.5, position = position_dodge(width = 1)) +
  #geom_point(aes(color = message_framing, shape = message_framing), size = 2, position = position_dodge(width = 1))+
  #scale_x_discrete("Support type", labels=c("Behaviour","Finance", "Suff", "Symp")) + # here you define coordinates for A and B 
  #theme_classic()+
  #theme(text = element_text(size = 13))


mf <- ggplot(message.finance, aes(x=message_framing, y=mean, colour = message_framing))+
  scale_color_manual(values=c("grey40", "sky blue", "grey60"), 
                     name="Treatment",
                     breaks=c("biodiversity", "es-global", "es-local"),
                     labels=c("Biodiversity", "ES-global", "ES-local"))+
  geom_pointrange(ymin=message.finance$mean-message.finance$se, 
                  ymax=message.finance$mean+message.finance$se,
                  size = 1.5)+
  #coord_cartesian(ylim = c(0, 2))+
  theme_classic()+
  theme(text = element_text(size = 13),
        legend.position = "none")+
  xlab(" \n Message framing") + ylab("Financial support")+
  #ylim(0.95,1.26)+
  scale_y_continuous(breaks = seq(0, 1.26, by=0.05), limits=c(0.95,1.26))+
                     #sec.axis = sec_axis(~ . * 72.78892, name = "Equivalent donation (£)\n ", breaks = seq(0, 1100, by=3.50)))+
  scale_x_discrete(labels = c("Biodiversity","ES-global", "ES-local"))

mf

ms <- ggplot(message.sufficiency, aes(x=message_framing, y=mean, colour = message_framing))+
  scale_color_manual(values=c("grey40", "sky blue", "grey60"), 
                     name="Treatment",
                     breaks=c("biodiversity", "es-global", "es-local"),
                     labels=c("Biodiversity", "ES-global", "ES-local"))+
  geom_pointrange(ymin=message.sufficiency$mean-message.sufficiency$se, 
                  ymax=message.sufficiency$mean+message.sufficiency$se,
                  size = 1.5)+
  #coord_cartesian(ylim = c(0, 2))+
  theme_classic()+
  theme(text = element_text(size = 13),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())+
  xlab(" \n Message framing") + ylab("Advert sufficiency")+
  #ylim(11.3,13)+
  scale_y_continuous(breaks = seq(0, 13, by=0.25), limits=c(11.35,13))+
                     #sec.axis = sec_axis(~ ., name = "Equivalent score\n ", breaks = seq(0, 13, by=0.25)))+
  scale_x_discrete(labels = c("Biodiversity","ES-global", "ES-local"))
ms


msym <- ggplot(message.sympathy, aes(x=message_framing, y=mean, colour = message_framing))+
  scale_color_manual(values=c("grey40", "sky blue", "grey60"), 
                     name="Treatment",
                     breaks=c("biodiversity", "es-global", "es-local"),
                     labels=c("Biodiversity", "ES-global", "ES-local"))+
  geom_pointrange(ymin=message.sympathy$mean-message.sympathy$se, 
                  ymax=message.sympathy$mean+message.sympathy$se,
                  size = 1.5)+
  #coord_cartesian(ylim = c(0, 2))+
  theme_classic()+
  theme(text = element_text(size = 13),
        legend.position = "none",
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())+
  xlab(" \n Message framing") + ylab("Sympathetic attitudes")+
  #ylim(31.2,32.6)+
  scale_y_continuous(breaks = seq(0, 8, by=0.05), limits=c(6.2,6.5))+
                     #sec.axis = sec_axis(~ . * 2.541596, name = "Equivalent score\n ", breaks = seq(0, 20, by=0.15)))+
  scale_x_discrete(labels = c("Biodiversity","ES-global", "ES-local"))

msym

mb <- ggplot(message.behaviour, aes(x=message_framing, y=mean, colour = message_framing))+
  scale_color_manual(values=c("grey40", "sky blue", "grey60"), 
                     name="Treatment",
                     breaks=c("biodiversity", "es-global", "es-local"),
                     labels=c("Biodiversity", "ES-global", "ES-local"))+
  geom_pointrange(ymin=message.behaviour$mean-message.behaviour$se, 
                  ymax=message.behaviour$mean+message.behaviour$se,
                  size=1.5)+
  #coord_cartesian(ylim = c(0, 2))+
  theme_classic()+
  theme(text = element_text(size = 13),
        legend.position = "none")+
  xlab(" \n Message framing") + ylab("Behavioural support")+
  #ylim(51,56.2)+
  scale_y_continuous(breaks = seq(0, 8, by=0.05), limits=c(3.5,3.8))+
                     #sec.axis = sec_axis(~ . * 2.776812, name = "Equivalent score\n ", breaks = seq(0, 20, by=0.15)))+
  scale_x_discrete(labels = c("Biodiversity","ES-global", "ES-local"))

mb

message_plot <- ggarrange(ms, msym, mf, mb,
                          labels = c("a", "b", "c", "d"),
                          ncol = 2, nrow = 2,
                          align = 'v')
message_plot
ggsave(path = "Figures", filename = "new_message_plot.png", message_plot, height =9, width =7)

##Scatter plots----

###Climate plot----
climateplot<- ggplot(combined_data, aes(x=climate_scores, y=sympathy^2, colour = message_framing))+
  geom_point(size=0.5) +
  geom_smooth(method = "lm", alpha = 0.08, se = T, size = 0.7)+
  theme_classic() +
  labs(x = "Climate change scepticism", y = "Sympathetic attitudes (transformed)")+
  theme(text = element_text(size = 10)) +
  guides(color=guide_legend("Message framing"), fill = "none")

climateplot<- climateplot + scale_colour_manual(values = c("black", "sky blue", "grey70"),
                                                labels = c("Biodiversity", "ES-global", "ES-local"))
climateplot

#ggsave(path = "Figures", filename = "sc_plot.png", width = 8, height = 5)


###Sufficiency plots ----
#nature_suff<- ggplot(combined_data, aes(x=connectedness, y=sufficiency))+
  #geom_point(alpha=0.2)+
  #geom_smooth(method = "lm", se=T)+
  #theme_classic()+
  #labs(x = "Nature connection", y = "Advert sufficiency")+
  #theme(text = element_text(size = 15),
        #axis.title.x = element_blank(),
        #axis.text.x = element_blank(),
        #legend.position = "none")+
  #guides(color=guide_legend("Message framing"), fill = "none")

#nature_suff

#efficacy_suff<- ggplot(combined_data, aes(x=efficacy, y=sufficiency))+
  #geom_point(alpha=0.2)+
  #geom_smooth(method = "lm", se=T)+
  #theme_classic()+
  #labs(x = "Self-efficacy", y = "Advert sufficiency")+
  #theme(text = element_text(size = 15),
        #axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.x = element_blank(),
        #legend.position = "none")+
  #guides(color=guide_legend("Message framing"), fill = "none")

#efficacy_suff

#social_suff <- ggplot(combined_data, aes(x=social_norm, y=sufficiency))+
  #geom_point(alpha=0.2)+
  #geom_smooth(method = "lm", se=T)+
  #theme_classic()+
  #labs(x = "Social norm", y = "Advert sufficiency")+
  #theme(text = element_text(size = 15),
        #axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        #axis.title.x = element_blank(),
        #legend.position = "none")+
  #guides(color=guide_legend("Message framing"), fill = "none")

#social_suff

###Sympathetic plots----

---- sympathy ~ connectedness
#updated plot using pred
#symp.mod.2 <- lm(sympathy ~ connectedness, data = combined_data)
#anova(symp.mod.2)
#pred.data.symp <- expand.grid(connectedness = 0:7)
#pred.data.symp <- mutate(pred.data.symp, sympathy = predict(symp.mod.2, pred.data.symp))
#pred.data.symp

#ggplot(pred.data.symp, aes(x = connectedness, y = sympathy)) + 
  #geom_line(color = "darkgreen", size = 1) + geom_point(data = combined_data) + 
  #xlab("Nature connection") + ylab("Sympathetic attitudes")+
  #theme_classic()

#not using pred
nature_symp <- ggplot(combined_data, aes(x=connectedness, y=sympathy))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T, color = "green3")+
  theme_classic()+
  labs(x = "Nature connection", y = "Sympathetic attitudes")+
  theme(text = element_text(size = 15),
        legend.position = "none")+
  guides(color=guide_legend("Message framing"), fill = "none")

#naturesymp<- naturesymp + scale_colour_manual(values = c("black", "sky blue", "grey70"),
                                              #labels = c("Biodiversity", "ES-global", "ES-local"))
nature_symp

---- sympathy ~ efficacy
#updated plot using pred
#symp.mod.3 <- lm(sympathy ~ efficacy, data = combined_data)
#anova(symp.mod.3)
#pred.data.symp.3 <- expand.grid(efficacy = 0:20)
#pred.data.symp.3 <- mutate(pred.data.symp.3, sympathy = predict(symp.mod.3, pred.data.symp.3))
#pred.data.symp.3

#ggplot(pred.data.symp.3, aes(x = efficacy, y = sympathy)) + 
  #geom_line(color = "green3", size = 1) + geom_point(data = combined_data) + 
  #xlab("Self-efficacy") + ylab("Sympathetic attitudes") +
  #theme_classic()

#using in-built
efficacy_symp<- ggplot(combined_data, aes(x=efficacy, y=sympathy))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T, color = "green3")+
  theme_classic()+
  labs(x = "Self-efficacy", y = "Sympathetic attitudes")+
  theme(text = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.position = "none")+
  guides(color=guide_legend("Message framing"), fill = "none")

efficacy_symp

social_symp <- ggplot(combined_data, aes(x=social_norm, y=sympathy))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T, color = "green3")+
  theme_classic()+
  labs(x = "Social norm support", y = "Sympathetic attitudes")+
  theme(text = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.position = "none")+
  guides(color=guide_legend("Message framing"), fill = "none")

social_symp

###Financial plots----

#updated plot using pred
#fin.mod.1 <- glm(financial ~ connectedness, data = combined_data, family = "quasipoisson")
#anova(fin.mod.1)
#pred.data.fin.1 <- expand.grid(connectedness = 0:7)
#pred.data.fin.1 <- mutate(pred.data.fin.1, financial = predict(fin.mod.1, pred.data.fin.1))
#pred.data.fin.1

#ggplot(pred.data.fin.1, aes(x = connectedness, y = financial)) + 
  #geom_line(color = "blue", size = 1) + geom_point(data = combined_data) + 
  #xlab("Self-efficacy") + ylab("Sympathetic attitudes") +
  #theme_classic()

#with in-built

nature_fin<- ggplot(combined_data, aes(x=connectedness, y=financial))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "glm", se=T,
              method.args = list(family = "quasipoisson"),
              color = "red4")+
  theme_classic()+
  labs(x = "Nature connection", y = "Financial support")+
  theme(text = element_text(size = 15),
        legend.position = "none")+
  scale_y_continuous(limits=c(0,6.5))+
  guides(color=guide_legend("Message framing"), fill = "none")

suppressWarnings(print(nature_fin))

#warnings ok - limited axis to not present outliers

efficacy_fin<- ggplot(combined_data, aes(x=efficacy, y=financial))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "glm", se=T,
              method.args = list(family = "quasipoisson"),
              color = "red4")+
  theme_classic()+
  labs(x = "Self-efficacy", y = "Finanical support")+
  theme(text = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.position = "none")+
  scale_y_continuous(limits=c(0,6.5))+
  guides(color=guide_legend("Message framing"), fill = "none")

suppressWarnings(print(efficacy_fin))

#warnings ok - limited axis to not present outliers

#fin.mod.3 <- glm(financial ~ log(1+social_norm_donation), data = combined_data, family = "quasipoisson")
#anova(fin.mod.3)
#pred.data.fin.3 <- expand.grid(social_norm_donation = 0:20)
#pred.data.fin.3 <- mutate(pred.data.fin.3, financial = predict(fin.mod.3, pred.data.fin.3))
#pred.data.fin.3

#ggplot(pred.data.fin.3, aes(x = log(1+social_norm_donation), y = financial)) + 
  #geom_line(color = "blue", size = 1) + geom_point(data = combined_data) + 
  #xlab("Self-efficacy") + ylab("Financial support") +
  #theme_classic()

social_fin <- ggplot(combined_data, aes(x=log(1+social_norm_donation), y=financial))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "glm", se=T,
              method.args = list(family = "quasipoisson"),
              color = "red4")+
  theme_classic()+
  labs(x = "Social norm donation (transformed) ", y = "Finanical support")+
  theme(text = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.position = "none")+
  scale_y_continuous(limits=c(0,9),
                     breaks = seq(0, 9, by=2))+
  scale_x_continuous(limits=c(0, 7))+
  guides(color=guide_legend("Message framing"), fill = "none")

suppressWarnings(print(social_fin))

#warnings ok - limited axis to not present outliers

###Behaviour plots----

nature_behav<- ggplot(combined_data, aes(x=connectedness, y=behaviour))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T)+
  theme_classic()+
  labs(x = "Nature connection", y = "Behavioural support")+
  theme(text = element_text(size = 15))+
  guides(color=guide_legend("Message framing"), fill = "none")

nature_behav

efficacy_behav<- ggplot(combined_data, aes(x=efficacy, y=behaviour))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T)+
  theme_classic()+
  labs(x = "Self-efficacy", y = "Behavioural support")+
  theme(text = element_text(size = 15),
        axis.title.y = element_blank())+
  guides(color=guide_legend("Message framing"), fill = "none")

efficacy_behav

social_behav <- ggplot(combined_data, aes(x=social_norm, y=behaviour))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T)+
  theme_classic()+
  labs(x = "Social norm support", y = "Behavioural support")+
  theme(text = element_text(size = 15),
        axis.title.y = element_blank())+
  guides(color=guide_legend("Message framing"), fill = "none")

social_behav

scatter_plot <- suppressWarnings(ggarrange(nature_symp, efficacy_symp, social_symp,
                          nature_fin, efficacy_fin, social_fin,
                          nature_behav, efficacy_behav, social_behav,
                          labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i"), #spacing needed for correct alignment
                          ncol = 3, nrow = 3,
                          align = 'hv'))

suppressWarnings(print(scatter_plot))
##warnings ok - limited axis to not present extreme values to improve visualization

ggsave("Figures/scatter_plot.png", scatter_plot, height =10, width =12)

#warnings ok - removed extreme values to make visualization easier



##Correlation matrix plot----

#remove all non-numeric variables
cor_data<- subset(combined_data, select = -c(prolific_id, message_framing, flood, nudge, alt, allocated_uk_300, allocated_wild_300, letter, gender, ethnicity, other_ethn, education, age, climate_scores, donation, allocation_100))
cor_data<- mutate_all(cor_data, function(x) as.numeric(as.character(x)))
NAs <- cor_data[is.na(cor_data$MD_index),]
cor_data<- subset(cor_data,  MD_index != is.na(MD_index))

colnames(cor_data) <- c("Sympathetic attitudes",
                        "Behavioural support",
                        "Advert sufficiency",
                        "Financial support",
                        "Climate change",
                        "Social norm 'support'",
                        "Social norm 'donation'",
                        "Egoistical attitudes",
                        "Self-efficacy", 
                        "Financial security", 
                        "Adversity awareness",
                        "Nature connection",
                        "Index of Multiple Deprivation",
                        "Education",
                        "Flood experience")

#compute a correlation matrix
res<- cor(cor_data, method = "pearson", use = "complete.obs")
res

#get p values
res2<- rcorr(as.matrix(cor_data), type = "pearson")
res2

res2$r
res2$P

#plot
plot.new()
dev.off()

cor_plot<- corrplot(res, method = 'color',
                    order = 'alphabet', 
                    tl.col = "black", 
                    addCoef.col = 'black',
                    tl.srt = 45)

#exported manually

##Tolerance analysis----
#A variance inflation factor (VIF) less than 5 indicates a low correlation of that predictor with other predictors. 
#A value between 5 and 10 indicates a moderate correlation
#VIF values larger than 10 are a sign for high, not tolerable correlation of model predictors (James et al. 2013)

# Group your models in a named list:
  tot_lm <- list(
    behaviour = mod_behaviour_main,
    sympathy  = mod_sympathy_main,
    finance = mod_financial_main,
    sufficiency = mod_sufficieny_main
  )
  
# Get a nice dataframe with Variance Inflation Factor values:
tolerance<- purrr::map_dfr(tot_lm, performance::check_collinearity, .id = "Response") %>%
  as_tibble() %>%
  select(Response, Predictor = Term, VIF, `SE factor` = SE_factor) %>%
  mutate(across(c(VIF, `SE factor`), ~round(.x, 3)))

View(tolerance) #all values below 2, therefore all predictors are acceptable to be included in the model

##Model summary plots----
#Variables for plotting
all.models <- list()
all.models[[1]] <- mod_sufficieny_main
all.models[[2]] <- mod_sympathy_main
all.models[[3]] <- mod_behaviour_main
all.models[[4]] <- mod_financial_main

treatment_terms <- c("message_framinges-global",
                     "message_framinges-local",
                     "nudgepresent")

treatment_terms_label <- c("Nudge",
                           "Local-ES",
                           "Global-ES")

socio_terms<- c("age",
                "finance_security",
                "gender(2) Other",
                "gender(3) Male",
                "ethnicity(2) Other",
                "education_rank",
                "MD_index")

socio_terms_labels <- c("MD Index",
                "Education",
                "Ethnicity (Other)",
                "Gender (Male)",
                "Age",
                "Financial security")

psyco_terms <- c("connectedness",
                 "experience",
                 "ego",
                 "climate_scores",
                 "meanflood",
                 "social_norm",
                 "log(1 + social_norm_donation)",
                 "efficacy")

psyco_terms_labels <- c("Percieved social norm (donation)",
                 "Percieved social norm",
                 "Flood experience",
                 "Climate change sceptisism",
                 "Egoism",
                 "Global South awareness",
                 "Nature connection",
                 "Self-efficacy")

model_names <- c("Advert sufficency", "Sympathetic attitudes", "Behavioural support", "Finance support")

###Treatments variables----
model_sum_treatments <- plot_models(all.models,
            vline.color = "black",
            show.values = TRUE,
            rm.terms = c(socio_terms, psyco_terms),
            spacing = 0.75,
            show.p = T,
            colors = NULL,
            m.labels = model_names,
            legend.title = "Outcome variables",
            axis.labels = treatment_terms_label,
            value.size = 4,
            dot.size = 3,
            p.shape = T,
            legend.pval.title = "Significance level") +
  theme_classic()

model_sum_treatments<- model_sum_treatments + theme(axis.text.y = element_text(angle = 40, vjust = 0.5, hjust=1))
model_sum_treatments

ggsave(path = "Figures", filename = "model_sum_treatments_plot.png", model_sum_treatments, height =6, width =8)

###Psyco variables----

model_sum_psyco <- plot_models(all.models,
                                    vline.color = "black",
                                    show.values = TRUE,
                                    rm.terms = c(socio_terms, treatment_terms),
                                    spacing = 0.75,
                                    show.p = T,
                                    colors = NULL,
                                    m.labels = model_names,
                                    legend.title = "Outcome variables",
                                    axis.labels = psyco_terms_labels,
                                    value.size = 4,
                                    dot.size = 3,
                                    p.shape = T,
                                    legend.pval.title = "Significance level") +
  theme_classic()

model_sum_psyco<- model_sum_psyco + theme(axis.text.y = element_text(angle = 40, vjust = 0.5, hjust=1))
model_sum_psyco

ggsave(path = "Figures", filename = "model_sum_psyco_plot.png", model_sum_psyco, height =12, width =8)


###sociodemo variables----

model_sum_socio <- plot_models(all.models,
                               vline.color = "black",
                               show.values = TRUE,
                               rm.terms = c(psyco_terms, treatment_terms, "gender(2) Other"),
                               spacing = 0.75,
                               show.p = T,
                               colors = NULL,
                               m.labels = model_names,
                               legend.title = "Outcome variables",
                               axis.labels = socio_terms_labels,
                               value.size = 4,
                               dot.size = 3,
                               p.shape = T,
                               legend.pval.title = "Significance level") +
  theme_classic()

model_sum_socio<- model_sum_socio + theme(axis.text.y = element_text(angle = 40, vjust = 0.5, hjust=1))
model_sum_socio

ggsave(path = "Figures", filename = "model_sum_socio_plot.png", model_sum_socio, height =10, width =8)


#Demographics-------------------------------------------------------------------

##Total----
nrow(combined_data) #1116

#age
(nrow(subset(combined_data, age=="21"))/nrow(combined_data))*100 #10.46312
(nrow(subset(combined_data, age=="29.5"))/nrow(combined_data))*100 #18.78216
(nrow(subset(combined_data, age=="39.5"))/nrow(combined_data))*100 #18.01029
(nrow(subset(combined_data, age=="49.5"))/nrow(combined_data))*100 #16.38079
(nrow(subset(combined_data, age=="59.5"))/nrow(combined_data))*100 #25.04288
(nrow(subset(combined_data, age=="69.5"))/nrow(combined_data))*100 #10.20583
(nrow(subset(combined_data, age=="79.5"))/nrow(combined_data))*100 #1.02916
(nrow(subset(combined_data, age=="89.5"))/nrow(combined_data))*100 #0.08576329

#gender
(nrow(subset(combined_data, gender=="(1) Female"))/nrow(combined_data))*100 #50.77187
(nrow(subset(combined_data, gender=="(3) Male"))/nrow(combined_data))*100 #48.79931
(nrow(subset(combined_data, gender=="(2) Other"))/nrow(combined_data))*100 #0.4288165

#ethnicity
(nrow(subset(combined_data, ethnicity=="(1) White or Caucasian"))/nrow(combined_data))*100 #85.24871
(nrow(subset(combined_data, ethnicity=="(2) Other"))/nrow(combined_data))*100 #14.75129

###Offsetting means to match original scales----

#Sufficiency

mean(combined_data$sufficiency)
sd(combined_data$sufficiency)/sqrt(1166) #0.15959

#Behavioral

max(combined_data$behaviour) #7.202505
mean(combined_data$behaviour) #3.635307
sd(combined_data$behaviour)/sqrt(1166) #0.04868924
7.202505/2 #3.601253
20/7.202505 #2.776812
3.601253*2.776812 #10
2.776812*3.635307 #10.09456
2.776812*3.5
0.04868924*2.776812

#Financial

max(combined_data$financial)#15.11219
mean(combined_data$financial) #1.091872
#max in £ is £1100 (donation + allocation_100)

1100/15.11219 #72.78892
72.78892*1.091872 #79.47618

mean(combined_data$donation) #21.16509
median(combined_data$donation)#10
sd(combined_data$donation)/sqrt(1166) #1.583261
max(combined_data$donation) #1000

mean(combined_data$allocation_100) #0.400922 (proportion)
median(combined_data$allocation_100)#0.3
sd(combined_data$allocation_100)/sqrt(1166) #0.0101862

mean(combined_data$financial)#1.091872
sd(combined_data$financial)/sqrt(1166) #0.03353252
max(combined_data$financial)#15.11219

1100/15.11219 #72.78892 constant to convert PCA to £
72.78892*1.091872 #72.78892

###Wilcoxon test to measure difference from zero----
wiltest1<- wilcox.test(combined_data$donation, mu = 0)
wiltest1
wiltest2<- wilcox.test(combined_data$allocation_100, mu = 0)
wiltest2

Zstat1<-qnorm(wiltest1$p.value/2)
Zstat1
Zstat2<-qnorm(wiltest2$p.value/2)
Zstat2

#Sympathy

max(combined_data$sympathy) #7.86907
mean(combined_data$sympathy) #6.353343
sd(combined_data$sympathy)/sqrt(1166) #0.03777554

20/7.86907 #2.541596
2.541596*6.353343 #16.14763
2.541596*3.9 #9.912224
2.541596*6.2 #9.912224
2.541596*0.03777554 #0.09601016

###One-sampled t-tests to measure overall support----
t.test(combined_data$sufficiency, mu = 10)
t.test(combined_data$behaviour, mu = 7.202505/2)
t.test(combined_data$sympathy, mu = 7.86907/2)

library(plotrix)
mean(combined_data$age) #45.84906
std.error(combined_data$age) #0.4551066

mean(combined_data$efficacy) #10.70497
std.error(combined_data$efficacy) #0.1459724

mean(combined_data$connectedness) #4.797027
std.error(combined_data$connectedness) #0.03081535

min(combined_data$experience) #0
max(combined_data$experience) #7.27827

mean(combined_data$experience) #5.010301
std.error(combined_data$experience) #0.03571201

min(combined_data$social_norm) #0
max(combined_data$social_norm) #  7.016525

mean(combined_data$social_norm) #4.620652
std.error(combined_data$social_norm) #0.03685843

min(combined_data$social_norm_donation) #0
max(combined_data$social_norm_donation) #  7.016525

mean(combined_data$social_norm_donation) #17.16639
std.error(combined_data$social_norm_donation) #4.449253

min(combined_data$climate_scores) #0
max(combined_data$climate_scores) # 7.295522

mean(combined_data$climate_scores) #1.634991
std.error(combined_data$climate_scores) #0.0454186

mean(combined_data$meanflood) #0.3536306
std.error(combined_data$meanflood) #0.0299505
max(combined_data$meanflood) #11.3333
min(combined_data$meanflood)#0

mean(combined_data$MD_index) #5.604631
std.error(combined_data$MD_index) #0.08188735

mean(combined_data$finance_security) #10.32933
std.error(combined_data$finance_security) #0.1447148

mean(combined_data$education_rank) #3.578045
std.error(combined_data$education_rank) # 0.02884719

##MS-BD----
BD <- subset(combined_data, message_framing=="biodiversity")
nrow(BD) #416
(nrow(subset(BD, age=="21"))/nrow(BD))*100 #10.57692
(nrow(subset(BD, age=="29.5"))/nrow(BD))*100 #18.26923
(nrow(subset(BD, age=="39.5"))/nrow(BD))*100 #18.50962
(nrow(subset(BD, age=="49.5"))/nrow(BD))*100 #18.75
(nrow(subset(BD, age=="59.5"))/nrow(BD))*100 #24.75962
(nrow(subset(BD, age=="69.5"))/nrow(BD))*100 #7.932692
(nrow(subset(BD, age=="79.5"))/nrow(BD))*100 #1.201923
(nrow(subset(BD, age=="89.5"))/nrow(BD))*100 #0

(nrow(subset(BD, gender=="(1) Female"))/416)*100 #48.55769
(nrow(subset(BD, gender=="(3) Male"))/416)*100 #50.96154
(nrow(subset(BD, gender=="(2) Other"))/416)*100 #0.4807692

(nrow(subset(BD, ethnicity=="(1) White or Caucasian"))/416)*100 # 87.01923
(nrow(subset(BD, ethnicity=="(2) Other"))/416)*100 #12.98077

mean(BD$age) #45.40385
std.error(BD$age) #0.7412572

mean(BD$efficacy) #10.48077
std.error(BD$efficacy) #0.2464513

mean(BD$connectedness) #4.780048
std.error(BD$connectedness) #0.05226082

mean(BD$experience) #4.92734
std.error(BD$experience) #0.05969652

mean(BD$social_norm) #4.592651
std.error(BD$social_norm) #0.0623709

mean(BD$social_norm_donation) #16.25962
std.error(BD$social_norm_donation) #3.273502

mean(BD$climate_scores) #1.613207
std.error(BD$climate_scores) #0.07676703

mean(BD$meanflood) #0.3509615
std.error(BD$meanflood) #0.04834053

mean(BD$MD_index) #5.778846
std.error(BD$MD_index) #0.1403287

mean(BD$finance_security) #10.29988
std.error(BD$finance_security) #0.2489119

mean(BD$education_rank) #3.591346
std.error(BD$education_rank) # 0.04569822


##MS-ES-L----
ES_L <- subset(combined_data, message_framing=="es-local")
nrow(ES_L) #379
(nrow(subset(ES_L, age=="21"))/nrow(ES_L))*100 #10.02639
(nrow(subset(ES_L, age=="29.5"))/nrow(ES_L))*100 #20.05277
(nrow(subset(ES_L, age=="39.5"))/nrow(ES_L))*100 #17.1504
(nrow(subset(ES_L, age=="49.5"))/nrow(ES_L))*100 #13.45646
(nrow(subset(ES_L, age=="59.5"))/nrow(ES_L))*100 #27.96834
(nrow(subset(ES_L, age=="69.5"))/nrow(ES_L))*100 #10.81794
(nrow(subset(ES_L, age=="79.5"))/nrow(ES_L))*100 #0.2638522
(nrow(subset(ES_L, age=="89.5"))/nrow(ES_L))*100 #0.2638522

(nrow(subset(ES_L, gender=="(1) Female"))/nrow(ES_L))*100 # 50.13193
(nrow(subset(ES_L, gender=="(3) Male"))/nrow(ES_L))*100 #49.07652
(nrow(subset(ES_L, gender=="(2) Other"))/nrow(ES_L))*100 #0.7915567

(nrow(subset(ES_L, ethnicity=="(1) White or Caucasian"))/nrow(ES_L))*100 # 82.8496
(nrow(subset(ES_L, ethnicity=="(2) Other"))/nrow(ES_L))*100 #17.1504

mean(ES_L$age) #46.06201
std.error(ES_L$age) #0.8047764

mean(ES_L$efficacy) #10.97625
std.error(ES_L$efficacy) #0.2543583

mean(ES_L$connectedness) #4.840369
std.error(ES_L$connectedness) #0.05474575

mean(ES_L$experience) #5.077807
std.error(ES_L$experience) #0.06477207

mean(ES_L$social_norm) #4.673955
std.error(ES_L$social_norm) #0.06408972

mean(ES_L$social_norm_donation) # 24.64908
std.error(ES_L$social_norm_donation) #13.19152

mean(ES_L$climate_scores) #1.686988
std.error(ES_L$climate_scores) #0.08275331

mean(ES_L$meanflood) #.3658751
std.error(ES_L$meanflood) #0.05299694

mean(ES_L$MD_index) #5.488127
std.error(ES_L$MD_index) #0.1455912

mean(ES_L$finance_security) #10.20646
std.error(ES_L$finance_security) #0.2548791

mean(ES_L$education_rank) #3.649077
std.error(ES_L$education_rank) # 0.04569822


##MS-ES-G----
ES_G <- subset(combined_data, message_framing=="es-global")
nrow(ES_G) #371
(nrow(subset(ES_G, age=="21"))/nrow(ES_G))*100 #10.78167
(nrow(subset(ES_G, age=="29.5"))/nrow(ES_G))*100 # 18.0593
(nrow(subset(ES_G, age=="39.5"))/nrow(ES_G))*100 #18.32884
(nrow(subset(ES_G, age=="49.5"))/nrow(ES_G))*100 #16.71159
(nrow(subset(ES_G, age=="59.5"))/nrow(ES_G))*100 #22.37197
(nrow(subset(ES_G, age=="69.5"))/nrow(ES_G))*100 #12.12938
(nrow(subset(ES_G, age=="79.5"))/nrow(ES_G))*100 #1.617251
(nrow(subset(ES_G, age=="89.5"))/nrow(ES_G))*100 #0

(nrow(subset(ES_G, gender=="(1) Female"))/nrow(ES_G))*100 #53.90836
(nrow(subset(ES_G, gender=="(3) Male"))/nrow(ES_G))*100 #46.09164
(nrow(subset(ES_G, gender=="(2) Other"))/nrow(ES_G))*100 #0

(nrow(subset(ES_G, ethnicity=="(1) White or Caucasian"))/nrow(ES_G))*100 # 85.71429
(nrow(subset(ES_G, ethnicity=="(2) Other"))/nrow(ES_G))*100 #14.28571


mean(ES_G$age) #46.13073
std.error(ES_G$age) #0.8256577

mean(ES_G$efficacy) #10.67925
std.error(ES_G$efficacy) #0.2580507

mean(ES_G$connectedness) #4.771788
std.error(ES_G$connectedness) #0.05474575

mean(ES_G$experience) #5.034362
std.error(ES_G$experience) #0.06103318

mean(ES_G$social_norm) #4.597597
std.error(ES_G$social_norm) #0.06521408

mean(ES_G$social_norm_donation) # 10.53911
std.error(ES_G$social_norm_donation) #0.7279678

mean(ES_G$climate_scores) #1.606298
std.error(ES_G$climate_scores) #0.07644157

mean(ES_G$meanflood) #0.344115
std.error(ES_G$meanflood) #0.05482633

mean(ES_G$MD_index) #5.528302
std.error(ES_G$MD_index) #0.1390068

mean(ES_G$finance_security) #10.48787
std.error(ES_G$finance_security) #0.2478059

mean(ES_G$education_rank) #3.649077
std.error(ES_G$education_rank) # 0.05111968

##MS-Nudge----
nudge<- subset(combined_data, nudge=="present")
nrow(nudge) #587
(nrow(subset(nudge, age=="21"))/nrow(nudge))*100 #10.39182
(nrow(subset(nudge, age=="29.5"))/nrow(nudge))*100 # 21.12436
(nrow(subset(nudge, age=="39.5"))/nrow(nudge))*100 #18.39864
(nrow(subset(nudge, age=="49.5"))/nrow(nudge))*100 #15.3322
(nrow(subset(nudge, age=="59.5"))/nrow(nudge))*100 #23.67973
(nrow(subset(nudge, age=="69.5"))/nrow(nudge))*100 #9.88075
(nrow(subset(nudge, age=="79.5"))/nrow(nudge))*100 #1.192504
(nrow(subset(nudge, age=="89.5"))/nrow(nudge))*100 #0

(nrow(subset(nudge, gender=="(1) Female"))/nrow(nudge))*100 #48.55196
(nrow(subset(nudge, gender=="(3) Male"))/nrow(nudge))*100 #51.27768
(nrow(subset(nudge, gender=="(2) Other"))/nrow(nudge))*100 #0.1703578

(nrow(subset(nudge, ethnicity=="(1) White or Caucasian"))/nrow(nudge))*100 # 86.61157
(nrow(subset(nudge, ethnicity=="(2) Other"))/nrow(nudge))*100 #13.11755


mean(nudge$age) #45.17547
std.error(nudge$age) #0.6433399

mean(nudge$efficacy) #10.41227
std.error(nudge$efficacy) #0.2046621

mean(nudge$connectedness) #4.82879
std.error(nudge$connectedness) #0.04192816

mean(nudge$experience) #5.014382
std.error(nudge$experience) # 0.04892343

mean(nudge$social_norm) #4.478831
std.error(nudge$social_norm) #0.0520761

mean(nudge$social_norm_donation) # 13.39353
std.error(nudge$social_norm_donation) #2.060518

mean(nudge$climate_scores) #1.603202
std.error(nudge$climate_scores) #0.06230578

mean(nudge$meanflood) #0.3963657
std.error(nudge$meanflood) #0.04770705

mean(nudge$MD_index) #5.616695
std.error(nudge$MD_index) #0.1166016

mean(nudge$finance_security) #10.2517
std.error(nudge$finance_security) #0.2001269

mean(nudge$education_rank) #3.613288
std.error(nudge$education_rank) # 0.04010326


##MS-No Nudge----
no_nudge<- subset(combined_data, nudge=="absent")
nrow(no_nudge) #579
(nrow(subset(no_nudge, age=="21"))/nrow(no_nudge))*100 #10.53541
(nrow(subset(no_nudge, age=="29.5"))/nrow(no_nudge))*100 # 16.4076
(nrow(subset(no_nudge, age=="39.5"))/nrow(no_nudge))*100 #17.61658
(nrow(subset(no_nudge, age=="49.5"))/nrow(no_nudge))*100 #17.61658
(nrow(subset(no_nudge, age=="59.5"))/nrow(no_nudge))*100 #26.42487
(nrow(subset(no_nudge, age=="69.5"))/nrow(no_nudge))*100 #10.53541
(nrow(subset(no_nudge, age=="79.5"))/nrow(no_nudge))*100 #0.8635579
(nrow(subset(no_nudge, age=="89.5"))/nrow(no_nudge))*100 #0.1727116

(nrow(subset(no_nudge, gender=="(1) Female"))/nrow(no_nudge))*100 #53.02245
(nrow(subset(no_nudge, gender=="(3) Male"))/nrow(no_nudge))*100 #46.2867
(nrow(subset(no_nudge, gender=="(2) Other"))/nrow(no_nudge))*100 #0.6908463

(nrow(subset(no_nudge, ethnicity=="(1) White or Caucasian"))/nrow(no_nudge))*100 #83.5924
(nrow(subset(no_nudge, ethnicity=="(2) Other"))/nrow(no_nudge))*100 #16.4076


mean(no_nudge$age) # 46.53195
std.error(no_nudge$age) #0.6431882

mean(no_nudge$efficacy) #11.00173
std.error(no_nudge$efficacy) #0.207685

mean(no_nudge$connectedness) #4.764824
std.error(no_nudge$connectedness) #0.04520915

mean(no_nudge$experience) #5.006163
std.error(no_nudge$experience) # 0.05211917

mean(no_nudge$social_norm) #4.764432
std.error(no_nudge$social_norm) #0.05153471

mean(no_nudge$social_norm_donation) # 20.99138
std.error(no_nudge$social_norm_donation) #8.714181

mean(no_nudge$climate_scores) #1.667219
std.error(no_nudge$climate_scores) #0.06617605

mean(no_nudge$meanflood) #0.3103051
std.error(no_nudge$meanflood) #0.03598957

mean(no_nudge$MD_index) #5.592401
std.error(no_nudge$MD_index) #0.1079687

mean(no_nudge$finance_security) #10.40803
std.error(no_nudge$finance_security) #0.2093262

mean(no_nudge$education_rank) #3.542314
std.error(no_nudge$education_rank) # 0.04147629

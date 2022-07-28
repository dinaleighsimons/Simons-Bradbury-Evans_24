#Simons, Bradbury and Evans (2022) main script

#Setting environment----
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
combined_data <- read.csv("Data/combined_data_PCA.csv")
str(combined_data)

#make positive for models
combined_data$behaviour= combined_data$behaviour - min(combined_data$behaviour)
combined_data$sympathy= combined_data$sympathy - min(combined_data$sympathy)
combined_data$climate_scores= combined_data$climate_scores - min(combined_data$climate_scores)

#Models####

##Behaviour models----

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
                          flood+
                          MD_index, data = combined_data)
autoplot(mod_behaviour_main) #ok
summary(mod_behaviour_main)
Anova(mod_behaviour_main)

##Sympathy models----

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
                         flood +
                         MD_index, data = combined_data)
autoplot(mod_sympathy_main)
summary(mod_sympathy_main)
Anova(mod_sympathy_main)

mod_sympathy_int3<- lm(sympathy ~ message_framing*climate_scores+
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
                         flood +
                         MD_index, data = combined_data)
autoplot(mod_sympathy_int3)
summary(mod_sympathy_int3) #interaction with climate change and global treatment (increased)
Anova(mod_sympathy_int3)

#Use - message framing * climate change significant

##Financial models----

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
                           flood+
                           MD_index, data = combined_data, family = quasipoisson)
par(mfrow=c(2,2))
plot(mod_financial_main)
summary(mod_financial_main)
Anova(mod_financial_main, test = "F")

with(summary(mod_financial_main), 1 - deviance/null.deviance) #R^2 value
mod_0<- glm(financial ~ 1, data = combined_data, family = quasipoisson)
anova(mod_financial_main, mod_0, test="F") #F stats and p-value

#check for overdispersion
#summary(mod_financial_int1)$deviance/summary(mod_financial)$df.residual
#Anova(mod_financial_int1, test = "F")

##Sufficency models----
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
                           flood +
                           MD_index, data = combined_data)
autoplot(mod_sufficieny_main)
summary(mod_sufficieny_main)
Anova(mod_sufficieny_main)

#Plots#####
##Mean and SE summaries for nudge####

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
  scale_y_continuous(breaks = seq(0, 1.27, by=0.05), limits=c(0.95,1.27),
                     sec.axis = sec_axis(~ . * 72.78892, name = "Equivalent donation (£)\n ", breaks = seq(0, 93, by=3.5)))+
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
  scale_y_continuous(breaks = seq(0, 13, by=0.25), limits=c(11.35,13),
                     sec.axis = sec_axis(~ ., name = "Equivalent score\n ", breaks = seq(0, 13, by=0.25)))+
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
  scale_y_continuous(breaks = seq(0, 8, by=0.05), limits=c(6.23,6.5),
                     sec.axis = sec_axis(~ . * 2.541596, name = "Equivalent score\n ", breaks = seq(0, 20, by=0.15)))+
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
  scale_y_continuous(breaks = seq(0, 8, by=0.08), limits=c(3.4,3.9),
                     sec.axis = sec_axis(~ . * 2.776812, name = "Equivalent score\n ", breaks = seq(0, 20, by=0.3)))+
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
ggsave(path = "Figures", filename = "nudge.png", nudge_plot, height =8, width = 8)


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
  scale_y_continuous(breaks = seq(0, 1.26, by=0.05), limits=c(0.95,1.26),
                     sec.axis = sec_axis(~ . * 72.78892, name = "Equivalent donation (£)\n ", breaks = seq(0, 1100, by=3.50)))+
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
  scale_y_continuous(breaks = seq(0, 13, by=0.25), limits=c(11.35,13),
                     sec.axis = sec_axis(~ ., name = "Equivalent score\n ", breaks = seq(0, 13, by=0.25)))+
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
  scale_y_continuous(breaks = seq(0, 8, by=0.05), limits=c(6.2,6.5),
                     sec.axis = sec_axis(~ . * 2.541596, name = "Equivalent score\n ", breaks = seq(0, 20, by=0.15)))+
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
  scale_y_continuous(breaks = seq(0, 8, by=0.05), limits=c(3.5,3.8),
                     sec.axis = sec_axis(~ . * 2.776812, name = "Equivalent score\n ", breaks = seq(0, 20, by=0.15)))+
  scale_x_discrete(labels = c("Biodiversity","ES-global", "ES-local"))

mb

message_plot <- ggarrange(ms, msym, mf, mb,
                          labels = c("a", "b", "c", "d"),
                          ncol = 2, nrow = 2,
                          align = 'v')
message_plot
ggsave(path = "Figures", filename = "new_message_plot.png", message_plot, height =9, width =9)

##Scatter plots----

combined_data$social_norm= combined_data$social_norm - min(combined_data$social_norm)

###Climate plot----
#climateplot<- ggplot(combined_data, aes(x=climate_scores, y=sympathy^2, colour = message_framing))+
  #geom_point(size=0.5) +
  #geom_smooth(method = "lm", alpha = 0.08, se = T, size = 0.7)+
  #theme_classic() +
  #labs(x = "Climate change scepticism", y = "Sympathetic attitudes (transformed)")+
  #theme(text = element_text(size = 10)) +
  #guides(color=guide_legend("Message framing"), fill = "none")

#climateplot<- climateplot + scale_colour_manual(values = c("black", "sky blue", "grey70"),
                                                #labels = c("Biodiversity", "ES-global", "ES-local"))
#climateplot

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

nature_symp<- ggplot(combined_data, aes(x=connectedness, y=sympathy))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T)+
  theme_classic()+
  labs(x = "Nature connection", y = "Sympathetic attitudes")+
  theme(text = element_text(size = 15),
        legend.position = "none")+
  guides(color=guide_legend("Message framing"), fill = "none")

#naturesymp<- naturesymp + scale_colour_manual(values = c("black", "sky blue", "grey70"),
                                              #labels = c("Biodiversity", "ES-global", "ES-local"))
nature_symp

efficacy_symp<- ggplot(combined_data, aes(x=efficacy, y=sympathy))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T)+
  theme_classic()+
  labs(x = "Self-efficacy", y = "Sympathetic attitudes")+
  theme(text = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.position = "none")+
  guides(color=guide_legend("Message framing"), fill = "none")

efficacy_symp

social_symp <- ggplot(combined_data, aes(x=social_norm, y=sympathy))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=T)+
  theme_classic()+
  labs(x = "Social norm", y = "Sympathetic attitudes")+
  theme(text = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.position = "none")+
  guides(color=guide_legend("Message framing"), fill = "none")

social_symp

###Financial plots----

nature_fin<- ggplot(combined_data, aes(x=connectedness, y=financial))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "glm", se=T,
              method.args = list(family = "quasipoisson"))+
  theme_classic()+
  labs(x = "Nature connection", y = "Financial support")+
  theme(text = element_text(size = 15),
        legend.position = "none")+
  scale_y_continuous(limits=c(0,6.5))+
  guides(color=guide_legend("Message framing"), fill = "none")

suppressWarnings(print(nature_fin))

#warnings ok - limited axis to not present extreme values to improve visualization

efficacy_fin<- ggplot(combined_data, aes(x=efficacy, y=financial))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "glm", se=T,
              method.args = list(family = "quasipoisson"))+
  theme_classic()+
  labs(x = "Self-efficacy", y = "Finanical support")+
  theme(text = element_text(size = 15),
        axis.title.y = element_blank(),
        legend.position = "none")+
  scale_y_continuous(limits=c(0,6.5))+
  guides(color=guide_legend("Message framing"), fill = "none")

suppressWarnings(print(efficacy_fin))

#warnings ok - limited axis to not present extreme values to improve visualization

social_fin <- ggplot(combined_data, aes(x=log(1+social_norm_donation), y=financial))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "glm", se=T,
              method.args = list(family = "quasipoisson"))+
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

#warnings ok - limited axis to not present extreme values to improve visualization

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
  labs(x = "Social norm", y = "Behavioural support")+
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
cor_data<- subset(combined_data, select = -c(X, prolific_id, message_framing, nudge, alt, allocated_uk_300, allocated_wild_300, letter, gender, ethnicity, other_ethn, education, age, climate_scores))
cor_data<- mutate_all(cor_data, function(x) as.numeric(as.character(x)))
#NAs <- cor_data[is.na(cor_data$MD_index),]
cor_data<- subset(cor_data,  MD_index != is.na(MD_index))

colnames(cor_data) <- c("Flood experience",
                        "Social norm 'donation'",
                        "Adversity awareness", 
                        "Nature connection",
                        "Financial support", 
                        "Advert sufficiency", 
                        "Social norm 'support'", 
                        "Sympathetic attitudes",
                        "Behavioural support", 
                        "Self-efficacy", 
                        "Climate change", 
                        "Egoistical attitudes", 
                        "Education", 
                        "Financial security", 
                        "Index of Multiple Deprivation")

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

cor_plot<- corrplot(res, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#exported manually


##Model summary plots----
library(jtools)
library(ggstance)
library(broom.mixed)


#plot_summs(mod_behaviour_main,mod_sufficieny_main, mod_financial_main,mod_sympathy_int3, scale = TRUE)

#plot_summs(mod_behaviour_main,mod_sufficieny_main, mod_financial_main, scale = TRUE, 
           #model.names = c("Behaviour", "Sufficency", "Finance"))

#Demographics####
##Total####
nrow(combined_data) #1116
(nrow(subset(combined_data, age=="21"))/nrow(combined_data))*100 #10.46312
(nrow(subset(combined_data, age=="29.5"))/nrow(combined_data))*100 #18.78216
(nrow(subset(combined_data, age=="39.5"))/nrow(combined_data))*100 #18.01029
(nrow(subset(combined_data, age=="49.5"))/nrow(combined_data))*100 #16.38079
(nrow(subset(combined_data, age=="59.5"))/nrow(combined_data))*100 #25.04288
(nrow(subset(combined_data, age=="69.5"))/nrow(combined_data))*100 #10.20583
(nrow(subset(combined_data, age=="79.5"))/nrow(combined_data))*100 #1.02916
(nrow(subset(combined_data, age=="89.5"))/nrow(combined_data))*100 #0.08576329

(nrow(subset(combined_data, gender=="(1) Female"))/nrow(combined_data))*100 #50.77187
(nrow(subset(combined_data, gender=="(3) Male"))/nrow(combined_data))*100 #48.79931
(nrow(subset(combined_data, gender=="(2) Other"))/nrow(combined_data))*100 #0.4288165

(nrow(subset(combined_data, ethnicity=="(1) White or Caucasian"))/nrow(combined_data))*100 #85.24871
(nrow(subset(combined_data, ethnicity=="(2) Other"))/nrow(combined_data))*100 #14.75129

###Offsetting means to match original scales----

#Behavioral
max(combined_data$behaviour) #7.202505
mean(combined_data$behaviour) #3.635307
7.202505/2 #3.601253

20/7.202505 #2.776812
3.601253*2.776812 #10
2.776812*3.635307

#Financial
max(combined_data$financial)#15.11219
mean(combined_data$financial) #1.091872
#max in £ is £1100 (donation + allocation_100)

1100/15.11219 #72.78892
72.78892*1.091872 #79.47618

#Sympathy
max(combined_data$sympathy) #7.86907
mean(combined_data$sympathy) #6.353343

20/7.86907
2.541596*6.353343 #16.14763
2.541596*3.9 #9.912224

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

combined_data$experience= combined_data$experience - min(combined_data$experience)
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

mean(combined_data$flood) #1.060892
std.error(combined_data$flood) #0.08985149

mean(combined_data$MD_index) #5.604631
std.error(combined_data$MD_index) #0.08188735

mean(combined_data$finance_security) #10.32933
std.error(combined_data$finance_security) #0.1447148

mean(combined_data$education_rank) #3.578045
std.error(combined_data$education_rank) # 0.02884719

##MS-BD####
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

mean(BD$flood) #1.052885
std.error(BD$flood) #0.1450216

mean(BD$MD_index) #5.778846
std.error(BD$MD_index) #0.1403287

mean(BD$finance_security) #10.29988
std.error(BD$finance_security) #0.2489119

mean(BD$education_rank) #3.591346
std.error(BD$education_rank) # 0.04569822


##MS-ES-L####
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

mean(ES_L$flood) #1.097625
std.error(ES_L$flood) # 0.1589908

mean(ES_L$MD_index) #5.488127
std.error(ES_L$MD_index) #0.1455912

mean(ES_L$finance_security) #10.20646
std.error(ES_L$finance_security) #0.2548791

mean(ES_L$education_rank) #3.649077
std.error(ES_L$education_rank) # 0.04569822


##MS-ES-G####
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

mean(ES_G$flood) #1.032345
std.error(ES_G$flood) # 0.164479

mean(ES_G$MD_index) #5.528302
std.error(ES_G$MD_index) #0.1390068

mean(ES_G$finance_security) #10.48787
std.error(ES_G$finance_security) #0.2478059

mean(ES_G$education_rank) #3.649077
std.error(ES_G$education_rank) # 0.05111968

##MS-Nudge####
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

mean(nudge$flood) #1.189097
std.error(nudge$flood) # 0.1431211

mean(nudge$MD_index) #5.616695
std.error(nudge$MD_index) #0.1166016

mean(nudge$finance_security) #10.2517
std.error(nudge$finance_security) #0.2001269

mean(nudge$education_rank) #3.613288
std.error(nudge$education_rank) # 0.04010326


##MS-No Nudge####
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

mean(no_nudge$flood) #0.9309154
std.error(no_nudge$flood) # 0.1431211

mean(no_nudge$MD_index) #5.592401
std.error(no_nudge$MD_index) #0.1079687

mean(no_nudge$finance_security) #10.40803
std.error(no_nudge$finance_security) #0.2093262

mean(no_nudge$education_rank) #3.542314
std.error(no_nudge$education_rank) # 0.04147629


#Other----

##Exploring using survey package

#combined_data<- mutate(combined_data,
                       #fpc=0)

#combined_data$fpc[combined_data$letter == "A"] <- sum(with(combined_data, letter=="A"))
#combined_data$fpc[combined_data$letter == "B"] <- sum(with(combined_data, letter=="B"))
#combined_data$fpc[combined_data$letter == "C"] <- sum(with(combined_data, letter=="C"))
#combined_data$fpc[combined_data$letter == "D"] <- sum(with(combined_data, letter=="D"))
#combined_data$fpc[combined_data$letter == "E"] <- sum(with(combined_data, letter=="E"))
#combined_data$fpc[combined_data$letter == "F"] <- sum(with(combined_data, letter=="F"))

#dstrat <- svydesign(id=~1,strata=~letter,  data=combined_data, weight = 1)
#summary(dstrat)

#svymean(~sympathy+behaviour+sufficiency+financial, dstrat)
#svyquantile(~sympathy+behaviour+sufficiency+financial, dstrat, quantile=c(0.25,0.5,0.75), ci=TRUE)

#svytotal(~message_framing, dstrat)
#svytotal(~nudge, dstrat)
#svytotal(~letter, dstrat)

#svyby(~sympathy+behaviour+financial+sufficiency, ~message_framing, design=dstrat, svymean)

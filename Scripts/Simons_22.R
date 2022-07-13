#Simons, Evans and Bradbury main script

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
combined_data <- read.csv("Data/combined_data_final.csv")
str(combined_data)

#make financial positive for the model
combined_data$financial = combined_data$financial - min(combined_data$financial)

#Models####

##Behaviour models----

mod_behaviour_main<- lm(behaviour ~ message_framing +
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
                          flood+
                          MD_index, data = combined_data)
autoplot(mod_behaviour_main) #ok
summary(mod_behaviour_main)
Anova(mod_behaviour_main)

##Sympathy models----

mod_sympathy_main<- lm(sympathy^(2) ~ message_framing+
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
                         flood +
                         MD_index, data = combined_data)
autoplot(mod_sympathy_main)
summary(mod_sympathy_main)
Anova(mod_sympathy_main)

mod_sympathy_int3<- lm(sympathy^(2) ~ message_framing*sqrt(1+climate_change)+
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
                           allocated_wild_300 +
                           finance_security + 
                           age +
                           gender +
                           ethnicity +
                           education_rank +
                           experience + 
                           ego + 
                           sqrt(1+ climate_change) + 
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
  scale_y_continuous(breaks = seq(0, 1.29, by=0.05), limits=c(0.95,1.29))+
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
  scale_y_continuous(breaks = seq(0, 32.5, by=0.2), limits=c(31.2,32.5))+
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
  scale_y_continuous(breaks = seq(0, 56.2, by=0.8), limits=c(51,56.2))+
  scale_x_discrete(labels = c("Absent","Present"))

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
  xlab("Message framing") + ylab("Financial support")+
  #ylim(0.95,1.26)+
  scale_y_continuous(breaks = seq(0, 1.26, by=0.05), limits=c(0.95,1.26))+
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
  xlab("Nudge") + ylab("Advert sufficiency")+
  #ylim(11.3,13)+
  scale_y_continuous(breaks = seq(0, 13, by=0.25), limits=c(11.35,13))+
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
  xlab("Message framing") + ylab("Sympathetic attitudes")+
  #ylim(31.2,32.6)+
  scale_y_continuous(breaks = seq(0, 32.5, by=0.2), limits=c(31.2,32.6))+
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
  xlab("Message framing") + ylab("Behavioural support")+
  #ylim(51,56.2)+
  scale_y_continuous(breaks = seq(0, 56.2, by=0.5), limits=c(52,55.3))+
  scale_x_discrete(labels = c("Biodiversity","ES-global", "ES-local"))

mb

message_plot <- ggarrange(ms, msym, mf, mb,
                          labels = c("a", "b", "c", "d"),
                          ncol = 2, nrow = 2,
                          align = 'v')
message_plot
ggsave(path = "Figures", filename = "new_message_plot.png", message_plot, height =9, width =7)

##Scatter plots----

climateplot<- ggplot(combined_data, aes(x=sqrt(climate_change), y=sympathy^2, colour = message_framing))+
  geom_point(size=0.5) +
  geom_smooth(method = "lm", alpha = 0.08, se = T, size = 0.7)+
  theme_classic() +
  labs(x = "Climate change scepticism (transformed)", y = "Sympathetic attitudes (transformed)")+
  theme(text = element_text(size = 10)) +
  guides(color=guide_legend("Message framing"), fill = "none")

climateplot<- climateplot + scale_colour_manual(values = c("black", "sky blue", "grey70"),
                                                labels = c("Biodiversity", "ES-global", "ES-local"))
climateplot

ggsave(path = "Figures", filename = "sc_plot.png", width = 7, height = 4)

natureplot<- ggplot(combined_data, aes(x=connectedness, y=sympathy^2, colour=message_framing))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=F)+
  theme_classic()+
  labs(x = "Nature connectedness", y = expression(paste("Sympathetic attitudes"^2)))+
  theme(text = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")+
  guides(color=guide_legend("Message framing"), fill = "none")

natureplot<- natureplot + scale_colour_manual(values = c("black", "sky blue", "grey70"),
                                              labels = c("Biodiversity", "ES-global", "ES-local"))
natureplot

egoplot<- ggplot(combined_data, aes(x=ego, y=sympathy^2, colour=message_framing))+
  geom_point(alpha=0.2)+
  geom_smooth(method = "lm", se=F)+
  theme_classic()+
  labs(x = "Egoistical attitudes", y = expression(paste("Sympathetic attitudes"^2)))+
  theme(text = element_text(size = 15),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")+
  guides(color=guide_legend("Message framing"), fill = "none")

egoplot<- egoplot + scale_colour_manual(values = c("black", "sky blue", "grey70"),
                                        labels = c("Biodiversity", "ES-global", "ES-local"))
egoplot

#scatter_plot <- ggarrange(climateplot, natureplot, egoplot,
                          #labels = c("a", "b", "c"),
                          #ncol = 3, nrow = 1,
                          #align = 'h')
#scatter_plot
#ggsave("interaction_plot.png", scatter_plot, height =5, width =25)

##Correlation matrix plot----
#Still need to fix this

#remove all non-numeric variables
cor_data<- subset(combined_data, select = -c(X, prolific_id, message_framing, nudge, alt, allocated_uk_300,letter, gender, ethnicity, other_ethn, education, age, climate_scores))
cor_data<- mutate_all(cor_data, function(x) as.numeric(as.character(x)))
#NAs <- cor_data[is.na(cor_data$MD_index),]
cor_data<- subset(cor_data,  MD_index != is.na(MD_index))

colnames(cor_data) <- c("Allocation to wildlife", "Sympathetic attitudes", "Behavioural support", "Advert sufficiency", "Financial support", "Flood experience", "Climate change", "Social norm support", "Social norm donation", "Egoistical attitudes", "Self-efficacy", "Financial security", "Adversity experience", "Nature connectedness", "IMD", "Education")

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


##Model summary plots----
library(jtools)
library(ggstance)


plot_summs(mod_behaviour_main,mod_sufficieny_main, mod_financial_main,mod_sympathy_int3, scale = TRUE)

plot_summs(mod_behaviour_main,mod_sufficieny_main, mod_financial_main, scale = TRUE, 
           model.names = c("Behaviour", "Sufficency", "Finance"))


#Other----

##Exploring using survey package

combined_data<- mutate(combined_data,
                       fpc=0)

combined_data$fpc[combined_data$letter == "A"] <- sum(with(combined_data, letter=="A"))
combined_data$fpc[combined_data$letter == "B"] <- sum(with(combined_data, letter=="B"))
combined_data$fpc[combined_data$letter == "C"] <- sum(with(combined_data, letter=="C"))
combined_data$fpc[combined_data$letter == "D"] <- sum(with(combined_data, letter=="D"))
combined_data$fpc[combined_data$letter == "E"] <- sum(with(combined_data, letter=="E"))
combined_data$fpc[combined_data$letter == "F"] <- sum(with(combined_data, letter=="F"))

dstrat <- svydesign(id=~1,strata=~letter,  data=combined_data)
summary(dstrat)

svymean(~sympathy+behaviour+sufficiency+financial, dstrat)
svyquantile(~sympathy+behaviour+sufficiency+financial, dstrat, quantile=c(0.25,0.5,0.75), ci=TRUE)

svytotal(~message_framing, dstrat)
svytotal(~nudge, dstrat)
svytotal(~letter, dstrat)

svyby(~sympathy+behaviour+financial+sufficiency, ~message_framing, design=dstrat, svymean)

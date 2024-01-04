#power analyses and FDR 2023

#set environment----
library(pwr)
library(pwr2)
library(lme4)
library(powerMediation)

#getting effect sizes - Give the conventional effect size (small, medium, large) for the tests available in this package
cohen.ES(test = "f2", size = "small") #small = 0.02, medium = 0.15, large= 0.35

#general linear model----
#(https://data-se.netlify.app/2018/07/24/power-calculation-for-the-general-linear-model/)

pwr.f2.test(u = 19,
            v = NULL,
            f2 = 0.02,
            sig.level = 0.05, 
            power = 0.8)

1023 + 19

#u = degrees of freedom for numerator
#v = degrees of freedom for denominator
#f2 =  	effect size (R2/(1-R2) where R2 is proportion of variance accounted)
#sig.level = Significance level (Type I error probability)
#power = Power of test (1 minus Type II error probability)

#v = 1023
#n = v + p = 1023 + 19
#n = 1042 overall, 174 per treatment (80% power, 19 variables, sig level = 0.05)

#with above parameters, model is 85.6% powered

#general linear model 2----
#(https://data-se.netlify.app/2018/07/24/power-calculation-for-the-general-linear-model/)

pwr.f2.test(u = 17,
            v = NULL,
            f2 = 0.02,
            sig.level = 0.05, 
            power = 0.8)

982 + 17

#u = degrees of freedom for numerator
#v = degrees of freedom for denominator
#f2 =  	effect size (R2/(1-R2) where R2 is proportion of variance accounted)
#sig.level = Significance level (Type I error probability)
#power = Power of test (1 minus Type II error probability)

#v = 982
#n = v + p = 982 + 17
#n = 999 overall, 167 per treatment (80% power, 17 variables, sig level = 0.05)


#for interaction, general advice is to reduce effect size to account for this
pwr.f2.test(u = 19,
            v = NULL,
            f2 = 0.015,
            sig.level = 0.05, 
            power = 0.8)

#v = 1366
#n = v + p = 1366 + 19
#n = 1385 overall, 231 per treatment (80% power, 19 variables, sig level = 0.05)



# Onteraction in linear models Chap 4 example HERS
# libraries
#
library(car)
library(multcomp)
library(tidyverse)
#
hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
# hers_nodi <- filter(hers, diabetes == "no") this is the no diabetes
# Now for the group with Hormone Therapy
# Model of cholesterol LDL and the effect of Hormone Therapy (HT) and
# Statin use model is at the one year visit (to se the HT effect)
hers <- mutate(hers, HT = factor(HT))
hers <- hers %>% mutate(HT = relevel(HT, ref = "placebo"))
# For the first year visit
LDL_model <- lm(LDL1 ~ HT * statins, data = hers)
summary(LDL_model)
# For the table 4.15 of the book, to test for the linear 
# combination of the coefficients for the interaction to
# TEST the linear combination of the coefficients for HT
# and the statin interaction b1 and b3 the third part of table 4.15
coefeq <- matrix(data=0, nrow = 1, ncol = length(LDL_model$coefficients))
colnames(coefeq) <- names(LDL_model$coefficients)
coefeq
coefeq[1, "HThormone therapy"] <- 1
coefeq[1, "HThormone therapy:statinsyes"] <- 1
coefeq %*% LDL_model$coefficients
# coeftest <- glht(model= LDL_model, linfct= coefeq, rhs= 0, alternative= "greater")
coeftest <- glht(model= LDL_model, linfct= coefeq, rhs= 0)
summary(coeftest)
# Example of LDL at the first year and physact
# This is the table 4.16 of the book
# The effects of the physica activity on the LDL model
# now to estimate the linear combination of the coefficients
hers <- mutate(hers, physact = factor(physact, levels=c("much less active","somewhat less active","about as active","somewhat more active","much more active")))
LDLphys_model <- lm(LDL1 ~ HT * physact, data = hers)
summary(LDLphys_model)
# The coefficients test for interaction
# can be done with glht()

# Example of BMI and statins, with interaction
# Model of LDL and BMI*statins and other variables
hers <- mutate(hers, statins = factor(statins))
hers <- mutate(hers, nonwhite = factor(nonwhite))
hers <- mutate(hers, smoking = factor(smoking))
hers <- mutate(hers, drinkany = factor(drinkany))
hers <- mutate(hers, BMIc = BMI - mean(BMI,na.rm=TRUE))
LDLbmi_model <- lm(LDL ~ statins*BMIc + age + nonwhite + smoking + drinkany, data = hers)
summary(LDLbmi_model)
# For the table 4.17 of the book, to test for the linear 
# combination of the coefficients for the interaction to
# TEST the linear combination of the coefficients for HT
coef_LDLbmi <- matrix(data=0, nrow = 1, ncol = length(LDLbmi_model$coefficients))
colnames(coef_LDLbmi) <- names(LDLbmi_model$coefficients)
# E[LDL|x] = b0 + b1 statins + b2 BMIc + b3 statins:BMIc + b4 age
#           + b5 nonwhite + b6 smoking + b7 + drinkany
# we will look at slope = b1 + b3 BMIc; coeff = 0, 1, 0, 1, 0, 0, 0, 0
coef_LDLbmi
coef_LDLbmi[1, "statinsyes"] <- 1
coef_LDLbmi[1, "statinsyes:BMIc"] <- 1
coef_LDLbmi %*% LDLbmi_model$coefficients
coef_LDLbmitest <- glht(model= LDLbmi_model, linfct= coef_LDLbmi, rhs= 0)
summary(coef_LDLbmitest)
# we will look at slope = b2 + b3 BMIc; coeff = 0, 0, 1, 1, 0, 0, 0, 0
coef_LDLbmi[1, "statinsyes"] <- 0
coef_LDLbmi[1, "BMIc"] <- 1
coef_LDLbmi[1, "statinsyes:BMIc"] <- 1
coef_LDLbmi %*% LDLbmi_model$coefficients
coef_LDLbmitest <- glht(model= LDLbmi_model, linfct= coef_LDLbmi, rhs= 0)
summary(coef_LDLbmitest)

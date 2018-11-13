# Examples HSAUR2 Logistic regression.
# Erythrocyte sedimentation rate (ESR). ESR increases when protein levels rise
# associated with chronic infectons, rheumatic conditions, etc
# the level of ESR is not important as long as it is < 20mm/hr (healthy)
# to asses ESR as useful diagnostic tool.
# ESR > 20mm/hr for proteins fibrinogen and gamma globulin, using glm
library(tidyverse)
library(HSAUR2)
data("plasma", package = "HSAUR2")
layout(matrix(1:2, ncol = 2))
cdplot(ESR ~ fibrinogen, data = plasma)
cdplot(ESR ~ globulin, data = plasma)
# glm general linear model default is logistic for binomial distrbution
plasma_glm01 <- glm(ESR ~ fibrinogen, data = plasma, family = binomial())
summary(plasma_glm01)
# coeff fibrinogen is sifnificative 5%
# one unit change in this variable increases the log-odds in favor of
# ESR > 20mm/hr
confint(plasma_glm01, parm = "fibrinogen")
exp(coef(plasma_glm01)["fibrinogen"])
exp(confint(plasma_glm01, parm = "fibrinogen"))
# full model with two variables
plasma_glm02 <- glm(ESR ~ fibrinogen + globulin, data = plasma, family = binomial())
summary(plasma_glm02)
# Comparing the resudial deviance of the models:
# residual deviance 01: 24.84 residual deviance 02: 22.971 -> 1.869 (1.87)
# to test for significance R take the lgm with a chisq
# the 1.87 -> globulin has no influence in the ESR
anova(plasma_glm01, plasma_glm02, test = "Chisq")

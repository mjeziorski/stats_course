# Multiple linear linear regression
#
# Chapter 4 examples. 4.1
library(tidyverse)
hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
hers_nodi <- filter(hers, diabetes == "no")
# Simple linear model with HERS data for women without diebetes
ggplot(data = hers_nodi, mapping = aes(x = exercise, y = glucose)) + 
  geom_boxplot(na.rm = TRUE) + facet_grid( . ~ diabetes) + geom_jitter(height = 0.15, width = 0.15)
# The simple linear model adjust the exercise like in table 4.1
hers_nodi_Fit <- lm(glucose ~ exercise, data = hers_nodi)
summary(hers_nodi_Fit)
# and for obtaining the table 4.2 with multiple linear model
hers_nodi_Fit2 <- lm(glucose ~ exercise + age + drinkany + BMI, data = hers_nodi)
summary(hers_nodi_Fit2)
# For the amount of exercise
ggplot(data = hers_nodi, mapping = aes(x = factor(physact), y = glucose)) + geom_boxplot(na.rm = TRUE) + geom_jitter(height = 0.15, width = 0.15)
#
hers_nodi_Fit3 <- lm(glucose ~ factor(physact), data = hers_nodi)
summary(hers_nodi_Fit3)
#
# Example of multiple linear regression 
# clouds from HSAUR
library(HSAUR2)
data(clouds)
# looking the datafor rainfall
boxplot(rainfall~seeding, data=clouds)
boxplot(rainfall~echomotion, data=clouds)
# y = Xb+e with X the design model matrix that consis of the q continuously measured
# explanatory variables and a column of ones corresponding to the intercept term
clouds_formula <- rainfall ~ seeding + seeding:(sne+cloudcover+prewetness+echomotion) + time
Xstar <- model.matrix(clouds_formula, data = clouds)
attr(Xstar, "contrasts")
clouds_lm <- lm(clouds_formula, data = clouds)
summary(clouds_lm)
# to list the betas* with the:
betaStar <- coef(clouds_lm)
betaStar
# and the Covariant matrix Cov(beta*) with:
VbetaStar <- vcov(clouds_lm)
# Where the square roots of the diagonal elements are the standart errors 
sqrt(diag(VbetaStar))

# Multiple linear linear regresion
#
# Chapter 4 examples. 4.1
library(tidyverse)
hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
hers_nodi <- filter(hers, diabetes == "no")
# Simple linear model with HERS data for women without diebetes
ggplot(data = hers_nodi, mapping = aes(x = exercise, y = glucose)) + 
  geom_boxplot(na.rm = TRUE) + facet_grid( . ~ diabetes) + geom_jitter(height = 0.15, width = 0.15)
# The simple linear model adjust the exercise
hers_nodi_Fit <- lm(glucose ~ exercise, data = hers_nodi)
summary(hers_nodi_Fit)
#
hers_nodi_Fit2 <- lm(glucose ~ exercise + age + drinkany + BMI, data = hers_nodi)
summary(hers_nodi_Fit2)

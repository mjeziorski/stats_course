# Multiple linear linear regresion
#
# Chapter 4 examples. 4.1
library(tidyverse)
hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
hers_nodi <- (filter(hers, diabetes == "no"))
# Simple linear model with HERS data for women without diebetes
boxplot(glucose ~ exercise, data = hers_nodi)
hers_nodi_Fit <- lm(glucose ~ exercise, data = hers_nodi)
summary(hers_nodi_Fit)

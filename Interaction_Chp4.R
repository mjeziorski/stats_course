# Onteraction in linear models Chap 4 example HERS
# libraries
#
library(car)
library(tidyverse)
#

hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
# hers_nodi <- filter(hers, diabetes == "no")
# Model of LDL and the effect of Hormone Therapy HT and Statin use
# 
hers <- mutate(hers, HT = factor(HT))
hers <- hers %>% mutate(HT = relevel(HT, ref = "placebo"))
LDL_model <- lm(LDL ~ HT * statins, data = hers)
summary(LDL_model)
# For the first year visit
LDL1_model <-lm(LDL1 ~ HT * statins, data = hers)

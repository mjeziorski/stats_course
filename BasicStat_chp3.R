# Examples of the Chapter 3 
#
library(tidyverse)
setwd("~/Dropbox/Fdo/ClaseStats/RegresionClass/RegresionR_code")
#

hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
ggplot(data = hers, mapping = aes(x = reorder(raceth, SBP, FUN = median), y=SBP, color = raceth)) + geom_boxplot(na.rm = TRUE) + geom_jitter(height=0.15, width=0.15)
SBP_race.aov=aov(SBP~raceth, data=hers)
summary(SBP_race.aov)
TukeyHSD(SBP_race.aov, conf.level = 0.99, ordered=TRUE)
plot(TukeyHSD(aov(SBP~raceth, data=hers), conf.level = 0.99, ordered=TRUE), col = "red")
par(mfrow=c(2,2))
plot(SBP_race.aov)
par(mfrow=c(1,1))
uhat_SBP <- resid(SBP_race.aov)
shapiro.test(uhat_SBP)
#
# Simple Linear Regression Model
# SBP by age in the HERS data Y variation with a single predictor
#

# Example of simplelinear model
Exr33 <- read_csv(file="DataOther/EXR_C09_S03_03.csv")
#
plot(Exr33$QTC ~ Exr33$DOSE, pch=20)
LinExr33 = lm(QTC ~ DOSE, data=Exr33)
summary(LinExr33)
abline(LinExr33, col=2)
# Res y = 559.9 + 0.139 x
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Problem 9.3.4 Hospital tests for anticoagulant
# Measures of INR (Y) from 90 subjects taking warfarin the 
# independent variable was the hospital essays (X)
library(tidyverse)
Exr3.4 <- read_csv(file="DataOther/EXR_C09_S03_04.csv")
plot(Y ~ X, data=Exr3.4, pch=20)
LinExr3.4 <- lm(Y ~ X, data=Exr3.4)
abline(LinExr3.4, col=2)
# y = 0.4885 + 0.8625 x  with r^2 = 0.5067 (50 % data exp)
summary(LinExr3.4)
par(mfrow=c(2,2))
plot(LinExr3.4)
par(mfrow=c(1,1))
#
ggplot(data = Exr3.4, mapping = aes(x = X, y = Y)) + 
  geom_point(na.rm = TRUE) + geom_jitter(height=0.05, width=0.05)
#
ggplot(data = Exr3.4, mapping = aes(x = X, y = Y)) + geom_point(na.rm = TRUE) + 
  geom_abline(aes(intercept = 0.4885, slope = 0.8625, colour = "RED")) + 
  geom_jitter(height=0.05, width=0.05)
#
# Problem 9.3.7 GFR is the most importatn parameter of 
# renal function assesment in transplan, insulin is the
# gold standar of GFR. Examied inverse Cystatin C as 
# insulin GFR, use the DTPA GFR as the predictor
# of the inverse Cystatin C
Exr3.7 <- read_csv(file="DataOther/EXR_C09_S03_07.csv")
plot(INVCYS ~ DTPA, data=Exr3.7, pch=20)
LinExr3.7 = lm(INVCYS ~ DTPA, data=Exr3.7)
summary(LinExr3.7)
abline(LinExr3.7, col=2)
# y = 0.1929 + 0.0063 x  with r^2 = 0.57 (57 % data exp)
par(mfrow=c(2,2))
plot(LinExr3.7)
par(mfrow=c(1,1))
#
ggplot(data = Exr3.7, mapping = aes(x = DTPA, y = INVCYS)) + geom_point(na.rm = TRUE) + 
  geom_abline(aes(intercept = 0.1929, slope = 0.0063, colour = "red")) + 
  geom_jitter(height=0.05, width=0.05)
#

# #############################################################################
# Contingency tables
# frequency variables
tab_wcgs <- table(wcgs$chd69, wcgs$arcus, dnn = c('CHD','arcus'))
ftab_wcgs <- ftable(chd69~arcus~chd69, data= wcgs, dnn = c('CHD','arcus'))
#
library(gmodels)
CrossTable(wcgs$chd69, wcgs$arcus, dnn = c('CHD','arcus'))
#
chisq.test(tab_wcgs)
# chisq.test(tab_wcgs, simulate.p.value = TRUE)

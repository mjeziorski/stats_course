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

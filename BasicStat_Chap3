# Basic Statistical Methods
# t-Test and ANOVA
# t-Test determine if the averages of two variables are different
# One-way ANOVA of the averages of n-variables, at least two are different
#
# From Daniel Chapter 7 exercise 3.3
#
library(tidyverse)

Ex733 = read.csv(file="DataOther/EXR_C07_S03_03.csv", header=TRUE)

var.test(Ex733$Length[Ex733$Group == 2], Ex733$Length[Ex733$Group == 1])
NoOSAS = Ex733$Length[Ex733$Group == 1]
OSAS = Ex733$Length[Ex733$Group == 2]
boxplot(NoOSAS,OSAS)
#
var.test(Ex733$Length[Ex733$Group == 2], Ex733$Length[Ex733$Group == 1])
t.test(NoOSAS, OSAS, alternative="less", mu=0, var.equal = FALSE, conf.level=0.99)
#
boxplot(Length ~ Group, data=Ex733)
t.test(Length ~ Group, data=Ex733, alternative="two.sided", var.equal = FALSE, conf.level=0.99)
#
ggplot(data = Ex733, aes(y = Length, group = Group)) + geom_boxplot()

# Prob 7.4.2
#
Ex742 <- read.csv(file="DataOther/EXR_C07_S04_02.csv", header=TRUE)
#
boxplot(Ex742)
# varianzas son iguales 
t.test(Ex742$Base, Ex742$Follow, alternative="g", mu=0, paired=TRUE, var.equal=TRUE)
# ggplot(data = Ex742, mapping = aes(y=Follow, x=Base)) + geom_boxplot() 
#
hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
boxplot(glucose ~ diabetes, data=hers)
boxplot(glucose[hers$diabetes == "no"] ~ exercise[hers$diabetes == "no"], alternative="two.sided", data=hers)
t.test(glucose[hers$diabetes == "yes"] ~ exercise[hers$diabetes == "yes"], data=hers, alternative="two.sided", mu=0, paired=FALSE, var.equal=TRUE)
#
ggplot(data = hers, mapping = aes(y=glucose, x=diabetes)) + geom_boxplot() 
ggplot(data = hers, mapping = aes(x = exercise, y = glucose)) + geom_boxplot() + facet_grid( . ~ diabetes)
ggplot(data = hers, mapping = aes(x = insulin, y = glucose)) + geom_boxplot() + facet_grid( . ~ diabetes)
#
# Exercise 7.44 Ex744 "DXA","ST","BR","SEX"
Ex744 <- read_csv("DataOther/REV_C07_44.csv")
boxplot(DXA ~ SEX, data=Ex744)
ggplot(data = Ex744, mapping = aes(y=DXA, group=SEX)) + geom_boxplot()
t.test(DXA ~ SEX, data=Ex744, alternative="two.sided", mu=0, paired=FALSE, var.equal=TRUE)

# Grouping the data ready for ANOVA
Ex744 <- read_csv("DataOther/REV_C07_44_mod.csv")
ggplot(data = Ex744, mapping = aes(y=BodyFat, x=Method)) + geom_boxplot(na.rm = TRUE)
# To use Sex as color, color must be >0
Ex744$Sex <- Ex744$Sex + 1
ggplot(data = Ex744, mapping = aes(y=BodyFat, x=Method, color = Sex)) + geom_boxplot(na.rm = TRUE)
ggplot(data = Ex744, mapping = aes(y = BodyFat, x = Method)) + geom_boxplot(na.rm = TRUE) 
    + geom_jitter(height=0.2, width=0.2)
ggplot(data = Ex744, mapping = aes(y = BodyFat, x = Method)) + geom_boxplot(na.rm = TRUE) 
    + geom_jitter(height=0.05, width=0.05, color=factor(Ex744$Sex), na.rm = TRUE)
# for a one-way ANOVA example with Ex744
is.factor(Ex744$Method)
aov744 <- aov(BodyFat~Method, data = Ex744)
summary(aov744)
TukeyHSD(aov744, conf.level = 0.99)
plot(TukeyHSD(aov744, conf.level = 0.99), las=1, col = "red")
#
# Diagnostic check
par(mfrow=c(2,2))
plot(aov744)
#
# Shapiro Wilk test
uhat <- resid(aov744)
shapiro.test(uhat)

bartlett.test(BodyFat~Method, data = Ex744)
library(car)
levene.test(BodyFat~Method, data = Ex744)

# Other example Chap 8 One-Way ANOVA
Ex82 <- read_csv("DataOther/EXA_C08_S02_01.csv")
boxplot(Ex82)
Ex82.dat = stack(Ex82)
names(Ex82.dat)
# ggplot(data = Ex82.dat, mapping = aes(y=values, x=ind, color = ind)) + geom_boxplot(na.rm = TRUE) 
    + geom_jitter(height=0.1, width=0.1)
ggplot(data = Ex82.dat, mapping = aes(x = reorder(ind, values, FUN = median), y  =values, color = ind)) 
    + geom_boxplot(na.rm = TRUE) + geom_jitter(height=0.1, width=0.1)
    
# Estimating the one-way ANOVA
Ex82.aov=aov(values~ind, data=Ex82.dat)
summary(Ex82.aov)
TukeyHSD(Ex82.aov, conf.level = 0.99, ordered=TRUE)
plot(TukeyHSD(Ex82.aov, conf.level = 0.99, ordered=TRUE), color = "red")
abline(v=0, col=2)
## Testing
par(mfrow=c(2,2))
# layout(matrix(c(1, 2, 3, 4), nrow=2, ncol=2, byrow=TRUE))
plot(Ex82.aov)
#
# Shapiro Wilk test
uhat_82 <- resid(Ex82.aov)
shapiro.test(uhat_82)

bartlett.test(values~ind, data=Ex82.dat)
library(car)
levene.test(values~ind, data=Ex82.dat)

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

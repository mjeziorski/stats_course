# Problem 9.3.4 Hospital tests for anticoagulant
# punto=c(1:25)
# plot(punto ~ punto, pch=punto)
#
Exa9.3 = read.csv(file="EXA_C09_S03_01.csv", header=TRUE)
names(Exa9.3)
plot(Exa9.3$Y ~ Exa9.3$X, pch = 20)
Ybar=mean(Exa9.3$Y)
Xbar=mean(Exa9.3$X)
abline(h=Ybar, col = 2, lty = 2)
abline(v=Xbar, col = 2, lty = 2)
Lin9.3 = lm(Y ~ X, data=Exa9.3)
summary(Lin9.3)
abline(Lin9.3, col=2)
#
#
Exa9.7=read.csv(file="EXA_C09_S07_01.csv", header=TRUE)
names(Exa9.7)
plot(Exa9.7$CV ~ Exa9.7$HEIGHT, pch = 20)
abline(h=mean(Exa9.7$CV), col = 2, lty = 2)
abline(v=mean(Exa9.7$HEIGHT), col = 2, lty = 2)
Lin9.7 = lm(CV ~ HEIGHT, data = Exa9.7)
abline(Lin9.7, col=2)
summary(Lin9.7)
#
cor(Exa7.1$HEIGHT,Exa7.1$CV)
cor.test(Exa9.7$HEIGHT, Exa9.7$CV, method="pearson", alternative = "two.sided")
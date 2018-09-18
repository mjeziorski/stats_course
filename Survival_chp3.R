#
# Survival analysis with the ALL data
#
library("survival")
library("tidyverce")
library("coin")
#
data("glioma", packaage = "coin")
leuk <- read_csv(file="DataRegressBook/Chap3/leuk.csv")

plot(survfit(Surv(time, cens) ~ group, data=leuk), main = "Acute Lymphoblastic Leukemia", lty = c(2,1), ylab = "Probability", xlab = "Remission time weeks", legend.text = c("Control", "6-MP"), legend.bty = "n")
plot(survfit(Surv(time, cens) ~ group, data=leuk), main = "Acute Lymphoblastic Leukemia", lty = c(1,2) )
plot(survfit(Surv(time, cens) ~ group, data=leuk), main = "Acute Lymphoblastic Leukemia", lty = c(1,2), ylab = "Probability", xlab = "Remission time weeks")
survdiff(Surv(time, cens) ~ group, data=leuk)
#
g3 <- subset(glioma, histology == "Grade3")
g4 <- subset(glioma, histology == "GBM")
#
plot(survfit(Surv(time, event) ~ group, data = g3), main = "Grade III Glioma", lty = c(2,1), ylab = "Probability", xlab = "Survival Months")
plot(survfit(Surv(time, event) ~ group, data = g4), main = "Grade IV Glioma", lty = c(2,1), ylab = "Probability", xlab = "Survival Months")
#
surv_test(Surv(time, event) ~ group, data = g3, distribution = "exact")
surv_test(Surv(time, event) ~ group, data = g4, distribution = "exact")
#
surv_test(Surv(time, event) ~ group | histology, data = glioma, distribution = approximate(B = 10000))

#
# Survival analysis with the ALL data
#
leuk <- read_csv(file="DataRegressBook/Chap3/leuk.csv")
survdiff(Surv(time, cens) ~ group, data=leuk)
plot(survfit(Surv(time, cens) ~ group, data=leuk), main = "Acute Lymphoblastic Leukemia", lty = c(2,1), ylab = "Probability", xlab = "Remission time weeks", legend.text = c("Control", "6-MP"), legend.bty = "n")
plot(survfit(Surv(time, cens) ~ group, data=leuk), main = "Acute Lymphoblastic Leukemia", lty = c(1,2) )
plot(survfit(Surv(time, cens) ~ group, data=leuk), main = "Acute Lymphoblastic Leukemia", lty = c(1,2), ylab = "Probability", xlab = "Remission time weeks")
#
#

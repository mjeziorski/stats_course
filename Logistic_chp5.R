# Examples HSAUR2 Logistic regression.
# Erythrocyte sedimentation rate (ESR). ESR increases when protein levels rise
# associated with chronic infectons, rheumatic conditions, etc
# the level of ESR is not important as long as it is < 20mm/hr (healthy)
# to asses ESR as useful diagnostic tool.
# ESR > 20mm/hr for proteins fibrinogen and gamma globulin, using glm
library(tidyverse)
library(HSAUR2)
data("plasma", package = "HSAUR2")
layout(matrix(1:2, ncol = 2))
cdplot(ESR ~ fibrinogen, data = plasma)
cdplot(ESR ~ globulin, data = plasma)
# glm general linear model default is logistic for binomial distrbution
plasma_glm01 <- glm(ESR ~ fibrinogen, data = plasma, family = binomial())
summary(plasma_glm01)
# coeff fibrinogen is sifnificative 5%
# one unit change in this variable increases the log-odds in favor of
# ESR > 20mm/hr
confint(plasma_glm01, parm = "fibrinogen")
exp(coef(plasma_glm01)["fibrinogen"])
exp(confint(plasma_glm01, parm = "fibrinogen"))
# full model with two variables
plasma_glm02 <- glm(ESR ~ fibrinogen + globulin, data = plasma, family = binomial())
summary(plasma_glm02)
# Comparing the resudial deviance of the models:
# residual deviance 01: 24.84 residual deviance 02: 22.971 -> 1.869 (1.87)
# to test for significance R take the lgm with a chisq
# the 1.87 -> globulin has no influence in the ESR
anova(plasma_glm01, plasma_glm02, test = "Chisq")
# Estimates conditional probability of a ESR > 20 for all observations
prob <- predict(plasma_glm02, type = "response")
layout(matrix(1:1, ncol = 1))
plot(globulin~ fibrinogen, data = plasma, xlim = c(2, 6), ylim = c(25, 55), pch = ".")
symbols(plasma$fibrinogen, plasma$globulin, circles = prob, add = TRUE)
# 
#
data("womensrole", package = "HSAUR2")
fmod <- cbind(agree, disagree) ~ gender + education
womensrole_glm01 <- glm(fmod, data = womensrole, family = binomial())
summary(womensrole_glm01)
role.fitted01 <- predict(womensrole_glm01, type = "response")
# definition of a plot of a fitted object
myplot <- function(role.fitted) {
  f <- womensrole$gender == "Female"
  plot(womensrole$education, role.fitted, type = "n", ylab= "Probability of agreeing", xlab= "Education", ylim = c(0,1))
  lines(womensrole$education[!f], role.fitted[!f], lty = 1)
  lines(womensrole$education[f], role.fitted[f], lty = 2)
  lgtxt <- c("Fitted (Males)", "Fitted (Females)")
  legend("topright", lgtxt, lty = 1:2, bty = "n")
  y <- womensrole$agree / (womensrole$agree + womensrole$disagree)
  text(womensrole$education, y, ifelse(f, "\\VE", "\\MA"), family = "HersheySerif", cex = 1.25)
}
#
myplot(role.fitted01)
# Now with interaction terms
fm02 <- cbind(agree, disagree) ~ gender * education
womensrole_glm02 <- glm(fm02, data = womensrole, family = binomial())
summary(womensrole_glm02)
role.fitted02 <- predict(womensrole_glm02, type = "response")
myplot(role.fitted02)
plot(womensrole_glm02)
# To estimate de fit, we will use deviace residual
# 
res <- residuals(womensrole_glm02, type = "deviance")
plot(predict(womensrole_glm02), res, xlab= "Fitted values", ylab= "Residuals", ylim = max(abs(res)) * c(-1,1))
abline(h = 0, lty = 2)
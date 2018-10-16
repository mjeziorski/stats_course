# Multiple linear linear regression
#
# Chapter 4 examples. 4.1
library(tidyverse)
hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
hers_nodi <- filter(hers, diabetes == "no")
# Simple linear model with HERS data for women without diebetes
ggplot(data = hers_nodi, mapping = aes(x = exercise, y = glucose)) + 
  geom_boxplot(na.rm = TRUE) + facet_grid( . ~ diabetes) + geom_jitter(height = 0.15, width = 0.15)
# The simple linear model adjust the exercise like in table 4.1
hers_nodi_Fit <- lm(glucose ~ exercise, data = hers_nodi)
summary(hers_nodi_Fit)
# and for obtaining the table 4.2 with multiple linear model
hers_nodi_Fit2 <- lm(glucose ~ exercise + age + drinkany + BMI, data = hers_nodi)
summary(hers_nodi_Fit2)
#
# Example of multiple linear regression using the clouds data
# clouds from HSAUR
library(HSAUR2)
data(clouds)
sample_n(clouds, 5)
# looking the datafor rainfall
# boxplot(rainfall~seeding, data=clouds)
# boxplot(rainfall~echomotion, data=clouds)
layout(matrix(1:2, ncol = 2))
bxpseeding <- boxplot(rainfall ~ seeding, data = clouds, ylab = "Rainfall", xlab = "Seeding")
bxpecho <- boxplot(rainfall ~ echomotion, data = clouds, ylab = "Rainfall", xlab = "Echo Motion")
# 
layout(matrix(1:4, nrow = 2))
plot(rainfall ~ time, data = clouds)
plot(rainfall ~ cloudcover, data = clouds)
plot(rainfall ~ sne, data = clouds, xlab="S-Ne criterion")
plot(rainfall ~ prewetness, data = clouds)
#
clouds_formula <- rainfall ~ seeding + seeding:(sne+cloudcover+prewetness+echomotion) + time
Xstar <- model.matrix(clouds_formula, data = clouds)
attr(Xstar, "contrasts")
clouds_lm <- lm(clouds_formula, data = clouds)
summary(clouds_lm)
layout(matrix(1:1, nrow = 1))
# to list the betas* with the:
betaStar <- coef(clouds_lm)
betaStar
# to understand the relation of seeding and sne
psymb <- as.numeric(clouds$seeding)
plot(rainfall ~ sne, data = clouds, pch = psymb, xlab = "S-Ne criterion")
abline(lm(rainfall ~ sne, data = clouds, subset = seeding == "no"))
abline(lm(rainfall ~ sne, data = clouds, subset = seeding == "yes"), lty = 2)
legend("topright", legend = c("No seeding", "Seeding"), pch = 1:2, lty = 1:2, bty = "n")
#
# and the Covariant matrix Cov(beta*) with:
VbetaStar <- vcov(clouds_lm)
# Where the square roots of the diagonal elements are the standart errors 
sqrt(diag(VbetaStar))
clouds_resid <- residuals(clouds_lm)
clouds_fitted <- fitted(clouds_lm)
# residuals and the fitted values can be used to construct diagnostic plot
plot(clouds_fitted, clouds_resid, xlab = "Fitted values", ylab = "Residuals", type = "n", ylim = max(abs(clouds_resid)) * c(-1, 1))
abline(h = 0, lty = 2)
textplot(clouds_fitted, clouds_resid, words = rownames(clouds), new = FALSE)
qqnorm(clouds_resid, ylab = "Residuals")
qqline(clouds_resid)

# Regression with categorical variables. Dummy Coding Essentials in R
# Examples from the STHDA
library(tidyverse)
library(car)
data(Salaries)
# Inspecting 5 lines of the Salaries database
sample_n(Salaries, 5)
# model of salary with sex
model <- lm(salary ~ sex, data = Salaries)
summary(model)$coef
# contrasts() will show the dummy coding of the sex variable of the 
# to see the way R has encoded rank head(res[, -1])
contrasts(Salaries$sex)
SalariesM <- Salaries %>%
  mutate(sex = relevel(sex, ref = "Male"))
# to change the reference dummy coding
model <- lm(salary ~ sex, data = Salaries)
summary(model)$coef
#
res <- model.matrix(~rank, data = Salaries)
head(res[, -1])
# for multiple variables with interaction
model_Salar <- lm(salary ~ yrs.service + rank + discipline + sex, data=Salaries)
summary(model_Salar)

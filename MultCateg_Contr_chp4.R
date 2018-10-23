# Multiple categorical predictors, coding of miltilevel categorical varibles
# 

# IMPORTANT : Specify the order of factor levels. Otherwise R will alphabetize them! 
#
# Multilevel categorical predictors Chap 4 example HERS with physact
# libraries
#
library(car)
library(emmeans)
library(multcomp)
library(tidyverse)
#

hers <- read_csv("DataRegressBook/Chap3/hersdata.csv")
hers_nodi <- filter(hers, diabetes == "no")
# Multilever categorical multiple linear model for women without diebetes
# To get table 4.4 Regression of physical activity on glucose
# hers_nodi$physact <- factor(hers_nodi$physact)
# hers_nodi <- mutate(hers_nodi, physact = factor(physact))
hers_nodi <- mutate(hers_nodi, physact = factor(physact, levels=c("much less active","somewhat less active","about as active","somewhat more active","much more active")))
levels(hers_nodi$physact)
ggplot(data = hers_nodi, mapping = aes(x = physact, y = glucose)) + geom_boxplot(na.rm = TRUE)
glucose_fit_act <- lm(glucose ~ physact, data = hers_nodi)
# betaStar <- coef(glucose_fit_act)
# betaStar
# Xstar <- model.matrix(glucose ~ physact, data = hers_nodi)
# and the Covariant matrix
# Cov_glucose_betaStar <- vcov(glucose_fit_act)
# Where the square roots of the diagonal elements are the standart errors 
# sqrt(diag(Cov_glucose_betaStar))
Anova(glucose_fit_act, type="II")
summary(glucose_fit_act)
layout(matrix(1:4, nrow = 2))
plot(glucose_fit_act)

glucose_lstsqr <- emmeans(glucose_fit_act, "physact")
# Contrasts
Contrasts_glu = list(MAvsLA          = c(-1, -1, 0,  1,  1),
                     MAvsLAforMuch   = c(-1,  0, 0,  0,  1),
                     MAvsLAforSome   = c( 0, -1, 0,  1,  0),
                     MLAvsC          = c(-1,  0, 1,  0,  0),
                     SLAvsC          = c( 0, -1, 1,  0,  0),
                     SMAvsC          = c( 0,  0,-1,  1,  0),
                     MMAvsC          = c( 0,  0,-1,  0,  1),
                     LinTrend_phys   = c(-2, -1, 0,  1,  2))
contrast(glucose_lstsqr, Contrasts_glu, adjust="sidak")
# compare the results with least-squares adjusted with sidak, FWE.
# With adjust="none", results will be the same as the aov method.
contrast(glucose_lstsqr, Contrasts_glu, adjust="bonferroni")

# Same cotrasts with multicomp library
Input = ("
Contrast.Name     MLA SLA AAA SMA MMA
 MAvsLA           -1  -1   0   1   1
 MAvsLAforMuch    -1   0   0   0   1
 MAvsLAforSome     0  -1   0   1   0
 MLAvsC           -1   0   1   0   0
 SLAvsC            0  -1   1   0   0
 SMAvsC            0   0  -1   1   0
 MMAvsC            0   0  -1   0   1
 LinearTrending   -2  -1   0   1   2
")
Cont_glucose_Matriz = as.matrix(read.table(textConnection(Input), header=TRUE, row.names=1))
Cont_glucose_Matriz
G = glht(glucose_fit_act, linfct = mcp(physact = Cont_glucose_Matriz))
G$linfct
summary(G, test=adjusted("single-step"))

# From https://rcompanion.org/rcompanion/h_01.html
# 
# Example from R companion: Contrasts in Linear Models
Input = ("
Treatment   Response
 'D1:C1'    1.0
 'D1:C1'    1.2
 'D1:C1'    1.3
 'D1:C2'    2.1
 'D1:C2'    2.2
 'D1:C2'    2.3
 'D2:C1'    1.4
 'D2:C1'    1.6
 'D2:C1'    1.7
 'D2:C2'    2.5
 'D2:C2'    2.6
 'D2:C2'    2.8
 'Control'  1.0
 'Control'  0.9
 'Control'  0.8
")
Data = read.table(textConnection(Input),header=TRUE)
Data$Treatment = factor(Data$Treatment, levels=unique(Data$Treatment))
# Specifying the order of factor levels
Data
levels(Data$Treatment)
ggplot(data = Data, mapping = aes(x = Treatment, y = Response), labs( x = "Treatment", y = "Response")) + geom_boxplot(na.rm = TRUE)
# boxplot(Response ~ Treatment, data = Data, ylab="Response", xlab="Treatment")

###  Define linear model
model = lm(Response ~ Treatment, data = Data)
# must have library car
Anova(model, type="II")
summary(model)
# to construct the contrasts you better see the orther 
leastsq = emmeans(model, "Treatment")
# Contrasts for factors
Contrasts = list(D1vsD2          = c(1,  1, -1, -1,  0),
                 C1vsC2          = c(1, -1,  1, -1,  0),
                 InteractionDC   = c(1, -1, -1,  1,  0),
                 C1vsC2forD1only = c(1, -1,  0,  0,  0),
                 C1vsC2forD2only = c(0,  0,  1, -1,  0),
                 TreatsvsControl = c(1,  1,  1,  1, -4),
                 T1vsC           = c(1,  0,  0,  0, -1),
                 T2vsC           = c(0,  1,  0,  0, -1),
                 T3vsC           = c(0,  0,  1,  0, -1),
                 T4vsC           = c(0,  0,  0,  1, -1))
### The column names match the order of levels of the treatment variable
### The coefficients of each row sum to 0
contrast(leastsq, Contrasts, adjust="sidak")
#
# Example with multicomp
Input = ("
Contrast.Name     D1C2  D1C2 D2C1 D2C2  Control
 D1vsD2            1     1   -1   -1     0
 C1vsC2            1    -1    1   -1     0
 InteractionDC     1    -1   -1    1     0
 C1vsC2forD1only   1    -1    0    0     0
 C1vsC2forD2only   0     0    1   -1     0
 TreatsvsControl   1     1    1    1    -4
 T1vsC             1     0    0    0    -1
 T2vsC             0     1    0    0    -1
 T3vsC             0     0    1    0    -1
 T4vsC             0     0    0    1    -1
")
Matriz = as.matrix(read.table(textConnection(Input), header=TRUE, row.names=1))
Matriz
G = glht(model, linfct = mcp(Treatment = Matriz))
G$linfct
summary(G, test=adjusted("single-step"))
# 
# More examples.
# Exmple global F-test within a group of treatments, post-hoc comparisons could still be made 
# within the independent variable.
Input = ("
Treatment          Response
 Merlot             5
 Merlot             6
 Merlot             7
 Cabernet           8
 Cabernet           9
 Cabernet          10
 Syrah             11
 Syrah             12
 Syrah             13
 Chardonnay         1
 Chardonnay         2
 Chardonnay         3
 Riesling           1
 Riesling           2 
 Riesling           2
 Gewürtztraminer    1 
 Gewürtztraminer    2
 Gewürtztraminer    4
")

Data = read.table(textConnection(Input),header=TRUE)
# factors: Specify the order of factor levels
Data$Treatment = factor(Data$Treatment, levels=unique(Data$Treatment))
levels(Data$Treatment)
#
ggplot(data = Data, mapping = aes(x = Treatment, y = Response), labs( x = "Treatment", y = "Response")) + geom_boxplot(na.rm = TRUE)
Wine_model = lm(Response ~ Treatment, data = Data)
summary(Wine_model)
# Least square means memeans library
leastsqr = emmeans(Wine_model, "Treatment")
Contrasts = list(Red_line1   = c(1, -1,  0,  0,  0,  0),
                 Red_line2   = c(0,  1, -1,  0,  0,  0))
# Is there an effect within red wine ?
Red_Test = contrast(leastsqr, Contrasts)
test(Red_Test, joint=TRUE)
# Is there an effect within white wine ?
Contrasts = list(White_line1   = c(0,  0,  0,  1, -1,  0),
                 White_line2   = c(0,  0,  0,  0,  1, -1))
White_Test = contrast(leastsqr, Contrasts)
test(White_Test, joint=TRUE)
# Is there a difference between red and white wines?  And, mean separation for red wine
Contrasts = list(Red_vs_white    = c( 1,  1,  1, -1, -1, -1),
                 Merlot_vs_Cab   = c( 1, -1,  0,  0,  0,  0),
                 Cab_vs_Syrah    = c( 0,  1, -1,  0,  0,  0),
                 Syrah_vs_Merlot = c(-1,  0,  1,  0,  0,  0))
contrast(leastsqr, Contrasts, adjust="sidak")

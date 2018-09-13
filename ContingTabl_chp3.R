# #############################################################################
# Contingency tables
# frequency variables
# Two-bytwo contngency table for CHD and corneal arcus
# in the WCGS data set for CHD
library(tidyverse)
wcgs <- read_csv(file="DataRegressBook/Chap2/wcgs.csv")
tab_wcgs <- table(wcgs$chd69, wcgs$arcus, dnn = c('CHD','arcus'))
ftab_wcgs <- ftable(chd69~arcus~chd69, data= wcgs, dnn = c('CHD','arcus'))
#
library(gmodels)
CrossTable(wcgs$chd69, wcgs$arcus, dnn = c('CHD','arcus'))
#
chisq.test(tab_wcgs)
# chisq.test(tab_wcgs, simulate.p.value = TRUE)

# Fisher exact test, for small data table Chp 3 example table 3.6 Female
# partner's HIV status by AIDS dignosis of male partner
#
female.partner = matrix(c(3,4,2,22), nrow = 2)
fisher.test(female.pertner)

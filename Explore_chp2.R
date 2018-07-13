# This is the code to follow the chapter 2 of the Regression Methods in 
# Biostatistics. We load the WCGS data and then vissualize the SBP
# we use tidyverse library to work.
#
library(tidyverse)
wcgs <- read_csv("DataRegressBook/wcgs.csv")

wcgs
# to see the variables and see some of the variable names
summary(wcgs$sbp)
# to estimate the (max - min)/#bins
# #bins 1+3.3log(3144) ~ 13 so using #bins 15
# binswith 8.8 -> 9
ggplot(data = wcgs) + geom_histogram(mapping = aes(x = sbp), binwidth = 9.0)
boxplot(wcgs$sbp)
# other boxplot comments

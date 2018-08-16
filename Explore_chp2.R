# This is the code to follow the chapter 2 of the Regression Methods in 
# Biostatistics. We load the WCGS data and then vissualize the SBP
# we use tidyverse library to work.
#
library(tidyverse)
wcgs <- read_csv("DataRegressBook/Chap2/wcgs.csv")
#
wcgs
glimpse(wcgs)
# freq() will show the variables that are cherecters and will make a bar plot of the frequencies
freq(wcgs)
profiling_num(wcgs)
# will show all the numerical values
# to see the variables and see some of the variable names using an R original function
summary(wcgs)
summary(wcgs$sbp)
#

# to estimate the (max - min)/#bins
# #bins 1+3.3log(3144) ~ 13 so using #bins 15
# binswith 8.8 -> 9
ggplot(data = wcgs) + geom_histogram(mapping = aes(x = sbp), binwidth = 9.0)
# to generate fig 2.3 or the systolic blood pressure
boxplot(wcgs$sbp)
ggplot(data = wcgs, aes(y = sbp)) + geom_boxplot()
# ggplot(data = wcgs, aes(y = sbp)) + geom_boxplot() + geom_jitter()
# other boxplot comments
# ggplot(data = wcgs) + geom_bar(mapping = aes(x = sbp, fill = weight))
#
# ggplot(data = wcgs) + geom_bar(mapping = aes(x = sbp, fill = weight), position = "dodge")
ggplot(data = wcgs) + geom_qq(aes(sample = sbp))
# ggplot(data = wcgs) + geom_qq(aes(sample = 1:310), dist = qt, dparam = list(df=5)) sample, sbp ..theoretical..
# scatter plot of the sbp data over weight 
ggplot(data = wcgs) + geom_point(mapping = aes(x = weight, y = sbp))
# to add a smoothed representative plot figure 2.9 
ggplot(data = wcgs) + geom_point(mapping = aes(x = weight, y = sbp)) + geom_smooth(mapping = aes(x = weight, y = sbp))
# To combine a categorical variable
# now to generate Figura 2.10 for systolic blood pressure by behavior pattern.
ggplot(data = wcgs) + geom_boxplot(mapping = aes(x = behpat, y = sbp))
ggplot(data = wcgs, aes(x = behpat, y = sbp)) + geom_boxplot()
# ggplot(data = wcgs, aes(x = behpat, y = sbp)) + geom_boxplot() + geom_jitter()
ggplot(data = wcgs, aes(x = behpat, y = sbp)) + geom_boxplot() + geom_jitter(height = .05, width = .05)

# Multivariable descriptions
# There is an R plot function for scatterplot matrix of correlation calcualtion (pairs(...)). To obtain the figure 2.11 of 
# the SBP, age weight and height
pairs(~sbp+age+weight+height, data=wcgs)
pairs(~sbp+age+weight+height, data=wcgs, main="SBP, age, weight and height scatterplot matrix")
pairs(~sbp+age+weight+height, data=wcgs, lower.panel=panel.smooth, main="WCGS Scatterplot Matrix")
# to plot the fig 2.12. Scatter plot of SBP vs Weight by Behavior Pattern
ggplot(data = wcgs) + geom_point(mapping = aes(x = weight, y = sbp)) + facet_wrap(~ behpat, nrow = 2)

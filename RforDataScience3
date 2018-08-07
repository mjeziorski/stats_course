#
# install.packages("tidyverse")
# install.packages("funModeling")
# install.packages("Hmisc")

setwd("~/Dropbox/Fdo/ClaseStats/RegresionClass/RegresionR_code")
library(tidyverse)
# library(funModeling)  
# library(Hmisc)
mpg
#
#
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + geom_point(mapping = aes(x = cyl, y = hwy))

# different points and color
#
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy), color = "red")

# Facet
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_wrap(~ class, nrow = 2)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid(drv ~ cyl)
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy)) + facet_grid( . ~ cyl)

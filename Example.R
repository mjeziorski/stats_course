# ######################################################
# Logistic regression in R from R-Bloggers
# Example from R-Bloggers, binomial logistic regression
# using Kaggle Titanic dataset to make a model multinomial legistic regression
# ############
#
# First in this public data frirst is fo read the set putting NA in the 
# missing data this is NA to the " " empty entries
# 
training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))

# to test for empty values
sapply(training.data.raw,function(x) sum(is.na(x)))
# to estimate the empty vaues
sapply(training.data.raw, function(x) length(unique(x)))
#
# Amelia data has a missmap function fo mising data
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")
#
# In the Titanic data set we will live out the data 1, 4, 9 
data <- subset(training.data.raw,select=c(1,2,4,5,6,7,9,11))

# For the missing data points tin age we will use the average 
# age to sustitute for the empty entry points
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

# As far as categorical variables are concerned, using the read.table() 
# or read.csv() by default will encode the categorical variables as factors 
# A factor is how R deals categorical variables
is.factor(data$Sex)
is.factor(data$Embarked)

# how R is going to deal with the categorical variables, we can use the 
# contrasts() function. This function will show us how the variables have
# been dummyfied by R and how to interpret them in a model
contrasts(data$Sex)
contrasts(data$Embarked)
#
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

# Model
# training set will be used to fit our model which we will be testing over 
# the testing set
train <- data[1:800,]
test <- data[801:889,]

# To fit the model important to specify the parameter family=binomial in 
# the glm() function
model <- glm(Survived ~. ,family=binomial(link='logit'), data=train)
summary(model)
anova(model, test="Chisq")

# To test the model
library(pscl)
pR2(model)

fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')

fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

# 
library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
# Multiple categorical predictors, coding of miltilevel categorical varibles
# 
# Example from R companion: Contrasts in Linear Models
# IMPORTANT : Specify the order of factor levels. Otherwise R will alphabetize them! 

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
boxplot(Response ~ Treatment, data = Data, ylab="Response", xlab="Treatment")

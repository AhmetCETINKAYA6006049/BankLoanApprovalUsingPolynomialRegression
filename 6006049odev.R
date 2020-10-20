#Polynomial Regression

#Import the dataset
#install.packages("readxl")
library("readxl")
dataset = read_excel('loandeneme.xlsx', sheet="bankcustomer")

#Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$LoanAmountinK, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

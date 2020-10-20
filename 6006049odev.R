#Polynomial Regression

#Import the dataset
#install.packages("readxl")
library("readxl")
dataset = read_excel('loandeneme.xlsx', sheet="bankcustomer")
dataset = dataset[2:5]

#Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$LoanAmountinK, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Feature Scaling
#training_set =scale(traning_set)
#test_set =scale(test_set)

#Fitting Linear Regresion to the dataset
lin_reg = lm(formula = LoanAmountinK ~ .,
             data = dataset)


#Fitting Polynomial Regresion to the dataset
dataset$Income2 = dataset$Income^2
dataset$CreditCards2 = dataset$CreditCards^2
dataset$LoanAccounts2 = dataset$LoanAccounts^2

poly_reg = lm(formula = LoanAmountinK ~ .,
              data = dataset)



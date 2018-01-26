# Multiples Linear Regression
# Using Backwareds Elimination
# E. Claire Fritzler

# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Process the Categorical data
dataset$State = factor(dataset$State,
                         levels = c('New York', 'California', 'Florida'),
                         labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
trainSet = subset(dataset, split == TRUE)
testSet = subset(dataset, split == FALSE)

# Fitting Regression to the Data Set
# Profit is a linear combination of the independent variables
regressor = lm(formula = Profit ~ .,
               data = trainSet)

# Predicting the Test set results
y_pred = predict(regressor, newdata = testSet)

# Building Optimal Model using Backwards Elimination
regressor = lm(formula = Profit ~ .,
               data = dataset)
summary(regressor)
# Remove State2 only (as opposed to removing State directly)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + factor(State, exclude = 2),
                data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)

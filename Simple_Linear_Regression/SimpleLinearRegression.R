# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('Salary_Data.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Regression to the Data Set
# Salary is proportional to the years of experience
regressor = lm(formula = Salary ~ YearsExperience, 
               data = training_set) 

# Predicting the Test set results
yPred = predict(regressor, newdata = test_set)

# Visualizing the Data
# install.packages('ggplot2')
library(ggplot2)
ggplot()+
  geom_point(aes(x = training_set$YearsExperience, 
                 y = training_set$Salary),
                colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, 
                y = predict(regressor, newdata = training_set)),
                colour = 'blue') +
  ggtitle('Salary vs Experience (Training Set') +
  xlab('Years of Experience') +
  ylab('Salary')

# Test Set 
ggplot()+
  geom_point(aes(x = test_set$YearsExperience, 
                 y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, 
                y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test Set') +
  xlab('Years of Experience') +
  ylab('Salary')


                



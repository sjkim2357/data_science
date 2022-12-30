## File 1_1_simple_variate_inferential
getwd()
setwd("C:/Users/sungj/Documents/Datasets")

# This is how we've done so far #
# Importing the dataset
dataset = read.csv('Salary_Data.csv')
View(dataset)

# Now our question is how much salary is predicted for those who have the YearsExperience value 6.5?
# Conduct Linear Regression 
regressor1 = lm(formula = Salary ~ YearsExperience,
               data = dataset) #we use training_set data

# if you want to know more about regressor:
summary(regressor1)

# y = 25792.2 + 9450.0x, therefore, y = 87802.2 when x = 6.5
(6.5*9540) + 25792.2
  
# plot a scatter plot
plot(dataset$YearsExperience,dataset$Salary,
     main='Regression for YearsExperience and Salary',
     xlab='YearsExperience',ylab='Salary')

# plot a regression line
abline(lm(Salary~YearsExperience,data=dataset),col='red')

##################################################################################
## File 1_2_simple_variate_predictive
# Now we're ready to begin another journey - Predictive statistics: simple linear model # 
# Importing the dataset
dataset = read.csv('Salary_Data.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3) 
# among 30 observations, 20 will be randomly selected for training. 
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
View(training_set)
View(test_set)

# Feature Scaling - because we only have one IV, we do not have to do feature scaling. 
# training_set = scale(training_set)
# test_set = scale(test_set) 

# Fitting (Conducting) Simple Linear Regression to the Training set
regressor2 = lm(formula = Salary ~ YearsExperience, data = training_set) #we use training_set data

# if you want to know more about regressor:
summary(regressor2) # y = 25592   + 9365x; regression from training set - predictive statistics           
summary(regressor1) # y = 25792.2 + 9450x; regression - inferential statistics

# Predicting the Test set results
?predict
y_pred = predict(regressor2, newdata = test_set)
## Using the regressor2 that we just built from training set, we want to predict the test set results
## we can then compare them to the test_set
y_pred #go to the test_set and then compare
View(test_set)

# Predicting a new result with linear regression
y_pred = predict(regressor2, data.frame(YearsExperience = 6.5))
y_pred
(6.5*9540) + 25792.2 # inferential statistics

# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor2, newdata = training_set)), #look newdata: we want a predicted line about training_set not test_set  
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

# Visualising the Test set results
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             colour = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = predict(regressor2, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')

########################################################################################
## 2_1_multivariate_inferential
## We're now handling more than one IV - Multiple linear regression ##
# We start from INFERENTIAL model first #
getwd()
setwd("C:/Users/sungj/Documents/Datasets")

# Importing the dataset
dataset = read.csv('50_Startups.csv')
View(dataset)

# dummy var - Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'))


# Fitting (Conducting) multiple linear regression to the whole data set
regressor3 = lm(formula = Profit ~ ., data = dataset)

summary(regressor3) #StateCalifornia, StateFlorida, Administration, Marketing.Spend - not meaningful     

# given the p-values, the formula can now be a simple linear regression
regressor4 = lm(formula = Profit ~ R.D.Spend, data = dataset)
summary(regressor4)

# Building the optimal model using Backward Elimination
## original
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
summary(regressor)

## original - State
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor)

## original - State - Marketing.Spend
regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)


## 2_2_multivariate_predictive
# Multiple linear regression (Predictive second) #
# Importing the dataset
dataset = read.csv('50_Startups.csv')

# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

View(dataset) #the name of the state now be encoded - 1, 2, 3

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
View(training_set)
View(test_set)

# Fitting (conducting) multiple linear regression to the training set
regressor2 = lm(formula = Profit ~ .,data = training_set)
summary(regressor2)

########################################################reference model
# given the p-values, the formula can now be a simple linear regression
regressor4 = lm(formula = Profit ~ R.D.Spend, data = training_set) 
summary(regressor4)  #bigger R^2
########################################################reference model

# Predicting the Test set results
y_pred1 = predict(regressor2, newdata = test_set) #regressor2 is the full regression model
#using the regressor, we want to predict our Test set results
y_pred1
View(test_set) #compare

# Predicting the Test set results
y_pred2 = predict(regressor4, newdata = test_set) #regressor4 only contains R.D.Spend in the model
#using the regressor, we want to predict our Test set results
y_pred2
View(test_set) #compare

# Because we are not sure about the accuracy of our model we can find the optimal model using Backward Elimination
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = training_set)
summary(regressor) 

regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = training_set)
summary(regressor)

regressor = lm(formula = Profit ~ R.D.Spend,
               data = training_set)
summary(regressor)

# Generally, We can refer to model performance metrics such as:
# adjusted R-squared, Akaike's Information Criteria, Bayesian information criteria.
# To learn more visit:
browseURL("http://www.sthda.com/english/articles/38-regression-model-validation/158-regression-model-accuracy-metrics-r-square-aic-bic-cp-and-more/")


#############automatic implementation of backward elimination #############
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.1 #0.1
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)



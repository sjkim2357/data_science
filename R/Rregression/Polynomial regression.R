# Now we're going to apply polynomial regression technique to build a prediction model # 
setwd("C:/Users/sungj/Documents/Datasets")

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
View(dataset)

# Now our question is how much salary is predicted for the level 6.5?
dataset = dataset[ , 2:3] #focus on the second and third columns

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$DependentVariable, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

# Feature Scaling #One IV
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting (conducting) linear regression to the dataset
lin_reg = lm(formula = Salary ~., data = dataset)
summary(lin_reg)

# Visualizing the linear regression results
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Linear regression') +
  xlab('Level') +
  ylab('Salary')

# Predicting a new result with linear regression
y_pred = predict(lin_reg, data.frame(Level = 6.5))
y_pred

# Fitting polynomial regression to the dataset
View(dataset)
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
View(dataset)
poly_reg = lm(formula = Salary ~., data = dataset)
summary(poly_reg)

# Visualizing the polynomial regression results
ggplot() + 
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Polynomial regression') +
  xlab('Level') +
  ylab('Salary')

# Predicting a new result with polynomial regression
y_pred2 = predict(poly_reg, data.frame(Level = 6.5,
                                       Level2 = 6.5^2,
                                       Level3 = 6.5^3,
                                       Level4 = 6.5^4))
y_pred2

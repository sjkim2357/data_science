#################### Classification #################

#################
###### KNN ######
#################
# Importing the dataset
getwd()
setwd("C:/Users/sungj/Documents/Datasets")
dataset = read.csv('Social_Network_Ads.csv')
View(dataset)
dataset = dataset[, 3:5] #from age to purchased
View(dataset)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
View(training_set)
View(test_set)

# Feature Scaling
training_set[-3] = scale(training_set[-3]) # feature scaling except the third column
test_set[-3] = scale(test_set[-3])
View(training_set)
View(test_set)

# Fitting K-NN to the Training set and Predicting the Test set results
## Unlike regression, there is no predict method for knn models. 
## Instead you train and receive predictions as part of a single call.
library(class)
?knn
y_pred = knn(train = training_set[, -3],
             test = test_set[, -3],
             cl = training_set[, 3], #factor of true classifications of training set
             k = 5)

y_pred
summary(y_pred)
## We fitted a knn model to training_set data (300 obs.) to see how well it predicts the test_set result. 
## The fitted knn model predicts 65 zeros and 35 ones about the test_set result.
## 0 0 0 0 0 1 1 1 0 0 <- prediction from the fitted model using training_set data
## 0 0 0 0 0 1 1 1 1 0 <- actual observation from the test_set

# To better compare the results, we make the Confusion matrix
y_pred
test_set[, 3]

cm = table(test_set[, 3], y_pred)
cm

# to interpret the cm:
browseURL("https://www.nbshare.io/notebook/626706996/Learn-And-Code-Confusion-Matrix-With-Python/")

# we have 6 + 5 incorrect predictions which is not that bad in ML.
# 5
cm[1,1]
cm[2,2]
cm.result = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
cm.result

# you need to install ElemStatLearn to visualize
browseURL("https://cran.r-project.org/web/packages/ElemStatLearn/index.html")
# download first, and then go Packages, check 'install from package archive file'


# Visualizing the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# we see a prediction boundary that is not linear, because knn is not a linear model. 
# There are many irregularities because that is how knn model work. 
# This prediction boundary is fitted to the training set using KNN - the visualized result of the model.
# If they did buy the SUV, it is green. 
# If they did not buy the SUV, it is red. 
# And the data points are actual observations from the training set. 
# Most of red points are in red region, and green points are in green region. 

# Visualizing the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train = training_set[, -3], test = grid_set, cl = training_set[, 3], k = 5)
plot(set[, -3],
     main = 'K-NN (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# We have a few incorrect predictions. 
# These data points are the 11 points in our confusion matrix. 
89/100 #11 incorrect predictions

#####################
### decision tree ###
#####################
# Importing the dataset
setwd("C:/Users/sungj/Documents/Datasets")
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]
View(dataset)

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Decision Tree classifier to the Training set
# Create the Decision Tree classifier
require(rpart)
?rpart
classifier = rpart(formula = Purchased ~.,
                   data = training_set)
summary(classifier) #too complicated

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3], type = 'class') 

#without 'type = 'class'', we will only get numeric probabilities.
y_pred
summary(y_pred)

## We fitted a decision tree model to training_set data (300 obs.) to see how well it predict the test_set result. 
## The fitted model predicts 59 zeros and 41 ones about the test_set result.
## 0 0 0 0 0 0 1... <- prediction from the fitted model using training_set data
## 0 0 0 0 0 1 1... <- actual observation from the test_set

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm

# we have 11 + 6 incorrect predictions.
cm.result = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
cm.result

# Plotting the decision tree
plot(classifier)
text(classifier)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3],
     main = 'Decision Tree Classifier (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = 'class')
plot(set[, -3], main = 'Decision Tree Classifier (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#####################
### random forest ###
#####################

# Importing the dataset
setwd("C:/Users/sungj/Documents/Datasets")
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting Random Forest classifier to the Training set
require(randomForest)
classifier = randomForest(x = training_set[-3],
                          y = training_set$Purchased,
                          ntree = 10)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])
y_pred

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm
cm.result = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
cm.result

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'Random Forest Classifier (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3], main = 'Random Forest Classifier (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#T-TEST#
library(datasets)  # Load/unload base packages manually

# LOAD DATA ################################################
head(iris)

### SELECT BY CATEGORY #######################################
# Versicolor
hist(iris$Sepal.Length[iris$Species == "versicolor"],
     main = "Sepal.Length: Versicolor")

# Virginica
hist(iris$Sepal.Length[iris$Species == "virginica"],
     main = "Sepal.Length: Virginica")

# Setosa
hist(iris$Sepal.Length[iris$Species == "setosa"],
     main = "Sepal.Length: Setosa")

# ALL DATA #################################################
hist(iris$Sepal.Length)
summary(iris$Sepal.Length)

#we can use xlim function: convenience Functions to set the limits Of the x and y Axis.
par(mfrow = c(3, 1)) #graphic parameter: put graphs in 3 rows and 1 column

hist(iris$Sepal.Length [iris$Species == "setosa"], #using selector([]) to specify the variable
     xlim = c(4, 9),#all three histograms can have the same x scale
     main = "Sepal.Length for Setosa", #title
     xlab = "", #no x-lable
     col = "red")

hist(iris$Sepal.Length [iris$Species == "versicolor"],
     xlim = c(4, 9),
     main = "Sepal.Length for Versicolor",
     xlab = "",
     col = "purple")

hist(iris$Sepal.Length [iris$Species == "virginica"],
     xlim = c(4, 9),
     main = "Sepal.Length for Virginica",
     xlab = "",
     col = "blue")

# TWO SAMPLE T-TEST
# We want to know the mean difference of Sepal.Length between Setosa and Versicolor is significant.
x <- iris[iris$Species == "setosa", ]$Sepal.Length
var(x)
y <- iris[iris$Species == "versicolor", ]$Sepal.Length
var(y)
## We assume non-equal variances: if equal variance is assumed, the pooled variance is used.    

## now let's make our ho first
### h0: The mean of setosa's Sepal.Length is equal to versicolor's Selpal.Length (no difference)
### such difference can be caused by either direction, therefore we assume two-sided tail
### h1: The mean of setosa's Sepal.Length is not equal to versicolor's Selpal.Length (no difference)
tt <- t.test(x, y, paired = FALSE, 
             alternative = "two.sided", 
             conf.level = 0.95, #95% confidence interval is used for the difference in means between the two 
             var.equal = FALSE) #non-equal variance
tt
#### h0 is within the rejection zone, meaning we can reject the null of no difference (i.e.equal).
#### mean of x and mean of y is indicated, and such difference is statistically significant. 

# clean up
detach("package:datasets", unload = TRUE)  
dev.off() 
cat("\014")

####################################################################
#######################practice with real data######################
####################################################################
getwd()
setwd("C:/Users/sungj/Documents/Datasets")

#####You want to import data.  
# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

# we are going to import STATA data.
mydata <- import("attend.dta")
head(mydata)
View(mydata)

attach(mydata)
plot(priGPA, termGPA, main="Plot of PriGPA and TermGPA" )

plot(priGPA, termGPA, col=priGPA+1, main="Plot of PriGPA and TermGPA" )
#different colors for every one point for priGPA

plot(priGPA, termGPA, col=termGPA+1, main="Plot of PriGPA and TermGPA" )
#different colors for every one point for termGPA

plot(priGPA, termGPA, col=priGPA+1, pch=priGPA+1, main="Plot of PriGPA and TermGPA" )
#different colors and different points

#you can also do this
plot(termGPA~priGPA, main="Plot of PriGPA and TermGPA")

#find missing values
is.na(mydata)
table(is.na(mydata))
summary(mydata)
#we have 6 missing values

#if you want to specify the rows with missing values
new_DF <- mydata[rowSums(is.na(mydata)) > 0,]
new_DF

####in case you have NA character values, you should run this first.
#mydata[mydata=='NA'] <- NA
##########################################################################

#na.omit
##Drop out any rows with missing values anywhere in them and forgets them forever.
#na.exclude
##Drop out rows with missing values, but keeps track of where they were (so that when you make predictions, for example, you end up with a vector whose length is that of the original response.)
na.exclude(mydata)
na.omit(mydata)

####In my case, I do this for the missing values
mydata2<-na.omit(mydata)
View(mydata2)
new_DF1 <- mydata2[rowSums(is.na(mydata2)) > 0,]
new_DF2
summary(mydata2)
##we have no missing values

##########################################################################
#if we want to extract specific columns and do the testing. 
#cbind: column bind
smallc<-cbind(termGPA,priGPA)
View(smallc)

#we can also get column means for each
meansmallc<-colMeans(smallc)
meansmallc

#covariace matrix for the two variables
covsmallc<-cov(smallc)
covsmallc

#Correlation Matrix between the two variables
Rxtab2 <- cor(smallc)
Rxtab2

#if we want to extract four specific columns
#cbind: column bind
smallc4<-cbind(termGPA,priGPA,ACT,final)
View(smallc4)
meansmallc4<-colMeans(smallc4)
meansmallc4

#covariace matrix for the four variables
covsmallc<-cov(smallc4)
covsmallc

#Correlation Matrix between the four variables
Rxtab4 <- cor(smallc4)
Rxtab4

#Clean up
dev.off()  
cat("\014")
rm(list=ls())

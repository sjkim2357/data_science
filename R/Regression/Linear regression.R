#regression#
library(ggplot2)

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

# we are going to import STATA data.
setwd("C:/Users/sungj/Documents/Datasets")
mydata <- import("attend.dta")
head(mydata)
View(mydata)

attach(mydata) #attach my data, then you can play with your column names
View(mydata)

#without jitter
ggplot(data=mydata, mapping=aes(x=attend, y=termGPA)) +
  geom_point() +
  labs(title="class attended out of 32 Versus GPA for term",
       x="attend",
       y="termGPA") +
  theme_bw()

#with jitter
ggplot(data=mydata, mapping=aes(x=attend, y=termGPA)) +
  geom_point(position="jitter") +
  labs(title="class attended out of 32 Versus GPA for term",
       x="attend",
       y="termGPA") +
  theme_bw()

#regression
reg1 <- lm(termGPA ~ attend, data=mydata)
#The object edulm is a list of many other objects that includes many summary statistics,
#e.g., hypothesis tests, and confidence intervals regarding the equation of the best fit line. 

summary(reg1)

reg1$coefficients
#the predicted value for termGPA (y) for person i with xi attendance is given by, y^i=0.625+0.0756x
#(Interpretation of coefficient) A one unit increase in attendance is associated with 0.0756 increase in term GPA.
#(Interpretation of R-squared) About 31.33% of the variation in the term GPA is association of students' attendance.

ggplot(data=mydata, mapping=aes(x=attend, y=termGPA)) +
  geom_point(position="jitter") +
  labs(title="class attended out of 32 Versus GPA for term",
       x="attend",
       y="termGPA") +
  geom_smooth(method="lm") +
  theme_bw()

resid(reg1)

x<-resid(reg1)
qqnorm(x)
qqline(x)
#if the data follows the linear line, it suggests the residuals are normally distributed. 

shapiro.test(x)
boxplot(x)
out <- boxplot.stats(x)$out
out_ind <- which(x %in% c(out))
out_ind

#With this information you can now easily go back to the specific rows in the dataset to verify them, 
#or print all variables for these outliers:
mydata[out_ind, ]
mean(attend)
mean(termGPA)

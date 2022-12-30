library(tidyverse)
library(lubridate)

mydata <- read_csv("C:/Users/sungj/Documents/Datasets/screening_exercise_orders_v201810.csv")
mydata
View(mydata)

mydata1<-arrange(mydata, customer_id)
mydata1

#group_by() to see the frequency of customer_id. 
count_customer_id <- group_by(mydata1, customer_id)
count_customer_id # Groups:   customer_id [8,814]  

#summarize creates a new data frame for each combination of grouping variables.
order_count1 <- summarise(count_customer_id,
                    count = n()) 
order_count1

#You can combine two tables with left_join():
mydata2<-mydata1 %>%
  left_join(order_count1, by = "customer_id")
mydata2
View(mydata2)

#you separate dttm vector into date and time and then remove the time column
mydata3<-mydata2 %>% 
  separate(date, into = c("date", "time"), sep = 10) %>% #separate dttm into two chr vectors
  select(-time) #removing time column

View(mydata3) #date is chr vector

mydata3$date<-as.Date(mydata3$date, format ="%Y-%m-%d") #change it to date vector
mydata3 

###fyi: you can do this way too##############
#now you remove time from date and then change it to dttm format
mydata2$date<-as.Date(substr(mydata2$date,1,10),format = "%Y-%m-%d")
##############################################

###rename
mydata3<-rename(mydata3, most_recent_order_date = date)
mydata3<-rename(mydata3, order_count = count)
mydata3

#plot the count of orders per week
?week
mydata3$week<-week(mydata3$most_recent_order_date)
mydata3

library(dplyr)
order_wk<- mydata3 %>% 
  group_by(week) %>%
  summarise(order_count = n())
order_wk
View(order_wk)
library(ggplot2)

#this is our original data representation
ggplot(data = order_wk) + 
  geom_point(mapping = aes(x = week, y = order_count))+
  labs(title = "Distribution of orders by week") +
  labs(
    x = "week",
    y = "counts of orders per week"
  )


#geom_smooth argument
ggplot(data = order_wk) + 
  geom_smooth(mapping = aes(x = week, y = order_count))+ 
  labs(title = "Distribution of orders by week")+
  labs(
    x = "week",
    y = "counts of orders per week"
  )

#compare
plot(order_wk$week,order_wk$order_count, xlab="Week", ylab = "count of orders per week", main = "Distribution of orders by week", type='b')

###what are the mean values of male and female? Are they different?
####################################
library(dplyr)
#as it is double precision, you cannot calculate means of genders
mydata4 <-mydata3 %>%
  mutate(gender = ifelse(gender == 0, "male", "female")) #as genders

mydata4$gender<-as.factor(mydata4$gender) #as.factor
mydata4

#we will have two different datasets, male and female
male_data<-mydata4 %>%
  filter(gender == 'male')
male_data
###you can do one of the below:
colMeans(male_data[,4]) #363.89
summary(male_data)

female_data<-mydata4 %>%
  filter(gender == 'female')
female_data

###you can do one of the below:
colMeans(female_data[,4]) #350.7084
summary(female_data)

####fyi: two sample t-test####
#method 1) Compute independent t-test using two datasets
res1 <- t.test(male_data$value, female_data$value, var.equal = TRUE)
res1
#method 2) Compute independent t-test by factor in a dataset
mydata4
res2 <- t.test(mydata4$value ~ mydata4$gender, var.equal = TRUE)
res2

###fyi, using some R base command, you can do this too:
attach(mydata4)
colMeans(mydata4[gender=='male', 4])
colMeans(mydata4[gender=='female', 4])

##################fyi: istead of ifelse(gender == 0, "male", "female")#################
##################we can also do this in making male and female #######################
mydata3
mydata3$gender[mydata3$gender == 0] <- "female"
mydata3$gender[mydata3$gender == 1] <- "male"
mydata3




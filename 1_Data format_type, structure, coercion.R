#################1-1#############################################################


#######################################################################################
#######################################################################################
############################Recap of vector concept in R###############################
#######################################################################################
#######################################################################################

#here is a vector containing three numeric values 2, 3, and 5:
c(2, 3, 5) #to run, ctrl+enter

#and here is a vector of logical values:
c(TRUE, FALSE, TRUE)

#the number of members in a vector is given by the length function:
length(c("aa", "bb", "cc"))



#use the c() function to combine vectors with more than one value:
fruits <- c("Apple", "oranges", "banana")
vegetables <- c("cabbage", "spinach", "tomatoes")
all_basket_items <- c(fruits, vegetables)
all_basket_items
#The result of this code is a vector with all 6 values.
#Look! Vectors have an order. it maintains the order of numbers
#it can also be confirmed by length function:
length(all_basket_items)



#Changing values in a vector
fruits
fruits[2] <- "strawberries" 
fruits
#This replaces the second element of the vector with "Strawberries"
all_basket_items <- c(fruits, vegetables)
all_basket_items

#repeating vectors
#To repeat the vector c(0, 0, 7) three times, use this code:
rep(c(0, 0, 7), times = 3)



#You also can repeat every value by specifying the argument each, like this:
rep(c(2, 4, 2), each = 3)

rep(c(2, 4, 2), times = 3) #compare


#You can tell R for each value how often it has to be repeated. eg:
rep(c(0, 7), times = c(4,2))
rep(c(0, 1, 7), times = c(0,1,3))
rep(c(0, 1, 7), times = c(3,1,4))




#functions
round(3.145)
#for more R base functions:
browseURL("http://www.sr.bham.ac.uk/~ajrs/R/r-function_list.html")
#do it by yourself!



#################1-2#############################################################
    

#######################################################################################
#######################################################################################
############################Data format: type and structure############################
#######################################################################################
#######################################################################################

###integer and double precision###

#if you want to know more about double precision.
browseURL("http://uc-r.github.io/integer_double/#:~:text=The%20two%20most%20common%20numeric,years%20without%20specifying%20these%20differences.")

# create a string of double-precision values
dbl_var <- c(1, 2.5, 4.5)  
dbl_var


#this is how double precision works
dbl_var1 <- c(1, 2.555, 4.577)  
dbl_var1


# placing an L after the values creates a string of integers
int_var <- c(1L, 6L, 10L)
int_var

#for instance: no L
int_var2 <- c(1, 6, 10)
int_var2


# identifies the vector type (double, integer, logical, or character)
typeof(int_var)

typeof(int_var2)

# converts integers to double-precision values
as.double(int_var)     
typeof(int_var)

int_var_coerced<-as.double(int_var)   
int_var_coerced
typeof(int_var_coerced)

# converts doubles to integers
dbl_var
dbl_var_coerced<-as.integer(dbl_var)   
dbl_var_coerced
typeof(dbl_var_coerced)




##########################################################
##########################################################
##########################################################
#######################17-Sep-2021########################
##########################################################
######################data formats########################
##########################################################
##########################################################
##########################################################


######data format: data types#####
# types of data: Numeric
n1 <- 15  # R is Double precision by default
n1
typeof(n1)
is.vector(n1)



n2 <- 1.5
n2
typeof(n2)



# types of data: Character
c1 <- "c"
c1
typeof(c1)
is.vector(c1)

c2 <- "a string of text"
c2
typeof(c2)


# types of data: Logical
l1 <- TRUE #you can do capital T too
l1
typeof(l1)

l2 <- F
l2
typeof(l2)



#####data format: data structure#####

###1. structure of data: Vector
v1 <- c(1, 2, 3, 4, 5) #concatenate (link things together in a chain or series)
v1 #call out the object, then it shows your vector without the comma
is.vector(v1) #we ask, is this vector?

v2 <- c("a", "b", "c")#I can also make a vector of characters
v2 # #call out the object, then it shows your vector without the comma 
is.vector(v2) 

v3 <- c(TRUE, TRUE, FALSE, FALSE, TRUE) #we can make vectors from logical values of true and false 
v3
is.vector(v3)

###it means that we can make vectors using all different data types(numeric, character, and logical)


###2. structure of data: Matrix 
#Now we want to make more than one dimension
#using the matrix function, combine the value T, T, F, F, T, F with 2 rows,
#R figures out the # of columns automatically

m1 <- matrix(c(T, T, F, F, T, F), nrow = 2)
m1
is.vector(m1)
is.matrix(m1)
#we can have 3 rows matrix
m1.1 <- matrix(c(T, T, F, F, T, F), nrow = 3)
m1.1
#we see how nrow works(nrow is the number of your rows)

m1.2 <- matrix(c(T, T, F, F, T, F), nrow = 5)
#it does not work. 


#the second matrix is something that I shaped it intentionally as we see below
#R does not break it up rows and columns

m2 <- matrix(c("a", "b", 
               "c", "d"), 
             nrow = 2,
             byrow = T) #Organize it by rows
m2
#if you use by row = t, R organizes your data in a row in the order in which they appear. 

m3 <- matrix(c("a", "b", 
               "c", "d"), 
             nrow = 2) #it now go by column
m3
#We now understand how R organizes the character by default.
#R organizes your data in a column in the order in which they appear. 





###3. structure of data:  Data frame
#data frame allows us to combine vectors of the same length of different types. 

vNumeric   <- c(1, 2, 3) #I am creating vector of numeric variable
vNumeric
vCharacter <- c("a", "b", "c")  #I am creating vector of character variable
vCharacter
vLogical   <- c(T, F, T) #I am creating vector of logical variable
vLogical
###we now have three different types of vectors
###What we are going to do is to bind the three different types of vectors


dfa <- cbind(vNumeric, vCharacter, vLogical) #cbind is function for column bind
dfa  # Matrix of one data type
#because, by just using (c), R turned your data into character variable
##we can confirm this
typeof(dfa)
typeof(vLogical)
typeof(vNumeric)


##If we do not want R to recognize our data as character variable,
##We have to add another function, i.e. as.data.frame 
df <- as.data.frame(cbind(vNumeric, vCharacter, vLogical)) #data frame consists of different data types
df  # Makes a data frame with three different data types
typeof(df) #structure of this data is list
##it maintains its original data types of each of the variable. 

typeof(dfa)
#we can also do this
dfa
dfa.1 <- as.data.frame(dfa)
dfa.1
typeof(dfa.1)

#dfa.1 and df is identical data frame.




###4. structure of data:  List
o1 <- c(1, 2, 3)
o1
o2 <- c("a", "b", "c", "d")
o2
o3 <- c(T, F, T, T, F)
o3

list1 <- list(o1, o2, o3)
list1
#01, 02, 03 is within list1
#it also maintains its original data formats. 

#we can make lists within lists too.
list2 <- list(o1, o2, o3, list1)  #our list1 is within list2
list2
#call out shows that o1, 02, 03, and list1(o1, o2, o3)








#################1-3#############################################################


#######################################################################################
#######################################################################################
############################Coercion: type and structure###############################
#######################################################################################
#######################################################################################



###1.type coercion: numeric(integer, double), character, logical 

## Automatic coercion ######################################
coerce1 <- c(1, "b", TRUE)
coerce1
typeof(coerce1)
# R regards the object as the most general format 



## Coerce double precision to integer ###############################
coerce2 <- 5
coerce2
typeof(coerce2)

coerce3 <- as.integer(coerce2)
coerce3
typeof(coerce3)



## Coerce character to double precision #############################
coerce4 <- c("1", "2", "3")
coerce4
typeof(coerce4)

coerce5 <- as.numeric(coerce4)
coerce5
typeof(coerce5)





###2.structure coercion: vector, matrix, data frame, list 

## Coerce matrix to data frame #############################
coerce6 <- matrix(1:9, nrow= 3) #by default, it arranges by column (c.f. by row = TRUE)
coerce6
typeof(coerce6)
is.matrix(coerce6)


coerce6.1 <- matrix(1:9, nrow= 3, byrow = T) #compare 
coerce6.1


coerce7 <- as.data.frame(coerce6)
coerce7
typeof(coerce7) 
#it was integer, but now it is list
#List is a multidimensional object that holds different data formats. 
is.matrix(coerce7)
#it is not matrix anymore
is.data.frame(coerce7)
#it is data frame (transformation of structure,i.e., matrix to data frame)

# CLEAN UP #################################################

# Clear environment
rm(list = ls())


# Clear console
cat("\014")  # ctrl+L






###################################################################
###################################################################
###################Why are we doing this?##########################
###################################################################
###################################################################


#####You want to import data.  

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

# we are going to import STATA data.
mydata <- import("C:/EMP530_R/attend.dta")
head(mydata)
View(mydata)

typeof(mydata) 
is.matrix(mydata) #matrix can only have index number as its column name
is.data.frame(mydata)
#it is not matrix but data frame. 

#if we want to extract two specific columns, termGPA(col. 2) and ACT(col. 4)
head(mydata)
#use cbind
smallmydata<-cbind(mydata[,2],mydata[,4]) #cbind it to make matrix
head(smallmydata)
#we have no head

typeof(smallmydata) 
is.matrix(smallmydata)
is.data.frame(smallmydata)
#Our original data was data frame, but now our subset data is matrix. 


#if we want to extract first, third, and fifth columns
#use cbind
head(mydata)
smallmydata2<-cbind(mydata[,1], mydata[,3], mydata[,5])
smallmydata2
head(smallmydata2)
#we have no head

typeof(smallmydata2) 
is.matrix(smallmydata2)
is.data.frame(smallmydata2)
#Now it is matrix. It is not data frame any more. 


#what is big deal about data frame and matrix?
###regression
head(mydata)
y<-mydata[,6] #column 6: percent classes attended (DV)
y
typeof(y) #it is double precision vector
is.matrix(y)
#we should change our variable y to matrix to do regression.
y_mat<-as.matrix(y)
y_mat
head(y_mat)
typeof(y_mat)
is.matrix(y_mat)

#we should also change our independent variables to matrix. 
head(mydata)
z<-as.matrix(mydata[,3:4]) # two IVs: cumulative GPA prior to term(Col 3) and ACT score (Col 4)
z
head(z)
is.matrix(z)
head(z)
#finally, we can do regression
reg73<-lm(y_mat~z)
summary(reg73)

#otherwise, you cannot do it 
head(mydata)
z1<-mydata[,3:4] #NO as.matrix
z1
head(z1) #looks same. compare head(z). Humans cannot see the difference.
is.matrix(z1)
is.data.frame(z1)
reg74<-lm(y~z1) #matrix by data frame
#invalid result


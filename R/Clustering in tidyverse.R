#row names#
#tibbles do not support rownames
browseURL("https://community.rstudio.com/t/will-rownames-on-tibbles-become-impossible-in-future/28904/2")
#Row names are not supported in tibble(), and are stripped by default in as_tibble().

#tidyverse community even locked conversation regarding this:
browseURL("https://github.com/tidyverse/tibble/issues/272")

# Detect row names
View(mtcars)
has_rownames(mtcars)
View(iris)
has_rownames(iris)

# Convert between row names and column
mtcars_tbl <- rownames_to_column(mtcars, var = "car") %>% #row names in mtcars are transferred to column, and named "car"
  as_tibble()

mtcars_tbl
View(mtcars_tbl)

# Adding rowid as a column
View(iris)
iris_rowid<-rowid_to_column(iris)
View(iris_rowid)

#if you want to visualize the row names in your visual representation,
#tibbles are not friendly
#you may want to study more about tibbles with row names:
browseURL("https://tbradley1013.github.io/2018/02/01/pca-in-a-tidy-verse-framework/")

#let's say you have a tibble that has no row names:
mtcars_tbl <- rownames_to_column(mtcars, var = "car") %>% as_tibble()
mtcars_tbl
View(mtcars_tbl) #car is not rownames, but variable!

#without row names, you will have hard time indicating the observation with its row names.
#let's do hierarchical clustering:
# Finding distance matrix using tibble
distance_mat2 <- dist(mtcars_tbl, method = 'euclidean')
distance_mat2

# Fitting Hierarchical clustering Model to dataset
set.seed(240)  # Setting seed
Hierar_cl <- hclust(distance_mat2, method = "average")
Hierar_cl

# Plotting dendrogram
plot(Hierar_cl) #because we have no row names!

# same thing happens when we do kmeans analysis:
km <- kmeans(mtcars_tbl[,2:12], centers = 4, nstart = 25)
fviz_cluster(km, data = mtcars_tbl[,2:12])

#we may want to put it back to the original one:
mtcars_WithRowName <- mtcars_tbl %>% 
  column_to_rownames(var="car")
View(mtcars_WithRowName)
mtcars_WithRowName

#one way of tackling tibble's no row name issue#
#we may try this to tackle tibble's no row name issue:
library(tidyverse)

#let's say you have a tibble that has no row names:
mtcars_tbl <- rownames_to_column(mtcars, var = "car") %>% as_tibble()
mtcars_tbl
View(mtcars_tbl) #no row names

#this is a new of building distance matrix with Row Name using tibble
library(magrittr)
attach(mtcars_tbl) #you need to attach the data

distanceMatrixWithRowNames <-mtcars_tbl %>%
  set_rownames(car) %>%
  dist(mtcars_tbl, method = 'euclidean')
distanceMatrixWithRowNames #cannot see it
View(distanceMatrixWithRowNames) #cannot see it

#Fitting Hierarchical clustering Model to dataset
set.seed(240)  # Setting seed
Hierar_cl <- hclust(distanceMatrixWithRowNames, method = "average")
Hierar_cl

#Plotting dendrogram
plot(Hierar_cl) #because we have no row names!

#kmeans analysis:
library(factoextra)
set.seed(240)
km <- kmeans(distanceMatrixWithRowNames, centers = 4, nstart = 25)
fviz_cluster(km, data = distanceMatrixWithRowNames)

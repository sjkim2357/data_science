#################### Clustering #########################

###########################
#####K-means Clustering####
###########################

#Importing the mall dataset
setwd("C:/Users/sungj/Documents/Datasets")
dataset <-read.csv("Mall_Customers.csv")
View(dataset)
X<-dataset[,4:5] #annual income and spending score
View(X)

#How many clusters are ideal?
#Using the Elbow method to find the optimal number of clusters.
#The elbow method is a heuristic used in determining the number of clusters in a data set. 
browseURL("https://www.oreilly.com/library/view/statistics-for-machine/9781788295758/c71ea970-0f3c-4973-8d3a-b09a7a6553c1.xhtml")

set.seed(1234)
wcss <-vector()
for(i in 1:10) wcss[i] <- sum(kmeans(X, i)$withinss) #X <-dataset[,4:5]
plot(1:10, wcss, type = "b", main = paste('Cluster of clinents'), 
     xlab = "Number of clusters", ylab = "WCSS")


#Applying K-means to the mall dataset
set.seed(1234)
kmeans <-kmeans(X, 5, nstart = 25)  #X <-dataset[,4:5], K = 5
kmeans

library(cluster)
?clusplot
clusplot(X, 
         kmeans$cluster,
         line = 0, 
         shade = TRUE,
         color = T,
         label = 2,
         plotchar = F,
         span = T,
         main = paste('Clusters of clients'),
         xlab = 'Annual income',
         ylab = 'Spending score'
         )


#K-means Clustering using tidyverse package
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

df <- read.csv("Mall_Customers.csv")
df
View(df)

df<-df[,4:5]
View(df)
df <- na.omit(df)
df <- scale(df) #feature scaling
head(df)

# 2 clusters
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
fviz_cluster(k2, data = df)

#3, 4, 5 clusters
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# let's compare the plots###########
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# Compute k-means clustering with k = 5
set.seed(123)
final <- kmeans(df, 5, nstart = 25)
print(final)

fviz_cluster(final, data = df)


###########################
##Hierarchical clustering##
###########################

# importing the mall dataset
setwd("C:/Users/sungj/Documents/Datasets")
dataset = read.csv('Mall_Customers.csv')
X = dataset[,4:5]
View(X)

# using the dendrogram to find the optimal number of clusters
dendrogram = hclust(dist(X, method = 'euclidean'), 
                    method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances',
     cex=0.85) 
# because we have too many observation in a plot, it isn't friendly to read the plot. 
# to change the label setting of each axis, see:
browseURL("http://rfunction.com/archives/2154")

# add color boxes to plot
rect.hclust(dendrogram, k = 2, border = "gray")
rect.hclust(dendrogram, k = 3, border = "blue")
rect.hclust(dendrogram, k = 4, border = "green4")
rect.hclust(dendrogram, k = 5, border = "darkred")

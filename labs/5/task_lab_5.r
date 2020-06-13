setwd("~/Desktop/Projects/data_mining/labs/5")


library(XLConnect) 
library(readxl) 
library(caret)
library(tidyverse)
library(dbscan)
library(fpc)
library(cluster)
library(factoextra)
library(dplyr)


#install.packages("car")
#install.packages("tidyverse")
#install.packages("factoextra")
#install.packages("dply")

#R.Version()



wine <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header=TRUE, sep=';')
summary(wine)
head(wine)
names(wine)
str(wine)
table(wine$quality)


# How many observations in the column are NA 
na_columns_rate = sapply(wine, FUN = function(x) sum(is.na(x))/length(x))
print(na_columns_rate)
#no NA columns
# Remove almost fully NA columns
#new_dataset = wine[,na_columns_rate < 0.9]
#new_dataset

# Make all the columns numeric
original_data_types = sapply(wine, typeof)
print(original_data_types) # already ok



# Last column shoould be dropped acording to task description (quality)
wine %>% select(1:11) -> data

names(data)

# we define a seed for purposes of reprodutability
set.seed(123)
?kmeans
wine.kmeans<-kmeans(data, centers = 6, nstart = 20) 
#getting information about clustering
print(wine.kmeans)
print(wine.kmeans$iter)
print(wine.kmeans$centers)

#compare clusters with original class labels
table(wine$quality,wine.kmeans$cluster)




#Plotting the result of K-means clustering can be difficult because of the high dimensional nature of the data. 
#To overcome this, the plot.kmeans function in useful performs 
#multidimensional scaling to project the data into two dimensions and then color codes the points according to cluster membership
#install.packages("useful",repos = "http://cran.us.r-project.org")
#library(useful)
plot(data, data=wine)


distance <- get_dist(data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#If there are more than two dimensions (variables) fviz_cluster will perform principal
#component analysis (PCA) and plot the data points according to the first two principal 
#components that explain the majority of the variance.
fviz_cluster(wine.kmeans, data = data)


# Average Silhouette Method
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}


#In short, the average silhouette approach measures the quality of a clustering. 
#That is, it determines how well each object lies within its cluster. 
#A high average silhouette width indicates a good clustering. 
#The average silhouette method computes the average silhouette of observations for different values of k. 
#The optimal number of clusters k is the one that maximizes the average silhouette over a range of possible values for k.

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}




#alternative execution of kmeans
wine.kmeans_alt<-eclust(data, "kmeans", k=6, graph=FALSE)
fviz_silhouette(wine.kmeans_alt, palette="jco")
fviz_silhouette(wine.kmeans, palette="jco")


silinfo<-wine.kmeans_alt$silinfo
names(silinfo)


#silhouette length for each observation
head(silinfo$widths[,1:3],10)
#silhouette length for each cluster
silinfo$clus.avg.widths
#average silhouette length
silinfo$avg.width

# Average silhoutte lenght is 0.3138577 for data without preprocessing etc.

















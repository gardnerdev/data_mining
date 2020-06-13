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


install.packages("car")
install.packages("tidyverse")
install.packages("factoextra")
install.packages("dply")

R.Version()



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
wine.cluster<-kmeans(data, centers = 6, nstart = 20) 
#getting information about clustering
print(wine.cluster)
print(wine.cluster$kmeans)
print(wine.cluster$iter)
print(wine.cluster$centers)


#It is a good idea to plot the cluster results. These can be used to assess the choice of the number
#of clusters as well as comparing two different cluster analyses.
#Now, we want to visualize the data in a scatter plot with coloring 
#each data point according to its cluster assignment.
#The problem is that the data contains more than 2 variables and 
#the question is what variables to choose for the xy scatter plot





#' Plots a chart showing the sum of squares within a group for each execution of the kmeans algorithm. 
#' In each execution the number of the initial groups increases by one up to the maximum number of centers passed as argument.
#'
#' @param data The dataframe to perform the kmeans 
#' @param nc The maximum number of initial centers
#'
wssplot <- function(data, nc=15, seed=123){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
  1    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of groups",
       ylab="Sum of squares within a group")}
wssplot(data, nc = 20)



#Plotting the result of K-means clustering can be difficult because of the high dimensional nature of the data. 
#To overcome this, the plot.kmeans function in useful performs 
#multidimensional scaling to project the data into two dimensions and then color codes the points according to cluster membership
#install.packages("useful",repos = "http://cran.us.r-project.org")
library(useful)


plot(data, data=wine)

















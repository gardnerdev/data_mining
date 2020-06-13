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



wine_raw <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header=TRUE, sep=';')
summary(wine_raw)
head(wine_raw)
names(wine_raw)
str(wine_raw)
table(wine_raw$quality)


# How many observations in the column are NA 
na_columns_rate = sapply(wine_raw, FUN = function(x) sum(is.na(x))/length(x))
print(na_columns_rate)
#no NA columns
# Remove almost fully NA columns
#new_dataset = wine[,na_columns_rate < 0.9]
#new_dataset

# Make all the columns numeric
original_data_types = sapply(wine_raw, typeof)
print(original_data_types) # already ok



# Last column shoould be dropped acording to task description (quality)
wine_raw %>% select(1:11) -> wine

names(wine)

# we define a seed for purposes of reprodutability
set.seed(123)
?kmeans
wine.kmeans<-kmeans(wine, centers = 6, nstart = 20) 
#getting information about clustering
print(wine.kmeans)
print(wine.kmeans$iter)
print(wine.kmeans$centers)

#compare clusters with original class labels
table(wine_raw$quality,wine.kmeans$cluster)




#Plotting the result of K-means clustering can be difficult because of the high dimensional nature of the data. 
#To overcome this, the plot.kmeans function in useful performs 
#multidimensional scaling to project the data into two dimensions and then color codes the points according to cluster membership
#install.packages("useful",repos = "http://cran.us.r-project.org")
#library(useful)
#plot(data, data=wine)


#distance <- get_dist(wine)
# very costly computation
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

#If there are more than two dimensions (variables) fviz_cluster will perform principal
#component analysis (PCA) and plot the data points according to the first two principal 
#components that explain the majority of the variance.
fviz_cluster(wine.kmeans, data = wine_raw)


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
wine.kmeans_alt<-eclust(wine, "kmeans", k=6, graph=FALSE)
fviz_silhouette(wine.kmeans_alt, palette="jco")


silinfo<-wine.kmeans_alt$silinfo
names(silinfo)


#silhouette length for each observation
head(silinfo$widths[,1:3],10)
#silhouette length for each cluster
silinfo$clus.avg.widths
#average silhouette length
silinfo$avg.width

# Average silhoutte lenght is 0.3138577 for data without preprocessing etc.


?scale
#### data scaling
wineScale <- scale(wine, center = FALSE)
str(wineScale)

wine.kmeansS = kmeans(wineScale, 6, iter.max = 20)
str(wine.kmeansS)

#after scaling
table(wine_raw$quality,wine.kmeansS$cluster)
#before
table(wine_raw$quality,wine.kmeans$cluster)


#alternative execution of kmeans
wine.kmeansS_alt<-eclust(wineScale, "kmeans", k=6, graph=FALSE)

silinfo<-wine.kmeansS_alt$silinfo
names(silinfo)


#silhouette length for each observation
head(silinfo$widths[,1:3],10)
#silhouette length for each cluster
silinfo$clus.avg.widths
#average silhouette length
silinfo$avg.width


#after scaling
fviz_silhouette(wine.kmeansS_alt, palette="jco")
#before scaling
fviz_silhouette(wine.kmeans_alt, palette="jco")
#worse 0.18 to  0.31




accuracyCalc <- function(confTbl, startCol)
{
  corr = 0;
  for(i in startCol:ncol(confTbl))
  {
    corr = corr + max(confTbl[,i])  
  }
  accuracy = corr/sum(confTbl)
  accuracy  
}


# Accuracy without scaling
res3 = table(wine_raw$quality,wine.kmeans_alt$cluster)
res3
accuracyCalc(res3,1)

# Accuracy with scaling
res3 = table(wine_raw$quality,wine.kmeansS_alt$cluster)
res3
accuracyCalc(res3,1)

#0.4583503 > 0.4505921

###############################################################
# finding the optimal number of groups with the "elbow" method
###############################################################


########### from laboratory ######################

wss <- vector(mode = "integer" ,length = 10)


#  1 to 10 clusters
for (i in 1:10) {
  kmeans.group <- kmeans(wine, centers = i, nstart=20)
  # total within-cluster sum of squares
  wss[i] <- kmeans.group$tot.withinss
}

# total within-cluster sum of squares per number of groups
plot(1:10, wss, type = "b", 
     xlab = "number of groups", 
     ylab = "total within-cluster sum of squares")


########## from data mining book ###################


#Fortunately, this process to compute the 
#“Elbow method” has been wrapped up in a single function (fviz_nbclust):
set.seed(123)

fviz_nbclust(wine, kmeans, method = "wss")


#Average Silhouette Method
#In short, the average silhouette approach measures the quality of a clustering. 
#That is, it determines how well each object lies within its cluster. A high average silhouette width indicates a good clustering.
#The average silhouette method computes the average silhouette of observations for different values of k

fviz_nbclust(wine, kmeans, method = "silhouette")

# BEST NUMBER OF CLUSTERS -> 4

# for scaling data -> 4 too
fviz_nbclust(wineScale, kmeans, method = "silhouette")

############# TEST FOR K = 2 ##########################
#alternative execution of kmeans
wine.kmeans2_alt<-eclust(wine, "kmeans", k=2, graph=FALSE)

fviz_silhouette(wine.kmeans2_alt, palette="jco")

res2 = table(wine_raw$quality,wine.kmeans2_alt$cluster)
res2
accuracyCalc(res2,1)

### accuracy -> 0.4487546 


############# TEST FOR K = 4 ##########################
#alternative execution of kmeans
wine.kmeans4_alt<-eclust(wine, "kmeans", k=4, graph=FALSE)

fviz_silhouette(wine.kmeans4_alt, palette="jco")

res4 = table(wine_raw$quality,wine.kmeans4_alt$cluster)
res4
accuracyCalc(res4,1)

### accuracy -> 0.4510004



############# TEST FOR K = 6 ##########################
#alternative execution of kmeans
wine.kmeans6_alt<-eclust(wine, "kmeans", k=6, graph=FALSE)

fviz_silhouette(wine.kmeans6_alt, palette="jco")

res6 = table(wine_raw$quality,wine.kmeans6_alt$cluster)
res6
accuracyCalc(res6,1)

### accuracy -> 0.4583503




wine <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header=TRUE, sep=';')

#### changing nstart parameter to 50
k6 <- kmeans(wine, centers = 6, nstart = 50)

res6_50 = table(wine_raw$quality,k6$cluster)
accuracyCalc(res6_50,1)

### accuracy -> 0.4577379

####################################
# PAM - partitioning around medoids#
####################################


# deciding on the optimal number of clusters
fviz_nbclust(wine, pam, method = "silhouette")+theme_classic()

# division into 2 clusters
pam.res <- pam(wine, 2)
print(pam.res)

#adding information on cluster assignment
wine_clus<-cbind(wine, pam.res$cluster)
head(wine_clus)



#cluster centers
print(pam.res$medoids)

#cluster assignment
pam.res$clustering

#clustering visualization
fviz_cluster(pam.res,
             palette = c("#00AFBB", "#FC4E07"), # color palette
             ellipse.type = "t", # ellipse of concentration
             repel = TRUE, # avoid overlapping (slows down)
             ggtheme = theme_light() #background color
)


wine_raw <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header=TRUE, sep=';')
wine_raw %>% select(1:11) -> wine_raw
#df <- na.omit(df_raw)
#df <- na.omit(df)
#df <- scale(df)
#head(df)

#alternative execution of kmeans
#df.kmeans6_alt<-eclust(df, "kmeans", k=6, graph=FALSE)
#fviz_silhouette(df.kmeans6_alt, palette="jco")

#res6 = table(wine_raw$quality,df.kmeans6_alt$cluster)
#res6
#accuracyCalc(res6,1)


##### dbscan algorithm ##########################

?dbscan
dbscan::kNNdistplot(wine_raw, k=6)
abline(h=13, lty="dashed")

#alg execution
wine.dbscan <- dbscan(wine_raw, eps=13, MinPts=5)

#compare clusters with original class labels
#cluster 0 means noise
table(wine_raw$quality, wine.dbscan$cluster)


wine.dbscan <- dbscan(wine, eps=0.4, MinPts=5)
table(wine_raw$quality, wine.dbscan$cluster)


# plot clusters
plot(wine.dbscan, wine_raw)
plot(wine.dbscan, wine_raw[c(1,2)])


res_dbscan = table(wine_raw$quality,wine.dbscan$cluster)
res_dbscan
accuracyCalc(res_dbscan,1)



#######################################################################3
wine_raw <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header=TRUE, sep=';')
wine_raw %>% select(1:11) -> wine

cor(wine)
print(cor(wine))


wine_scale <- scale(wine[-1])
wine_scale

head(wine_scale,3)

library(cluster)
library(factoextra)

# determination the optimal number of clusters
fviz_nbclust(wine_scale,kmeans,method= 'wss',) + geom_vline(xintercept=3,linetype=5,col='darkred')

# extracting results
k.means <- kmeans(wine_scale, 3, nstart=20)
k.means

# Centroids
k.means$size

#visualization
clusplot(wine_scale, k.means$cluster, main='2D representation of the Cluster', color=TRUE, shade=TRUE, labels=,lines=0)


fviz_cluster(object=k.means, #kmeans object
            data=wine_scale, # data used for clustering
            ellipse.type="norm",
            geom="point",
            palette="jco",
            main="",
            ggtheme= theme_minimal())



res_dbscan = table(wine_raw$quality,k.means$cluster)
res_dbscan
accuracyCalc(res_dbscan,1)

# accuracy 0.455492







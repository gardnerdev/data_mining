
# Authors: Grzegorz Protaziuk, Robert Bembenik
# Script EDAMI lab6
# Clustering

#setting working directory - adjust a path to your directory with a dataset

library(dbscan)
library(fpc)
library(cluster)
library(factoextra)

##### kmeans ##################
set.seed(7777)

data("iris")
summary(iris)
iris2 = iris[,-5]
iris2


?kmeans
iris2.kmeans = kmeans(iris2,3, iter.max = 20, nstart=20)

#getting information about clustering
print(iris2.kmeans)
print(iris2.kmeans$iter)
print(iris2.kmeans$centers)

#compare clusters with original class labels
table(iris$Species,iris2.kmeans$cluster)

#plot clusters
plot(iris2[,1:2], col = iris2.kmeans$cluster)
#add cluster centers
points(iris2.kmeans$centers[,1:2], col = 1:3, pch = 8, cex=2)


# Quality of the clustering            

km<-kmeans(iris2,3)

#alternative execution of kmeans
km_alt<-eclust(iris2, "kmeans", k=3, graph=FALSE)
fviz_silhouette(km_alt, palette="jco")

# From the output, avg.silwidth represents the average silhouette width. A silhouette is a measurement
# that considers how closely related objects are within the cluster and how clusters are separated
# from each other. The silhouette value usually ranges from 0 to 1; a value closer to 1 suggests
# the data is better clustered.

silinfo<-km_alt$silinfo
names(silinfo)

#silhouette length for each observation
head(silinfo$widths[,1:3],10)
#silhouette length for each cluster
silinfo$clus.avg.widths
#average silhouette length
silinfo$avg.width

# Rand index
# The corrected Rand index provides a measure for assessing the similarity between
# two partitions, adjusted for chance. Its range is -1 (no agreement) to 1 (perfect agreement).
species <- as.numeric(iris$Species)
clust_stats<-cluster.stats(d=dist(iris2), species, km_alt$cluster)
clust_stats$corrected.rand


#### data scaling
?scale
irisScale <- scale(iris2, center = FALSE)
str(irisScale)
iris2.kmeansS = kmeans(irisScale,3, iter.max = 20)
str(iris2.kmeansS)
table(iris$Species,iris2.kmeansS$cluster)

plot(irisScale[,3:4], col = iris2.kmeansS$cluster)
#add cluster centers
points(iris2.kmeansS$centers[,3:4], col = 1:3, pch = 8, cex=2)

###############################################################
# finding the optimal number of groups with the "elbow" method
###############################################################

wss <- vector(mode = "integer" ,length = 15)

#  1 to 15 clusters
for (i in 1:15) {
  kmeans.group <- kmeans(irisScale, centers = i, nstart=20)
  # total within-cluster sum of squares
  wss[i] <- kmeans.group$tot.withinss
}

# total within-cluster sum of squares per number of groups
plot(1:15, wss, type = "b", 
     xlab = "number of groups", 
     ylab = "total within-cluster sum of squares")


####################################
# PAM - partitioning around medoids#
####################################

# deciding on the optimal number of clusters
fviz_nbclust(iris2, pam, method = "silhouette")+theme_classic()

# division into 2 clusters
pam.res <- pam(iris2, 2)

# clustering results together with information on objects being cluster centers
print(pam.res)

#adding information on cluster assignment
iris_clus<-cbind(iris2, pam.res$cluster)
head(iris_clus)

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


#### hierarchical clustering
#sample data
idx <- sample(1:nrow(iris), 50)
irisSample <- iris[idx,1:4]

?hclust
#calculation of a distance matrix
?dist
distM = dist(irisSample)
distT = as.matrix(distM)

dim(distT)
distT[1:5,1:5]

#hierarchical clustering for different linkage methods
iris2.hc_complete <- hclust(distM, method="complete")
iris2.hc_single <- hclust(distM, method="single")
iris2.hc <- hclust(distM, method="average")
iris2.hc_centroid <- hclust(distM, method="centroid")

#dendrograms for different clustering
?plot
par(mfrow=c(2,2))
plot(iris2.hc, hang = -1, labels=iris$Species[idx])
rect.hclust(iris2.hc, k=3, border=2:4)

plot(iris2.hc_complete, hang = -1, labels=iris$Species[idx])
rect.hclust(iris2.hc_complete, k=3, border=2:4)

plot(iris2.hc_single, hang = -1, labels=iris$Species[idx])
rect.hclust(iris2.hc_single, k=3, border=2:4)

plot(iris2.hc_centroid, hang = -1, labels=iris$Species[idx])
rect.hclust(iris2.hc_centroid, k=3, border=2:4)
par(mfrow=c(1,1))

#generates clusters
?cutree
iris2.hc.groups <- cutree(iris2.hc, k=3)
iris2.hc.groups

#compare clusters with original class labels
table(iris$Species[idx], iris2.hc.groups)

##### dbscan algorithm ##########################

?dbscan

# MinPts parameter estimation
# The idea is to calculate the average of the distances of every point to its k nearest
# neighbors. The value of k will be specified by the user and corresponds to MinPts.
# Next, these k-distances are plotted in an ascending order. The aim is to determine
# the “knee”, which corresponds to the optimal eps parameter.
# A knee corresponds to a threshold where a sharp change occurs along the k-distance
# curve.

dbscan::kNNdistplot(iris2, k=5)
abline(h=0.5, lty="dashed")

# dbscan alg. execution

iris2.dbscan <- dbscan(iris2, eps=0.5, MinPts=5)

#compare clusters with original class labels
#cluster 0 means noise
table(iris$Species, iris2.dbscan$cluster)

iris2.dbscan <- dbscan(iris2, eps=0.4, MinPts=5)
table(iris$Species, iris2.dbscan$cluster)

# plot clusters
plot(iris2.dbscan, iris2)
plot(iris2.dbscan, iris2[c(1,4)])

############ Laboratory task ###################
#calculation of accuracy
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

#data set for the laboratory task
#http://archive.ics.uci.edu/ml/datasets/Cardiotocography 

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/cardioto_noClass_corr.csv','cardioto_noClass_corr.csv')
ctg_noClass <- read.csv("cardioto_noClass_corr.csv",row.names = 1)

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/cardioto_all_corr.csv','cardioto_all_corr.csv')
ctg_all <- read.csv("cardioto_all_corr.csv",row.names = 1)

#example
card.kmeans = kmeans(ctg_noClass,10)
res3 = table(ctg_all$CLASS,card.kmeans$cluster )
res3
accuracyCalc(res3,1)

#wines
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');
wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")
wineRed_dsC <- wineRed_ds[,-12]

#example - wines
card.kmeans = kmeans(wineRed_dsC,6)
res3 = table(wineRed_ds$quality,card.kmeans$cluster )
res3
accuracyCalc(res3,1)
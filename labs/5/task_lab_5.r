#The CTGs were also classified by three expert 
#obstetricians and a consensus classification 
#label assigned to each of them. Classification 
#was both with respect to a morphologic pattern 
#(A, B, C. ...) and to a fetal state (N, S, P). 
#Therefore the dataset can be used either for 10-class 
#or 3-class experiments.


#Attribute Information:

#LB - FHR baseline (beats per minute)
#AC - # of accelerations per second
#FM - # of fetal movements per second
#UC - # of uterine contractions per second
#DL - # of light decelerations per second
#DS - # of severe decelerations per second
#DP - # of prolongued decelerations per second
#ASTV - percentage of time with abnormal short term variability
#MSTV - mean value of short term variability
#ALTV - percentage of time with abnormal long term variability
#MLTV - mean value of long term variability
#Width - width of FHR histogram
#Min - minimum of FHR histogram
#Max - Maximum of FHR histogram
#Nmax - # of histogram peaks
#Nzeros - # of histogram zeros
#Mode - histogram mode
#Mean - histogram mean
#Median - histogram median
#Variance - histogram variance
#Tendency - histogram tendency
#CLASS - FHR pattern class code (1 to 10)
#NSP - fetal state class code (N=normal; S=suspect; P=pathologic)

setwd("~/Desktop/Projects/data_mining/labs/5")

#install.packages("XLConnect",dependencies=TRUE)
#install.packages("readxl",dependencies=TRUE)
#install.packages("readxl",repos = "http://cran.us.r-project.org")
#install.packages("caret")
#install.packages("tidyverse",repos = "http://cran.us.r-project.org")

library(XLConnect) 
library(readxl) 
library(caret)
library(tidyverse)
library(dbscan)
library(fpc)
library(cluster)
library(factoextra)




wk = loadWorkbook("CTG.xls") 
data = readWorksheet(wk, sheet="Raw Data")
#Dataset <- read.xlsx("CTG.xls",sheetName="Raw Data")

names(data)

# First 3 columns are unnecessary: filenames and date
data = data[,-1:-3]
names(data)

# Last 2 columns shoould be dropped acording to task (NSP and Class)
data = data[1:(length(data)-2)]
names(data)


# According to dataset documentation, columns
# for clustering - last 10:

#install.packages("tidyverse",dependencies=TRUE)

data %>% select(26:35) -> new_dataset

print(new_dataset)

names(new_dataset)


# k-means algorithm for 10 groups with
# default values of parameters and without
# preprocessing of data


#kmeans cannot handle data that 
#has NA values

# How many observations in the column are NA 
na_columns_rate = sapply(new_dataset, FUN = function(x) sum(is.na(x))/length(x))

# Remove almost fully NA columns
new_dataset = new_dataset[,na_columns_rate < 0.9]
new_dataset

# Remove NA rows. There are 3 of them
new_dataset = new_dataset[complete.cases(new_dataset),]


# Make all the columns numeric
original_data_types = sapply(new_dataset, typeof)


print(original_data_types) # already ok
#data <- data.frame(lapply(data, FUN = function(x) as.numeric(sub(",", ".", x, fixed = TRUE))))

raw_data <- data.frame(new_dataset)



?kmeans
data.kmeans=kmeans(raw_data,10)


#getting information about clustering
print(data.kmeans)

print(data.kmeans$iter)

print(data.kmeans$centers)


#compare clusters with original class labels
table(data$SUSP,data.kmeans$cluster)

#plot clusters
plot(data[,1:2], col = data.kmeans$cluster)

#plot all combinations
plot(data[,1:15], col = data.kmeans$cluster)


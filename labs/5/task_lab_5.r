library(dbscan)
install.packages("fpc")
library(fpc)
library(cluster)
install.packages("factoextra")
library(factoextra)


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

setwd("/home/users/chojnar1/Desktop/data_mining/labs/5")

install.packages("XLConnect",dependencies=TRUE)
install.packages("readxl",dependencies=TRUE)
install.packages("readxl",repos = "http://cran.us.r-project.org")
library(XLConnect) 
library(readxl) 
library(caret)

wk = loadWorkbook("CTG.xls") 
data = readWorksheet(wk, sheet="Raw Data")
#Dataset <- read.xlsx("CTG.xls",sheetName="Raw Data")

# First 3 columns are unnecessary: filenames and date
data = data[,-1:-3]
print(data)

# Last 2 columns shoould be dropped acording to task (NSP and Class)
data = data[1:(length(data)-2)]
print(data)


# k-means algorithm for 10 groups with
# default values of parameters and without
# preprocessing of data


#kmeans cannot handle data that 
#has NA values

# How many observations in the column are NA 
na_columns_rate = sapply(data, FUN = function(x) sum(is.na(x))/length(x))
na_columns_rate

# Remove almost fully NA columns
data = data[,na_columns_rate < 0.9]
data

# Remove NA rows. There are 3 of them
data = data[complete.cases(data),]


# Make all the columns numeric
original_data_types = sapply(data, typeof)
print(original_data_types) # already ok
#data <- data.frame(lapply(data, FUN = function(x) as.numeric(sub(",", ".", x, fixed = TRUE))))

raw_data <- data.frame(data)

names(data)


?kmeans
data.kmeans=kmeans(data,10)
#getting information about clustering
print(data.kmeans)
print(data.kmeans$iter)
print(data.kmeans$centers)


#compare clusters with original class labels
table(data$Species,iris2.kmeans$cluster)

#plot clusters
plot(data[,1:2], col = data.kmeans$cluster)

#plot all combinations
plot(data[,1:15], col = data.kmeans$cluster)


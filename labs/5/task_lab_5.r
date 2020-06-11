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

options(max.print=50)
print(data)


# First 3 columns are unnecessary: filenames and date
data = data[,-1:-3]
print(data)

# Last 2 columns shoould be dropped acording to task (NSP and Class)

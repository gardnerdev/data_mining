setwd("~/Desktop/Projects/data_mining/labs/6")

#library(XLConnect) 
library(readxl) 
library(caret)
library(tidyverse)
library(dbscan)
library(fpc)
library(cluster)
library(factoextra)
library(dplyr)

#install.packages("XLConnect")
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


#wine_raw$Q2 = lapply(wine_raw[,12], function (x)
#{
#  if(x >6)  { "A"}
#  else if(x >4)  {"B"}
#  else { "C"}   
#})

#wine_raw$Q2 = unlist(wine_raw$Q2)
#wine_raw$Q2 = as.factor(wine_raw$Q2)


library(C50)
library(gmodels)
library(party)
library(RColorBrewer)
library(rpart)
library(rpart.plot)


################################################################
# 2.  rpart - recursive partitioning trees                       #
################################################################



str(wine_raw)
summary(wine_raw)
# historgram of the quality of wine
hist(wine_raw$quality)
set.seed(7777)



#creating training and test datasets 
sam <- sample(2, nrow(wine_raw), replace=TRUE, prob=c(0.7, 0.3))
sam
wine_train <- wine_raw[sam==1,]
wine_test <- wine_raw[sam==2,]



# creating wine train and test models
# wine_train <- wine_raw[1:3750, ]
# wine_test <- wine_raw[3751:4898, ]


wine_train
wine_test



######################################################################################################################

#class distribution in sets
prop.table(table(wine_train$quality))
prop.table(table(wine_test$quality))

print("Wine train:")
table(wine_train$quality)
print("Wine test:")
table(wine_test$quality)




################################################################
# 1.  C5.0 classifier                                         #
################################################################

?C5.0
#model building - a decision tree

#Error: C5.0 models require a factor outcome
wine_train$quality<-as.factor(wine_train$quality)
wine_test$quality<-as.factor(wine_test$quality)

wine_C50 <- C5.0(wine_train[1:11], wine_train$quality) 
summary(wine_C50)

#too long to render
#plot(wine_C50)



#quality of classification for training data
wine_c50_trainPred <- predict(wine_C50, wine_train, type="class")


?CrossTable
CrossTable(wine_c50_trainPred, wine_train$quality, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))


?confusionMatrix
confusionMatrix(wine_c50_trainPred, wine_train$quality, mode="everything")



#quality of classification for test data
wine_c50_testPred <- predict(wine_C50, wine_test)
CrossTable(wine_c50_testPred, wine_test$quality, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))


str(wine_test$quality)
str(wine_c50_testPred)

?confusionMatrix

confusionMatrix(wine_c50_testPred, wine_test$quality, mode="everything")

print(wine_c50_testPred)
print(wine_test$quality)




#model building - rules
wine_C50R <- C5.0(wine_train[1:11], wine_train$quality,  rules = TRUE) 
summary(wine_C50R)



#quality of classification for test data
wine_c50_testPred <- predict(wine_C50R, wine_test)
CrossTable(wine_c50_testPred, wine_test$quality, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))


#wine_train$quality<-as.factor(wine_train$quality)
#wine_test$quality<-as.factor(wine_test$quality)


confusionMatrix(wine_c50_testPred, wine_test$quality, mode="everything")



#quality of classification for test data
wine_c50_testPred <- predict(wine_C50R, wine_test)
CrossTable(wine_c50_testPred, wine_test$quality, prop.chisq = FALSE,prop.c = FALSE, 
           prop.r = FALSE, dnn = c('predicted class', 'actual class'))

confusionMatrix(wine_c50_testPred, wine_test$quality, mode="everything")




################################################################
#RandomForest                                                  #
################################################################
?randomForest

wine_Forest = randomForest(quality~., data = wine_train, importance = TRUE, nodesize = 10, mtry = 4, ntree = 100 )
#nodesize = minimal number of objects in a node
#mtry - the number of randomly selected attributes for searching the best test split in nodes
#ntree -  number of trees in a forest
#importance - calculation of attriubte importance

print(wine_Forest)
plot(wine_Forest)


?importance
round(importance(wine_Forest, type = 1),2)

wine_Forest_testPred = predict (wine_Forest, newdata = wine_test[1:11])
confusionMatrix(wine_Forest_testPred, wine_test$quality, mode = "everything")

#looking for the best values of parameters by means of K-fold validation
?trainControl
trControl <- trainControl(method = "cv", number = 10, search = "grid")




#arguments
#- method = "cv": The method used to resample the dataset. 
#- number = n: Number of folders to create
#- search = "grid": Use the search grid method. For randomized method, use "grid"

?train
tuneGrid <- expand.grid(mtry = c(1:11))
tuneGrid
wine_Frestores_mtry <- train(quality~.,  data = wine_train,
                            method = "rf",
                            metric = "Accuracy",
                            tuneGrid = tuneGrid,
                            trControl = trControl,
                            importance = TRUE,    # randomForest function parameter
                            nodesize = 10,        # randomForest function parameter
                            ntree = 250)          ## randomForest function parameter
print(wine_Frestores_mtry)



treesModels <- list()
for (nbTree in c(5,10,25, 50, 100, 250, 500)) 
{
  wine_F_maxtrees <- train(quality~.,  data = wine_tain,
                          method = "rf",
                          metric = "Accuracy",
                          tuneGrid = tuneGrid,
                          trControl = trControl,
                          importance = TRUE,
                          nodesize = 10,
                          ntree = nbTree)
  key <- toString(nbTree)
  treesModels[[key]] <- car_F_maxtrees
}

?resamples
results_tree <- resamples(treesModels)
summary(results_tree)

#końcowy model
wine_Forest2 = randomForest(quality~., data = wine_train, importance = TRUE, mtry = 6, ntree = 250, nodesize = 10)

print(wine_Forest2)
plot(wine_Forest2)


wine_Forest2_testPred = predict (wine_Forest, newdata = wine_test[1:11])
confusionMatrix(wine_Forest2_testPred, wine_test$quality, mode = "everything")

varImpPlot(wine_Forest2)


################################################
#Comparision of classifiers
###############################################
wine_rpart <- rpart(quality~., data=wine_train)
wine_rpart_testPred = predict(wine_rpart, wine_test, type = "class")

klasyfikator = c('C50', 'rpart',  'rForest')
dokladnosc = c( mean(wine_c50_testPred == wine_test$quality), 
                mean(wine_rpart_testPred == wine_test$quality),
                mean(wine_Forest2_testPred == wine_test$quality))

res <- data.frame(klasyfikator, dokladnosc)
View(res)



#What attribute is the class attribute and how many classes are involved in the task.
#In task there are 11 attributes involved
#What is the potential practical application of the results generated by the already
#Application which measure wine quality based on different attributes with recomendation 
#of buing


################################################Conclusion#############################################################################
#Used classifiers
# C50	0.5619439
#	rpart	0.5167693
#	rForest	0.6570842
# Measuring wine quality was pretty hard because of number of classes involved in dataset.
# Turns out our random forest model is best.


#wine_raw <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header=TRUE, sep=';')

# I will change approach and classify wines not into quality level by providing different, more general granularity.
barplot(table(wine_raw$quality))

#As we can see, there are a lot of wines with a quality of 6 as compared to the others. 
#The dataset description states – there are a lot more normal wines than excellent or poor ones.
#For the purpose of this discussion, let’s classify the wines into good, bad, and normal based on their quality

#creating a new attribute with 3 values(classses) based on 
# the orignal class atribute - quality
wine_raw$taste <- ifelse(wine_raw$quality < 6, 'bad', 'good')
wine_raw$taste[wine_raw$quality == 6] <- 'normal'
wine_raw$taste <- as.factor(wine_raw$taste)


#distribution
table(wine_raw$taste)



#creating training and test datasets 
sam <- sample(2, nrow(wine_raw), replace=TRUE, prob=c(0.7, 0.3))
sam
wine_train_new <- wine_raw[sam==1,]
wine_test_new <- wine_raw[sam==2,]
#wine_raw$taste <- as.factor(wine_raw$taste)

# Random forest once again
library(randomForest)
model <- randomForest(taste ~ . - quality, data = wine_train_new)

#We can use ntree and mtry to specify the total number of trees 
#to build (default = 500), and the number of predictors to randomly sample at each split respectively
model

#We can see that 500 trees were built, and the model randomly sampled 3 predictors at each split. It also shows a matrix 
#containing prediction vs actual, as well as classification error for each class. Let’s test the model on the test data set
pred <- predict(model, newdata = wine_test_new)
table(pred, wine_test_new$taste)


accuracy=(365 + 183 + 499) / nrow(wine_test_new)
accuracy



##################### FINAL OUTPUTS ######################################


klasyfikator = c('C50', 'rpart',  'rForest', 'rForestNEW')
dokladnosc = c(mean(wine_c50_testPred == wine_test$quality), 
                mean(wine_rpart_testPred == wine_test$quality),
                mean(wine_Forest2_testPred == wine_test$quality),
                accuracy)

res <- data.frame(klasyfikator, dokladnosc)
View(res)





# tree building
#setting the class attribute
#The rpart() will be used to specify quality as the outcome variable and use
#the dot notation to allow all the other columns in the wine_train data frame to be used in predictors
#m.rpart <- rpart(quality ~ ., data = wine_train)
#m.rpart


#rpart.plot(m.rpart, digits = 3)


#decision tree visualization
#rpart.plot(m.rpart, digits = 3)



#fallen.leaves() addition to the decision tree
#This addition will show visualizations with the dissemination of
#regression tree results, as they are readily understood even without 
#a mathematics background. The lead nodes are predicted values for the examples reaching that node.
#rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)



#test data prediction
#now it's time to test data, using predict() function. It will return the estimated numeric value 
#for the outcome variable
#p.rpart <- predict(m.rpart, wine_test)
#summary(p.rpart)

#summary(wine_train$quality)
#summary(wine_test$quality)

#CORELATION
#We can now check the correlation between the predicted and actual
#quality values provides a simple way to gauge the model’s performance
#This correlation only measures how strong the predictions are related 
#to the true value. This is not a measure of how far off the predictions were from the true values


#cor(p.rpart, wine_test$quality)
#0.4819914


#MEAN ABSOLUTE ERROR
#Let’s turn the tables and thinking of another way we could improve 
#the model’s performance. We could consider how far, on average, its prediction was from the true value
#MAE <- function(actual, predicted) { mean(abs(actual - predicted))}
#MAE(p.rpart, wine_test$quality)
#0.5956675



#mean(wine_train$quality)
#output 5.875145
#MAE(5.88, wine_test$quality)
#output 0.6546314

#The above shows room improvement. As one can see, MAE shows 0.57, 
#which comes closer to the average to the true quality score than the imputed mean, MAE 0.5741115






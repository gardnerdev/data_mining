#EDAMI 2020l Lab1
#autor: Grzegorz Protaziuk, Robert Bembenik
library(arules)
setwd("/home/users/chojnar1/Desktop/STUD/EDAMI/Labs");
getwd()


#####################################
#data preprocessing
#####################################
#data imputation
#example vector with pseudorandom numbers  - "runif" function
?runif
wRand <- runif(100,1,10)
wRand
#pseudorandom selection of  10 natural numbers - "sample" function
#setting generator seed
set.seed(1234)
?sample
wek2 = sample(100,10)
wek2
#assigning the spacial value NA to selected positions in a vector
wRand[wek2] <- NA 
wRand
#a list of numbers indicating vector fields with the special value NA -"which" and "is.na" functions
#?which
#?is.na
which(is.na(wRand) == TRUE)  

#which(wRand == NA) # it doesn't give the expected result

#data copy
wRand2 <- wRand
mean(wRand)
?mean

#mean caluculation based on valid values 
average = mean(na.omit(wRand)) 
average

#replacement of the special value NA by the mean
wRand2[is.na(wRand2)==TRUE] = mean(na.omit(wRand)) 

is.na(wRand2)
is.na(wRand)
#checking
which(is.na(wRand2))
wRand2
wRand2 == wRand

?complete.cases


################################################3
#preliminary  data analysis
?AdultUCI
data("AdultUCI")

#?summary
summary(AdultUCI)

str(AdultUCI)
#numeric attribute
summary(AdultUCI$workclass)
#standard deviation
sd(AdultUCI$age)
#quantiles
quantile(AdultUCI$age)
#deciles
quantile(AdultUCI$age, prob = seq(0, 1, length = 11), type = 5)
#histogram
hist(AdultUCI$age)


#nominal attribute
#number of occurences of values 
table(AdultUCI$education) 
#visualization of values distribution
pie(table(AdultUCI$"education"))
barplot(table(AdultUCI$"education"), cex.names=0.7)
plot(table(AdultUCI$"education"),ylab = "number of occurences")

#saving picture into file with the high resolution
#?png
#png("file_name.png", res=80, height=800, width=2400) 
#pie(table(AdultUCI$"education"))
#dev.off(); 


#####################################
#discretization  - "cut" function 
?cut

age = sample(130, 300, replace = TRUE)
age

#division into ranges based on the given borders
age_p = cut(age, c(0,15,25,67,200))
age_p
#ladding labels
age_p = cut(age, c(0,15,25,67,200), labels = c("child", "pupil", "working", "pensioner"))
age_p
age_p[1] > age_p[3]
#adding the order relation
age_p = cut(age, c(0,15,25,67,200), labels = c("child", "pupil", "working", "pensioner"), ordered_result = TRUE)
age_p[1] > age_p[3]

#division into ranges based on the mean and the standard deviation values
?sd
age_p1 = cut(age, c(-Inf,mean(age)-sd(age), mean(age), mean(age)+sd(age), +Inf), labels=c('p1','p2','p3','p4'))
age_p1

#division into ranges based on the given borders
AdultUCI$age.d  <- ordered(cut(AdultUCI[["age"]], c(15,25,45,65,100)), labels = c("Young", "Middle-aged", "Senior", "Old"))

summary(AdultUCI["age.d"])

#division into ranges based on the median 
AdultUCI$"capital_gain.d" <- ordered(cut(AdultUCI[[ "capital-gain"]], c(-Inf, 0, median(AdultUCI[[ "capital-gain"]][AdultUCI["capital-gain"] > 0 ]),Inf)), labels = c("None", "Low", "High"))
#cross table
xtabs(formula = ~ age.d + capital_gain.d , data = AdultUCI)
#####################################
#removing attributes with only one values
delOneValued <- function(inputData11)
{
  res <- c(which(sapply(inputData11, function(x) {length(unique(x))}) == 1));
  if(length(res) > 0)         
  {
    data11 <- inputData11[,-res];
  }   
  else
  {
    data11 <-inputData11;
  }
  data11 
}

#testing

which(sapply(AdultUCI, function(x) {length(unique(x))}) == 1);
dim(AdultUCI)
AdultUCI <- delOneValued(AdultUCI)

#adding two new attributes
AdultUCI$Att1 = 0
AdultUCI$Att2.Cat = 'cat1'
colnames(AdultUCI)

which(sapply(AdultUCI, function(x) {length(unique(x))}) == 1);
AdultUCI <- delOneValued(AdultUCI)
colnames(AdultUCI)

###########################################################
#removing attributes with unique values
delUniqueValueAtt<- function(inputData11)
{
  res <- c(which(sapply(inputData11, function(x) {length(unique(x))}) == nrow(inputData11)));
  if(length(res) >0)         
  {
    data11 <- inputData11[,-res];
  }   
  else
  {
    data11 <-inputData11;
  }
  
  data11 
}

#testing
which(sapply(AdultUCI, function(x) {length(unique(x))}) == nrow(AdultUCI))

AdultUCI <- delUniqueValueAtt(AdultUCI)
dim(AdultUCI)
#adding two new attributes
AdultUCI$Att1 = sample(nrow(AdultUCI),nrow(AdultUCI));
AdultUCI$Att2.Cat = sample.int(nrow(AdultUCI))

which(sapply(AdultUCI, function(x) {length(unique(x))}) == nrow(AdultUCI))

AdultUCI <- delUniqueValueAtt(AdultUCI)
colnames(AdultUCI)

###########################################################
#removing duplicates
#?duplicated
#?anyDuplicated

which(duplicated(AdultUCI) == TRUE)
length(which(duplicated(AdultUCI) == TRUE))

AdultUCI.U <- unique(AdultUCI)
dim(AdultUCI)
dim(AdultUCI.U)


###########################################################

#replacement of nominal attribute by numeric attributes

#setting working directory

#download data
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data', 'car.data')

#read data
cars = read.csv("car.data", header = FALSE,
                col.names = c('buying', 'maint', 'doors', 'persons', 'lug_boot','safety', "category") )


#variable internal structure
str(cars)
# data view
View(cars)

#creation of additional attribute for each value of a given nominal attribute in a given dataframe
genNumericAttrsForFactor = function(dataF, attName)
{
  #validation of input data  
  if(is.data.frame(dataF) == FALSE)
  {
    print("A given object is not a dataFrame");
    return(dataF);
  }
  
  if(is.character(attName) == FALSE)
  {
    print("The name of an attribute is not a text value");
    return(dataF);    
  }
  
  if(is.factor(dataF[[attName]]) == FALSE)
  {
    print("The indicated attribute is not a nominal one.");
    return(dataF);
  }
  
  #names for new attributes
  attsName = levels(dataF[[attName]]);
  
  #creation of new attributes
  for(name1 in attsName)
  {    
    dataF[paste0(attName,'_',name1)] = 0;
  } 
  #data updating 
  for( id in 1:nrow(dataF))
  {
    dataF[id,paste0(attName,'_',as.character(dataF[id,attName]))] = 1
  } 
  
  dataF
}

#function checking
cars2 = genNumericAttrsForFactor(cars,'maint')
View(cars)
View(cars2)


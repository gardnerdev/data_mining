#Decision trees
#Examples illustrate the major algorithmic operations
#related to decision trees using the weather data, the small size of which makes 
#it easy to manually verify the results. to illustrate continuous attribute handling,
#the weatherc data will be used. The following code prepares the environment for 
#subsequent examples by loading the datasets as well as DMR and CRAN packages that 
#will be needed

setwd("/home/users/chojnar1/Desktop/STUD/MOW/Book/Packages") #set the working directory to the path of the packages
pkgs <- list.files()
print(pkgs)

install.packages(c(print(as.character(pkgs), collapse="\",\"")), repos = NULL)

library(dmr.claseval)
library(dmr.stats)
library(dmr.util)
library(rpart)
library(rpart.plot)
library(lattice)
data(weather, package="dmr.data")
data(weatherc, package="dmr.data")

install.packages("/home/users/chojnar1/Desktop/STUD/MOW/Book/Packages/dmr.rpart",
repos = NULL, type = "source")


dtdat <- expand.grid(a1=seq(1, 10, 3), a2=seq(1, 10, 3))
dtdat$c <- as.factor(ifelse(dtdat$a1<=7 & dtdat$a2<=1, 1,
                            ifelse(dtdat$a1<=7 & dtdat$a2<=7, 2,
                                   ifelse(dtdat$a1<=7, 3,
                                          ifelse(dtdat$a2<=4, 4, 5)))))
  # decision tree structure
prp(rpart(c`., dtdat, minsplit=2, cp=0))
  # the corresponding domain decomposition
levelplot(c`a1*a2, dtdat, at=0.5+0:5, col.regions=gray(seq(0.1, 0.9, 0.1)),
          colorkey=list(at=0.5+0:5))


##############################################################################
data <- weather
attributes <- names(weather)[1:4]
print(attributes)
class <- names(weather)[5]
print(class)


init <- function()
{
  clabs <<- factor(levels(data[[class]]),
                   levels=levels(data[[class]]))      # class labels
  tree <<- data.frame(node=1, attribute=NA, value=NA, class=NA, count=NA,
                      `names<-`(rep(list(NA), length(clabs)),
                                paste("p", clabs, sep=".")))
  cprobs <<- (ncol(tree)-length(clabs)+1):ncol(tree)  # class probability columns
  nodemap <<- rep(1, nrow(data))
  n <<- 1
}
init()


#pdisc function for estimating discrete probability distributions
#to implement class probability calculation for a decision tree node
class.distribution <- function(n)
{
  tree$count[tree$node==n] <<- sum(nodemap==n)
  tree[tree$node==n,cprobs] <<- pdisc(data[nodemap==n,class])
}
print(class.distribution(n))


class.label <- function(n)
{
  tree$class[tree$node==n] <<- which.max(tree[tree$node==n,cprobs])
}
class.label(n)
print(class.label(n))


maxprob <- 0.999
minsplit <- 2
maxdepth <- 8
stop.criteria <- function(n)
{
  n>=2^maxdepth || tree$count[tree$node==n]<minsplit ||
    max(tree[tree$node==n,cprobs])>maxprob
}
print(stop.criteria(n))











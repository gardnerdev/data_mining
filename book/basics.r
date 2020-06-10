weather <- read.table(text="
      outlook temperature humidity   wind play
   1    sunny         hot     high normal   no
   2    sunny         hot     high   high   no
   3 overcast         hot     high normal  yes
   4    rainy        mild     high normal  yes
   5    rainy        cold   normal normal  yes
   6    rainy        cold   normal   high   no
   7 overcast        cold   normal   high  yes
   8    sunny        mild     high normal   no
   9    sunny        cold   normal normal  yes
  10   rainy        mild   normal normal  yes
  11    sunny        mild   normal   high  yes
  12 overcast        mild     high   high  yes
  13 overcast         hot   normal normal  yes
  14    rainy        mild     high   high   no")
  
weatherc <- read.table(text="
      outlook temperature humidity   wind play
   1     sunny          27       80 normal   no
   2     sunny          28       65   high   no
   3  overcast          29       90 normal  yes
   4     rainy          21       75 normal  yes
   5     rainy          17       40 normal  yes
   6     rainy          15       25   high   no
   7  overcast          19       50   high  yes
   8     sunny          22       95 normal   no
   9     sunny          18       45 normal  yes
  10     rainy          23       30 normal  yes
  11     sunny          24       55   high  yes
  12  overcast          25       70   high  yes
  13  overcast          30       35 normal  yes
  14     rainy          26       85   high   no")
summary(weatherc)


weatherr <- read.table(text="
       outlook temperature humidity   wind playability
   1     sunny          27       80 normal        0.48
   2     sunny          28       65   high        0.46
   3  overcast          29       90 normal        0.68
   4     rainy          21       75 normal        0.52
   5     rainy          17       40 normal        0.54
   6     rainy          15       25   high        0.47
   7  overcast          19       50   high        0.74
   8     sunny          22       95 normal        0.49
   9     sunny          18       45 normal        0.64
  10     rainy          23       30 normal        0.55
  11     sunny          24       55   high        0.57
  12  overcast          25       70   high        0.68
  13  overcast          30       35 normal        0.79
  14     rainy          26       85   high        0.33")
summary(weatherr)
weathercl <- weatherc[,-5]

install.packages("/home/users/chojnar1/Desktop/STUD/MOW/Book/Packages/dmr.data_1.0.tar.gz")


data(weather, package="dmr.data")
data(weatherc, package="dmr.data")
data(weatherr, package="dmr.data")

# mean calculation
bs.mean <- function(v) {sum(v)/length(v)}

# my variant
bs.mean(weatherc$temperature)
# typical approach
mean(weatherc$temperature)


bs.weighted.mean <- function(v, w=rep(1, length(v))) { sum(w*v)/sum(w) }
# demonstration
bs.weighted.mean(weatherc$temperature, ifelse(weatherc$play=="yes",5,1))
weighted.mean(weatherc$temperature, ifelse(weatherc$play=="yes",5,1))


# Median - notice that k1 and k2 are equal if m (the dataset size) is odd.
bs.median <- function(v)
{
  k1 <- (m <- length(v))%/%2+1
  k2 <- (m+1)%/%2
  ((v <- sort(v))[k1]+v[k2])/2
}

# demonstration
bs.median(weatherc$temperature)
bs.median(weatherc$temperature[weatherc$play=="yes"])
median(weatherc$temperature)
median(weatherc$temperature[weatherc$play=="yes"])


#The rank of instance x with respect 
#to attribute a on dataset S,
#is the ordinal number of x
#after sorting S nondecreasingly by a

#The R code presented below implements and demonstrates weighted median calculation. 
#Since there is no equivalent standard R function, the results are verified by applying 
#the median function to appropriately resampled data, simulating the effect of weighting. 
#The shift.right utility function is used to shift the cumulative weight sum to the right.

weighted.median <- function(v, w=rep(1, length(v)))
{
  v <- v[ord <- order(v)]
  w <- w[ord]
  tw <- (sw <- cumsum(w))[length(sw)]
  mean(v[which(sw>=0.5*tw & tw-shift.right(sw, 0)>=0.5*tw)])
}

  # demonstration
weighted.median(weatherc$temperature, ifelse(weatherc$play=="yes", 5, 1))
median(c(weatherc$temperature[weatherc$play=="no"],
         rep(weatherc$temperature[weatherc$play=="yes"], 5)))
weighted.median(weatherc$temperature, ifelse(weatherc$play=="yes", 0.2, 1))
median(c(weatherc$temperature[weatherc$play=="yes"],
         rep(weatherc$temperature[weatherc$play=="no"], 5)))


#Rank calculation is implemented and demonstrated by the following R code
bs.rank <- function(v)
{
  r.min <- match(v, sort(v))
  r.max <- length(v)+1-match(v, rev(sort(v)))
  (r.min+r.max)/2
}

#demonstration
print(weatherr$playability)
bs.rank(weatherr$playability)
rank(weatherr$playability)
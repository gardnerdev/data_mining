### Euclidean distance 

It handles both continuous 
and discrete attributes using
avdiff function that returns 
attribute value differences 
appropriately depending on attribute types
```
avdiff <- function(x1, x2)
{
  mapply(function(v1, v2) ifelse(is.numeric(v1), v1-v2, v1!=v2), x1, x2)
}
euc.dist <- function(x1, x2) { sqrt(sum(avdiff(x1,x2)ˆ2, na.rm=TRUE)) }
  # Euclidean distance dissimilarity matrix for the weathercl data
dissmat(weathercl, euc.dist)
```

### Minkowski distance

```
mink.dist <- function(x1, x2, p) { (sum(abs(avdiff(x1,x2))ˆp, na.rm=TRUE))ˆ(1/p) }
  # Minkowski distance dissimilarity matrices for the weathercl data
dissmat(weathercl, function (x1, x2) mink.dist(x1, x2, 1))
dissmat(weathercl, function (x1, x2) mink.dist(x1, x2, 3))
```

###  Manhattan distance 
man.dist <- function(x1, x2) { mink.dist(x1, x2, 1) }
  ##### Manhattan distance dissimilarity matrix for the weathercl data
dissmat(weathercl, function (x1, x2) man.dist(x1, x2))


### R code implements the Hamming distance and applies it to the weathercl data

```
ham.dist <- function(x1, x2) { sum(x1!=x2, na.rm=TRUE) }
dissmat(weathercl, ham.dist)
```


### Chebyshev distance and demonstrates its application to the weathercl data
For comparison, the Minkowski distance dissimilarity matrix for c11-math-0041 is also calculated. It can be verified to approximate the Chebyshev dissimilarity matrix quite closely
```
cheb.dist <- function(x1, x2) { max(abs(avdiff(x1,x2)), na.rm=TRUE) }
  # Chebyshev distance dissimilarity matrix for the weathercl
dissmat(weathercl, cheb.dist)
  # roughly the same as
dissmat(weathercl, function (x1, x2) mink.dist(x1, x2, 10))
```
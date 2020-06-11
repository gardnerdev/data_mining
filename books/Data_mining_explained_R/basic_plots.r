# boxplots for the playability attribute in the weatherr data: 
# one with outlying values identified
par(mfrow=c(1, 2))
boxplot(weatherr$playability, range=0.5, col="grey", main="playability")
boxplot(playability~outlook, weatherr, col="grey", main="playability")

#histograms
#Two histograms for the playability attribute in the weatherr data are produced by 
#the following R code: one displaying occurrence counts 
#and the other displaying relative frequencies for the same set of nonequal-weight intervals
par(mfrow=c(1, 2))
hist(weatherr$playability, breaks=c(0.3, 0.4, 0.5, 0.7, 0.9), probability=FALSE,
     col="grey", main="")
hist(weatherr$playability, breaks=c(0.3, 0.4, 0.5, 0.7, 0.9), probability=TRUE,
     col="grey", main="")


#artificial barplot illustration using the weatherr data

par(mar=c(7, 4, 4, 2))
barplot(`names<-`(ave(weatherr$playability, weatherr$outlook, weatherr$wind),
                  interaction(weatherr$outlook, weatherr$wind)),
        las=2, main="Mean playability in outlook-wind subsets")
lines(c(0, 17), rep(mean(weatherr$playability), 2), lty=2)
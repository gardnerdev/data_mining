#The naïve Bayes classifier is one of the simplest approaches to the 
#classification task that is still capable of providing reasonable accuracy

#It is superior for certain specific application domains, with text classification 
#being the most prominent example. Its simplicity—conceptual, implementational, and 
#computational—makes it easy and inexpensive to try besides or before more 
#sophisticated classifiers.


library(dmr.stats)
library(dmr.util)
data(weather, package="dmr.data")
data(weatherc, package="dmr.data")


## calculate the posterior probability given prior and inverse probabilities
bayes.rule <- function(prior, inv)
{
  prior*inv/sum(prior*inv)
}

# posterior probabilities
bayes.rule(c(0.2, 0.3, 0.5), c(0.9, 0.9, 0.5))

  # let P(burglery)=0.001,
  # P(alarm|burglery)=0.95,
  # P(alarm|not burglery)=0.005
  # calculate P(burglery|alarm)
bayes.rule(c(0.001, 0.999), c(0.95, 0.005))[1]

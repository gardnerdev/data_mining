## Classification
Classification is a process of assigining a given object to a certain 
class (croup) from predefined set of classes
- typically only one class is assigned

Classification in data mining areas refers to the methods 
of automatic building of classifiers based on training 
data including labelled objects.

The purpose of the classification is to find rule (rules)
which gives for any object 'w' its class c.

### Evaluation of classification quality
* precision
* recall
* accuracy
* error


### Decision tree problems
* how to partition the data at each step?
* when to stop partitioning?
* how to predict the value of a class/category for each object in a partition?

A decision tree is built in two phases:
- builiding
- pruning for avoiding overfitting

A decision tree is built by recursive splitting input sets in nodes until:
- input data includes only objects belonging to one class
- the number of objects in an input set is small enough
 

### Classification in R
* caret - dedicated to creation of predictive models, contains tools for:
  - data splitting
  - pre-processing
  - feature selection
  - model tuning using resambling
* rpart, C50, randomForest - functionality for building decision trees 
  and random forest classifiers 






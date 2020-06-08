# Data mining laboratories

Laboratory tasks from data mining classes


## Association rules

* Transaction - a finite subset of items belonging to a certain domain
* Data base (BD) - a set of transactions

### Assosiation rule is an expression in the following form
```
A => B
where A and B are set of items
A - antecedent of a rule
B - consequent of a rule
```

### Parameters of association rules
* Support
* Confidence
* Lift
* Filters put on presence of items in rules or their parts


#### Support
The support says how common is a given rule in an analysed DB.

* Absolute support
  a number of transactions in DB in which a given rule is contained
  ```
  absolute support of a rule (A=>B) = number of transactions in DB including A u B
  ```
* Relative support
  frequency of occurence of a given rule in DB
  ```
  relative support of a rule (A=>B) = absolute support(A=>B) / number of trans. in DB
  ```

#### Condfidence
Confidence: The confidence shows how often a rule is found to be true, e.g. if x is bought, how often is y bought.

The confidence indicates how strong the antecedent of a rule 
determines occurrence of the consequente of a rule

The confidence of a rule shows probability of occurence 
of the consequent of a rule in a transaction providing 
that the antecedent of that rule is present in the considered 
transaction
```
confidence of a rule (A=>B) = support(AuB) / support(A)
```
Example
If a person P bought smartphone then she/he bought also data packet (supp. 20%, con. 80%)


#### Lift
Lift provides the information if a rule: "if x is bought, how often is y bought" is random or not.
If Lift > 1, both occurances are dependent. Only for Lift greater 1 a potential useful rule can be found.

Lift parameter shows how much the confidence of a rule 
is greater than confidence calculated based on probability
of occurrence of consequent of that rule in DB
```
Lift(A=>B) = confidence of a rule(A=>B) / relative support(B)
```
* lift > 1 -shows that presence of the antecedent of a rule 
            in a given transaction increases probability of 
            occurrence of the consequent of a rule in that 
            transaction
* lift < 1 -shows that presence of the antecedent of a rule
            in a given transaction decreases probability of
            occurrence of a consequent of a rule in that 
            transaction
* lift = 1 -shows that there is a no relation between the 
            antecedent and the consequent of a rule (presence
            of the antecedent of a rule does not influence 
            occurence of the consequent of a rule)

#### Association rules discovery with R
Package **arules** - an environment for association rules discovery 
* offers funcionality for discovering association rules only with
  one item in consequent
* requires data in transactional form i.e transactions are defined
  by a set of binary properties (attributes). If a given property
  occurs in a transaction we have value 1 for that property, otherwise
  we have value 0
Package **arulesViz** - visualization of frequent itemsets and association
  rules /various versions of plot function

##### Package arules - selected functions 
* itemFrequency - calculation of occurrence frequence of items
* ruleInduction - an association rules generation based on prior discovered frequent itemsets
* apriori, eclat - frequent itemsets discovery
* inspect - showing frequent itemsets and association rules
* subset - selection of association rules meeting a user's requirements
  

## Sequential rules 
### Basic notions 
* Transaction - a finite subset of items belonging
                to a certain domain with a time stamp
* Sequence - an ordered set of transactions concerning one  object
* Data base (BD) - a set of sequences
* Event - occurence of a set of items in some transaction
  
### Sequential rules
A sequential rules (SR) is an expressino in the following form:
```
A => B
where A and B are sequences
A - antecedent of a rule
B - consequent of a rule
```
A sequential rule expresses certain time consecution of events

### Parameters of sequential rules 
Parameters of SR available in arulesSequences package
* Support 
* Confidence
* Lift
Parameters of SR available in arulesSequences package
* maxsize - the maximal number of items of an element of       sequence, default 10
* maxlen - the maximal number of elements of a sequence,
default 10
* mingap - the minimum time difference between consecutive
elements of a sequence, default none 
* maxgap - the maximum time difference between consecutive
elements of a sequence, default none 
* maxwin - the maximum time difference between any elements
of a sequence, default none 

#### Support
Support of a sequential rule is calculated as a number
of sequences including that rule. Only the fact of occurrence
of that rule in a given sequence is important, not a number 
of occurrences.

```
absolute support of a rule (A=>B) - number of sequences in DB
including A U B with a given time constrains
```
Relarive support - a frequency of occurrence of a given rule in DB
```
relative support of a rule (A=>B)=absolute support(A=>B)/number
of sequences in DB
```
#### Support, Confidence, Lift
Interpretation of support, confidence and lift parameters for 
sequential rules is analogous to interpretation of these para-
meters for association rules

#### Sequential rules discovery with R
Package arulesSequences - selected methods:
* read_baskets - read data with sequences
* itemFrequency - calculation of frequency of occurrence of items
* ruleInduction - an sequential rules generation based on prior
discovered sequences
* cspade - frequent sequences discovery
* inspect - showing sequences and rules
* subset - selection of sequences or sequential rules meeting a user's requirements
  

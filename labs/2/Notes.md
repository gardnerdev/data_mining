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
* package **arulesViz** - visualization of frequent itemsets and association
  rules /various versions of plot function
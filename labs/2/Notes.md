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


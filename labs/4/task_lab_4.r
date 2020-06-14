library(arules)
library(arulesSequences)


download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans.data','diab_trans.data')
#reading data - into dataframe
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)
View(diab.df)
#example of saving data into a file  - removing the header line
#write.table(diab.df, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )


################################################ PREPROCESSING #############################################################

#All records which do not refer to the blood glucose measurement or hypoglycemic symptoms should
#be discarded from the dataset

summary(diab.df)


#The Code field is deciphered as follows:
#33 = Regular insulin dose
#34 = NPH insulin dose
#35 = UltraLente insulin dose
#48 = Unspecified blood glucose measurement
#57 = Unspecified blood glucose measurement
#58 = Pre-breakfast blood glucose measurement
#59 = Post-breakfast blood glucose measurement
#60 = Pre-lunch blood glucose measurement
#61 = Post-lunch blood glucose measurement
#62 = Pre-supper blood glucose measurement
#63 = Post-supper blood glucose measurement
#64 = Pre-snack blood glucose measurement
#65 = Hypoglycemic symptoms
#66 = Typical meal ingestion
#67 = More-than-usual meal ingestion
#68 = Less-than-usual meal ingestion
#69 = Typical exercise activity
#70 = More-than-usual exercise activity
#71 = Less-than-usual exercise activity
#72 = Unspecified special even

#Discarding unnecessary records
diab.df %>% filter(
  code %in% c("id_48", "id_57","id_58","id_59","id_60","id_61","id_62","id_63","id_64","id_65")
) -> diab.filtered

nrow(diab.filtered)



#The values associated with the blood glucose measurement event
#should be discretized according to information available on
#https://en.wikipedia.org/wiki/Blood_sugar_level#Normal_values or
#https://www.medicinenet.com/normal_blood_sugar_levels_in_adults_with_diabetes/article.htm





diab.filtered %>% mutate(glucose_level = case_when((code %in% c("id_48","id_57") & value < 100)  ~ "normal", 
                                              (code %in% c("id_58", "id_60","id_62","id_64") & (value > 70 & value <130)) ~ "normal",
                                              (code %in% c("id_59","id_61","id_63") & (value <180)) ~ "normal",
                                              (code %in% c("id_65")  ~ "hypoglycemia"))) -> new_diab

df <- new_diab %>%
  mutate(glucose_level = if_else(is.na(glucose_level), "no-hypoglycemia",glucose_level))



df %>% select(1:3,5) -> dataframe


dataframe

#example of saving data into a file  - removing the header line
write.table(dataframe, "diabetes.data", sep = "," , row.names = FALSE, col.names = FALSE )


#reading data in transactional form
diabSeq <- read_baskets(con = "diabetes.data", sep =",", info = c("sequenceID","eventID"))
View(as(diabSeq,"data.frame"))




#reading data in transactional form
diabSeq <- read_baskets(con = "dataframe", sep =",", info = c("sequenceID","eventID"))
View(as(diabSeq,"data.frame"))

summary(diabSeq)




#setting parameters
#time(eventid) in the diab_trans.data set is given as a number of seconds from some date.
#the following values of parameters are the example of values which allow obtaining any sequential rules.

seqParam = new ("SPparameter",support = 0.5, maxsize = 4, mingap=600, maxgap =172800, maxlen = 3 )
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))


#discovery of sequential rules
seqRules = ruleInduction(patSeq,confidence = 0.8)

length(seqRules)
#summary for the set of rules
summary(seqRules)
#view of of rules
inspect(head(seqRules,5))

View(as(diabSeq,"data.frame"))



#information about data concerning times 
?timeFrequency
timeSeq  = as(diabSeq,"timedsequences")
freqT = timeFrequency(timeSeq, "times")
freqT

spanT= timeFrequency(timeSeq, "span")
spanT



#calculation of frequency of items
?itemFrequency
freqItem = itemFrequency(diabSeq)
#str(freqItem)

freqItem = sort(freqItem, decreasing = TRUE )
head(freqItem,5)
str(diabSeq)

diabSeq

#transaction for in a given sequence
trans5 <- diabSeq[diabSeq@itemsetInfo$sequenceID == 5]
inspect(trans5)

#frequent sequences discovery
#parameters of Spade algorithm
?'SPparameter-class'
seqParam = new ("SPparameter",support = 0.003, maxsize = 5, mingap=1, maxgap = 3, maxlen = 5)
print(seqParam)

#execution of the algorithm
?cspade
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))

#information about discoverd sequences
summary(patSeq)
#length(patSeq)

inspect(head(patSeq,66))
#patterns with more than one element
seq2elem <- patSeq[size(patSeq)>1]
length(seq2elem)
inspect(seq2elem)



#patters supported by a given sequence of transactions(without time constraints))
inspect(patSeq[which(support(patSeq,trans5,type='absolute') >0)])

#searching for patterns with given items
?subset
?match



#execution the algorithm with different parameters
seqParam1 = new ("SPparameter",support = 0.001, maxsize = 5, mingap=1, maxgap = 3, maxlen = 5)
patSeq1= cspade(diabSeq,seqParam1, control = list(verbose = TRUE, tidLists = TRUE, summary= TRUE))

#selecting new discovered sequences

seqdiff = patSeq1[which(!(patSeq1 %in% patSeq))]

length(patSeq1)
length(seqdiff)

#discovery sequential rules
?ruleInduction
seqRules = ruleInduction(patSeq1,confidence = 0.8)

length(seqRules)
#summary of set of rules
summary(seqRules)
#view of rules and sequneces
inspect(seqRules)




?arulesSequences::size

size(lhs(seqRules))
size(lhs(seqRules), 'length')
size(lhs(seqRules), 'itemsets')
size(lhs(seqRules), 'items')


#sequences in the left part of rules
inspect(lhs(seqRules))
#sequences in the right part of rules
inspect(rhs(seqRules))


#items in rules
seqRules@elements@items@itemInfo

#all patterns included in rules (from left and right parts of rules)
allSeq <- c(rhs(seqRules),lhs(seqRules))
allSeq <- unique(allSeq)
inspect(allSeq)
str(allSeq)



#selecting interesting rules
rulesI = subset(seqRules, rhs(seqRules) %in% c('design','webdesign'))
inspect(rulesI)
View(as(rulesI,"data.frame"))

rulesI = subset(seqRules, lhs(seqRules) %ein% c('design','webdesign'))
inspect(rulesI)

rulesI = subset(seqRules, lhs(seqRules) %ein% c('design','webdesign') & rhs(seqRules) %in% c('webdesign'))
inspect(rulesI)




























        
        
        


        
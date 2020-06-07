
##Data import
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
mushrooms <- read.csv(file = url, header = F)

colnames(mushrooms) <- c("edibility", "cap_shape", "cap_surface", "cap_color", "bruises", "odor", "gill_att", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surf_above", "stalk_surf_below", "stalk_color_above", "stalk_color_below", "veil_type", "veil_color", "ring_nr", "ring_type", "spore_print_color", "population", "habitat")

# Before I can perform market basket analysis we need to transform our dataframe to type transaction
trans <- as(mushrooms, "transactions")

# We can extract the rules with apriori() function. We pass our data. 
# The resulting RHS is filtered for only edible mushrooms, 
# because we want to find rules, that clearly define this outcome.
# Some minimum values for support and confidence are passed as well.
rules <- apriori(data = mushrooms, 
         appearance = list(rhs=c("edibility=e"), default='lhs'),
         parameter = list(supp = 0.1, conf = .1))


rules <- sort(rules, by = "lift", decreasing = T)
inspect(rules[1:10])

# The highest lift is 1.93. For all these rules we get a confidence of 1. 
# All these inputs on LHS lead to an edible mushroom. 
# Since we want to make absolutely sure, that we don’t eat a poisonous mushroom, 
# we should only rely on rules with confidence of 1.


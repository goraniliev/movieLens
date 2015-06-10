library(arules)
library(arulesViz)
itemsData = read.table('data/itemsData.txt', header = T, sep = '\t', quote = "")

# t is amatrix representation of the movie genres of each movie (binary matrix)
t = data.matrix(itemsData[, -1])
# the initial rules generated (we could expiriment with the params and obtain different rules)
rules = apriori(t, parameter = list(supp = 0.01, conf = 0.1, target = "rules", minlen = 2))
# checking the produced rules
inspect(rules) # 54 in total, some redundant
# sorting the rules by lift in decending order
rules.sorted = sort(rules, by = "lift")
# checking the sorted list
inspect(rules.sorted) # we can notice many redundant rules

# finding redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA

redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant) # checking which are redundant associations

rules.pruned = rules.sorted[!redundant] # removing the redundant rules
inspect(rules.pruned) # checking the non-redundant 32 rules
plot(rules.pruned, method = "graph")

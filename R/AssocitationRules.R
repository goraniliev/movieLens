library(arules)
library(arulesViz)

setwd('/home/goran/git/movieLens/movieLens/R')

# loading data important for the associations
itemsData = read.table('data/itemsData.txt', header = T, sep = '\t', quote = "")

# t is amatrix representation of the movie genres of each movie (binary matrix)
t = data.matrix(itemsData[, -1])
# the initial rules generated (we could expiriment with the params and obtain different rules)
rules = apriori(t, parameter = list(supp = 0.01, conf = 0.1, target = "rules", minlen = 2))
# checking the produced rules

# printing all rules
sink(file = "out//AssociationRules/allRules.txt", type = "output")
cat('All rules (the redundant are not removed:\n\n\n')
inspect(rules) # 54 in total, some redundant
sink()

rules.sorted = sort(rules, by = "lift")
sink(file = "out//AssociationRules/allRulesSortedByLift.txt", type = "output")
# sorting the rules by lift in decending order
# checking the sorted list
inspect(rules.sorted) # we can notice many redundant rules
sink()

# finding redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA

redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant) # checking which are redundant associations

rules.pruned = rules.sorted[!redundant] # removing the redundant rules
# Printing non-redundant rules into file
sink(file = "out/AssociationRules/prunedRules.txt")
inspect(rules.pruned)
sink()

# store the image for associtation rules
jpeg('out/AssociationRules/GenresAssociationRules.jpg')
plot(rules.pruned, method = "graph")
dev.off()

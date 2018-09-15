# Market Basket Analysis (Refined code)

getwd()
setwd("C:/Users/HP/Downloads/dcsporder copy.xlsx")

install.packages("arules")
library(arules)

install.packages("arulesViz")
library(arulesViz)

mba = read.transactions(file="lawson.csv",rm.duplicates=FALSE, format="single", sep=",",cols=c(1,2));
rules = apriori(mba, parameter=list(support=0.001, confidence=0.8, maxlen=3, minlen=2))
inspect(rules[1:5])
rules<-sort(rules, by="support", decreasing=TRUE)
ruling = inspect(rules[1:5])

write.csv(ruling, "Support_basedapriori.csv")
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

#Targeting for one particular Combination [Right Hand side]

rules<-apriori(data=mba, parameter=list(supp=0.001,conf = 0.8), 
               appearance = list(default="lhs",rhs="KT12401"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

#Targeting for one particular Combination [Left Hand side]

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="rhs",lhs="KT12401"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])


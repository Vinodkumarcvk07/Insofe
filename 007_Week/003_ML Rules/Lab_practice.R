library("arules")

trans = read.transactions(file="/Users/suresh/Desktop/Insofe/007_Week/002_ML Rules/Transactions.csv", rm.duplicates= FALSE,format="single",sep=",",cols =c(1,2))
#data = read.transactions("/Users/suresh/Desktop/Insofe/007_Week/002_ML Rules/Transactions.csv")
inspect(trans)
trans
image(trans)

itemFrequency(trans)
itemFrequencyPlot(trans)
summary(trans)

rules <- apriori(trans,parameter = list(sup = 0.2, conf = 0.6,target="rules"))
summary(rules)

inspect(rules)

top_rules = sort(rules, by = c("lift","confidence","support"),decreasing = TRUE) 
inspect(top_rules)
head(as(top_rules, "data.frame"), n=5)

rules_by_conf = rules[sort(rules, by = "confidence", order = TRUE)] 
rules_by_conf = as(rules_by_conf,"data.frame")

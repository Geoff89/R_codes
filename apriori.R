#apriori algoruthsms- associatio rules to create recommendation systems
#finding strong rules with high support and confidence

grocery <- read.csv(file = "C:\\users\\jeffnerd\\Desktop\\groceries.csv", header = TRUE,
                      sep = ",", stringsAsFactors = FALSE)
groceries
head(groceries)

library(arules)
library(ggplot2)

#reading the groceries data
groceries <- read.transactions("C:\\Users\\jeffnerd\\Desktop\\groceries.csv", sep = ",")
groceries
summary(groceries)
colnames(groceries)

#checking the items in gocery basket transactions
inspect(groceries[1:5])
inspect(groceries[250:500])
#checkingthe support level of the first three items
itemFrequency(groceries[,1:3])
itemFrequency(groceries[,15:35])
#plotting the itemfrequecy
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, support = 0.0006)

#plotting the support of the first 2 items
itemFrequencyPlot(groceries, topN = 20)
itemFrequencyPlot(groceries, topN = 30)

#visualizing the sparse matrix
image(groceries[1:5])
image(sample(groceries, 100))


#training our model
#using thre default parametrs we get zero rules
apriori(groceries)
#a modified apriori association rule
groceryrules <- apriori(groceries, parameter = list(support = 0.006,
                        confidence = 0.25, minlen = 2 ))
groceryrules

#evaluating models perfomance
summary(groceryrules)
#insecting the rules
inspect(groceryrules[1:3])
inspect(groceryrules[1:100])

#improving our model ie using lift, confidence and support parametrs
inspect(sort(groceryrules, by = "lift") [1:5])
inspect(sort(groceryrules, by = "support") [1:5])
inspect(sort(groceryrules, by = "confidence") [1:5])

#using subsets
berryrules <- subset(groceryrules , items %in% "berries")
inspect(berryrules)
berryrules1 <- subset(groceryrules , lhs %in% "berries")
inspect(berryrules1)
berryrules2 <- subset(groceryrules , rhs %in% "berries")
inspect(berryrules2)
berryrules3 <- subset(groceryrules , items %pin% "fruit")
inspect(berryrules3)
berryrulee4 <- subset(groceryrules , items %ain% c("berries", "yogurt"))
inspect(berryrulee4)
berryrules5 <- subset(groceryrules , confidence > 0.5)
inspect(berryrules5)
berryrules6 <- subset(groceryrules , support <= 0.5)
inspect(berryrules6)
berryrules7 <- subset(groceryrules , lift = 0.5)
inspect(berryrules7)
berryrules8 <- subset(groceryrules , confidence > 0.5 || lift < 0.5)
inspect(berryrules8)

#writing the ===grocery rules in file
write(groceryrules, file = "C:\\Users\\jeffnerd\\Desktop\\groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)
#coercing the rules to a dataframe
grocerries_df <- as.data.frame(groceryrules)
grocerries_df
str(grocerries_df)

#summary transactions two
x <- c(1,2,3,4,5,6,7)
x[1]
x[1:3] 
bookbaskets <- read.transactions("C:\\Users\\jeffnerd\\Desktop\\machine2nded\\bookdata.tsv",
                                 format = "single",
                                 sep = "\t",
                                 cols = c("userid", "title"),
                                 rm.duplicates = T)
bookbaskets
summary(bookbaskets)
inspect(bookbaskets[1:5])
inspect(bookbaskets[200:205])
itemFrequency(bookbaskets[,1:3])
itemFrequency(bookbaskets[,5:10])
itemFrequencyPlot(bookbaskets, support=0.1)
itemFrequencyPlot(bookbaskets, topN=20)
image(bookbaskets[1:5])
image(sample(bookbaskets, 300))
#the other analysis
class(bookbaskets)
dim(bookbaskets)
colnames(bookbaskets)[1:10]
rownames(bookbaskets)[1:10]
#examining distribution of the basket sizes/transaction sizes
basketsizes <- size(bookbaskets)
basketsizes
summary(basketsizes)
#looing further the size of rthe distributin
quantile(basketsizes, probs = seq(0,1,0.1))
quantile(basketsizes, probs = c(0.99, 1))
#plotting our distribution
ggplot(data.frame(count=basketsizes)) +
  geom_density(aes(x=count))+
  scale_x_log10()
#which books are they reading
bookFreq <- itemFrequency(bookbaskets)
bookFreq
summary(bookFreq)#this summary doesnt add up to 1
#we can normalize the above
sum(bookFreq)
sum(basketsizes)
bookCount <- (bookFreq/sum(bookFreq)) * sum(basketsizes)
bookCount
summary(bookCount)
#sort the bookcount and list the 10 most popular books read
orderedBooks <- sort(bookCount, decreasing = T)
orderedBooks[1:10]
orderedBooks[1] #wild animus
#how frequent did it occuro
orderedBooks[1]/dim(bookbaskets)[1] #0.0271
#finalyy dealing with rare events b4 minng our data
#should have at least 2 books in our basket
bookbaskets_use <- bookbaskets[basketsizes>1]
dim(bookbaskets_use)

support = 100/dim(bookbaskets_use)[1]
support
rules <- apriori(bookbaskets_use,
                 parameter = list(support = 0.002, confidence=0.75))
summary(rules)

#measuring annd evaluating our rules
measures <- interestMeasure(rules,
                           measure = c("coverage", "fishersExactTest"),
                           transactions = bookbaskets_use)
summary(measures)
#inspect our rules
inspect(head(sort(rules, by="confidence"), n = 5))
inspect(sort(rules, by = "confidence"))[1:5]
#controlling what to mine
brules <- apriori(bookbaskets_use,
                 parameter = list(support = 0.001, confidence=0.6),
                 appearance = list(rhs=c("The Lovely Bones: A Novel"),
                 default = "lhs"))
summary(brules)

inspect(brules[1:5])
brulesConf <- sort(brules, by ="confidence")
inspect(head(lhs(brulesConf), n=5))

#subsetting this data to filter out author of lovely
brulesSub <- subset(brules, subset = !(lhs %in% "Lucky : A Memoir"))
brulesConf <- sort(brulesSub, by = "confidence")
inspect(head(lhs(brulesConf), n=5))

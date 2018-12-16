mushrooms <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\mushrooms.csv", header = TRUE, sep =",",stringsAsFactors = FALSE )
mushrooms
str(mushrooms)
head(mushrooms)
#veil type has one factor..
mushrooms$veil_type <- NULL
table(mushrooms$type)
library(RWeka)
#since we want all the rules from al the features ad records we dont split the data
#we use the entire dataset
#training the model
set.seed(148)
sample_train <- sample(8125, 6000)

mushroom_train <- mushrooms[sample_train,]
mushroom_test <- mushrooms[-sample_train,]#error incorrect number of dimensions
mushrooms$type <- factor(mushrooms$type, levels = c("e", "p"), 
                        labels = c("edible","poisonous"))

mushrooms$type
head(mushrooms)
mushrooms <- lapply(mushrooms,  as.factor)
mushroom_1R <- OneR(type ~., data = mushrooms)
mushroom_1R
summary(mushroom_1R)

#For a more comlex ripper algorithm
mushrooms_jrip <- JRip(type~.,data = mushrooms)
mushrooms_jrip
summary(mushrooms_jrip)

#regression treees and modl trees
tee <- c(1, 1, 1, 2, 2, 3, 4, 5, 5, 6, 6, 7, 7, 7, 7)
at1 <- c(1, 1, 1, 2, 2, 3, 4, 5, 5)
at2 <- c(6, 6, 7, 7, 7, 7)
bt1 <- c(1, 1, 1, 2, 2, 3, 4)
bt2 <- c(5, 5, 6, 6, 7, 7, 7, 7)
sdr_a <- sd(tee) - (length(at1) / length(tee) * sd(at1) +
                        length(at2) / length(tee) * sd(at2))
sdr_b <- sd(tee) - (length(bt1) / length(tee) * sd(bt1) +
                        length(bt2) / length(tee) * sd(bt2))
sdr_a
sdr_b

#loading the data
wine <- read.csv(file = "C:\\Users\\jeffnerd\\Desktop\\whitewines.csv", header = TRUE, 
                 sep = ",", stringsAsFactors = FALSE) 
head(wine)
str(wine)
class(wine)
#checking for the distribution
hist(wine$quality)
library(ggplot2)
ggplot(wine, aes(x=wine$quality))+geom_histogram(bins = 30)+scale_fill_brewer(palette = "Set1")
ggplot(wine, aes(x=wine$quality))+geom_histogram(bins = 30)+scale_fill_manual(values =c("red"))

summary(wine$quality)

#splitting this data into training and test data
wine_train <- wine[1:3750,]
wine_test <- wine[3751:4898,]

#training this model
library(rpart)
library(rpart.plot)
m.rpart <- rpart(quality~., data = wine)
m.rpart
#summary of the model important features
summary(m.rpart)
#plotting the above model
rpart.plot(m.rpart, digits = 3)
#alterig how our tree appears
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)
#predicting our data on the test data set
p.rpart <- predict(m.rpart, wine_test)
p.rpart
summary(p.rpart)
summary(wine$quality)
#viewing the models perfomance
cor(p.rpart, wine_test$quality)
plot(wine_test$quality, p.rpart)
#finding how well our model performs btwee the actual values and predicted values
#known as mean absolute error
MAE <- function(actual,predicted){
     mean(abs(actual- predicted))
}
MAE(p.rpart, wine_test$quality)
MAE(wine_test$quality, p.rpart)
#another version
mean(wine_train$quality)
MAE(5.89,wine_test$quality)

#model tree to improve from regression trees
library(RWeka)
m.m5p <- M5P(quality~., data = wine_train)
m.m5p

#how well the model fitted the training data
summary(m.m5p)
#ow training it on the unseen data
p.m5p <- predict(m.m5p, wine_test)
p.m5p

summary(p.m5p)
#correlation of the models
cor(p.m5p, wine_test$quality)
#Finding the MAE
MAE(wine_test$quality, p.m5p)

#using the agreement way
agreement_t <- p.m5p == wine_test$quality
mean(agreement_t)
prop.table(table(agreement_t))

agreement_t <- p.m5p != wine_test$quality
table(agreement_t)



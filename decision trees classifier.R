curve(-0.60 * log2(0.60) - (1 - 0.40) * log2(1 - 0.40),
      col = "red", xlab = "x", ylab = "Entropy", lwd = 4)
#Decision trees and rule learners
credit <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\credit.csv", header = TRUE, sep =",",stringsAsFactors = FALSE )
credit
head(credit)
str(credit)
dim(credit)
#checking features that can be used as predictors of defaulting to pay a loan
table(credit$checking_balance)
table(credit$savings_balance)
prop.table(table(credit$savings_balance))

summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

set.seed(123)
train_sample <- sample(1000, 900)
train_sample
str(train_sample)
#creating training and test data
credit_train <- credit[train_sample,]
credit_test <- credit[-train_sample,]
str(credit_train)
str(credit_test)
#putting labels to our credit default data
credit$default <- factor(credit$default, levels = c(1,2), labels = C("default",
                          "not default"))
#proportion of test and training labels
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))
#alternatively for the labels
credit_train_label <- credit[train_sample,]$default
credit_test_label <- credit[-train_sample,]$default
prop.table(table(credit_train_label))
prop.table(table(credit_test_label))

#Training the model
library(C50)
credit_model <- C5.0(credit_train[-17],factor(credit_train$default))
credit_model <- C5.0(credit_train[-17], factor(credit_train_label)) #require a factor outcome
credit_model
plot(credit_model)
summary(credit_model)
#predicting our model
credit_pred <- predict(credit_model,credit_test)

credit_pred
library(gmodels)
CrossTable(credit_test$default,credit_pred,prop.chisq = FALSE, prop.r = FALSE,prop.c = FALSE,
           dnn = c("actual default","predicted default"))
#using adaptive boosting to improve accuracy of our model using trials
credit_boost10 <- C5.0(credit_train[-17], factor(credit_train$default), trials = 10)
credit_boost10
summary(credit_boost10)
#viweing this data on test data and their accuracy
credit_boost_pred10 <- predict(credit_boost10, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_boost_pred10,prop.chisq = FALSE,
           prop.r = FALSE, prop.c = FALSE,
           dnn = c("actual default","predicted default"))

#cost error mistakes is another way to improve our model
matrix_dimensions <- list(c(1,2),c(1,2))
matrix_dimensions
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions
#next is to assign the penalty for various types of errors
error_cost <- matrix(c(0,1,4,0), nrow = 2, dimnames = matrix_dimensions)
error_cost
#applying this to c5.0 model
credit_cost <- C5.0(credit_train[-17], factor(credit_train$default), costs = error_cost)
credit_cost
summary(credit_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
credit_cost_pred
CrossTable(credit_test$default, credit_cost_pred,prop.chisq = FALSE,
           prop.r = FALSE, prop.c = FALSE,
           dnn = c("actual default","predicted default"))

credit_cost <- C5.0(credit_train[-17], factor(credit_train$default), rules = TRUE)
credit_cost
summary(credit_cost)
library(ggplot2)
system.time(credit_cost <- C5.0(credit_train[-17], factor(credit_train$default), rules = TRUE),
            credit_cost)
ggplot(credit, aes(x = credit_test$default, y = credit_pred )) + geom_line() +
  scale_color_brewer()
 
credit_pred1 <- predict(credit_model,credit_test, type="prob")
credit_pred1

#holdout validation
random_ids <- order(runif(1000))
credit_train <- credit[random_ids[1:500], ]
credit_validate <- credit[random_ids[501:750], ]
credit_test <- credit[random_ids[751:1000], ]

library(caret)
in_train <- createDataPartition(credit$default, p = 0.75,
                                list = FALSE)
credit_train <- credit[in_train, ]
credit_test <- credit[-in_train, ]
credit_model <- C5.0(factor(default)~., data = credit_train)
credit_pred <- predict(credit_model, credit_test)
kappa <- kappa2(data.frame(credit_actual,credit_pred))$value
#using cross validation
library(caret)
library(C50)
library(irr)

set.seed(123)
folds <- createFolds(credit$default, k=10)
str(folds)

cv_results <- lapply(folds, function(x){
  credit_train <- credit[-x,]
  credit_test <- credit[x,]
  credit_model <- C5.0(factor(default)~., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual,credit_pred))$value
  return(kappa)
  })
str(cv_results)
#finding mean
mean(unlist(cv_results))
#0.269409
#getting probabilities of the model classifier
predicted_prob <- predict(credit_model, credit_test, type = "prob")
predicted_prob
head(predicted_prob)

library(caret)
modelLookup("C5.0")#loks three models threore 3^3
modelLookup("nb")
modelLookup("nnet")
modelLookup("OneR")
modelLookup("lm")
modelLookup("knn") #look up to 1
modelLookup("JRIP")
modelLookup("svmLinear")
modelLookup("svmRadial")
modelLookup("rpart")
modelLookup("M5")
modelLookup("treebag")
modelLookup("AdaBoost.M1")
modelLookup("rf")

set.seed(300)
credit1 <- as.data.frame(lapply(credit, as.factor))#not working
m <- train(factor(default)~ ., data = credit, method = "C5.0")
str(m)
#now we can predict our tuned model above
p <- predict(m , credit)
table(p,  credit$default)
table(credit$default, p)
#advantagses of using training and predict
head(predict(m, credit))
#getting the probabilities
head(predict(m, credit, type = "prob"))

#choosing our model tuning on our own rather than choosing automatically
#creating our traincontrol
ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
ctrlrcv <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                     selectionFunction = "best")
ctrlboot <- trainControl(method = "boot", number = 25, 
                         selectionFunction = "tolerance")
ctrlboot632 <- trainControl(method = "boot632", number = 25, 
                            selectionFunction = "oneSE")
ctrlLGOCV <- trainControl(method = "LGOCV", p = 0.75, selectionFunction = "oneSE")
ctrlLOOCV <- trainControl(method = "LOOCV",selectionFunction = "oneSE")

#now grid tuning dataframe for our parameter seach
grid <- expand.grid(.model = "tree",
                    .trials = c(1,5,10,15,20,25,30,35),
                    .winnow = "FALSE")
grid
#model preparation
set.seed(300)
m <- train(factor(default)~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)

m_rcv <- train(factor(default)~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrlrcv,
           tuneGrid = grid)
m_boot <- train(factor(default)~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrlboot,
           tuneGrid = grid)
m_boot632 <- train(factor(default)~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrlboot632,
           tuneGrid = grid)
m_lgocv <- train(factor(default)~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrlLGOCV,
           tuneGrid = grid)
m_loocv <- train(factor(defaulmt)~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrlLOOCV,
           tuneGrid = grid)
p <- predict(m, credit)                  
table(p, credit$default)

#Ensemble methods 
#Bagging
library(ipred)
set.seed(300)
myBag <- bagging(factor(default)~., data = credit, nbagg = 25)
myBag1 <- bagging(factor(default)~., data = credit, nbagg = 25)
myBag
#prediction
credit_pred_bagg <- predict(myBag, credit)
credit_pred_bagg1 <- predict(myBag1, credit)
table(credit_pred_bagg, credit$default)
table(credit_pred_bagg1, credit$default)

library(caret)
set.seed(300)
modelLookup("treebag")
ctrlBag <- trainControl(method = "cv", number = 10)
#we want to check the kappa
train(factor(default)~., data = credit, method = "treebag", trControl = ctrlBag)
tunegrid_bag <- expand.grid(.parameter = "TRUE")

#bagging beyond decision trees
str(svmBag)
str(nbBag)
str(ctreeBag)
str(nnetBag)
svmBag$fit
#creating a bag control
bagctrl <- bagControl(fit = svmBag$fit,
                      predict = svmBag$pred,
                      aggregate = svmBag$aggregate)
set.seed(300)
svmbag <- train(factor(default) ~., data = credit, "bag",
                trControl = ctrlBag, 
                bagControl=bagctrl)
svmbag <- train(default ~ ., data = credit_factor, "bag",
                trControl = ctrlBag, bagControl = bagctrl)

library(adabag)
set.seed(300)
#training the credit data
credit_factor <- as.data.frame(lapply(credit, as.factor))
m_adaboost <- boosting(default~., data = credit_factor)
p_adaboost <- predict(m_adaboost, credit_factor)
head(p_adaboost$class)
p_adaboost$confusion

adaboost_cv <- boosting.cv(default~., data = credit_factor)
adaboost_cv$confusion

library(vcd)
Kappa(adaboost_cv$confusion)

#tring to convert this to caret
modelLookup("Adaboost.M1")
ctrl <- trainControl(method = "cv", number = 10)
boost_train <- train(default~., data = credit_factor, method = "AdaBoost.M1",
                      trControl = ctrl )
predict_boot <- predict(boost_train, credit)
table(predict_boot, credit$default)
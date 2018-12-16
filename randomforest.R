#random forests
credit_factor <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\credit.csv", header = TRUE, sep =",",stringsAsFactors = FALSE )
credit
credit
head(credit)
library(randomForest)
set.seed(200)
rf <- randomForest(factor(default)~. , data = credit)

warnings(rf)

#lets compare two models the tuned boosted tree and random forest
library(caret)
ctrl_rf_c5 <- trainControl(method = "repeatedcv", number = 10,
                           repeats = 10)
#creating grid
modelLookup("rf")
grid_rf <- expand.grid(.mtry = c(2,4,8,16))
set.seed(300)
#create our auto-tuned model
m_rf <- train(factor(default)~., data = credit, method = "rf",
              metric ="Kappa", trControl = ctrl_rf_c5,
              tuneGrid = grid_rf)
m_rf
#now creating grid for c5.0 decision tree
grid_c5.0 <- expand.grid(.model = "tree",
                         .trials = c(10,20,30,40),
                         .winnow = "FALSE")
m_C50 <- train(factor(default)~., data = credit, method = "C5.0",
               metric = "Kappa", trControl = ctrl_rf_c5,
               tuneGrid = grid_c5.0)

iris_rf <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\IRIS.csv", header = TRUE, sep =",",
                    stringsAsFactors = FALSE )
str(iris_rf)
irismodel <- randomForest(factor(Name)~., data = iris_rf)
varimp <- importance(irismodel)
varimp[1:4,]
varImpPlot(irismodel)

varImp <- importance(fmodel)
varImp[1:10, ]
varImpPlot(fmodel, type=1)
selVars <- names(sort(varImp[,1], decreasing=T))[1:25]
fsel <- randomForest(x=spamTrain[,selVars],y=spamTrain$spam,
                     ntree=100,
                     nodesize=7,
                     importance=T)

library(Amelia)
missmap(credit, cols = c("red", "blue"), legend = F)

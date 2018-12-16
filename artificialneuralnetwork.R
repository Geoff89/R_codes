#Neural network
concrete <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\concrete.csv", header = TRUE, 
                     sep =",",stringsAsFactors = FALSE )
concrete
head(concrete)
str(concrete)
View(concrete)

#preprocessing/transfrming this data
normalize <- function(x){
  return((x - min(x))/(max(x) - min(x)))
}
body(normalize)
formals(normalize)
environment(normalize)
#normalizing the data
concrete_norm <- as.data.frame(lapply(concrete, normalize))
#seeing that they work
summary(concrete_norm$strength)
summary(concrete$strength)
summary(concrete_norm$coarseagg)
summary(concrete$coarseagg)

#splitting this data since it is in random order
concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]
#train the model
library(neuralnet)

concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic +
                              coarseagg + fineagg + age, data = concrete_norm)
concrete_model3 <- neuralnet(strength ~ cement + slag + ash + water + superplastic +
                              coarseagg + fineagg + age, data = concrete_norm,
                            hidden = 3)
concrete_model5 <- neuralnet(strength ~ cement + slag + ash + water + superplastic +
                               coarseagg + fineagg + age, data = concrete_norm,
                             hidden = 5)
concrete_model
summary(concrete_model)
plot(concrete_model)

plot(concrete_model3)
plot(concrete_model5)

#predicting our model on unseee data
#model with one node
model_results <- compute(concrete_model, concrete_test[1:8])
model_results
predicted_strength <- model_results$net.result
predicted_strength
#finding the accuracy correlation
cor(predicted_strength, concrete_test$strength)# 0.81611


#predicting our model on unseee data
#model with three node
model_results3 <- compute(concrete_model3, concrete_test[1:8])
model_results
predicted_strength3 <- model_results3$net.result
predicted_strength3
#finding the accuracy correlation
cor(predicted_strength3, concrete_test$strength)#0.89965

#predicting our model on unseee data
#model with one node
model_results5 <- compute(concrete_model5, concrete_test[1:8])
model_results5
predicted_strength5 <- model_results5$net.result
predicted_strength5
#finding the accuracy correlation
cor(predicted_strength5, concrete_test$strength)#0.9186

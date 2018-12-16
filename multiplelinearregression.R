#regression analysis
library(MASS)
eruption.lm =  lm(eruptions~waiting, data = faithful)
# waiting time is 80 minutes
newdata = data.frame(waiting = 80)
newdata
predict(eruption.lm, newdata )
summary(eruption.lm)

#Fit in the parameters of estimated regression equation
#by first getting the coefficients
coeffs = coefficients(eruption.lm)
coeffs
coeffs[1]
coeffs[2]
waiting = 80
duration = coeffs[1]+coeffs[2]*80
duration

#coefficient of determination for data set faithful
summary(eruption.lm)$r.squared

#significance test
summary(eruption.lm)

#confidence interval
attach(faithful)  #attach the data frame
eruption.lm =lm(eruptions ~ waiting)
newdata = data.frame(waiting=80)
predict(eruption.lm, newdata, interval = "confidence")
detach(faithful) # For data clean up
# The 95% onfidence interval of the eruption duration for the waiting time of 
#80 minutes is between 3.1961 and 5.1564 minutes

#prediction interval
predict(eruption.lm, newdata, interval = "predict")
#The 95% prediction interval of the eruption dutration for the waiting time of 80
#minutes is between 3.1961 and 5.1564 minutes

#Residual plot .Residual data is the difference between the observed data of the dependent
#variable y and fitted values y>
eruption.lm = lm(eruptions~waiting, data=faithful)
eruption.res = resid(eruption.lm)
#Now plot the residual against the observed data
plot(faithful$waiting, eruption.res, ylab = "Residuals", xlab = "Waiting Time",
     main = "Old Faithful Eruption")

#standardized Residual
eruption.lm = lm(eruptions~waiting, data=faithful)
eruption.stdrres = rstandard(eruption.lm)

#We plot the standardized residual against the obserbed values of the variable waiting
plot(eruption$waiting ,eruption.stdrres, ylab = "Standardized Residual", 
     xlab = "Waiting time", main = "Old faithful eruptions")

#Normal probability plot for the standardized residual
#USe qqnorm function and qqline for further comparison
qqnorm(eruption.stdrres, ylab = "Standardized Residuals", xlab = "Norma Scores",
       main = "Old Faisthful Eruptions")
qqline(eruption.stdrres)

# Multiple regression
stackloss.lm = lm(stack.loss~Air.Flow+Water.Temp+Acid.Conc.,
                  data = stackloss)
newdata = data.frame(Air.Flow =72,Water.Temp = 20, 
                     Acid.Conc.=85 )
predict(stackloss.lm, newdata)

#Multiple coefficient of determination
summary(stackloss.lm)$r.squared

#Significance test
summary(stackloss.lm)

#Condidence interval of MLR
predict(stackloss.lm, newdata, interval = "confidence")

#Prediction interval
predict(stackloss.lm, newdata, interval = "predict")

#Logistics regression workings. finding the probability
#for mtcars am column 0 = automatic and 1 = manual
am.glm = glm(formula = am ~ hp + wt, data = mtcars, family = binomial)
#wrap what we willl predict probability in a dataframe
newdata = data.frame(hp = 120, wt = 2.8)
newdata
#apply the predict glm function to obtain the predicted probability
predict(am.glm, newdata)
predict(am.glm, newdata, type = "response")

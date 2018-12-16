#ISLR package
#cross validation
library(ISLR)
library(boot)
set.seed(1)
train <- sample(392, 196)
#fit the model
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
lm.fit
summary(lm.fit)

#to predict these values
attach(Auto)
mean((mpg - predict(lm.fit, Auto)) [-train]^2) #26.14142 for liner=ar model

#other models for polynomial and cubic regressions
lm.fit2=lm(mpg~poly(horsepower ,2) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit2 ,Auto))[-train ]^2)
#19.82
 
lm.fit3=lm(mpg~poly(horsepower ,3) ,data=Auto ,subset =train )
mean((mpg -predict (lm.fit3 ,Auto))[-train ]^2)
#19.78

predict(lm.fit3, Auto)[-train]^2

#leave one out cross validation
glm.fit <- glm(mpg~horsepower, data = Auto)
coef(glm.fit)
#same results as above
lm.fit <- lm(mpg~horsepower, data = Auto)
coef(lm.fit)

library(boot)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta
#for leave one out
cv.error = rep(0,5)
cv.error

for (i in 1:5){
   glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
   cv.error[i]=cv.glm (Auto ,glm.fit)$delta [1]
}
cv.error
#24.23151 19.24821 19.33498 19.42443 19.03321

#for k fold times
set.seed(10)
cv.error.10 = rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower ,i),data=Auto)
  cv.error.10[i]=cv.glm (Auto,glm.fit, K = 10)$delta [1]
}
cv.error.10
#this output is faster24.13461 19.26651 19.16194 19.41352 19.07260 19.08658 18.94624 19.17225 18.78082
#19.51004

#bootstrap in subsamling data
#estimating the accuracy of linear regression model
boot.fn <- function(data, index){
  return(coef(lm(mpg~horsepower, data = Auto, subset = index)))
}
boot.fn(Auto, 1:392)

set.seed(123)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))

#using  boot function to perform the final residuals
boot(Auto, boot.fn,1000)

#alternatively to check the standar residua errors
summary(lm(mpg~horsepower, data = Auto))$coef

#putting this data into a quadratic mood
boot.fn <- function(data, index){
  return(coef(lm(mpg~horsepower+I(horsepower^2), data = Auto, subset = index)))
}
set.seed(1)
boot.fn(Auto, 1:392)

#using boot 
boot(Auto, boot.fn, 1000)

#alternatively
summary(lm(mpg~horsepower+I(horsepower^2), data = Auto))$coef

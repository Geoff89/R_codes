# linear regression ad logistic regression methods
launch <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\challenger.csv",
                       header = TRUE, sep =",",stringsAsFactors = FALSE )
launch
str(launch)
#Finding the value of b which is slope
b <- cov(launch$temperature, launch$distress_ct)/
     var(launch$temperature)
b
#from there find the value of a y intercept
a <- var(launch$temperature)
a
#Finding correlation among variables
r <- cov(launch$temperature,launch$distress_ct)/
  (sd(launch$temperature) * sd(launch$distress_ct))
r
#alternatively
cor(launch$temperature,launch$distress_ct)

#For multiple regression functions for findind beta fucntions for our model
reg <- function(y, x) {
  x <- as.matrix(x)
  x <- cbind(Intercept = 1, x)
  b <- solve(t(x) %*% x) %*% t(x) %*% y
  colnames(b) <- "estimate"
  print(b)
}
reg
reg(y = launch$distress_ct, x = launch[3])
#appyng this mdodel now to three columns
reg(y = launch$distress_ct, x = launch[3:4])

#insurance data collection
insurance <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\insurance.csv",
                   header = TRUE, sep =",",stringsAsFactors = TRUE )
str(insurance)
#checking  for normalization in r for  dependent variable for better fit
head(insurance)
summary(insurance$charges)
hist(insurance$charges)
ggplot(insurance, aes(x=insurance$charges))+geom_histogram(bins = 30)+scale_color_brewer(palette = "Set1")
boxplot(insurance$charges)
#Finding the correlatonmatrix
cor(insurance[c("age","bmi","children","charges")])
#plotting a  scatter matrix to see ralationship between each depedent and indepedent variable
pairs(insurance[c("age","bmi","children","charges")])
library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])

#Training the model
ins_model <- lm(charges~age+children+bmi+sex+smoker+region, data = insurance)
ins_model
ins_model <- lm(charges~., data = insurance)
ins_model
#how well the model is fitted
summary(ins_model)
#improving the perfomance of the model to reduce sum of squared errors and increasing more logic
#addding non linear model in a polynomial equation
insurance$age2 <- insurance$age^2
#Transformation - converting a numeric variable to a
#binary indicator
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)
#adding interactions of features
ins_model2 <- lm(charges~age+age2+children+bmi+sex+
                   bmi30*smoker+region, data = insurance)
ins_model2
summary(ins_model2)


library(caret)


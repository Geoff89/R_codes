#time series analysis
# define custom function
mysum <- function(a, b, c) {
  sum <- a + b + c
  return(sum)
}
# call custom function
mysum(1,2,3)

#Decision structures
# while loop
a <- 100
while (a < 500) {
  a <- a + 100
}
a

# for loop
mylist <- c(55, 66, 77, 88, 99)
for (value in mylist) {
  print(value)
}

# if then else
a <- 66
if (a > 55) {
  print("a is more than 55")
} else {
  print("A is less than or equal to 55")
}  
#read the data
wdbc <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\wdbc.csv", header = TRUE, sep =",")
wdbc <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\wdbc.csv", header = FALSE, sep =",")
usedcars <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\usedcars.csv", header = TRUE, sep =",")
usedcars <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\usedcars.csv", header = TRUE, sep =",",stringsAsFactors = FALSE )
usedcars
#Determining the structure of our data
str(usedcars)
summary(usedcars$year)
summary(usedcars[c("price", "mileage")])
range(usedcars$price)
diff(range(usedcars$price))
IQR(usedcars$price)
quantile(usedcars$price)
quantile(usedcars$price, probs = c(0.01, 0.99))
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20))
mean(usedcars$price)
var(usedcars$price)
sd(usedcars$price)
var(usedcars$mileage)
sd(usedcars$mileage)
#Table of used cars in categorical data
table(usedcars$year)
table(usedcars$model)
#calculating the table proportions
model_table <- table(usedcars$model)
prop.table(model_table)
color_table <- table(usedcars$color)
color_pct <- prop.table(color_table)
round(color_pct, digits = 1)
#visualizing the mileage and price data
boxplot(usedcars$price, main = "Boxplot of used car prices",
        ylab = "Price ($)")
boxplot(usedcars$mileage, main = "Boxplot of used car prices",
        ylab = "Odometer(Mi.)")

hist(usedcars$price, main = "Histogram of Used Car Prices",
     xlab = "Price ($)")
hist(usedcars$mileage, main = "Histogram of Used Car Prices",
     xlab = "Mileage ($)")
#plotting the variable relationships
#plotting scatter plot
plot(x = usedcars$mileage, y = usedcars$price,
     main = "Scatterplot of price vs mileage",
     xlab = "Used car odometer (mi.)",
     ylab = "USed car price ($)")
ggplot(usedcars, aes(x=usedcars$mileage,y = usedcars$price))+geom_line()
ggplot(usedcars, aes(x=usedcars$mileage,y = usedcars$price))+geom_point()

#Examinig rltionships two way cross tabulation
#cross table also known as contingency table
usedcars$conservative <- usedcars$color %in% c("Black","Gray","Silver","White")
usedcars$conservative
table(usedcars$conservative)
CrossTable(x = usedcars$model,y = usedcars$conservative)

#Reading winews table and determining the common statistical values
whitewines
str(whitewines)
summary(whitewines)
whitewines$fixed.acidity
whitewines$density
whitewines[,c("fixed.acidity","density","alcohol","quality")]
summary(whitewines$fixed.acidity)
summary(whitewines$alcohol)
#measures of central tendency(mean,median)
#measures of spread(sd,var)
table(whitewines$alcohol)
table(whitewines$density)
range(whitewines$alcohol)
range(whitewines$density)
diff(range(whitewines$alcohol))
diff(range(whitewines$density))
#interquantile range
quantile(whitewines$alcohol)
IQR(whitewines$alcohol)
quantile(whitewines$alcohol, probs = c(0.1,0.99))
quantile(whitewines$fixed.acidity, seq(from = 0.20, to = 0.8, by = 0.1))
boxplot(whitewines$quality, main = "Quality of wine", ylab = "Quality values")
ggplot(whitewines, aes(y = whitewines$quality, main = "Quality of wine")) + geom_boxplot()
#Distributionwhite
hist(whitewines$alcohol, main = "Alcohol Distribution", xlab = "Alcohol Values")
#Regression analysis
#create our model
whitewines.lm = lm(whitewines$quality~whitewines$alcohol, data = whitewines)
coefficients(whitewines.lm)
summary(whitewines.lm)
alcoholpre = data.frame(alcohol = 60)
predict(whitewines.lm, alcoholpre)

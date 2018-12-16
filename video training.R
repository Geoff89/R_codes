library(ggplot2)
x <- sample(x = 1:100, size =100, replace = TRUE)
x
mean(x)
summary(x)

x <- sample(x = 1:100, size =100, replace = FALSE) <- NA

#weigted mean
grades <- c(95,72,87,66)
weighted <- c(1/2, 1/4, 1/8, 1/8)
grades
weighted
weighted.mean(x = grades, w = weighted)
var(x)
std(x)
sqrt(var(x))
sd(x)
quantile(x)
quantile(x, probs = c(.1, .25,.50, .75,.9))
economics
head(economics)
 

#correlation
cor(economics$pce, economics$psavert)
cor(economics[,c(2,4:6)])

#visualize this data
econCor <- cor(economics[,c(2,4:6)])
econCor

pce    psavert    uempmed   unemploy
pce       1.0000000 -0.8370690  0.7273492  0.6139997
psavert  -0.8370690  1.0000000 -0.3874159 -0.3540073
uempmed   0.7273492 -0.3874159  1.0000000  0.8694063
unemploy  0.6139997 -0.3540073  0.8694063  1.0000000

require(reshape2)
econMelt <- melt(econCor, varnames = c("x","y"), value.name = "correlation")
econMelt
x        y correlation
1       pce      pce   1.0000000
2   psavert      pce  -0.8370690
3   uempmed      pce   0.7273492
4  unemploy      pce   0.6139997
5       pce  psavert  -0.8370690
6   psavert  psavert   1.0000000
7   uempmed  psavert  -0.3874159
8  unemploy  psavert  -0.3540073
9       pce  uempmed   0.7273492
10  psavert  uempmed  -0.3874159
11  uempmed  uempmed   1.0000000
12 unemploy  uempmed   0.8694063
13      pce unemploy   0.6139997
14  psavert unemploy  -0.3540073
15  uempmed unemploy   0.8694063
16 unemploy unemploy   1.0000000

#Order the correlation values
econMelt <- econMelt[order(econMelt$correlation),]
econMelt
x        y correlation
2   psavert      pce  -0.8370690
5       pce  psavert  -0.8370690
7   uempmed  psavert  -0.3874159
10  psavert  uempmed  -0.3874159
8  unemploy  psavert  -0.3540073
14  psavert unemploy  -0.3540073
4  unemploy      pce   0.6139997
13      pce unemploy   0.6139997
3   uempmed      pce   0.7273492
9       pce  uempmed   0.7273492
12 unemploy  uempmed   0.8694063
15  uempmed unemploy   0.8694063
1       pce      pce   1.0000000
6   psavert  psavert   1.0000000
11  uempmed  uempmed   1.0000000
16 unemploy unemploy   1.0000000

#Visualize the data
require(scales)
ggplot(econMelt, aes(x=x, y=y))+geom_tile(aes(fill=correlation))+
  scale_fill_gradient(low = muted("red"), mid = "white", high = "steeblue"),
  guides = guide_colorbar(ticks = FALSE, barheight = 10),
  theme_minimal() + labs(x = NULL, NULL)

#Dealing wit misssing data in R
m <- c(9,9,NA,3,NA,5,8,1,10,4)
n <- c(2,NA,1,6,6,4,1,1,6,7)
p <- c(8,4,3,9,10,NA,3,NA,9,9)
q <- c(10,10,7,8,4,2,8,5,5,2)
v <- c(1,9,7,6,5,6,2,7,9,10)
theMat <-cbind(m,n,p,q,v)
theMat
cor(theMat, use = "everything")
m  n  p          q          v
m  1 NA NA         NA         NA
n NA  1 NA         NA         NA
p NA NA  1         NA         NA
q NA NA NA  1.0000000 -0.4242958
v NA NA NA -0.4242958  1.0000000
cor(theMat, use = "complete.obs")
m          n          p          q          v
m  1.0000000 -0.5228840 -0.2893527  0.2974398 -0.3459470
n -0.5228840  1.0000000  0.8090195 -0.7448453  0.9350718
p -0.2893527  0.8090195  1.0000000 -0.3613720  0.6221470
q  0.2974398 -0.7448453 -0.3613720  1.0000000 -0.9059384
v -0.3459470  0.9350718  0.6221470 -0.9059384  1.0000000
cor(theMat, use = "na.or.complete")
m          n          p          q          v
m  1.0000000 -0.5228840 -0.2893527  0.2974398 -0.3459470
n -0.5228840  1.0000000  0.8090195 -0.7448453  0.9350718
p -0.2893527  0.8090195  1.0000000 -0.3613720  0.6221470
q  0.2974398 -0.7448453 -0.3613720  1.0000000 -0.9059384
v -0.3459470  0.9350718  0.6221470 -0.9059384  1.0000000

#Regression of the dax = 
head(faithful)
require(ggplot2)
ggplot(faithful, aes(x = eruptions, y = waiting))+geom_point()+
       geom_smooth(method = "lm")+labs(x = 'Eruptions', y = 'Waiting')

#Fitting this data into an M model
eruptions.lm <- lm(eruptions ~ waiting, data = faithful)
eruptions.lm
summary(eruptions.lm)

#Testing both anova and linear regression
eruptionsaov <- aov(eruptions~waiting-1,data = faithful)
eruptionsaov

#loading data
data()
iris <-read.csv(file = 'C:\\Users\\jeffnerd\\desktop\\IRIS.csv', header = TRUE, sep =",")
iris
iris[c(1,2,3,4,5,6,7,8,9),]
iris[2:3]
#Simple linear regression
head(faithful)
#create the model for simple linear regression
#two methods (1) Determine the coeffcients
eruption.lm = lm(eruptions~waiting, data = faithful)
eruption.lm
#Coefficients
coeff = coefficients(eruption.lm);coeff
(Intercept)     waiting 
-1.87401599  0.07562795 
#Now we fit the coefficents in the equation
waiting = 80
duration = coeff[1] + coeff[2]*80
duration
(Intercept) 
4.17622
#ALternatively you can predict the data
#waiting tie is 80 minutes the next eruption wil be
newdata = data.frame(waiting=80)
predict(eruption.lm, newdata )
4.17622

#Determining signifocance level
summary(eruption.lm)
#Interval estimatiom
predict(eruption.lm, newdata, interval = "confidence")
fit      lwr      upr
1 4.17622 4.104848 4.247592
predict(eruption.lm, newdata, interval = "predict")

#Residual plot
eruption.res = resid(eruption.lm)
eruption.res
#Residual is the diffrene between observed independent variable y and fitted values y

#Multiple regresssion
head(stackloss)
stackloss.lm = lm(stack.loss ~ Air.Flow + Water.Temp + Acid.Conc., data = stackloss)
stackloss.lm
coef = coefficients(stackloss.lm)
coef
coef[1]
coef[2]
coef[3]
coef[4]
newdata = data.frame(Air.Flow = 72, Water.Temp = 20, Acid.Conc. = 85)
newdata
predict(stackloss.lm, newdata )
summary(stackloss.lm)
#Logistic regression
head(mtcars)
am.glm = glm(formula = am ~ hp + wt , data = mtcars, family = binomial)
am.glm
#putting this data into a dataframe
newdata = data.frame(hp = 120, wt = 2.8)
predict(am.glm, newdata)



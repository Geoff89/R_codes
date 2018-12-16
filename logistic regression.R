data <- read.table("natal2010Sample.tsv.gz", sep="\t", header=T, stringsAsFactors=F)

# y variable: atRisk = APGAR5 < 7  - APGAR5
# 1.8%, excluding unknowns
data$atRisk = with(data, ifelse(APGAR5==99, NA,APGAR5 < 7))

# make a boolean from Y/N data
makevarYN = function(col) {
  ifelse(col %in% c("", "U"), NA, ifelse(col=="Y", T, F))
}

# make a numeric var w/NAs from numeric data
makevarNum = function(col, sentinel) {
  ifelse(col==sentinel, NA, col)
}

# make a boolean from 1/2/9 data.
makevar12 = function(col) {
  ifelse(col==9, NA, ifelse(col==1, T, F))
}

# tobacco use: CIG_REC (Y, N, U, Blank)
data$CIG_REC = makevarYN(data$CIG_REC)
# maternal prepregnancy weight (pounds), capped at 400lbs
data$PWGT = makevarNum(data$PWGT, 999)
# weight gain during pregnancy
data$WTGAIN = makevarNum(data$WTGAIN, 99)
# birth weight in grams
data$DBWT = makevarNum(data$DBWT, 9999)

# complications:
# meconium: mod/heavy staining of amniotic fluid with fetal fecal matter
# precipitous labor = really short (membrane ruptures, etc)
# breech birth
complications = c("ULD_MECO","ULD_PRECIP","ULD_BREECH")
data[, complications] = as.data.frame(lapply(data[, complications], FUN=makevar12))

# obstetric procedures:
# tocolysis -- anti-contraction medication given to prevent premature labor
# induc -- labor was induced
obsproc = c("UOP_TOCOL", "UOP_INDUC")
data[, obsproc] = as.data.frame(lapply(data[, obsproc], FUN=makevar12))

# number of prenatal visits
data$UPREVIS = makevarNum(data$UPREVIS, 99)

#risk factors (1,2,9,Blank)
# diabetes, chronic hypertension, pregnancy-associated hypertension, eclampsia
riskfactors = c("URF_DIAB", "URF_CHYPER", "URF_PHYPER", "URF_ECLAM")
data[, riskfactors] = as.data.frame(lapply(data[, riskfactors], FUN=makevar12))

# reset the "default" level on categorical variabls
recode = function(col, map, ref) {
  relevel(as.factor(map[col]), ref=ref)
}

# gestation length
# GESTREC3 (1,2,3 -- <37weeks(premie), >=37wks, NA)
grmap = c("< 37 weeks",
          ">= 37 weeks",
          NA)
data$GESTREC3 = recode(data$GESTREC3, grmap, grmap[[2]])

# DPLURAL : birth plurality
plmap = c("single",
          "twin",
          "triplet or higher",
          "triplet or higher",
          "triplet or higher")
data$DPLURAL = recode(data$DPLURAL, plmap, "single")

# Select variables we will use for the analysis
y = "atRisk"
x = c("PWGT", 
      "UPREVIS", 
      "CIG_REC",
      "GESTREC3", 
      "DPLURAL",
      complications,
      riskfactors)
fmla = paste(y, paste(x, collapse="+"), sep="~")

sdata = data[, c(x, y, c("DBWT", "ORIGRANDGROUP"))]

# get rid of the NA data before splitting into train and test
# noNAs is T if there are no NAs in the row
noNAs = rowSums(as.data.frame(lapply(sdata, FUN=is.na))) == 0
sdata = sdata[noNAs, ]

save(sdata, file="NatalRiskData.rData")

paste(x, paste(1:3, collapese="+"), sep = "~")
View(mtcars)

#creating logistic regression for mtcars
am.glm = glm(formula = am~hp+wt, data = mtcars,  family = binomial(link = "logit"))
am.glm
summary(am.glm)
newdata = data.frame(hp = 120, wt = 2.8)

#predict
predict(am.glm, newdata, type = "response")
help(summary.glm)


#regression  model 2
library(ISLR)
library(Amelia)
ISLR::Smarket
View(Smarket)
names(Smarket)
head(Smarket)
summary(Smarket)
 #visualizing the data
par(mfrow = c(1,8))
for(i in 1:8){
  hist(Smarket[,i], main = names(Smarket)[i])
}
#using box and whiskers
par(mfrow = c(1,8))
for(i in 1:8){
  boxplot(Smarket[,i], main = names(Smarket)[i])
}
#There is no sign of outliers in the above
#view amount of missing data as it has a big impact on modelling
library(mlbench)
missmap(Smarket, col = c("blue", "red"), legend = FALSE)
#looks like there is no missing data
#Plotting correlation of this data
library(corrplot)
correlations <- cor(Smarket[,1:8])
correlations
corrplot(correlations)

library(psych)
pairs(Smarket, col = Smarket$Direction)
pairs.panels(Smarket)

library(caret)
x <- Smarket[,1:8]
y <- Smarket[,9]
#we want to view the denstity distributions
scales <- list(x = list(relation = "free"), y = list(relation = "free"))
featurePlot(x=x, y=y, plot = "density", scales = scales)


#fitting the logistic regression model
#creating the training and test data sets
train <- createDataPartition(Smarket$Direction, p = .75, list = FALSE)
train <-Smarket$Year<2005
train <- sample(1250,900)
train_data <- Smarket[train,]
test_data <- Smarket[-train,]
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket,
               family = binomial,
               subset = train)
summary(glm.fit)
glm.prob <- predict(glm.fit, newdata = Smarket[-train], type = "response")
glm.prob
glm.pred <- ifelse(glm.prob > 0.5, "UP", "Down")
glm.pred
#Creating the validation dataset
Direction.2005 <- Smarket$Direction[-train]
dim(Direction.2005)
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

dim(train)

#reading the data
usedcars <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\usedcars.csv", header = TRUE, sep =",",stringsAsFactors = FALSE )
wbcd <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\wisc_bc_data.csv", header = TRUE, sep =",",stringsAsFactors = FALSE )
wbcd
str(wbcd)
class(wbcd)

#Excluding row 1 which is irrelevant to our model to avoid overfiiting
wbcd <- wbcd[-1]
wbcd
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), 
                         labels = c("Benign","Malignant"))
wbcd$diagnosis
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)
#View the variables we will work with
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
diff(range(wbcd$radius_mean))
diff(range(wbcd$area_mean))
diff(range(wbcd$smoothness_mean))
#The distance calculation of area_mean is huge and therefore will dwarf the rest of
#the features.So we need to normalize
#we create a fuucntion for normalization.we rescale the features to standardized
#range values
normalize <- function(x){
  return((x - min(x))/(max(x)-min(x)))
}
normalize(c(1,2,3,4,5))#-0.2  0.8  1.8  2.8  3.8
normalize(c(10,20,30,40,50))#-0.2  9.8 19.8 29.8 39.8

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
wbcd_n
summary(wbcd_n$area_mean)

#Dataset preparation for training and testing datasets
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]
#we provide the factor vectors for training for training and test forour K-NN model
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]
#Trainig a model on the test data
library(class)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl=wbcd_train_labels, k = 21)
wbcd_test_pred#Gives the predicted values

#Next is checking the accuracy of our prediction
library(gmodels)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)
#we have 2FN meaning 2% was not predicted acccurately thus 98% was predicted accurately
#try normalizing with z standardizatio ad other K neighbors values
wbcd_z <- as.data.frame(scale(wbcd[-1]))
wbcd_z
summary(wbcd_z$area_mean)
#splt the data into training and test data sets
wbcd_train <- wbcd_z[1:469,]
wbcd_test <- wbcd_z[470:569,]
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]
#create our knn model function
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, 
                      cl = wbcd_train_labels, k=21)
wbcd_test_pred
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

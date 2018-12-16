#support vector machines

letters <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\letterdata.csv", header = TRUE, sep =",",stringsAsFactors = TRUE )
str(letters)

#since all the data performed by ksvm is scaled automatically we dont need
#to normalize or scale our data

#splitting the data into training and test data
letters_train = letters[1:16000,]
letters_test = letters[16001:20000,]

#training the model
library(kernlab)

letter_classifier <- ksvm(letter ~., data = letters_train, kernel = "vanilladot")
letter_classifier
#using polynomial kernel
letter_classifier1 <- ksvm(letter ~., data = letters_train, kernel = "polydot")
letter_classifier1
#using sigmoid kernel
letter_classifier2 <- ksvm(letter ~., data = letters_train, kernel = "tanhdot")
letter_classifier2
#using gaussian r3bdf kernel
letter_classifier3 <- ksvm(letter ~., data = letters_train, kernel = "rbfdot")
letter_classifier3

#putting our model to unseen data
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)

#looking at how well our modl predicted the letters
table(letter_predictions, letters_test$letter)
#to check the accuracy of the data we simplify our model
agreement <- letter_predictions == letters_train$letter
table(agreement)
prop.table(table(agreement))

#to improve our model we use a complex kernel to make our data more high dimension
letter_predict_rbf <- predict(letter_classifier3, letters_test)
head(letter_predict_rbf)
#perfomance of the model
table(letter_predict_rbf, letters_test$letter)

#seeing the perfomance of this model
agreement1 <- letter_predict_rbf == letters_test$letter
table(agreement1)
prop.table(table(agreement1))

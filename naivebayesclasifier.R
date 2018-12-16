library(tm)#for nlp
library(SnowballC)#for nlp
library(wordcloud)#for nlp
library(e1071)

#reading the data
sms_raw<- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\sms_spam.csv",stringsAsFactors = FALSE )
sms_raw
str(sms_raw)
#being a categorical data we put type character to a vector
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
#we need to create a corpus
install.packages("installr")
library(installr)
updateR()
install.packages("pacman")
pacman::p_load(tm)

#create a corpus
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
#tm is a complex list we can use list operations to get individual documents
inspect(sms_corpus[1:2])#the first two elements
#to display them as character use as character fucntion
as.character(sms_corpus[[1]])
#to display the first two document lists
lapply(sms_corpus[1:2], as.character)
#standardization of the corpus
sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
sms_corpus_clean
#to see the transfrmations
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
#second clean up process
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
getTransformations()
#third clea up is to remove stopwords
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
stopwords()#enabls you to see stop words
#fourth clean up is to remove punctuations
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
#fifth clean p standardization is known as stemming
library(SnowballC)
wordStem(c("Learner", "Learned", "Learning"))
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
#stemdocument is aplied to entire corpus to bring their root form
#final step of clean up is to remove additional_whitespaces
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
#comparing now sms_corpus dcument with sms_corpus_clean
lapply(sms_corpus[1:3], as.character)
lapply(sms_corpus_clean[1:3], as.character)
#Next process is tokenization.converting text documents into words. create a token
#Documenttermmatrix() function takes a corpus to tokenize
#Document(sms messages) is rows and term which is words is columns
#Alternative TermDocumentmatrix
sms_dtm <-DocumentTermMatrix(sms_corpus_clean)
sms_dtm #tokenized corpus

#preprocessing automatically
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
  tolower = TRUE,
  removeNumbers = TRUE,
  stopwords = TRUE,
  removePunctuation = TRUE,
  stemming = TRUE
))
#splitting the data into traiing and testing data sets
sms_dtm_train <- sms_dtm[1:4180,]
sms_dtm_test <- sms_dtm[4181:5574,]
#putting the labels
sms_train_labels <- sms_raw[1:4180,]$type
sms_test_labels <- sms_raw[4181:5574,]$type
#to check the accuracy
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

library(wordcloud)
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
#now creating subsets of wordcloud
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

#Now applying word cloud
wordcloud(spam$text, max.words = 40, scale = c(3,0.5))
wordcloud(ham$text, max.words = 40, scale =c(3,0.5))
#now we find the most frequent terms
sms_freq_words <- findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)
#now our columns should contain frequent terms and all rows of training and testing
sms_dtm_freq_train <- sms_dtm_train[,sms_freq_words]
sms_dtm_fre_test <- sms_dtm_test[,sms_freq_words]
#the above split data now have 1162 features
#convert the numerical data in the dtm matrix to categorical data
convert_counts <- function(x){
  x <- ifelse(x>0, "yes", "no")
}
#apply this to the dtm
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_fre_test, MARGIN = 2, convert_counts)
sms_train
library(e1071)
#creating our naive bayes classifier
sms_classifier <- naiveBayes(sms_train, sms_train_labels)
sms_classifier
sms_test_pred <- predict(sms_classifier, sms_test)
sms_test_pred
library(gmodels)
CrossTable(sms_test_pred, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
#using a table
prop.table(table(sms_test_labels, sms_test_pred))
mean(sms_test_labels == sms_test_pred)
mean(sms_test_labels != sms_test_pred)
#improving the model perfomace
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
                              laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_test_labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

quantile(c(10,20,30))
quantile(50, probs = seq(from=0.1,to=0.5, by=0.5))

sms_test_pred1 <-predict(sms_classifier,sms_test,type="raw")
sms_test_pred1
sms_test_pred1[1]

#confusion matrix
table(sms_test_labels,sms_test_pred)
#another option
CrossTable(sms_test_labels,sms_test_pred)

#using caret package to use the confusioon matrix
library(caret)
confusionMatrix(sms_test_pred,sms_test_labels, positive = "spam")
#computing the kappa value
agreement = sms_test_labels==sms_test_pred
table(agreement)
prop.table(table(agreement))

#kappa manually
pr_a <- 0.863 + 0.116
pr_a
pr_b = 0.869*0.877+0.131*0.123
pr_b
k <- (pr_a - pr_b)/(1 - pr_b)
k
#using vcd to compute kappa automatically
library(vcd)
Kappa(table(sms_test_labels,sms_test_pred))

#sensitic]vity and specificity
sens <-162 /(162+20)
sens#0.8901
#specificity -true negative values
spec <- 1203 / (1203 +9 )
spec#0.9926

sensitivity(sms_test_labels, sms_test_pred, positive = "spam")
specificity(sms_test_labels, sms_test_pred, positive = "spam")
#precision 
prec <- 162/(162+9)#0.94
prec
posPredValue(sms_test_labels, sms_test_pred, positive = "spam")

#recall
rec <- 162/(162+20)
rec
recall(sms_test_labels, sms_test_pred, positive = "spam")
sensitivity(sms_test_labels, sms_test_pred, positive = "spam")

#computing F-measure/F1 score or F-score
f_measure = (2 * prec * rec)/(prec + rec)
f_measure#0.9178

#
library(ROCR)
#true labels/actual labels
sms_test_labels
#put the predicted vakues as a dataframe to get predicted probabilities as
#columns of spam and ham probabilities
sms_test_pred1 <- as.data.frame(sms_test_pred1)
sms_test_pred1
head(sms_test_pred1)
sms_test_pred1$ham
sms_test_pred1$spam
#prediction function of
pred <- prediction(predictions = sms_test_pred1$spam, labels = sms_test_labels)
#create ROC curve using perfomance
perf <-performance(pred, measure = "tpr", x.measure = "fpr")
#now plotting the curve to see how it looks
plot(perf,main = "ROC curve for sms spam filter" ,col = "blue", lwd = 3)
#we also need to plot classifier with no predictive value
abline(a=0, b=1, lwd=2, lty=2)
#to measure area under the crurve(auc)of roc curve
perf.auc <- performance(pred, measure = "auc")
perf.auc
#to access in s4 form slots we use str
str(perf.auc)
unlist(perf.auc@y.values)

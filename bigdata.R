sdata <- load("~/NatalRiskData.rData")

train <- sdata[sdata$ORIGRANDGROUP<=5,]
test <- sdata[sdata$ORIGRANDGROUP>5,]

bigdata <- read.csv(file ="C:\\Users\\jeffnerd\\Desktop\\jeff 2016 targets\\big data and spark\\big lines of data\\1500000SalesRecords.csv" , 
                    header = TRUE , sep = ",", stringsAsFactors = FALSE)
summary(bigdata)
library(Amelia)

missmap(bigdata, col = c("blue", "red"), legend = FALSE)

par(mfrow = c(9,14))
for(i in 9:14){
  hist(bigdata[,i], main = names(bigdata[i]))
}

library(psych)
pairs.panels(bigdata[,9:14])

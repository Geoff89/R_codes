

#clustering algorithmteens
teens <- read.csv(file ="C:\\users\\jeffnerd\\Desktop\\snsdata.csv", header = TRUE,
                  sep = ",", stringsAsFactors = TRUE)
str(teens)
dim(teens)
#there is some missing data in both age(numerical) and gender(categorical)
table(teens$gender)
table(teens$gender, useNA = "ifany")

#summary is used for numerical data
summary(teens$age)
#adjust the data for age in high school
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)
#unfortuately we have created an even bigger daa problem with missing data

#data preparation for categorical data using  dummy coding
#creating dummy variable for female and unknown gender 
teens$gender <- ifelse(teens$gender == "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)
#confirm hether our data works well
table(teens$gender, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")

#data preparation for imputing missing values in numerical data
mean(teens$age)
mean(teens$age, na.rm = TRUE)
#actualy we need the average graduation for the four years
aggregate(data = teens, age~gradyear, mean, na.rm = TRUE)
#ave function can be used as an alternative as it can be used to merge the
#originaal data
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x)mean(x, na.rm = TRUE))
ave_age
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)

#training our model
interests <- teens[5:40]
#apply z_core standardizatio
interests_z <- as.data.frame(lapply(interests, scale))
#from there we apply k-means but set seed before applying k-means
set.seed(2345)
#train our model
teen_clusters <- kmeans(interests_z, 5)
#evaluate our moel perfomance
teen_clusters$size
#examining the cluster further
teen_clusters$centers
teen_clusters$cluster
#assigning clusters back to the original data
teens$cluster <- teen_clusters$cluster
#getting the first five teens data in sns data
teens[1:5, c("cluster", "gender", "age", "friends")]

#looking at the demographic age of the cluters
aggregate(data = teens, age~cluster, mean)
aggregate(data = teens, female~cluster, mean)
aggregate(data = teens, friends~cluster, mean)
plot(teens, col(teen_clusters$cluster+1))

#prcomp principal compent analysis
states <- row.names(USArrests)
states
rownames(USArrests)
names(USArrests)
colnames(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
lapply(USArrests, mean)

pr.out <- prcomp(USArrests, scale = TRUE)
names(pr.out)
pr.out$center #mean prior to scaling
pr.out$scale #sd prior to scaling
#rotation matrix provides the principal componets loadings
pr.out$rotation
dim(pr.out$x)
pr.out$x
#plottting the first two pca
biplot(pr.out, scale = 0)
#uniquenesss of principal components
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0)
#standard deviation
pr.out$sdev
#variance of the principal components is compute as follows
pr.var = pr.out$sdev^2
pr.var
#comouting the proportion of the variance 
pve = pr.var/sum(pr.var)
pve
#plotting the variance explained
plot(pve , xlab=" Principal Component ", ylab=" Proportion of
 Variance Explained ", ylim=c(0,1), type='b')
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b')

#clustering
set.seed (2)
x=matrix (rnorm (50*2) , ncol =2)
x[1:25 ,1]=x[1:25 ,1]+3
x[1:25 ,2]=x[1:25 ,2] -4

km.out = kmeans(x, 2, nstart = 20)
km.out$centers
km.out$cluster
#plotting the clusters with unique colors
plot(x, col =(km.out$cluster +1) , main="K-Means Clustering
Results with K=2", xlab ="", ylab="", pch =20, cex =2)


set.seed (4)
km.out =kmeans (x,3, nstart =20)
km.out

set.seed (3)
km.out =kmeans (x,3, nstart =1)
km.out$tot.withinss

km.out =kmeans (x,3, nstart =20)
km.out$tot.withinss

#hclusters
hc.complete = hclust(dist(x), method = "complete")
hc.average = hclust(dist(x), method = "average")
hc.single = hclust(dist(x), method = "single")
hc.centroid = hclust(dist(x,method = "manhattan"), method = "centroid")
#plotting the dendrograms
par(mfrow = c(1,3))
plot(hc.complete, main = "Complete Linkage", xlab = "", sub = "", cex = .9)
plot(hc.average, main = "average Linkage", xlab = "", sub = "", cex = .9)
plot(hc.single, main = "single Linkage", xlab = "", sub = "", cex = .9)
plot(hc.centroid, main = "centroid Linkage", xlab = "", sub = "", cex = .9)

#to see the labels of the clusters
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.centroid, 2)

#drawing rectangles of the dendrogram
rect.hclust(hc.complete, k=3)
rect.hclust(hc.centroid, k =3)
rect.hclust(hc.average, k=3)
rect.hclust(hc.single, k=3)
#for the single cluster it doesnt describe well increase number of clusters
cutree(hc.single, 4)
#we can also scale numeric variables in clusters
xsc = scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "hierachical clustering with
     scaled features")

#correlation based distance
x = matrix(rnorm(30*3), ncol = 3)
dd = as.dist(1 - cor(t(x)))
xsc = scale(x)
plot(hclust(dd, method = "complete"), main = "complete linkage with correlation
     based distance", xlab = "", sub ="")

x=matrix (rnorm (30*3) , ncol =3)
dd=as.dist(1- cor(t(x)))
plot(hclust (dd, method="complete"), main="Complete Linkage
       with Correlation -Based Distance", xlab="", sub ="")

library(ISLR)
#cancer cells analyzing
nci.labs = NCI60$labs
nci.data = NCI60$data
dim(nci.data)

nci.labs[1:4]
table(nci.labs)
prop.table(table(nci.labs))
#perfoming PCA
pr.out =prcomp(nci.data, scale=TRUE)

Cols=function (vec ){
   cols=rainbow (length (unique (vec )))
   return (cols[as.numeric (as.factor (vec))])
}

par(mfrow =c(1,2))
plot(pr.out$x [,1:2], col =Cols(nci.labs), pch =19,
       xlab ="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3) ], col =Cols(nci.labs), pch =19,
       xlab ="Z1",ylab="Z3")

summary(pr.out)
plot(pr.out)

pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve, type ="o", ylab="PVE ", xlab="Principal Component",
       col ="blue")
plot(cumsum(pve), type="o", ylab ="Cumulative PVE", xlab="
       Principal Component", col ="brown3")

sd.data=scale(nci.data)

par(mfrow =c(1,3))
data.dist=dist(sd.data)
plot(hclust (data.dist), labels =nci.labs , main=" Complete
       Linkage ", xlab ="", sub ="", ylab ="")
plot(hclust (data.dist , method ="average"), labels =nci.labs ,
       main="Average Linkage", xlab ="", sub ="", ylab ="")
plot(hclust (data.dist , method ="single"), labels =nci.labs ,
       main="Single Linkage", xlab="", sub ="", ylab ="")

hc.out =hclust (dist(sd.data))
hc.clusters =cutree(hc.out, 4)
table(hc.clusters,nci.labs)

par(mfrow =c(1,1))
plot(hc.out , labels=nci.labs)
abline (h=139, col ="red")

hc.out

set.seed (2)
km.out =kmeans(sd.data , 4, nstart =20)
km.clusters =km.out$cluster
km.out$tot.withinss
km.out$withinss
km.out$centers
table(km.clusters, hc.clusters)
table(km.clusters, nci.labs)

hc.out =hclust(dist(pr.out$x[ ,1:5]))
plot(hc.out , labels =nci.labs , main=" Hier. Clust . on First
       Five Score Vectors ")


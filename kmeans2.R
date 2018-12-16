protein <- read.table(file ="C:\\Users\\jeffnerd\\Desktop\\protein.txt", 
                      sep = "\t", header = TRUE)
summary(protein)
View(protein)
summary(protein$Eggs)

library(Amelia)
missmap(protein, col = c("blue", "red"), legend = FALSE)

library(corrplot)
y <- cor(protein[,2:10])
y
corrplot(y, method = "square",type = "upper")
pairs(y)

library(psych)
pairs.panels(y)

#doing clustering to see how each countries consume proteins in europe
vars.to.use <- colnames(protein)[-1]
vars.to.use
#scaling the above data
pmatrix <- scale(protein[,vars.to.use])
summary(pmatrix)
pcenter <- attr(pmatrix, "scaled:center") #display mean of dataframe
pcenter
pscale <- attr(pmatrix, "scaled:scale") #display standard deviation
pscale

#Hierachical clustering
d <- dist(pmatrix, method = "euclidean") # create a distance matrix
pfit <- hclust(d, method = "ward.D")  #do clustering
plot(pfit, labels = protein$Country)
#model suggests 5 clusters
#drawing recatngles on the dendrogram
rect.hclust(pfit, 5)
#extracting members of each class
groups <- cutree(pfit, 5)
groups

print_clusters <- function(labels, k){
  for(i in 1:k){
    print(paste("cluster", i))
    print(protein[labels==i, c("Country","RedMeat","Fish","Fr.Veg")])
  }
}
print_clusters(groups, 5)

#visualizing this data into PCA
library(ggplot2)
princ <- prcomp(pmatrix) #calculate the principal compnents
princ
ncomp <- 2
project <-predict(princ, newdata = pmatrix)[,1:ncomp]
project
#we porject the data into two dimensional space
project.plus <- cbind(as.data.frame(project),
                      cluster = as.factor(groups),
                      country = protein$Country)
project.plus

#plotting data
ggplot(project.plus, aes(x=PC1, y=PC2)) +
  geom_point(aes(shape = cluster)) +
  geom_text(aes(label=country), hjust=0, vjust=1)

#cluster boot to evaluate the clusters
library(fpc)
#sett the desired number of clusters
kbest.p <- 5

cboot.hclust <- clusterboot(pmatrix,clustermethod = hclustCBI, method = "ward.D",
                            k = kbest.p)
#to get the results of the cluster
summary(cboot.hclust$result)
#we get the labels
groups <- cboot.hclust$result$partition
print_clusters(groups, kbest.p) #gives us the same clusters as hclust
#vector of class stabilities i.e jaccard coefficient
cboot.hclust$bootmean
#count of how much each cluster dissove
cboot.hclust$bootbrd

for (i in 1:5){
  print(paste("hello", i))
  print("Sagini")
}

#bversion 2
sqr_edist <- function(x, y) {
  sum((x-y)^2)
}

wss.cluster <- function(clustermat) {
  c0 <- apply(clustermat, 2, FUN=mean)
  sum(apply(clustermat, 1, FUN=function(row){sqr_edist(row,c0)}))
}

wss.total <- function(dmatrix, labels) {
  wsstot <- 0
  k <- length(unique(labels))
  for(i in 1:k)
    wsstot <- wsstot + wss.cluster(subset(dmatrix, labels==i))
  wsstot
}

totss <- function(dmatrix) {
  grandmean <- apply(dmatrix, 2, FUN=mean)
  sum(apply(dmatrix, 1, FUN=function(row){sqr_edist(row, grandmean)}))
}

ch_criterion <- function(dmatrix, kmax, method="kmeans") {
  if(!(method %in% c("kmeans", "hclust"))) {
    stop("method must be one of c('kmeans', 'hclust')")
  }
  npts <- dim(dmatrix)[1] # number of rows.
  totss <- totss(dmatrix)
  wss <- numeric(kmax)
  crit <- numeric(kmax)
  wss[1] <- (npts-1)*sum(apply(dmatrix, 2, var))
  for(k in 2:kmax) {
    if(method=="kmeans") {
      clustering<-kmeans(dmatrix, k, nstart=10, iter.max=100)
      wss[k] <- clustering$tot.withinss
    }else { # hclust
      d <- dist(dmatrix, method="euclidean")
      pfit <- hclust(d, method="ward")
      labels <- cutree(pfit, k=k)
      wss[k] <- wss.total(dmatrix, labels)
    }
  }
  bss <- totss - wss
  crit.num <- bss/(0:(kmax-1))
  crit.denom <- wss/(npts - 1:kmax)
  list(crit = crit.num/crit.denom, wss = wss, totss = totss)
}

library(reshape2)
clustcrit <- ch_criterion(pmatrix, 10, method="hclust")
critframe <- data.frame(k=1:10, ch=scale(clustcrit$crit),
                        wss=scale(clustcrit$wss))

critframe <- melt(critframe, id.vars=c("k"),
                  variable.name="measure",
                  value.name="score")

ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10) +
  theme_bw() +
  theme(axis.text.x = element_text(face = "italic", colour = "blue"))

#perfoming kmeans cluistering
pclusters <- kmeans(pmatrix, kbest.p, nstart = 100, iter.max = 100)
summary(pclusters)
pclusters$centers
groups <- pclusters$cluster #cluster labels
print_clusters(groups, kbest.p)
pclusters$totss #216
pclusters$withinss#8.012133 16.994661 18.925874  5.900318 22.110431
pclusters$tot.withinss#71
pclusters$betweenss#144.0566
pclusters$size#4 5 4 4 8
pclusters$iter#2

#kmeansrun function for picking k
library(fpc)
#calinski-habass index
clustering.ch <- kmeansruns(pmatrix, krange = 1:10, criterion = "ch")
clustering.ch$bestk #states 2 clusters are the best
#average silhoutte width
clustering.asw <- kmeansruns(pmatrix, krange = 1:10, criterion = "asw")
clustering.asw$bestk #states the best k to use is 3
clustering.ch$crit # for kmeans "ch index
clustcrit$crit #for our hcluste means"ch index. Not the same as they did not pick same clusters
#plotting the results
critframe <- data.frame(k=1:10, ch = scale(clustering.ch$crit),
                        asw = scale(clustering.asw$crit))

library(reshape2)
library(ggplot2)
critframe <- melt(critframe, id.vars = c("k"),
                  variable.name = "measure",
                  value.name = "score")

#plot this dataframe
ggplot(critframe, aes(x=k, y=score, colour = measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure))
summary(clustering.asw)
summary(clustering.ch)

#using clusterboot
kbest.p <-5
cboot <- clusterboot(pmatrix, clustermethod = kmeansCBI, runs = 100, 
                     iter.max = 100, krange = kbest.p, seed = 15555)

groups <- cboot$result$partition
groups
print_clusters(groups, kbest.p)
cboot$bootmean
cboot$bootbrd

#assigning new data points to cluster
#clustering is often used in data exploration or as precusor to suprvised leaning
assign_cluster <- function(newpt, centers, xcenter=0, xscale=1){
  xpt <- (newpt-xcenter)/xscale #scale new data points
  dists <- apply(centers, 1, FUN = function(c0){sqr_edist(c0,xpt)})
  which.min(dists)
}

#below is  a function to generate n points from Gaussian distribution 
#with centroid mean and standard deviation.Dimension of the distribution is given
#the lenght of the vector mean
rnorm.multidim <- function(n, mean, sd, colstr="x"){
  ndim <- length(mean)
  data <- NULL
  for(i in 1:ndim){
    col <- rnorm(n, mean=mean[[i]], sd = sd[[i]])
    data <- cbind(data,col)
  }
  cnames <- paste(colstr, 1:ndim, sep="")
  colnames(data) <-cnames
  data
}

mean1 <- c(1,1,1)#the gaussian from the three gaussian distributions
sd1 <- c(1,2,1)

mean2 <- c(10,-3,5)
sd2 <- c(2,1,2)

mean3 <- c(-5,-5,-5)
sd3 <- c(1.5,2,1)


clust1 <- rnorm.multidim(100, mean1, sd1)#create a dataset with 100 points
clust2 <- rnorm.multidim(100, mean2, sd2)#each drwan from the above distribution
clust3 <- rnorm.multidim(100, mean3, sd3)
toydata <- rbind(clust3, rbind(clust1,clust2))
toydata

tmatrix <- scale(toydata)
tcenter <- attr(tmatrix, "scaled:center")
tscale <- attr(tmatrix, "scaled:scale")

kbest.t <- 3
tclusters <- kmeans(tmatrix, kbest.t, nstart=100, iter.max=100)
summary(tclusters)

tclusters$size
#a function to unscale the data back to their original points
unscale <- function(scaledpt, centervec, scaledvec){
  scaledpt * scaledvec + centervec
}

unscale(tclusters$centers[1,], tcenter, tscale)#unscale the first centroid.mean2
unscale(tclusters$centers[2,], tcenter, tscale)#unscale the second cemtroid.mean
unscale(tclusters$centers[3,], tcenter, tscale)#unscale the third centroid.mean1

#generate a reandom  point from original distributions 1 and assign
assign_cluster(rnorm.multidim(1, mean1, sd1),
               tclusters$centers,
               tcenter, tscale)
assign_cluster(rnorm.multidim(1, mean2, sd1),
               tclusters$centers,
               tcenter, tscale)
assign_cluster(rnorm.multidim(1, mean3, sd1),
               tclusters$centers,
               tcenter, tscale)

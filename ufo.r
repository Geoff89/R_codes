 
ufo <- read.delim("C:\\Users\\jeffnerd\\Desktop\\ufo_awesome.tsv", sep = "\t", 
                  header = FALSE, na.strings = "", stringsAsFactors = FALSE )
head(ufo)
#providing meaningful descritpion to our data
names(ufo) <- c("DateOccurred", "DateReported", "Location", "ShortDescription",
                "Duration", "LongDescription")
head(ufo)
#converting date strings to date objects in r
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format = "%Y%m%d")
ufo$DateOccurred
str(ufo)
#cleansing and trasforming this data
head(ufo[which(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateReported)!=8),1])
good.rows <- ifelse(nchar(ufo$DateOccurred)> !=8 | nchar(ufo$DateReported)!=8, FALSE,
                    TRUE)
length(which(!good.rows))
ufo<-ufo[good.rows,]
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format = "%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format = "%Y%m%d")

#organizing and location
get.location <- function(i){
  split.location <- tryCatch(strsplit(i, ",")[[i]], error = function(e) return(c(NA,NA)))
  clean.location<- gsub("^ ", "", split.location)
  if(length(clean.location)>2) {
    return(c(NA,NA))
  } 
  else {
    return(clean.location)
  }
}

city.state <- lapply(ufo$Location, get.location)
city.state
head(city.state)

location.matrix<-do.call(rbind, city.state)
ufo<-transform(ufo, USCity=location.matrix[,1], USState=tolower(location.matrix[,2]),
               stringsAsFactors=FALSE)

us.states<-c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il",
             "in","ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh",
             "nj","nm","nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt",
             "wa","wi","wv","wy")
ufo$USState<-us.states[match(ufo$USState,us.states)]
ufo$USCity[is.na(ufo$USState)]<-NA

data.file <- file.path("C:\\Users\\jeffnerd\\Desktop\\mlhackers\\heightsweightsgenders.csv")
heights.weights <- read.csv(file = data.file, header = TRUE, sep = ",")
heights.weights <- read.csv(data.file, header = TRUE, sep = ",")
heights.weights
head(heights.weights)
heights <- with(heights.weights, Height)
summary(heights)

#computing the height and means
my.mean <- function(x){
  return(sum(x)/length(x))
}
my.mean(c(1,2,3,4,5,6,7,8,9,10))

#computing the median
my.median <- function(x){
  sorted.x <- sort(x)
  
  if(length(x) %% 2 == 0)
  {
    indices <-c(length(x)/2, length(x)/2+1)
    return(mean(sorted.x[indices]))
  }
  else{
    index <- ceiling(length(x)/2)
    return(sorted.x[index])
  }
}
x <- 1:10
my.median(x)
my.mean(heights)
mean(heights) - my.mean(heights)
median(heights) -my.median(heights)

#Quantiles
min(heights)
max(heights)
c(min(heights), max(heights))

quantile(heights)
#getting other values
quantile(heights, probs = seq(0,1,by=0.2 ))
seq(0, 1, by = 0.20)
#standard deviation and variance-measures spread
c(quantile(heights, probs = 0.25), quantile(heights, probs = 0.75))
var(heights)
sd(heights)
sqrt(var(heights))
#using a function
my.var <- function(x){
  m <- mean(x)
  return(sum((x-m)^2)/length(x))
}
my.var(heights)
my.var(heights) - var(heights)

my.sd <- function(x) {
  return(sqrt(my.var(x)))
}
my.sd(heights)
c(mean(heights) - sd(heights), mean(heights) + sd(heights))
range(heights)
#plotting the heights of this data
library(ggplot2)
ggplot(heights.weights, aes(x=heights)) + geom_histogram(binwidth = 1)
ggplot(heights.weights, aes(x=heights)) + geom_histogram(binwidth = 5)
ggplot(heights.weights, aes(x=heights)) + geom_histogram(binwidth = 0.001)
#solving this problem of oversmoothing and undersmoothing use geom_density
ggplot(heights.weights, aes(x=heights)) + geom_density()
ggplot(heights.weights, aes(x=heights, fill = Gender)) + geom_density()
ggplot(heights.weights, aes(x=heights, fill = Gender)) + geom_density()+
  facet_grid(Gender~.)
m <- 0
s <- 1
ggplot(data.frame(X = rnorm(100000, m, s)), aes(x=X)) + geom_density() 

set.seed(1)
normal.values <- rnorm(250, 0, 1)
cauchy.values <- rcauchy(250, 0, 1)

range(normal.values)
range(cauchy.values)

#plotting the values
ggplot(data.frame(X=normal.values),  aes(x=X)) + geom_density()
ggplot(data.frame(X=cauchy.values),  aes(x=X)) + geom_density()

#skewedd distribution
gamma.values <- rgamma(10000, 1, 0.001)
ggplot(data.frame(X=gamma.values),  aes(x=X)) + geom_density()

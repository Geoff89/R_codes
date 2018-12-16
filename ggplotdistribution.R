faithful
head(faithful)
library(ggplot2)
ggplot(faithful, aes(x=waiting)) + geom_histogram()

#store values in a simple vector
w <- faithful$waiting
ggplot(NULL, aes(x=w)) + geom_histogram()

#changing binwidth for 5
ggplot(faithful,  aes(x=waiting)) + 
  geom_histogram(binwidth = 5, fill = "white", colour = "black")
#divide range of x into 15 bins
binsize <- diff(range(faithful$waiting))/15
ggplot(faithful,  aes(x=waiting)) + 
  geom_histogram(binwidth = binsize, fill = "white", colour = "black")

#using origin to shift boundaries
h <- ggplot(faithful, aes(x=waiting)) #base data
h + geom_histogram(binwidth = 8, fill = "white", colour = "black", boundary = 30)

#making multiple histograms from grouped data
library(MASS)
library(plyr)
View(birthwt)
is.factor(birthwt$smoke) #FALSE

ggplot(birthwt,  aes(x=bwt)) + geom_histogram(fill = "white", colour = "black") +
  facet_grid(smoke ~.)

birthwt1 <- birthwt #make a copy of the above data
birthwt1$smoke <- factor(birthwt1$smoke) 
levels(birthwt1$smoke)
#use revalue fucntion in plyr to rename this factor smoke/levels
birthwt1$smoke <- revalue(birthwt1$smoke, c("0"="No Smoke", "1"= "Smoke"))

ggplot(birthwt1,  aes(x=bwt)) + geom_histogram(fill = "white", colour = "black") +
  facet_grid(smoke ~.)

ggplot(birthwt, aes(x=bwt)) + geom_histogram(binwidth = 30,fill="white", 
      colour="black") +
  facet_grid(race ~ ., scales = "free")

#Density curves
h <- ggplot(faithful, aes(x=waiting))
h + geom_density()
#to remove the straight lines at the edges
h + geom_line(stat = "density") + expand_limits(y=0)
h + geom_line(stat = "density", adjust = .25, colour = "red") +
  geom_line(stat = "density") +
  geom_line(stat = "density", adjust = .2, colour = "blue")

h + geom_density(fill = "blue", alpha = .2) + xlim(35, 105)

h + geom_density(fill = "blue", colour = NA, alpha = .2) +
  geom_line(stat = "density") +
  xlim(35, 105)
#faceting thisdensity grouped data
ggplot(birthwt1,  aes(x=bwt, y =..density..)) +
  geom_histogram(binwidth = 200, fill = "cornsilk", colour = "grey60", size = .2) +
  geom_density() +
  facet_grid(smoke~.)

#making a frequency polygon
h + geom_freqpoly(binwidth = 4)
biasize <- diff(range(faithful$waiting))/15
h + geom_freqpoly(binwidth = biasize)

#Box plot
library(MASS)
p <- ggplot(birthwt, aes(x= factor(race), y =bwt))
p + geom_boxplot()
p + geom_boxplot(width =.5)
p + geom_boxplot(outlier.size = 1.5, outlier.shape = 21)

ggplot(birthwt,  aes(x=1, y=bwt)) + geom_boxplot() +
  scale_x_continuous(breaks = NULL) +
  theme(axis.text.x = element_blank())

#adding notches to a box plot
p + geom_boxplot(notch = TRUE)
#adding means to a boxplot
p + geom_boxplot() +
  stat_summary(fun.y = "mean", geom = "point", shape = 23, size =3 ,fill = "white")

#Violin plot
library(gcookbook)
#Base plot
v <- ggplot(heightweight,  aes(x=sex, y=heightIn))
v + geom_violin()
v + geom_violin() + geom_boxplot() +
  stat_summary(fun.y ="median", geom ="point", fill="white", shape=21, size=2.5)

v + geom_violin(trim = TRUE)
v + geom_violin(scale = "count")

#dotplot
countries2009 <- subset(countries, Year == 2009 & healthexp > 2000)
d <- ggplot(countries2009, aes(x=infmortality))
d + geom_dotplot()
countries$
 
d + geom_dotplot(binwidth=.25) + geom_rug() +
  scale_y_continuous(breaks=NULL) + # Remove tick markers
  theme(axis.title.y=element_blank()) # Remove axis label

d + geom_dotplot(method = "histodot", binwidth=.25) + geom_rug() +
  scale_y_continuous(breaks=NULL) + # Remove tick markers
  theme(axis.title.y=element_blank())

d + geom_dotplot(binwidth=.25, stackdir = "center") + geom_rug() +
  scale_y_continuous(breaks=NULL) + # Remove tick markers
  theme(axis.title.y=element_blank())

d + geom_dotplot(binwidth=.25, stackdir = "centerwhole") + geom_rug() +
  scale_y_continuous(breaks=NULL) + # Remove tick markers
  theme(axis.title.y=element_blank())
c("#FFFFFF", "#FFFFFF", "#FFFFFF")#making multiple dot  plots for grouped data
ggplot(heightweight, aes(x = sex, y=heightIn)) + 
  geom_boxplot(outlier.colour = NA, width = .4) +
  geom_dotplot(binaxis = "y", binwidth = .5, stackdir = "center", fill = "black")

#making a density plot on a 2dimensional data
#base plot
p <- ggplot(faithful, aes(x=eruptions, y=waiting))
p + geom_point()
p + geom_point() + stat_density2d()
p + stat_density2d(aes(colour = ..level..))
#map density estimate to fill colour
p + stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE)
p + stat_density2d(aes(alpha = ..density..), geom = "tile", contour = FALSE)
p + stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE,
                   h = c(.5,5))

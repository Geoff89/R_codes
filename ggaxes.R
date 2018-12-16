#plotting the axes
library(ggplot2)
library(gcookbook)
gcookbook::cabbage_exp

PlantGrowth
head(PlantGrowth)

ggplot(PlantGrowth, aes(x = group, y = weight)) + geom_boxplot() 
ggplot(PlantGrowth, aes(x = group, y = weight)) + geom_boxplot() + coord_flip()
ggplot(PlantGrowth, aes(x = group, y = weight)) + geom_boxplot() + coord_flip() + 
  scale_x_discrete(limits = rev(levels(PlantGrowth$group)))

  
#setting the range of a contionous axis
p <- ggplot(PlantGrowth, aes(x = group, y = weight)) + geom_boxplot() 
p 
#the below two will not work together
p +ylim (0, 10)  + scale_y_continuous(breaks = NULL)

P + scale_y_continuous(limits = c(0,10), breaks = NULL)
p
P + scale_y_continuous(limits = c(0,10), breaks = NULL)
p + scale_y_continuous(limits = c(0,10))
p + scale_y_continuous(limits = c(5,6.5))
p + coord_cartesian(ylim = c(5, 6.5))
p + expand_limits(y = 0)

#reversing a contionus axis
p + scale_y_reverse()
p + scale_y_reverse(limits = 6.5, 3.5)
p + scale_y_reverse()

#changing the order of items in a categorical axis
p + scale_x_discrete(limits = c("trt1", "ctrl", "trt2"))
p + scale_x_discrete(limits = rev(levels(PlantGrowth$group)))

#setting the scaling ratio of the  x and y  axis ie 1:1
sp <- ggplot(marathon, aes(x = Half, y = Full )) + geom_point()
sp
sp + coord_fixed()

sp + coord_fixed()+
  scale_y_continuous(breaks = seq(0,420,30 )) + 
  scale_x_continuous(breaks = seq(0, 420, 30))

sp + coord_fixed(ratio = 1/2)+
  scale_y_continuous(breaks = seq(0,420,30 )) + 
  scale_x_continuous(breaks = seq(0, 420, 15))

#setting the position of tick marks
p + scale_y_continuous(breaks = c(4,4.25, 4.5, 5,6,8))
p + scale_y_continuous(breaks = seq(4, 7, by = .5))

#for discrete variable limits remove labels and brreaks control items with labels
p + scale_x_discrete(limits = c("trt2", "ctrl"), breaks = "ctrl")

#removing tick marks, tick abels and grid lines
p + theme(axis.ticks = element_blank(), axis.text.y = element_blank())
p
p + scale_y_continuous(breaks = NULL)

hwp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) +
  geom_point()
hwp
hwp + scale_y_continuous(breaks=c(50, 56, 60, 66, 72),
                         labels=c("Tiny", "Really\nshort", "Short",
                                  "Medium", "Tallish"))
footinch_formatter <- function(x) {
  foot <- floor(x/12)
  inch <- x %% 12
  return(paste(foot, "'", inch, "\"", sep=""))
}
footinch_formatter(56:64)

hwp + scale_y_continuous(labels=footinch_formatter)
hwp + scale_y_continuous(breaks=seq(48, 72, 4), labels=footinch_formatter)

timeHMS_formatter <- function(x) {
  h <- floor(x/60)
  m <- floor(x %% 60)
  s <- round(60*(x %% 1)) # Round to nearest second
  lab <- sprintf("%02d:%02d:%02d", h, m, s) # Format the strings as HH:MM:SS
  lab <- gsub("^00:", "", lab) # Remove leading 00: if present
  lab <- gsub("^0", "", lab) # Remove leading 0 if present
  return(lab)
}
timeHMS_formatter(c(.33, 50, 51.25, 59.32, 60, 60.1, 130.23))

#changing the appearnce of tick labels

bp <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() +
  scale_x_discrete(breaks=c("ctrl", "trt1", "trt2"),
                   labels=c("Control", "Treatment 1", "Treatment 2"))
bp
bp + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))
bp + theme(axis.text.x = element_text(angle=30, hjust=1, vjust=.5))

#using lines
p <- ggplot(heightweight,  aes(x = ageYear, y = heightIn)) + geom_point()
p
p + theme_bw()+ theme(
  panel.border = element_blank(), 
  axis.line = element_line(colour = "black" ,size = 4)
)
p + theme_bw()+ theme(
  panel.border = element_blank(), 
  axis.line = element_line(colour = "black" ,size = 4, lineend = "square")
)
p + theme_dark()+ theme(
  panel.border = element_blank(), 
  axis.line = element_line(colour = "black" ,size = 4, lineend = "square")
)
#logarithmic axis
library(MASS) # For the data set
# The base plot
p <- ggplot(Animals, aes(x=body, y=brain, label=rownames(Animals))) +
  geom_text(size=3)
p
#to remove the exponential distribution
p + scale_x_log10() + scale_y_log10()
p + scale_x_log10(breaks = 10^(-1:5)) + scale_y_log10(breaks = 10^(0:3))

library(scales)
p + scale_x_log10(breaks=10^(-1:5),
                  labels=trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks=10^(0:3),
                labels=trans_format("log10", math_format(10^.x)))
ggplot(Animals, aes(x=log10(body), y=log10(brain), label=rownames(Animals))) +
  geom_text(size=3)

# Use natural log on x, and log2 on y
p + scale_x_continuous(trans = log_trans(),
                       breaks = trans_breaks("log", function(x) exp(x)),
                       labels = trans_format("log", math_format(e^.x))) +
  scale_y_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x)))
#log for one axis
ggplot(aapl, aes(x=date,y=adj_price)) + geom_line()
ggplot(aapl, aes(x=date,y=adj_price)) + geom_line() +
  scale_y_log10(breaks=c(2,10,50,250))
#adding logarithmic ticks
p + annotation_logticks() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))
p + annotation_logticks() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                minor_breaks = log10(5) + -2:5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                minor_breaks = log10(5) + -1:3) +
  coord_fixed() +
  theme_bw()

#drawing circular graphs
ggplot(wind, aes(x=DirCat, fill=SpeedCat)) +
  geom_histogram(binwidth=15, origin=-7.5) +
  coord_polar() +
  scale_x_continuous(limits=c(0,360))

#impriving the above plot
ggplot(wind, aes(x=DirCat, fill=SpeedCat)) +
  geom_histogram(binwidth=15, origin=-7.5, colour="black", size=.25) +
  guides(fill=guide_legend(reverse=TRUE)) +
  coord_polar() +
  scale_x_continuous(limits=c(0,360), breaks=seq(0, 360, by=45),
                     minor_breaks=seq(0, 360, by=15)) +
  scale_fill_brewer()

#put mdeath time series data into a dataframe
md <- data.frame(deaths = as.numeric(mdeaths),
                 month = as.numeric(cycle(mdeaths)))
md
head(md)

library(plyr)
md <- ddply(md, "month", summarise,  deaths = mean(deaths))
md
#plotting the cood pola
p <- ggplot(md, aes(x = month, y = deaths)) + geom_line() + 
  scale_x_continuous(breaks = 1:12)
p
p + coord_polar()

#With coord_polar and y (r) limits going to zero
p + coord_polar() + ylim(0, max(md$deaths))
p + coord_polar() + ylim(0, max(md$deaths)) + xlim(0, 12)

# Connect the lines by adding a value for 0 that is the same as 12
mdx <- md[md$month==12, ]
mdx
mdx$month <- 0
mdnew <- rbind(mdx, md)
# Make the same plot as before, but with the new data, by using %+%
p %+% mdnew + coord_polar() + ylim(0, max(md$deaths))

#using dates in plots
str(economics)
ggplot(economics, aes(x = date, y = psavert)) + geom_line()

# Take a subset of economics
econ <- subset(economics, date >= as.Date("1992-05-01") &
                 date < as.Date("1993-06-01"))
# Base plot - without specifying breaks
p <- ggplot(econ, aes(x=date, y=psavert)) + geom_line()
p
# Specify breaks as a Date vector
datebreaks <- seq(as.Date("1992-06-01"), as.Date("1993-06-01"), by="2 month")
# Use breaks, and rotate text labels
p + scale_x_date(breaks=datebreaks) +
  theme(axis.text.x = element_text(angle=30, hjust=1))

library(scales)
p + scale_x_date(breaks=datebreaks, labels=date_format("%Y %b")) +
  theme(axis.text.x = element_text(angle=30, hjust=1))
#using time in axis
# Convert WWWusage time-series object to data frame
www <- data.frame(minute = as.numeric(time(WWWusage)),
                  users = as.numeric(WWWusage))
# Define a formatter function - converts time in minutes to a string
timeHM_formatter <- function(x) {
  h <- floor(x/60)
  m <- floor(x %% 60)
  lab <- sprintf("%d:%02d", h, m) # Format the strings as HH:MM
  return(lab)
}
# Default x axis
ggplot(www, aes(x=minute, y=users)) + geom_line()
# With formatted times
ggplot(www, aes(x=minute, y=users)) + geom_line() +
  scale_x_continuous(name="time", breaks=seq(0, 100, by=10),
                     labels=timeHM_formatter)

library(ggplot2)
#coord polar
mtcars$car = row.names(mtcars)

p = ggplot(mtcars, aes(x=car, y=mpg, fill=mpg)) +
  geom_histogram(binwidth=1, stat="identity") +theme_light() +
  scale_fill_gradient(low="red", high="white", limits=c(5,40)) +
  theme(axis.title.y=element_text(angle=0))
p + theme(axis.text.x = element_text(angle=45, vjust = 1, hjust=1))

p + coord_polar()
p + coord_polar() + aes(x=reorder(car, mpg)) +
  theme(axis.text.x = element_text(angle=-20)) 
# text with angle to avoid name overlap

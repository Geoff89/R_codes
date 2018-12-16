#geom_line
library(ggplot2)
library(gcookbook)
ggplot(BOD, aes(x=Time, y=demand))+geom_line()

#when x variable is a factor under aes use group=1 to show that thw data pints belong
#to one another and they should be connetced with a line
BOD1<-BOD
View(BOD1)
BOD1$Time <- factor(BOD1$Time)
is.factor(BOD1$Time)
levels(BOD1$Time)
ggplot(BOD1, aes(x = Time, y = demand, group = 1)) + geom_line()

#plotting a base plot
p <- ggplot(BOD, aes(x=Time, y=demand))+geom_line()
p
#adding range to start y at 0
p + ylim(0, max(BOD$demand))
p + expand_limits(y=0)
#ading points to a line graph
p + geom_point()

View(worldpop)
ggplot(worldpop, aes(x=Year, y=Population)) +geom_line()+geom_point()
ggplot(worldpop, aes(x=Year, y=Population)) +geom_line()+ geom_point() +
      scale_y_log10()

#plotting muliple lines
library(plyr)
tg <- ddply(ToothGrowth, c("supp","dose"), summarise, Length = mean(len))
tg
#mapp to colour to createlegend
ggplot(tg, aes(x=dose,y=Length,colour=supp)) + geom_line() +
  scale_color_brewer(palette ="Set1")
ggplot(tg, aes(x=dose,y=Length,linetype=supp)) + geom_line()

ggplot(tg, aes(x=factor(dose), y=Length, colour=supp, group=supp)) + geom_line()

ggplot(tg, aes(x=dose,y=Length,shape=supp)) + geom_line()+
  geom_point(size = 4)

ggplot(tg, aes(x=dose,y=Length,fill=supp)) + geom_line()+
  geom_point(size = 4)

ggplot(tg, aes(x=dose,y=Length,fill=supp)) + 
  geom_line(position = position_dodge(0.2))+
  geom_point(position=position_dodge(0.2),size = 4)

#changing appearance of the lines
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="dashed", size=1, colour="blue")

#changing appearance of the points
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line() +
  geom_point(size=4, shape=22, colour="darkred", fill="pink")
# Save the position_dodge specification because we'll use it multiple times
pd <- position_dodge(0.2)

ggplot(tg, aes(x=dose, y=Length, fill=supp)) +
  geom_line(position=pd) +
  geom_point(shape=21, size=3, position=pd) +
  scale_fill_manual(values=c("black","white"))

#making a shaded graph area.use geom_area
sunspot.year #toime series data
# Convert the sunspot.year data set into a data frame for this example
sunspotyear <- data.frame(
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)
head(sunspotyear)
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + geom_area()
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) +
  geom_area(colour="black", fill="blue", alpha=.2)

ggplot(sunspotyear, aes(x=Year, y=Sunspots)) +
  geom_area(fill="blue", alpha=.2) +
  geom_line()
is.factor(uspopage$AgeGroup)

h <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup))
h + geom_area()
h + geom_area(colour="black",size=.2, alpha=.4)+
  scale_fill_brewer(palette = "Blues", breaks = rev(levels(uspopage$AgeGroup)))

ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup))) +
  geom_area(colour=NA, alpha=.4) +
  scale_fill_brewer(palette="Blues") +
  geom_line(position="stack", size=.2)

#Making a Proportional Stacked Area Graph
# Convert Thousands to Percent
uspopage_prop <- ddply(uspopage, "Year", transform,
                       Percent = Thousands / sum(Thousands) * 100)
head(uspopage_prop)

ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))

#adding a confidence region
library(gcookbook)#for the dataset
#grabbing a subset of the data
clim <- subset(climate, Source=="Berkeley",
               select=c("Year","Anomaly10y","Unc10y"))
head(clim)
# Shaded region
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y),
              alpha=0.2) +
  geom_line()

# With a dotted line for upper and lower bounds
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_line(aes(y=Anomaly10y-Unc10y), colour="grey50", linetype="dotted") +
  geom_line(aes(y=Anomaly10y+Unc10y), colour="grey50", linetype="dotted") +
  geom_line()

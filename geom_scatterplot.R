library(gcookbook)
library(ggplot2)
View(heightweight)
heightweight

ggplot(heightweight, aes(x=ageYear, y=heightIn, shape = sex, colour = sex)) + 
  geom_point(size=16) + 
  scale_shape_manual(values = c(1,2)) +
  scale_colour_brewer(palette = "Set1")

ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) +
  geom_point() +
  scale_shape_manual(values=c(1,2)) +
  scale_colour_brewer(palette="Set1")

ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) +
  geom_point(shape=21, size=2.5) +
  scale_fill_gradient(low="black", high="white")


ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, colour=sex)) +
  geom_point(alpha=.5) +
  scale_size_area() + # Make area proportional to numeric value
  scale_colour_brewer(palette="Set1")

#avoiding overplotting
sp <- ggplot(diamonds,  aes(x=carat, y=price))
sp + geom_point()
#using alpha to reduce overplotting
sp + geom_point(alpha=.1)
sp + geom_point(alpha=.01)
#using binning
sp + stat_bin2d()
sp + stat_bin2d(bins = 50)+
  scale_fill_gradient(low = "lightblue", high = "red", limits=c(0,6000))

library(hexbin)
sp + stat_binhex() +
  scale_fill_gradient(low="lightblue", high="red",
                      limits=c(0, 8000))
sp + stat_binhex() +
  scale_fill_gradient(low="lightblue", high="red",
                      breaks=c(0, 250, 500, 1000, 2000, 4000, 6000),
                      limits=c(0, 6000))
sp1 <- ggplot(ChickWeight, aes(x=Time, y=weight))
sp1 + geom_point()
sp1 + geom_point(position="jitter")
# Could also use geom_jitter(), which is equivalent
sp1 + geom_point(position=position_jitter(width=.5, height=0))
sp1 + geom_boxplot(aes(group=Time))
is.factor(ChickWeight$Time)



library(gcookbook)
library(ggplot2)
library(MASS)
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()
#Bar graphs
ggplot(pg_mean, aes(x=group,y=weight))+geom_bar(stat = "identity")
#when x is numeric and contionoues variable
BOD
ggplot(BOD, aes(x=Time, y=demand))+geom_bar(stat = "identity")
ggplot(BOD, aes(x=factor(Time), y=demand))+geom_bar(stat = "identity")
ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", fill="lightblue", colour="black")
#grouping categorical variables in a bar graph
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar))+
  geom_bar(stat= "identity", position = "dodge")

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity",position="dodge", colour="black") +
  scale_fill_brewer(palette="Pastel1")
#counting i the bar graph
ggplot(diamonds, aes(x=cut)) + geom_bar()
ggplot(diamonds, aes(x=carat)) + geom_bar()
#using colors on bar graphs
upc <- subset(uspopchange, rank(Change)>40)
upc
ggplot(upc, aes(x=Abb, y=Change, fill=Region)) + geom_bar(stat="identity")
#filling the colors manually
ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
  geom_bar(stat="identity", colour="black") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) +
  xlab("State")
#color palette
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point(colour="red")
library(MASS) # For the data set
ggplot(birthwt, aes(x=bwt)) + geom_histogram(bins = 30, fill="red")

# These both have the same effect
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity",colour="black", position="dodge")
ggplot(cabbage_exp, aes(x=Date, y=Weight)) +
  geom_bar(stat = "identity",aes(fill=Cultivar), colour="black", position="dodge")
# These both have the same effect
ggplot(mtcars, aes(x=wt, y=mpg, colour=cyl)) + geom_point()
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point(aes(colour=cyl))
# Convert to factor in call to ggplot()
ggplot(mtcars, aes(x=wt, y=mpg, colour=factor(cyl))) + geom_point()

# Base plot
p <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
# These three have the same effect
p
p + scale_fill_discrete()
p + scale_fill_hue()
# ColorBrewer palette
p + scale_fill_brewer()

#colour lightness
h <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex))+
        geom_point()
h
h + scale_colour_hue(l=65)
library(RColorBrewer)
display.brewer.all()
p+scale_fill_brewer(palette = "Oranges")
p+scale_fill_brewer(palette = "Greys")
p+scale_fill_brewer(palette = "Blues")
p+scale_fill_brewer(palette = "Set3")
#Grey palletes
p+scale_fill_grey()
p+scale_fill_grey(start = 0.7, end = 0)
# Using color names
h + scale_colour_manual(values=c("red", "blue"))
# Using RGB values
h + scale_colour_manual(values=c("#CC6666", "#7777DD"))
#you can run colors to see allthe available colors in r
colors()
#color palettes for color blind people
# The palette with grey:
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")
# Add it to the plot
p + scale_fill_manual(values=cb_palette)
#using manually define palletes for continuos variables
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) +
  geom_point(size=3)
# With a gradient between two colors
p + scale_colour_gradient(low="black", high="white")
# A gradient with a white midpoint
library(scales)
p + scale_colour_gradient2(low=muted("red"), mid="white", high=muted("blue"),
                           midpoint=110)
# A gradient of n colors
p + scale_colour_gradientn(colours = c("darkred", "orange", "yellow", "white"))

cb <- subset(climate, Source=="Berkeley")
cb$valence[cb$Anomaly10y >= 0] <- "pos"
cb$valence[cb$Anomaly10y < 0] <- "neg"

ggplot(cb, aes(x=Year, y=Anomaly10y)) +
  geom_area(aes(fill=valence)) +
  geom_line() +
  geom_hline(yintercept=0)

# approx() returns a list with x and y vectors
interp <- approx(cb$Year, cb$Anomaly10y, n=1000)
# Put in a data frame and recalculate valence
cbi <- data.frame(Year=interp$x, Anomaly10y=interp$y)
cbi$valence[cbi$Anomaly10y >= 0] <- "pos"
cbi$valence[cbi$Anomaly10y < 0] <- "neg"

ggplot(cbi, aes(x=Year, y=Anomaly10y)) +
  geom_area(aes(fill=valence), alpha = .4) +
  geom_line() +
  geom_hline(yintercept=0) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE) +
  scale_x_continuous(expand=c(0, 0))
#coloring both positive and negative colors separately
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
head(csub)
csub$pos <- csub$Anomaly10y >= 0
head(csub)

ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity")
#putting leged to be redundant
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)
#adjusting the bar graph width and space
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity")
#For narrower bars:
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=0.5)
#And for wider bars (these have the maximum width of 1):
ggplot(pg_mean, aes(x=group, y=weight)) + geom_bar(stat="identity", width=1)
#stacked bar graph
#For a grouped bar graph with narrow bars:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position="dodge")
#And with some space between the bars:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))

#stacked graph
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity")
#changing the order of stack of legend tomatch the graph stack
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE))

#adding texts along bar graphs
# Below the top
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white")
# Above the top
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2)

# Adjust y limits to be a little higher
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)
# Map y positions slightly above bar top - y range of plot will auto-adjust
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=Weight+0.1, label=Weight))

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white",
            position=position_dodge(.9), size=3)

#legends revision

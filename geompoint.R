library(ggplot2)
library(gcookbook)
View(tophitters2001)

tophit <- tophitters2001[1:25,]
ggplot(tophit, aes(x = avg, y = name)) + geom_point()

tophit[,c("name", "lg", "avg")]
View(tophit)


ggplot(tophit, aes(x=avg, y=reorder(name, avg))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

ggplot(tophit, aes(x=reorder(name, avg), y=avg)) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))

# Get the names, sorted first by lg, then by avg
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
# Turn name into a factor, with levels in the order of nameorder
tophit$name <- factor(tophit$name, levels=nameorder)

ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=lg)) +
  scale_colour_brewer(palette="Set1", limits=c("NL","AL")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(), # No horizontal grid lines
        legend.position=c(1, 0.55), # Put legend inside plot area
        legend.justification=c(1, 0.5))

ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=lg)) +
  scale_colour_brewer(palette="Set1", limits=c("NL","AL"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales="free_y", space="free_y")

#legends revision
p <- ggplot(PlantGrowth, aes(x =  group, y = weight, fill = group)) + geom_boxplot()
p
p <- ggplot(PlantGrowth, aes(x =  group, y = weight)) + geom_boxplot(aes(fill = group))
p
p + guides(fill = FALSE)
p + scale_fill_discrete(guide = FALSE)
p + theme(legend.position = "one")

p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot() +
  scale_fill_brewer(palette="Pastel2")
p
p + theme(legend.position = "top")
p + theme(legend.position = "bottom")
p + theme(legend.position = "left")
p + theme(legend.position = "right")

#justification and postioning used together
p + theme(legend.position = c(1,0),  legend.justification = c(1,0))
p + theme(legend.position = c(0,0),  legend.justification = c(0,0))

p + theme(legend.position=c(.85,.2)) +
  theme(legend.background=element_rect(fill="white", colour="black"))

p + theme(legend.position=c(.85,.2)) +
  theme(legend.background=element_blank()) + # Remove overall border
  theme(legend.key=element_blank()) # Remove border around each item
#adding labels to the legends
p + labs(fill = "conditional")
p + scale_fill_discrete(name = "condition")

#if ou have mulgtiple legends
hw <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = sex)) +
      geom_point(aes(size = weightLb)) + scale_size_continuous(range = c(1,4))
hw
#adding title
hw + labs(colour = "male/female", size = "weight\n(pounds)")


hw1 <- ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) +
  geom_point()
hw1
# Change both shape and colour
hw1 + labs(shape="Male/Female", colour="Male/Female")

#changing the appearnce of a legend title
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
p

p + theme(legend.title = element_text(face = "italic", family = "Times", colour = "red",
                                      size = 14))
#removing titles of legends
ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot() +
  guides(fill=guide_legend(title=NULL))
#changing labels of legends'# Change the legend labels
p + scale_fill_discrete(labels=c("Control", "Treatment 1", "Treatment 2"))
p + scale_fill_grey(start=.5, end=1,
                    labels=c("Control", "Treatment 1", "Treatment 2"))
p + scale_fill_discrete(limits=c("trt1", "trt2", "ctrl"),
                        labels=c("Treatment 1", "Treatment 2", "Control"))
# The base plot
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) +
  geom_point()
p# Change the labels for one scale
p + scale_shape_discrete(labels=c("Female", "Male"))

# Change the labels for both scales
p + scale_shape_discrete(labels=c("Female", "Male")) +
  scale_colour_discrete(labels=c("Female", "Male"))

#changing appearance of the labels
# The base plot
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
# Change the legend label appearance
p + theme(legend.text=element_text(face="italic", family="Times", colour="red",
                                   size=14))

#using labels with multiple texts
# Labels that have more than one line
p + scale_fill_discrete(labels=c("Control", "Type 1\ntreatment",
                                 "Type 2\ntreatment"))
#adjusting this to fit proprly
library(grid)
p + scale_fill_discrete(labels=c("Control", "Type 1\ntreatment",
                                 "Type 2\ntreatment")) +
  theme(legend.text=element_text(lineheight=.8),
        legend.key.height=unit(1, "cm"))

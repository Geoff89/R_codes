#cleveland plots
library(gcookbook)
library(ggplot2)

str(tophitters2001)
tophit <- tophitters2001[1:25,] #picking the tp 25 rows
#we want the three columns name,lg,avg
tophit <- tophit[,c("name", "lg", "avg")]
tophit

ggplot(tophit, aes(x=avg, y=name)) + geom_point()
#building cleveland plots
ggplot(tophit, aes(x=avg, y=reorder(name, avg))) +
  geom_point(size=3) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey60",linetype = "dashed"))
#reordering the x axis
ggplot(tophit, aes(x=reorder(name,avg), y=avg)) +
  geom_point(size=3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey60",linetype = "dashed"))
#what about orderign from two factors levels.
#we have to do this manually
#get the names first sorted by lg then by avg
tophit$lg
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
nameorder
tophit$name <- factor(tophit$name, levels = nameorder)
tophit$name

#plotting 
ggplot(tophit, aes(x=avg, y=name)) +
  geom_point(size=3, aes(colour = lg)) +
  geom_segment(aes(yend=name), xend=0, colour = "grey60") +
  scale_color_brewer(palette = "Set1", limits= c("NL", "AL")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position = c(1,0.55),
        legend.justification = c(1,0.5)) #putting the legend inside the plot

#still you can separate two groups by facet grid
ggplot(tophit, aes(x=avg, y=name)) +
  geom_point(size=3, aes(colour = lg)) +
  geom_segment(aes(yend=name), xend=0, colour = "grey60") +
  scale_color_brewer(palette = "Set1", limits= c("NL", "AL"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg~., scales = "free_y", space = "free_y")

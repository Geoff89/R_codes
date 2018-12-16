#facet wrap
library(ggplot2)
library(gcookbook)
View(mpg)

str(mpg)
#base plot
p <- ggplot(mpg,  aes(x=displ, y=hwy))+geom_point()
p
#faceting by drv in vertically arranged subpanels
p + facet_grid(drv~.)
#faceting by cyl in horizontal arranged subpanels
p + facet_grid(.~cyl)
p + facet_grid(.~drv)
#splitting boh veticall and horizontally
p + facet_grid(drv~cyl)
#using facet wrap
p + facet_wrap(~class)
#using the samenumbre of rows and columns
p + facet_wrap(~class,  nrow = 2)
p + facet_wrap(~class, ncol = 4)
#using facet wihh different items on the axes
p + facet_grid(drv~cyl, scales = "free_y")
p + facet_grid(drv~cyl, scales = "free_x")
p + facet_grid(drv~cyl, scales = "free")

View(mpg)
levels(mpg$drv)
levels(mpg$model)
is.factor(mpg$drv)

#changing labels of the facets
mpg2 <- mpg
 #renaminng 4 to 4wd, f to front, r to Rear
levels(mpg2$drv)[levels(mpg2$drv=="4")] <- "4wd"
levels(mpg2$drv)[levels(mpg2$drv=="f")] <- "Front"
levels(mpg2$drv)[levels(mpg2$drv=="r")] <- "Rear"
#plotting
ggplot(mpg2, aes(x=displ, y=hwy )) + geom_point() + facet_grid(drv~.)
#alternatively we can use labeller to label the the variables
ggplot(mpg2, aes(x=displ, y=hwy )) + geom_point() + facet_grid(drv~.,
        labeller=label_both)

mpg3 <- mpg
#using formulaes
levels(mpg3$drv)[levels(mpg3$drv=="4")] <- "4^{wd}"
levels(mpg3$drv)[levels(mpg3$drv=="f")] <- "- Front %.% e^{pi * i}"
levels(mpg3$drv)[levels(mpg3$drv=="r")] <- "4^{wd} - Front"
#plotting with formulaes
ggplot(mpg3, aes(x=displ, y=hwy )) + geom_point() + facet_grid(drv~.,
              labeller=label_parsed)

#changing the appearance of the texts
ggplot(cabbage_exp, aes(x=Cultivar, y=Weight)) + geom_bar(stat = "identity") + 
  facet_grid(.~Date) +
  theme_bw()+ 
  theme(strip.text = element_text(face = "bold",size = rel(1.5)),
        strip.background = element_rect(fill = "lightblue",colour = "black",
                                        size = 1))

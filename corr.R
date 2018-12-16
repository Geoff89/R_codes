#correlation plot
mtcars

mcor <- cor(mtcars)
#rounding the above
round(mcor, digits = 2)

#plotting the above correlations
library(corrplot)
corrplot(mcor)
#displaying rectangular shades
corrplot(mcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45)
corrplot(mcor, method = "ellipse")


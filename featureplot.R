#caret visuaization
library(caret)
library(AppliedPredictiveModeling)

str(iris)
transparentTheme(trans = .4)
featurePlot(x = iris[,1:4],
            y = iris$Name,
            plot = "pairs",
            ##Add a key at the top
            auto.key = list(columns = 3))

featurePlot(x = iris[, 1:4], 
            y = iris$Name, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))

#density plot
featurePlot(x = iris[, 1:4], 
            y = iris$Name,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))

#Box plot
featurePlot(x = iris[, 1:4], 
           y = iris$Species, 
           plot = "box", 
           ## Pass in options to bwplot() 
           scales = list(y = list(relation="free"),
                         x = list(rot = 90)),  
           layout = c(4,1 ), 
           auto.key = list(columns = 2))

library(mlbench)
data(BostonHousing)
regVar <- c("age", "lstat", "tax")
str(BostonHousing[, regVar])

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = BostonHousing[, regVar], 
            y = BostonHousing$medv, 
            plot = "scatter", 
            layout = c(3, 1))

featurePlot(x = BostonHousing[, regVar], 
            y = BostonHousing$medv, 
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 1))

library(ggplot2)
library(gcookbook)
library(MASS)
#fitting models
#base plot
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn))
sp + geom_point() + stat_smooth(method = lm)
#changing to 99% confidence interval
sp + geom_point() + stat_smooth(method = lm, level = .99)
sp + geom_point() + stat_smooth(method = lm, se = FALSE, colour = "black")
sp + geom_point() + stat_smooth(method = loess)

#Fitting a regression model
b <- biopsy

b$classn[b$class == "benign"] <- 0
b$classn[b$class == "malignant"] <- 1
b
head(b)
ggplot(b, aes(x=V1, y=classn)) +
  geom_point(position = position_jitter(width = 0.3, height = 0.06), alpha=0.4,
             shape = 21, size = 1.5) +
  stat_smooth(method = glm, family = binomial)

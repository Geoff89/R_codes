library(ggplot2)

#dnorm
x <- seq(from = -5, to =5, length.out = 100) #interval [-5,5]
x
f <- dnorm(x) #normal with mean 0 and std 1
ggplot(data.frame(x=x, y=f), aes(x=x, y=y)) + geom_line()

#drawing 1000 points from norma with ean 0 and std 1
u <- rnorm(1000)
#plotting the distribution of the points
ggplot(data.frame(x=u), aes(x=x)) + geom_density() +
  geom_line(data = data.frame(x=x, y=f), aes(x=x, y=y),linetype=2)
#pnorm probability of normal distriution.unit area under curve
pnorm(0)
pnorm(-2)
pnorm(2) - pnorm(-2)

#qnorm quantile function for the normal distribution
qnorm(0.9544997)
qnorm(0.5)
qnorm(0.75)
#a dataframe
nframe <- data.frame(x=x, y=f)
#calculating the  75th percentile
line <- qnorm(0.75)
line
xstr <- sprintf("qnorm(0.75) = %1.3f", line)
xstr
#part of the normal dstribution to the left of f
nframes <- subset(nframe, nframe$x < line)
nframes
#plotting. the shaded area is 75% of the area under the normal curve
ggplot(nframe, aes(x=x, y=y)) + geom_line() + 
  geom_area(data = nframes, aes(x=x, y=y), fill = "gray") +
  geom_vline(aes(xintercept = line), linetype = 2) +
  geom_text(x=line, y=0, label = xstr, vjust =1)

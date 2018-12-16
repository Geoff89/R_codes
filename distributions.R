help("sprintf")
library(ggplot2)
x <-seq(from = -5, to =5, length.out = 100)
f <- dnorm(x)
ggplot(data.frame(x=x, y=f), aes(x=x,y=y))+geom_line()

u <- rnorm(1000)
u
#plot the distribution of points
ggplot(data.frame(x=u), aes(x=x)) +geom_density()+
  geom_line(data=data.frame(x=x, y=f), aes(x=x, y=y), linetype =2)

#estimating propbabilites of curves
pnorm(0)
pnorm(-2)
qnorm(0.5)
qnorm(0.75)

#Tutorials
z <- c(10,20,30,40,40)
pnorm(z)
qnorm(z)
dnorm(z)
rnorm(z)
pbinom(z, size=4, prob = 0.2)

#binomial distribution
dbinom(4, size = 12, prob = 0.2)
#or
dbinom(0, size = 12, prob = 0.2)+dbinom(1, size = 12, prob = 0.2)+
  dbinom(2, size = 12, prob = 0.2)+dbinom(3, size = 12, prob = 0.2)+
  dbinom(4, size = 12, prob = 0.2)
#or you can use cumulative probability distribution
pbinom(4, size = 12, prob = 0.2)

#poisson distribution
ppois(16,  lambda = 12)#0.898709
#upper tail
ppois(16, lambda = 12, lower.tail = FALSE)#0.101291

#continous uniform distribution
runif(10, min = 1, max = 3)
#exponential distribution
pexp(2, rate = 1/3)
#normal distribution
pnorm(84, mean = 72, sd = 15.2, lower.tail = FALSE)

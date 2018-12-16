
#probability distributions
# Binomial distribution
dbinom(0,  size = 12, prob = 0.2) +
  dbinom(1,  size = 12, prob = 0.2) +
  dbinom(2,  size = 12, prob = 0.2) +
  dbinom(3,  size = 12, prob = 0.2) +
  dbinom(4,  size = 12, prob = 0.2) 
  
#or using the binomial duistribution cumulative distribution
pbinom(4, size = 12, prob = 0.2)

#poisson dustribution where independent events occur at intervals
ppois(16, lambda = 12, lower.tail = FALSE)

#Continous uniform distribution
runif(10, min = 1, max = 10)

#Exponential distribution
pexp(2, rate = 1/3)

#Normal distrbibution
pnorm(84, mean = 72, sd = 15.2, lower.tail = FALSE)

#Chisquare test of independence
library(MASS)
tbl = table(survey$Smoke, survey$Exer)
tbl
chisq.test(tbl)

#Chisquare distribution
qchisq(0.95,  df = 7)

#Student t distribution
qt(c(.25, .975), df = 5)

#F distribution
qf(.95, df1 = 5, df2 = 2)

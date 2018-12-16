# Binomial distribution

dbinom(4, size = 12, prob=0.2)

dbinom(0, size =12, prob = 0.2)+
  dbinom(1, size = 12, prob = 0.2)+
  dbinom(2, size = 12, prob =0.2)+
  dbinom(3, size =12, prob = 0.2)+
  dbinom(4, size=12, prob= 0.2)
#cumulativey addding both the sequences
pbinom(4, size=12, prob=0.2 )

#Poisson distribution
ppois(16, lambda = 12)
ppois(16, lambda = 12, lower.tail = FALSE) #upper tail

#contionous unoform distribution
runif(10, min = 1, max = 3)
dunif(10, min=1, max=3)
qunif(10, min =1, max = 3)

#Exponential distribution
pexp(2, rate=1/3)
help(pexp)

#Normal distribution
pnorm(84, mean = 72, sd = 15.2, lower.tail = FALSE)

#chi square distribution. this has degrees freedom
qchisq(.95, df=7)

#student t distribution. two independent variable one having standard normal 
#distribution and another having chi squared distribution
qt(c(.025, .975), df=5)

#f distribution. Indeendent variables having chi squared distribution
qf(.95, df1 = 5, df2=2)

#Goodness of fit
library(MASS)
levels(survey$Smoke)
head(survey$Smoke)

#to get the frequency of smoking on the levels we use the head fucntion
smoke.freq = table(survey$Smoke)
smoke.freq
#omouting the chi square test using chisq.test fucntion
smoke.prob = c(.045, .795, .085, .075)
chisq.test(smoke.freq, p = smoke.prob)

#Chi square independence test
tbl = table(survey$Smoke, survey$Exer)
tbl

chisq.test(tbl)
#we can merge the table values and then apply chi square on the merged table
ctbl = cbind(tbl[,"Freq"], tbl[,"None"] +tbl[,"Some"])
ctbl
chisq.test(ctbl)

#sign test
binom.test(5,18)

#wilcoxon signed ranked test
head(immer)
wilcox.test(immer$Y1, immer$Y2, paired = TRUE) 

#man whitney wilcoxxon test
mtcars$mpg
mtcars$am
wilcox.test(mpg~am, data=mtcars)

#kruskal wallis test
head(airquality)
kruskal.test(Ozone~Month, data=airquality)
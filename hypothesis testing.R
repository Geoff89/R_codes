# Hypotheis testing
#Approaches to hypothesis testing
#bCritical values approach or p value which is the commonly used test

#lower tail test
xbar = 9900   #sample mean
moo = 10000  # hypothesized mean
sigma = 120  #population standard deviation
n =30        # sample size
#compute our test statistic
z = (xbar- moo)/sigma/sqrt(n)
z

#compute the critical value at .05 significance level
alpha = .05
z.alpha = qnorm(1-alpha)
-z.alpha

z = -0.1521452
z.alpha = -1.644854
#since test statistic is greatetr than critical value we accept the null hypothesis
#at .05 significance level that themean ife of lightbulb is greater than 10,000 hours

#computing the p value
pval = pnorm(z)
pval
pval = 0.04395362 # is less han significance value hence we reject the hypothesis

#Upper tail hypothesis
xbar = 2.1 #Sample mean
moo = 2 # hypothesized mean
sigma = 0.25 # population standard deviation
n = 35 # sample size
#compute our test statistic
z = (xbar- moo)/sigma/sqrt(n)
z
#Test statisitc results
0.06761234
pval = pnorm(z, lower.tail = FALSE) #upper tail test
pval
#Result of pnorm
0.4730471
#since pnorm value is less than the significance level we reject the null hypothesis

#Two tailed test hypothesis with known variance
xbar = 14.6 #Sample mean
moo = 15.4 # hypothesized mean
sigma = 2.5 # population standard deviation
n = 35 # sample size
#compute our test statistic
z = (xbar- moo)/sigma/sqrt(n)
z
#The result of the test statistic is
-0.05408987
#Computing the pnorm
pval = 2 * pnorm(z)
pval
#The result for the pvalue is
0.9568636
#We fail to reject the null hypothesis since pvalue is > than significance level

# Lower tail test with unknown variance
xbar = 9900   #sample mean
moo = 10000  # hypothesized mean
sigma = 120  #population standard deviation
n =30        # sample size
#compute our test statistic
z = (xbar- moo)/sigma/sqrt(n)
z
#Use pt to compute the lower tail p value of test statistic option of critical value
pval = pt(z, df = n -1)
pval
#Result is 0.440063. since it is less than significance level we reject the null hypothesis

#Upper tail test for unknown variance
xbar = 2.1 #Sample mean
moo = 2 # hypothesized mean
sigma = 0.25 # population standard deviation
n = 35 # sample size
#compute our test statistic
z = (xbar- moo)/sigma/sqrt(n)
z
#z = 0.06761234
pval = pt(z, df = n-1, lower.tail = FALSE)
pval # is 0.0432451 < than significance level. Reject the null hypothesis

#Two tailed test
xbar = 14.6 #Sample mean
moo = 15.4 # hypothesized mean
sigma = 2.5 # population standard deviation
n = 35 # sample size
#compute our test statistic
z = (xbar- moo)/sigma/sqrt(n)
z #Result is  -0.05408987
#Compute the p value of two taile test
pval = 2 * pt(z, df = n-1)
pval #0.9571801 > than significance level therefore wefail to reject null hypothesis. Alternate hypothesis is true

#proportion test of hypothesis
#Lower tail test of population proportion test
pbar = 85/148 #Equivalent to 0.5743243 #Sample proportion
po = 0.6 #hypothesized value
n = 148
pbar
z = (pbar-po)/sqrt(po*(1-po)/n) #test statistic
z # -0.6375983

#computing the pvalue of the test statistic we get
pval = pnorm(z)
pval # 0.2618676 is > than significance level..we fail to reject the null hypothesis

#Upper tail test of population proportion
pbar = 30/214 #Equivalent to 0.1401869#Sample proportion
po = 0.12 #hypothesized value
n = 214
pbar
z = (pbar-po)/sqrt(po*(1-po)/n) #test statistic
z #  0.908751

#computing the pvalue of the test statistic we get
pval = pnorm(z, lower.tail = FALSE)
pval # 0.18174080.8182592 > than o.o5 significance level  we fail to reject the null hypothesis

#Two tailed test of a population proportion
pbar = 12/30 #Equivalent to 0.4 #Sample proportion
po = 0.5 #hypothesized value
n = 20
pbar
z = (pbar-po)/sqrt(po*(1-po)/n) #test statistic
z #  -0.8944272

#computing the pvalue of the test statistic we get
pval = 2 * pnorm(z, lower.tail = FALSE)
pval#  1.628907 > than o.o5 significance level  we fail to reject the null hypothesis

#interval estimate
library(MASS)
head(survey)
help(survey)
height.response = na.omit(survey$Height)
height.response
#computing the standard error mean
n = length(height.response)
n
sigma = 9.48
sem = sigma/sqrt(n)
sem
#since there are two tails at 95% CI would be 97.5 percentile
#margin of error
E =qnorm(.975)*sem;E
#computing the interval
xbar = mean(height.response)
xbar
xbar + c(-E, E)

#point estimate of population mean and proportion is standard error
#point estimate for population mean
height.survey = na.omit(survey$Height)
mean(height.survey)
#alternatively
height.survey = survey$Height
mean(height.survey, na.rm = TRUE)

#point estimate of population prorpotion
gender.response = survey$Sex
n = length(gender.response)
n
k = sum(gender.response == "Female")
k
pbar = k/n;pbar
#sampling size of population mean
zstar = qnorm(.975)
sigma = 9.48
E = 1.2
zstar^2*sigma^2/E^2

library(ggcookbook)
library(gcookbook)
library(ggplot2)
ggplot(pg_mean, aes(x=group, y = weight)) + geom_bar(stat = "identity")
BOD
str(BOD)
cor(BOD$Time,BOD$demand)
sd(Time)
sd(BOD$Time)
ggplot(BOD, aes(x = Time, y = demand )) + geom_bar(stat = "identity")
ggplot(BOD, aes(x = factor(Time), y = demand )) + geom_bar(stat = "identity", colour = "black", fill = "lightblue")
ggplot(cabbage_exp, aes(x = Date,  fill = Cultivar)) +
  geom_bar(position = "dodge")
ggplot(diamonds, aes(x=cut)) + geom_bar()
#using x as continous variable we get a histogram
ggplot(diamonds, aes(x=carat)) + geom_bar()
ggplot(diamonds, aes(x=carat)) + geom_histogram()

#Color for the bar graph
upc <- subset(uspopchange, rank(Change)>40)
upc
#we plot the data
ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
       geom_bar(stat="identity", colour="black") +
       scale_fill_manual(values=c("#669933", "#FFCC66")) +
       xlab("State")
#color for negative and positive numbers
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
csub
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity")
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)

x <- c(1,2,3,4,5,6,7)
x[x==2]
y <- data.frame(a=1:10, b=11:20)
y$a
y$b
y[c("a","b")]
y$a[y$a==2]

#functionals
set.seed(1234)
df <- data.frame(replicate(6, sample(c(1:10,-99), 6, rep = TRUE)))
df
names(df) <- letters[1:6]
df

for(i in seq_along(1:10)){
  print(i)
}
#fix missing i dontt 99.so it should be treated as -na\
fix_missing<- function(x){
  x[x==-99] <- NA
  x
}
df[] <- lapply(df, fix_missing)
df
as.data.frame(lapply(df, fix_missing))
df[1:5] <- lapply(df[1:5], fix_missing)
df
#having different columns used for missing values
fix_missing<- function(x){
  x[x==-99] <- NA
  x
}
fix_missing<- function(x){
  x[x==-999] <- NA
  x
}
#the above will results in duplication hence bug creation in software
#using closure i.e function inside ficntion is  good measure
missing_fixer <- function(na_value){
  function(x){
    x[x==na_value] <- NA
    x
  }
}
fix_missing_99 <- missing_fixer(-99)
fix_missing_999 <- missing_fixer(-999)
fix_missing_99(c(-99, 999))
fix_missing_999(c(-99, 999))
#doing summary to the values though it introduces bugs
summary <- function(x) {
  c(mean(x, na.rm = TRUE),
    median(x, na.rm = TRUE),
    sd(x, na.rm = TRUE),
    mad(x, na.rm = TRUE),
    IQR(x, na.rm = TRUE))
}
#an alternative is to out functons to lists
summary <- function(x) {
  funs <- c(mean, median, sd, mad, IQR)
  lapply(funs, function(f) f(x, na.rm = TRUE))
}

#anonymous functions
lapply(mtcars, function(x)length(unique(x)))
Filter(function(x)!is.numeric(x), mtcars)

formals(function(x = 4) g(x) + h(x))
body(function(x = 4) g(x) + h(x))
environment(function(x = 4) g(x) + h(x))
#this does not call an anoynomous functions
function(x)3()
(function(x)3)()
#closures
power <- function(exponent){
  function(x){
    x^2
  }
}
square <- power(2)
square(2)
square(4)
#for cube fucntion
cube <- power(3)
cube(2)
cube(4)
as.list(environment(square))
as.list(environment(cube))

power <- function(exponent){
  print(environment())
  function(x){
    x^2
  }
}
zero <- power(0)
environment(zero)

#mutabble states
new_counter <- function(){
  i <- 0
  function(){
    i <<- i + 1
    i
  }
}
counter_one <- new_counter()
counter_two <- new_counter()
counter_one()#1
counter_one() #2

#functionals
#.Lapply
#creating a replicate datai
l <- replicate(20, runif(sample(1:10,1)), simplify = FALSE)
l

#with a for loop
out <- vector("list", length(l))
out
for (i in seq_along(l)){
  out[[i]] <- length(l[[i]])
}
unlist(out)
#alternatively you can use lapply
unlist(lapply(l, length))
#since dtaframes are lists we can apply lists to dataframes
unlist(lapply(mtcars, class))
#divide each column by the mean
mtcars[] <- lapply(mtcars, function(x) x/mean(x))

#looping patterns
xs <- runif(1e3)
res <- numeric(length(xs))
for(i in seq_along(xs)){
  res[i] <- sqrt(xs[i])
}
res
#vapply and sapply
sapply(mtcars, is.numeric)
vapply(mtcars, is.numeric, logical(1))
sapply(list(), is.numeric)
vapply(list(), is.numeric, logical(1))

#maps
#generate some sample data
xs <- replicate(5, runif(10), simplify = FALSE)
xs
ws <- replicate(5, rpois(10,5) + 1, simplify = FALSE)
ws
unlist(Map(weighted.mean, xs,ws))
#parallellisation
library(parallel)
unlist(mclapply(1:10, sqrt, mc.cores = 4))
system.time(unlist(parLapply(1:10, sqrt)))

#manipulating matrices and arrays
a <- matrix(1:20, nrow = 5)
a
apply(a,1, mean)
apply(a,2, mean)
#sweep is used for standadization
x <- matrix(rnorm(20,0,10), nrow = 4)
x1 <- sweep(x,1, apply(x,1,min), '-')
x1
x2<- sweep(x1,1, apply(x,1,max), '/')
x2
#outer
outer(1:3, 1:10,"*")

#grouped apply also known as tapply
pulse <- round(rnorm(22,70,10/3)) + rep(c(0,5), c(10,12))
pulse
group <- rep(c("A","B"),c(10,12))
group
tapply(pulse,group,length)
tapply(pulse,group,mean)
split(pulse,group)

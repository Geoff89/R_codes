#revision 3
#optimising code
library(pryr)
library(microbenchmark)
x <- 6 + 6
x
y <- 4 + 4
y
microbenchmark(x, y)
system.time(x)
system.time(y)

#another chunk for timing
x <- runif(100)
microbenchmark(
  sqrt(x),
  x ^ 0.5
  )
microbenchmark(unit = "eps", sqrt(x), x^0.5)

#base types
#this function is closure
f <- function(){}
typeof(f)
is.function(f)
 #builtin function
typeof(sum)
is.primitive(sum)
#s3
df <- data.frame(x=1:10, y=letters[1:10])
pryr::otype(df)
otype(df$x)
otype(df$y)

mean
ftype(mean)

#getting methods and generics
mean.Date()#mean is generic and Dateis method
ftype(print)
#ftype determine whether a funcction is s3 or generic
ftype(t.data.frame)
ftype(t.test)
methods("t.test")
methods(mean)
methods(class = "ts")

#definig classes and creating objects
#create ans assign a class step by step
foo <- structure(list(), class = "foo")
class(foo)
foo1 <- structure(vector(), class = "foo1")
class(foo1)

inherits(foo, "foo")
#s3 classes can also create constructors
foo <- function(x){
  if(!is.numeric(x))stop("X must be numeric")
  structure(list(), class = "foo")
}
foo
foo(10)
#create new methods and generics
f <- function(x) UseMethod("f")
#adding methods to our generic for it to be useful
a <- structure(list(), class = "a")
f.a <- function(x) "Class a"
#dispatching our method
f(a)
#adding a method to an existing generic
mean.a <- function(x) "a"
mean(a)
methods(mean)
#setting up a fallback method for otherwise unknown classes
f <- function(x)UseMethod("f")
f.a <- function(x) "Class a"
f.default <- function(x)"Unknown class"
f(structure(list(), class = "a"))
f(structure(list(), class = c("b","a")))
f(structure(list(), class = "c"))

#s4 oop
library(stats4)
library(methods)
# From example(mle)
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
nLL <- function(lambda) - sum(dpois(y, lambda, log = TRUE))
fit <- mle(nLL, start = list(lambda = 5), nobs = length(y))

#an s4 object
isS4(fit)
otype(nobs)
#an s4 generic
isS4(nobs)
ftype(nobs)
#retrieve an s4 method 
mle_nobs <- method_from_call(nobs(fit))
isS4(mle_nobs)
ftype(mle_nobs)

is(fit)
is(fit, "mle")
getGenerics()
getClasses()

#implementing s4 classes,objects, generics, method and method dispatch
#creating the class
setClass("person",
         slots = list(name = "character", age = "numeric"))
setClass("employee", 
         slots = list(boss = "person"), contains = "person")
#creating the objects of teh classes
alice <- new("person", name = "Alice", age = 40)
john <- new("employee", name = "John", age = 20, boss = alice)
alice@name #alice
slot(john, "boss")
john@boss

setClass("RangedNumeric",contains = "numeric", 
         slots = list(min = "numeric", max = "numeric"))
rn <- new("RangedNumeric", 1:10, min =1, max = 10)
rn@min
rn@.Data
#creating new methods and generics
#for existing base package or fucntion
setGeneric("union")
setMethod("union",
          c(x="data.frame", y="data.frame"),
          function(x,y){
            unique(rbind(x,y))
          }
)
#creating generic from scartch
setGeneric("myGeneric", function(x){
  standardGeneric("myGeneric")
})
#method dispatch 
selectMethod("nobs", list("mle"))
selectMethod("myGeneric", list("person"))
method_from_call(nobs(fit))

#RC oo
Account <- setRefClass("Account",
                       fields = list(balance = "numeric"))
a <- Account$new(balance = 100)
a$balance
#> [1] 100
a$balance <- 200
a$balance
b <- a
b$balance
a$balance <- 0
b$balance
c <- a$copy()
c$balance
a$balance <- 100
c$balance

Account <- setRefClass("Account",
                       fields = list(balance = "numeric"),
                       methods = list(
                         withdraw = function(x) {
                           balance <<- balance - x
                         },
                         deposit = function(x) {
                           balance <<- balance + x
                         }
                       )
)

a <- Account$new(balance = 100)
a$deposit(100)
a$balance

NoOverdraft <- setRefClass("NoOverdraft",
                           contains = "Account",
                           methods = list(
                             withdraw = function(x) {
                               if (balance < x) stop("Not enough money")
                               balance <<- balance - x
                             }
                           )
)
accountJohn <- NoOverdraft$new(balance = 100)
accountJohn$deposit(50)
accountJohn$balance
#> [1] 150
accountJohn$withdraw(200)
#> Error: Not enough money

#environments
e <- new.env()
e$a <-FALSE
e$b <- "a"
e$c <- 2:3
e$d <- 1:3
e$.e <- 5
#since objects are not stored in the environment the same names can reference 
#multiple values
e$a <- e$b
e$a <- 1:3
ls(e)
ls(e, all.names = TRUE)
parent.env(e)
#types of environments
globalenv()
baseenv()
emptyenv()
environment()
#looking for parents of the global environment
search()
#accessing any environment
as.environment("package:stats")

e <- new.env()
e$a <- 1
e$a
e[["a"]]
get("a", envir = e)
ls(e)
parent.env(e)

rm("a", envir = e)
ls(e)
#to see if a binding exists in an environment
str(e)
ls.str(e)
x <- 10 
exists("x", envir = e)#uses the parent environment
exists("x", envir = e, inherits = FALSE)
#to compare envirionments
identical(globalenv(), environment())
identical(baseenv(), emptyenv())
#this will raise an error only works with list and atomic
baseenv() == emptyenv()
 
library(pryr)
x <- 5
where("x")
where("mean")
#recursing over environments
where <- function(name, env = parent.frame()){
  if(identical(env, emptyenv())){
    #Base case
    stop("Cant find", name, call. = TRUE)
  }else if(exists(name, envir = env, inherits = FALSE)){
    #success case
    env
  }else{
    #Recursive case
    where(name, parent.env(env))
  }
}
#bindng environments
environment(sd)
where("sd")
#execution environment
g <- function(x) {
  if (!exists("a", inherits = FALSE)) {
    message("Defining a")
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
g(10)
g(10)
#calling environment
h <- function() {
  x <- 10
  function() {
    x
  }
}
i <- h()
x <- 20
i()
#binding values
?Reserved
_abc <- 1 #error will result
if <- 1 #error
`a+b` <-3
`a+b`
`:3` <- "smile"
`:3`
ls()
 
x <- 0 
f <- function(){
  x <<-1
}
f()
x
#there is also delayed and active binding
library(pryr)
system.time(b %<d-% {Sys.sleep(1);1})
system.time(b)
#active binding
x %<a-% runif(1)
x
pas

#debugging,condition handling and defensive programming
f <-  function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) 1+d
f(10)

#do.call you pass alist to a function
do.call(mean, list(1:10, na.rm = TRUE))

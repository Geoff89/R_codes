#oop classes
library(pryr)

#dataframr
df <- data.frame(x = 1:10, y = letters[1:10])
df
otype(df) #s3
pryr::otype(df) #s3
otype(df$x) #base
otype(df$y) #s3
ftype(df$x)

mean
ftype(mean)
#s3, generic
sum
ts
ftype(t.data.frame)#s3 "method"
ftype(t.test)#s3 and generic

methods("mean")
methods("t.test")
methods(class = "ts")

#creating a class foo
foo <- structure(list(), class = "foo")
class(foo)
#the alternative way for creating this class
foo <- list()
class(foo) <- "foo"

inherits(foo, "foo")

#constructor
foo <- function(x){
  if(!is.numeric(x))stop("It must be numeric")
  structure(list(), class = "foo")
}
class(foo)

#creating the generic fuction and methods
f <- function(x) UseMethod("f")
f <- function(x){UseMethod("f")}

f.a <- function(x)"class a" #creating methdd of this generic function
a <- structure(list(), class = "a")
f(a)#give us the methid of the generic function f

#environment
e <- new.env()
e$a <- FALSE
e$b <- "a"
e$c <- 2.3
e$d <- 1:3

#searching the environment
search()
globalenv()
baseenv()
emptyenv()
environment()

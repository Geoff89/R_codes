#revision
(x<-1:10 %% 2 == 0)
(x2<-which(x))

#functions revision
f <- function(x) x^2
f
formals(f)
body(f)
environment(f)
#primitive functions are in built fucnions like sum
sum()
body(sum)
formals(sum)
environment(sum)

f <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
f()
#If a name isn't defined inside a function, R will look one level up.
x <- 2
g <- function() {
  y <- 1
  c(x, y)
}
g()

#Same rule aplies if a fucntion is defined inside another funtion
x <- 1
h <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
h()

j <- function(x) {
  y <- 2
  function() {
    c(x, y)
  }
}
k <- j(1)
k()

j <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  print(a)
}
j()

f <- function() x + 1
codetools::findGlobals(f)
#another way to find this problemis
environment(f) <- emptyenv()
f()

#errors
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) "a" + d
f(10)

message2error <- function(code) {
  withCallingHandlers(code, message = function(e) stop(e))
}
f <- function() g()
g <- function() message("Hi!")
g()
message2error(g())
#usin handlert try(), tryCatch() and withCallinghandlers()
f1 <- function(x){
  log(x)
  10
}
f1("a")

#using try function
f2 <- function(x){
  try(log(x))
  10
  print("trycatch continues to handle the error")
}
f2("c")

try({
  a <- 1
  b <- "x"
  a + b
})
success <- try(1 + 2)
class(success)
failure <- try("a" + "b")
class(failure)
#applying try to a ,list of functions
elements <- list(1:10, c(-1, 10), c(T, F), letters)
elements
results <- lapply(elements, log)
results <- lapply(elements, function(x) try(log(x)))

is.error <- function(x) inherits(x, "try-error")
succeeded <- !sapply(results, is.error)



# look at successful results
str(results[succeeded])
# look at inputs that failed
str(elements[!succeeded])
#trycatch
show_condition <- function(code) {
  tryCatch(code,
           error = function(c) "error",
           warning = function(c) "warning",
           message = function(c) "message"
  )
}
show_condition()
show_condition(stop("!"))
#> [1] "error"
show_condition(warning("?!"))
#> [1] "warning"
show_condition(message("?"))
#> [1] "message"
# If no condition is
try2 <- function(code, silent = FALSE) {
  tryCatch(code, error = function(c) {
    msg <- conditionMessage(c)
    if (!silent) message(c)
    invisible(structure(msg, class = "try-error"))
  })
}
try2(1)
#> [1] 1
try2(stop("Hi"))
try2(stop("Hi"), silent = TRUE)

# Don't let the user interrupt the code
i <- 1
while(i < 3) {
  tryCatch({
    Sys.sleep(0.5)
    message("Try to escape")
  }, interrupt = function(x) {
    message("Try again!")
    i <<- i + 1
  })
}

#With calling handlers
f <- function() stop("!")
tryCatch(f(), error = function(e) 1)
withCallingHandlers(f(), error = function(e) 1)

f <- function() g()
g <- function() h()
h <- function() stop("!")
tryCatch(f(), error = function(e) print(sys.calls()))
withCallingHandlers(f(), error = function(e) print(sys.calls()))

logical_abbr <- function(x) {
  if (is.atomic(x)) {
    FALSE
  } else if (is.name(x)) {
    identical(x, quote(T)) || identical(x, quote(F))
  } else if (is.call(x) || is.pairlist(x)) {
    for (i in seq_along(x)) {
      if (logical_abbr(x[[i]])) return(TRUE)
    }
    FALSE
  } else {
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}
logical_abbr(quote(TRUE))
#> [1] FALSE
logical_abbr(quote(T))
#> [1] TRUE
logical_abbr(quote(mean(x, na.rm = T)))
#> [1] TRUE
logical_abbr(quote(function(x, na.rm = T) FALSE))

find_assign <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    NULL
  } else if (is.call(x)) {
    if (identical(x[[1]], quote(`<-`))) {
      x[[2]]
    } else {
      lapply(x, find_assign)
    }
  } else if (is.pairlist(x)) {
    lapply(x, find_assign)
  } else {
    stop("Don't know how to handle type ", typeof(x),
         call. = FALSE)
  }
}
find_assign(quote(a <- 1))
#> a
find_assign(quote({
  a <- 1
  b <- 2
}))
#> [1] TRUE
unlist(lapply(mtcars,class))

#anotger example
trims <- c(0,0.1,0.2,0.5)
x <- rcauchy(1000)
x
unlist(lapply(trims, function(trim) mean(x, trim = trim)))
for(i in seq_along(runif(20))){
  print(i)}

#use of sapply and vapply
class(mtcars)
sapply(mtcars, is.numeric)
vapply(mtcars, is.numeric, logical(1))
replicate(5, runif(10))

# Generate some sample data
xs <- replicate(5, runif(10), simplify = FALSE)
ws <- replicate(5, rpois(10, 5) + 1, simplify = FALSE)
xs
ws
unlist(lapply(xs, mean))
unlist(Map(weighted.mean, xs,ws))
unlist(Map(weighted.mean, xs, ws))
#parralellisation
lapply3 <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in sample(seq_along(x))) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}
unlist(lapply(1:10, sqrt))
unlist(lapply3(1:10, sqrt))

#plyr package
set.seed(1)
d <- data.frame(year = rep(2000:2002, each=3), count = round(runif(9, 0,20)))
print(d)
library(plyr)
ddply(d, "year", function(x){
                 mean.count <- mean(x$count)+
                 sd.count <- sd(x$count)+
                 cv <- sd.count/mean.count+
                 data.frame(cv.count = cv)
})
plyr::ddply(d, "year", transform, sum.count = sum(count))
ddply(d, "year", summarise, mean.count = mean(count))
ddply(d, "year", transform, total.count = sum(count))
ddply(d, "year", mutate, mu = mean(count), sigma = sd(count),
       cv = sigma/mu)
#parallel processing
wait <- function(i) Sys.sleep(0.1)
> wait
function(i) Sys.sleep(0.1)
> system.time(llply(x, wait))
user  system elapsed 
0.00    0.00    1.03 
 
library(parallel)
parallel::detectCores()
detectCores()
 
pause <- function(i){
         function(x) Sys.sleep(i)
}
system.time(lapply(1:10, pause(0.25)))
system.time(mclapply(1:10, pause(0.25), detectCores()))

#in windows you first have to set up a local clusterdete
core <- detectCores()
cluster <- makePSOCKcluster(core)
cluster
system.time(parApply(cluster, 1:10, function(i)Sys.sleep(0.25)))

replicate(10,5)
rep(10,5)
#usig tapply
pulse <- pulse <- round(rnorm(22, 70, 10 / 3)) + rep(c(0, 5), c(10, 12))
group <- rep(c("A","B"),c(10,12))
pulse
group
tapply(pulse, group, length)
tapply(pulse, group, mean)
split(pulse, group)

tapply2 <- function(x, group,f,...,simplify = TRUE){
        pieces <- split(x,group)
        sapply(pieces,f,simplify = TRUE)
}
tapply2(pulse, group, length)
tapply2(pulse, group, mean)

#file creation
file.create("C:\\Users\\jeffnerd\\Desktop\\jeff.txt", showWarnings = TRUE)
file.exists("jeff.txt")
list.files()

#date class
date.lookup <- format(seq(as.Date("2000-01-01"), as.Date("2010-12-31"), 
                          by = "1 day"))
match("2004-01-19", date.lookup)
#date class
# create a date
as.Date("2012-08-30")
## [1] "2012-08-30"
# specify the format
as.Date("08/30/2012", format = "%m/%d/%Y")
# use a different origin, for instance importing values from Excel
as.Date(41149, origin = "1900-01-01")
# take a difference
Sys.Date() - as.Date("1970-01-01")
## Time difference of 15582 days
# alternate method with specified units
difftime(Sys.Date(), as.Date("1970-01-01"), units = "days")
## Time difference of 15582 days
# see the internal integer representation
unclass(Sys.Date())
Sys.time()

#posixct package for date time
unclass(Sys.time())
#as posixlt
unclass(as.POSIXlt(Sys.time()))
# create POSIXct variables
as.POSIXct("080406 10:11", format = "%y%m%d %H:%M")
## [1] "2008-04-06 10:11:00 CDT"
as.POSIXct("2008-04-06 10:11:01 PM", format = "%Y-%m-%d %I:%M:%S %p")
## [1] "2008-04-06 22:11:01 CDT"
as.POSIXct("08/04/06 22:11:00", format = "%m/%d/%y %H:%M:%S")
## [1] "2006-08-04 22:11:00 CDT"
# convert POSIXct variables to character strings
format(as.POSIXct("080406 10:11", format = "%y%m%d %H:%M"), "%m/%d/%Y %I:%M %p")
## [1] "04/06/2008 10:11 AM"
as.character(as.POSIXct("080406 10:11", format = "%y%m%d %H:%M"), 
             format = "%m-%d-%y %H:%M")
## [1] "04-06-08 10:11"

# when do I turn 1 billion seconds old?
billbday <- function(bday, age = 10^9, format = "%Y-%m-%d %H:%M:%S") {
x <- as.POSIXct(bday, format = format) + age
togo <- round(difftime(x, Sys.time(), units = "days"))
if (togo > 0) {
msg <- sprintf("You will be %s seconds old on %s, which is %s days from now.",
               age, format(x, "%Y-%m-%d"), togo)}
  else{
msg <- sprintf("You turned %s seconds old on %s, which was %s days ago.", 
               age, format(x,"%Y-%m-%d"), -1 * togo)
  }                 
if (age > 125 * 365.25 * 86400){
  msg <- paste(msg, "Good luck with that.")
print(msg)
format(x, "%Y-%m-%d")}

}
billbday("1981-04-13 15:00:00")

#melting and casting
library(MASS)
library(reshape2)
library(reshape)
print(head(ships, n=10))

#melt this data
shipdata <- head(ships, n=10)
shipdata
molten.ships <- melt(shipdata, id = c("type", "year"))
print(molten.ships)

#casting
recasted.ship <- cast(molten.ships, type+year~variable, sum)
print(recasted.ship)

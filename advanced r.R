+#advanced R revision
#Atomic vectors.They are logical,integer,double(called numeric) and character
dbl_var <- c(1,2.5,4.5)#double
int_var <- c(1L, 6L,10L)
log_var <- c(TRUE, FALSE, T, F)
chr_var <- c("these are","some strings" )
#NA are missing values.NA_real_ (a double vector),NA_integer_ and NA_character_.
typeof(int_var)
typeof(chr_var)
is.integer(int_var)
is.atomic(int_var)
is.double(dbl_var)
#coercion. all items in an atomic vector must be of the same type
#otherwise they will be coerced into the same data type
str(c("a",1))
x <- c(FALSE,FALSE,TRUE)
as.numeric(x)
#other coercion items as.character(), as.double(), as.integer(),
#or as.logical().
x <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
str(x)
x <- list(list(1, 2), c(3, 4))
y <- c(list(1, 2), c(3, 4))
str(x)
str(y)
#Dataframe ad linear models of regression are lists
is.list(mtcars)
mod <- lm(mpg ~ wt, data = mtcars)
is.list(mod)
#applying functions
sapply(mtcars, is.numeric) #applied in atomic vectors
vapply(mtcars, is.numeric, logical(1) )#applied in atomic vectors
sapply(list(), is.numeric)
vapply(list(), is.numeric, logical(1))

sapply2 <- function(x, f, ...) {
  res <- lapply2(x, f, ...)
  simplify2array(res)
}
sapply2
Map(function(x, w) weighted.mean(x, w, na.rm = TRUE), xs, ws)
#
#apply(), sweep(), and outer() with work with matrices.
#tapply() summarises a vector by groups defined by another vector.
#the plyr package, which generalises tapply() to make it easy to work
#with data frames, lists, or arrays as inputs, and data frames, lists, or
#arrays as outputs.
#xcreating matrix
x <- matrix(9, nrow = 3, ncol = 3)
x
rownames(x)<- c("a","b","c")
colnames(x) <- c("a","b","c")
x
z <- c(1,2,3,4)
y <- c(10,11,12,13)
cbind(z,y)
rbind(z,y)
#constructing arrays
b <- array(1:18, dim = c(3,3,2))
b
dimnames(b) = list(c("a","b","c"), c("a","b","c"),c("A","B"))
b

p1 = 0.14
p2 = 0.12
n =214
 z = p1-p2/sqrt(p2*(1-p2)/n)
 z
 qnorm(1-0.05)#11.644854
 pnorm(z)
 
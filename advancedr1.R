#advanced r
#vectors: atomic vectors and lists
#atomic vectors: logical, interger, double and character
dbl_var <- c(1,2.5,4.5)
int_var <- c(1L, 6L, 10L)
log_var <- c(TRUE, FALSE, T, F)
chr_var <- c('These are', 'some strings')
#atomic vectors are flat
c(1,2,3,4)
c(1,c(2),c(3,4))
#types and tests
typeof(dbl_var)
typeof(int_var)
typeof(log_var)
typeof(chr_var)
is.integer(dbl_var)
is.integer(int_var)
is.atomic(dbl_var)
is.logical(log_var)
is.numeric(int_var)
is.numeric(dbl_var)
#coercion
str(c("a",1))
x <- c(FALSE, FALSE,TRUE)
as.numeric(x)
mean(x)
sum(x)

#lists
x <- list(1:3, "a", c(TRUE, FALSE,TRUE), c(2.3,5.9))
str(x)
is.list(x)
x
x[[1]]
x <- list(list(list(list())))
str(x)
is.recursive(x)
x <- list(list(1,2), c(3,4))
str(x)
y <- c(list(1,2), c(3,4))
str(y)       
#building on lists
is.list(mtcars)
mod <- lm(mpg~wt, data = mtcars)
is.list(mod)
#attributes
y <- 1:10
attr(y, "my attribute") <- "This is a vector"
attr(y, "my attribute")
#accesing all at once as a list
str(attributes(y))
#structure is used gto create a new object with mofified attributes
structure(1:10, my_attribute="This is a vector")
#attributes are lost except for names, dimensions and class
attributes(y[1])
attributes(sum(y))
#names
x <- c(a=1, b=2, c=3)
x
names(x)
#option 2 in creating names
x <- 1:3; names(x) <- c("a", "b", "c")
x
#alrenative three
x <- setNames(1:3, c("a", "b", "c"))
x
#factors
x<- factor(c("a", "b","b","a"))
x
class(x)
levels(x)
x[2] <- "c"
#you cant combine factors
c(factor("a"), factor("b"))
#setting levels
sex_char <- c("m", "m", "m")
sex_factor <- factor(sex_char, levels = c("m", "f"))
table(sex_char)
table(sex_factor)
#reading texxt or files
z <- read.csv(text = "value\n12\n1\n.\n9")
typeof(z$value)
as.double(z$value)
class(z$value)
as.double(as.character(z$value))
z <- read.csv(text = "value\n12\n1\n.\n9",na.strings = ".")
typeof(z$value)
class(z$value)

#making arrays and matrices
a <- matrix(1:6, ncol = 3, nrow = 2)
#one vector argument to describe all dimensions
b <- array(1:12, c(2,3,2))
a
b
#we can modify by also passing a dim in place
c <- 1:6
dim(c) <- c(3,2)
c
length(a)
nrow(a)
ncol(a)
rownames(a) <- c("A", "B")
colnames(a) <- c("a", "b", "c")
a
length(b)
dim(b)
dimnames(b) <- list(c("one", "two"), c("a", "b", "c"), c("A","B"))
b
#determining the one d staructure
str(1:3)
str(matrix(1:3, ncol = 1))
str(array(1:3, 3))
l <- list(1:3, "a",TRUE, 1.0)
dim(l) <- c(2,2)
l
#dataframes
df <- data.frame(x=1:3, y=c("a", "b", "c"),stringsAsFactors = FALSE)
df
class(df)
typeof(df)
is.data.frame(df)
#binding data frames
cbind(df, data.frame(z=3:1))
rbind(df, data.frame(x=10,y="z"))
df$f <- list(1:2,1:3,1:4)
df
#alternatively use fucntioni(i) to make them look as if they are one
df1 <- data.frame(x=1:3, y = I(list(1:2, 1:3,1:4)))
df1
#subsetting atomics lists arrays matrices and dataframes
#atomice vectors-positive integers, negative integers, logics, nothing and zero
x <- c(2.1, 4.2, 3.3, 5.4)
x[2.1]#preserving subsettng
x[[2.1]]#simplfying subsetting
x[c(3,1)]
x[order(x)]
x[c(1,1)]
x[c(2.1,2.9)]#4.2,4.2
#negative integers
x[-c(3,1)]
#logical vectors
x[c(TRUE, TRUE,FALSE,FALSE)]
x[c(T,F)]
#Nothing
x[]#gives the original frame
#zero
x[0]
x[x>2]
#for character namesuse vector of names
(y <- setNames(x, letters[1:4]))#runs directly
y <- setNames(x, letters[1:4])#doesnt run directly
y
y[c("d", "c","a")]
y[c("a","a","a")]
#for a matrix
(a <- matrix(1:9, nrow = 3))
colnames(a) <- c("A","B","C")
a
a[1:2,]
a[c(T,F,T), c("B", "A")]
a[0,-2]
#for arrays
(val <- outer(1:5, 1:5, FUN = "paste", sep=","))
val[c(4,5)]
#Dataframe
df <- data.frame(x=1:3, y=3:1, z=letters[1:3])
df
df[df$x==2,]#row 2
df$x
df[c(1,3),]
#ways of selecting columns in a dataframe
df[c("x","y")]#will act as a list
df[,c("x","y")]
df["x"]
#preserving subsetting
str(df[,"x", drop = F])
#list
a <- list(a=1, b=2)
a["a"]
a[["a"]]
a$a
a[[1]]
#out of bound/ oop
x <- 1:4
x[5]#NA
x[[5]]#Error
#application subsetting
x <- c("m","f","u","f","m","m")
lookup <- c(m = "male",f = "female", u = NA)
lookup[x]
unname(lookup[x])

#matching with match
grades <- c(1,2,2,3,1)
info <- data.frame(
            grade = 3:1,
            desc = c("Excellent", "Good", "Poor"),
            fail = c(F, F, T)
            )
id <- match(grades, info$grade)
info[id,]
#paste -- concatenates vectors
paste(1:12)
paste0(1:12)
as.character(1:12)
rep("th", 6)
replicate(6, "th")

(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))
paste0(nth, collapse = ",")
paste("1st", "2nd", "3rd", sep = ",", collapse = ":")
#sampling subsetting
df <- data.frame(x=rep(1:3, each=2), y=6:1,z=letters[1:6])
df
df[sample(nrow(df)),]
df[sample(nrow(df),3),]
df[sample(nrow(df),6,rep=T),]
#ordering
x <- c("b","c","a")
order(x)
x[order(x)]
#for 2 dimensional data
df2 <- df[sample(nrow(df)),3:1]
df2
df2[order(df2$x),]
df2[,order(names(df2))]
#removing columns from a dataframe
df
df$z <- NULL
df[c("x","y")]
df[setdiff(names(df), "z")]
#selecting rows based on logical subsetting or condition
mtcars[mtcars$gear == 5,]
mtcars[mtcars$gear == 5 & mtcars$cyl == 4,]
#subset
subset(mtcars, gear==5)
subset(mtcars, gear == 5 & cyl == 4)

#set and integer subsetting
x <- sample(10) < 4
x
which(x)
(x1 <- 1:10 %% 2 == 0)
(x2 <- which(x1))
(y1 <- 1:10 %% 5 == 0)
(y2 <- which(y1))
#union
x2 | y2
union(x2, y2)
x2 & y2
intersect(x2,y2)
#setdiff
setdiff(x2, y2)
setequal(x2, y2)
setequal(1:5,1:5)

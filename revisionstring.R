#revision character
head(USArrests)
states <- rownames(USArrests)
#abbreviate state names
states2 <- abbreviate(states)
names(states2) <- NULL
states2

#abbreviate state names with 5 letters
abbreviate(states, minlength = 5)

#getting the longest names
state_chars <- nchar(states)
state_chars
#largest names
states[which(state_chars == max(state_chars))]

#slecting states or patterns
#one which has 'k'
grep(pattern = "w", x = states, value = TRUE)
grep(pattern = "k", x = states, value = TRUE)
grep(pattern = "[wW]", x = states, value = TRUE)
grep(pattern = "w", x = tolower(states), value = TRUE)
grep(pattern = "W", x = toupper(states), value = TRUE)
grep(pattern = "w", x = states, value = TRUE, ignore.case = TRUE)

#regular expression using grexpres
position_a <- gregexpr(pattern = "a", text = states, ignore.case = TRUE)
position_a


#ow many a's
num_a <- sapply(position_a, function(x) ifelse(x[1]>0, length(x), 0))
num_a

#library stringr
library(stringr)
str_count(states, "a")
str_count(tolower(states), "a")

#this can be done for many vowels
vowels  = c("a", "e", "i", "o", "u")
#vector for storing results
num_vowels <- vector(mode = "integer", length = 5)

#calculate number of vowels in each name
for(j in seq_along(vowels)){
  num_aux = str_count(tolower(states),  vowels[j])
  num_vowels[j] = sum(num_aux)
}

#add name of vowels
names(num_vowels) = vowels
#total  number of vowels
num_vowels
#sorting them 
sort(num_vowels, decreasing = TRUE)
#paste functions
# paste
PI = paste("The life of", pi)
PI
IloveR = paste("I", "love", "R", sep = "-")
IloveR
# paste with objects of different lengths
paste("X", 1:5, sep = ".")
# paste with collapsing
paste(1:3, c("!", "?", "+"), sep = "", collapse = "")
## [1] "1!2?3+"
# paste without collapsing
paste(1:3, c("!", "?", "+"), sep = "")
## [1] "1!" "2?" "3+"
# collapsing with paste0
paste0("lets", "collapse", "all", "these", "words")
## [1] "letscollapseallthesewords"

#printing function
my_string <- "programming with data is fun"
print(my_string)
#printing without quotes
print(my_string, quote = FALSE)

#unquoted characters
noquote(my_string)
#class quote
no_quotes = noquote(c("some", "quoted", "txt","!%$"))
no_quotes

#concatenating
cat(my_string)
# concatenate and print
cat(my_string, "with R")
# especifying 
cat(my_string, "with R", sep = " =) ")
# another example
cat(1:10, sep = "-")
# first four months
cat(month.name[1:4], sep = " ")
# fill = 30
cat("Loooooooooong strings", "can be displayed", "in a nice format",
    "by using the 
    fill
    argument", fill = 30)
#fromat
format(13)
format(c(6,13.1))
#sprintf
sprintf("%f",pi)
sprintf("%1.0f", pi)
#converting objects to string
toString(17.04)
toString(c("one", "two",33333), width = 8)
#nchar
nchar(c("hello","world"))
length(c("hello","world"))
#toLower
tolower(c("Hello WORLD GOE"))
#toupper
toupper(c("hello world word"))
#using casefold
casefold(c("hello world"))
casefold(c("hello world"), upper = TRUE)
#character translator
chartr("a","b","hullo")
#abbreviate
some_colors = colors()[1:4]
some_colors
abbreviate(some_colors)
abbreviate(some_colors,minlength=3)
#replacing substrings
substr("abcdef",2,4)#bcd
x <-c("max","the", "force","be","with","you")
substr(x,2,2) <- "#"
x
#msustituting by replacement
substr(x,2,3) <- c("#","@")
x
#union
set1 = c("some", "random","words","some")
set2 = c("some", "many", "name", "few")
union(set1,set2)
#intersect
intersect(set1,set2)

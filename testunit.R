#debugging a function
test_debug <- function(x,y){
  print(environment())
  n <- x + y
  q <- n + 2
  w <- q + "b"
  print(w)
}
test_debug(5,4)
#the first mode of debugging. traceback
traceback()
#second one
options(error = recover)
options(error = browser)
test_debug()
options(error = NULL)
#using debug
debug(test_debug)
test_debug()
undebug()

#traceback
trace(test_debug, edit = TRUE)
test_debug <- function(x,y){
  n <- x + y
  q <- n + 2
  w <- q + "b"
  print(w)
}

test_debug()
debug(test_debug)
test_debug(5,4)
undebug()

#unit tests
devtools::use_t

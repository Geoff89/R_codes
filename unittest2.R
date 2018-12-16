#test unit one
library(devtools)
library(stringr)
library(testthat)


context("String length")
test_that("str_length is a number of characters",{
  expect_equal(str_length("a"),1)
  expect_equal(str_length("ab"),2)
  expect_equal(str_length("abc"),3)
})

test_that("str_length of factor is length level",{
  expect_equal(str_length(factor("a")),1)
  expect_equal(str_length(factor("ab")),2)
  expect_equal(str_length(factor("abc")),3)
})

test_that("str_length of  issing values",{
  expect_equal(str_length(NA),NA_integer_)
  expect_equal(str_length(c(NA,1)),c(NA,1))
  expect_equal(str_length("NA"),2)
})

#other way s interactively rather than using exects in test
expect_equal(10,10)
expect_equal(10, 10+1e-7)
expect_equal(10,11)
expect_identical(10, 10+1e-7)

#for matching
string <- "Testing is fun"
expect_match(string, "Testing")
expect_match(string, "testing")
expect_match(string, "is")

#additional arguments
expect_match(string, "teting", ignore.case = TRUE)
#there are variants of expect_match
#expect_output, expect_warning, expect_error, expect_message
a <- list(1:10, letters)
expect_output(str(a), "List of 2")
expect_output(str(a), "int [1:10]", fixed = TRUE)
expect_message(library(mgcv), "This is mgcv")
expect_warning(log(-1))
expect_error(1/"a")
#we can roduce messages
expect_warning(log(-1), "NaNs produced")
expect_error(1/"a", "non-numeric argument")

#expect is choses an object that inherits to
model <- lm(mpg~wt, data = mtcars)
expect_is(model, "lm")
expect_is(model, "glm")

#expect true and expect false
expect_true(2 == 2)
expect_true(2 != 2)
expect_false(2 != 2)

## Not run: 
expect_silent(f())

tmp <- tempfile()
# The first run always succeeds
expect_known_output(mtcars[1:10, ], tmp, print = TRUE)
# Subsequent runs will suceed only if the file is unchanged
# This will succeed:
expect_known_output(mtcars[1:10, ], tmp, print = TRUE)
## Not run: 
# This will fail
expect_known_output(mtcars[1:9, ], tmp, print = TRUE)

f <- factor("a")
expect_is(f, "factor")
expect_s3_class(f, "factor")
expect_type(f, "integer")
a <- 9
expect_lt(a, 10)
#not run
expect_lt(11, 10)
expect_gt(11,10)
#not run
expect_gt(9,10)

library(lubridate)
test_that("floor_date works for different units", {
  base <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")
  expect_equal(floor_date(base, "second"),
               as.POSIXct("2009-08-03 12:01:59", tz = "UTC"))
  expect_equal(floor_date(base, "minute"),
               as.POSIXct("2009-08-03 12:01:00", tz = "UTC"))
  expect_equal(floor_date(base, "hour"),
               as.POSIXct("2009-08-03 12:00:00", tz = "UTC"))
  expect_equal(floor_date(base, "day"),
               as.POSIXct("2009-08-03 00:00:00", tz = "UTC"))
  expect_equal(floor_date(base, "week"),
               as.POSIXct("2009-08-02 00:00:00", tz = "UTC"))
  expect_equal(floor_date(base, "month"),
               as.POSIXct("2009-08-01 00:00:00", tz = "UTC"))
  expect_equal(floor_date(base, "year"),
               as.POSIXct("2009-01-01 00:00:00", tz = "UTC"))
})
#alternativelyyou can shorten expect equals to be on the same line
#by use of helper fucntions
test_that("floor dateworks with different functions",{
  base <- as.POSIXct("2009-08-03 12:01:59.23", tz = "UTC")
  floor_base <- function(unit) floor_date(base, unit)
  as_time <- function(x) as.POSIXct(x, tz = "UTC")
  
  expect_equal(floor_base("second"), as_time("2009-08-03 12:01:59"))
  expect_equal(floor_base("minute"), as_time("2009-08-03 12:01:00"))
  expect_equal(floor_base("hour"), as_time("2009-08-03 12:00:00"))
  expect_equal(floor_base("day"), as_time("2009-08-03 00:00:00"))
  expect_equal(floor_base("week"), as_time("2009-08-02 00:00:00"))
  expect_equal(floor_base("month"), as_time("2009-08-01 00:00:00"))
  expect_equal(floor_base("year"), as_time("2009-01-01 00:00:00"))
})

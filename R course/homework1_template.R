## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Advanced Data Analysis Software Development with R - Batch 2
## Homework 1
##
## Student:      Mierzwa Olga
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# All the blocks should be kept as-is,
# do not modify the structure of special "## ---" comments.
# This file will be pre-processed automatically.
#
# Solutions non conforming to this very template will not be checked.
#
#
# Before submitting your homework, clear RStudio's workspace
# (e.g. by calling `rm(list=ls(all=TRUE))`) and source() (CTRL+SHIFT+S)
# this script to make sure everything is OK with your code.


## ------------------------ Exercise 01.01 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# ... write at least one paragraph about what the function does ...
#
# ARGUMENTS
# ... document each of the function's arguments ...
#
# RETURN VALUE
# ... describe what kind of object the function returns ...

## ---- Function ----

is.natural<- function(x, tol = .Machine$double.eps^0.5){
  x > tol & abs(x - round(x)) < tol
}  

# function to calculate winsorized mean
winsor <- function(x, k){
  #check conditions
  stopifnot(is.vector(x), is.numeric(k)) 
  stopifnot(is.natural(k))
  len <- length(x)
  stopifnot(k <= ((len-1)/2))
  #sort
  x <- sort(x)
  smallest <- x[k+1]
  largest <- x[len-k]
  begin <- rep(smallest,k)
  end <- rep(largest,k)
  #calculate mean
  mean(c(begin, x[(k+1):(len-k)], end)) 
}

## ---- Examples ----
#unit test
library('testthat')
expect_that(winsor(1:10,2), is_a("numeric"))
expect_equal(winsor(1:10,3), 5.5)
expect_error(winsor(1:10,1))
expect_error(winsor(1,0))
expect_error(winsor(rep(1,100),0.8))
expect_error(winsor(rep(1,100),-1))
expect_error(winsor(rep(1,100),50))
expect_error(winsor(rep(1,100)))
expect_error(winsor(c(),1))
expect_identical(winsor(c(1, 2, 3, Inf, Inf), 1), Inf)
expect_identical(winsor(c(NA, 2, NA, Inf, Inf), 1), as.numeric(NA))
expect_that(winsor(c(NA, 2, NA, Inf, Inf), 1), is_a("numeric"))

# examples
do.call(winsor, list(x = rep(1,100), k = 3))
do.call(winsor, list(x = rnorm(10), k = 3))
do.call(winsor, list(x = c(NA,1,2), k = 3))
do.call(winsor, list(x = c(NA, NA, NA, NA), k = 0.5))
do.call(winsor, list(x = seq(1,100,3), k = 10))
do.call(winsor, list(x = rnorm(1000000), k = -6))
do.call(winsor, list(x = rnorm(1000000), k = 6))
do.call(winsor, list(x = rnorm(1000000), k = 'one'))
do.call(winsor, list(x = c(1, 2, 3, Inf, Inf), k = 1))
do.call(winsor, list(x = c(1, NaN, NaN, Inf, Inf), k = 1))
do.call(winsor, list(x = c(NA, 2, NA, Inf, Inf), k = 1))
do.call(winsor, list(x = 1, k = 0))

## ------------------------ Exercise 01.02 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# This function changes the sign of each element in a given numeric vector
#
# ARGUMENTS
# x - a numeric vector
#
# RETURN VALUE
# a numeric vector, -x

## ---- Function ----

chgsgn <- function(x) # Just an example, TO DO: DEL ME
{
  stopifnot(is.numeric(x))
  -x
}

## ---- Examples ----

# Just a bunch of examples, TO DO: DEL ME
# tests:
library(testthat)
expect_identical(chgsgn(numeric(0)), numeric(0))
expect_error(chgsgn(mean))
expect_equivalent(chgsgn(c(-1,2,3)), c(1,-2,-3))
expect_equivalent(chgsgn(c(-1,2,NA)), c(1,-2,NA))
test <- rnorm(10)
expect_equivalent(chgsgn(test), -test)
# ... (at least 5 more testthat tests...)

# examples:
chgsgn(rnorm(10)) # some random data



## ------------------------ Exercise 01.03 ----------------------------

## ---- Documentation ----

# I don't know how to solve this exercise. :(

## ---- Function ----

# I don't know how to solve this exercise. :(

## ---- Examples ----

# I don't know how to solve this exercise. :(




## ------------------------ Exercise 01.04 ----------------------------

## ---- Documentation ----

# ... TO DO ...



## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...









## ------------------------ Exercise 01.05 ----------------------------

## ---- Documentation ----

# ... TO DO ...



## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...









## ------------------------ Exercise 01.06 ----------------------------

## ---- Documentation ----

# ... TO DO ...


## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...




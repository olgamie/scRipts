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
# Function calculates winsorized mean.
#
# ARGUMENTS
# x - numeric vector, k - elements to replace
#
# RETURN VALUE
# winsor returns a winsorized mean if the values in x is computed as numeric vector of length one. If x contains NA
# they are put at the begining of the vector when sorting x values in ascending order.

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
  x <- sort(x, na.last = F)
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
# This function replaces each missing value in a given numeric vector
# with an arithmetic mean of its preceding and following element
#
# ARGUMENTS
# x - a numeric vector
#
# RETURN VALUE
# a numeric vector

## ---- Function ----
replacena <- function(x){
  stopifnot(is.numeric(x))
  pos <- which(is.na(x)==T)
  res <- sapply(pos, function(i) (x[i-1]+x[i+1])/2)
  stopifnot(length(which(is.na(res)))==0)
  x[pos] <- res
  as.numeric(x)
}

## ---- Examples ----
# tests:
library(testthat)
expect_identical(replacena(numeric(0)), numeric(0))
expect_error(replacena(mean))
expect_equivalent(replacena(c(-1,NA,3)), c(-1,1,3))
expect_error(replacena(c(-1,2,NA)))
test <- c(1,2,3,NA,7,9,NA,100,30,NA,60)
expect_equivalent(replacena(test), c( 1, 2, 3, 5, 7, 9, 54.5, 100, 30, 45, 60))
test2 <- c(1, NA, Inf)
expect_equivalent(replacena(test2), c(1,Inf,Inf))
test3 <- c()
expect_error(replacena(test3))
test4 <- rep(NA, 10)
expect_error(replacena(test4))
test5 <- rnorm(100)
expect_identical(replacena(test5),test5)
# examples:
vec <- c(NA, NA, 2, 3)
replacena(vec) # some random data



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




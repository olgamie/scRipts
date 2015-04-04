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

# DESCRIPTION
# function neighboreq() determines the set of all the indices i such that x_i = x_{i+1}.
#
# ARGUMENTS
# x - atomic vector
#
# RETURN VALUE
# integer vector of idices which fullfil x_i = x_{i+1}

## ---- Function ----

neighboreq <- function(x){
  stopifnot(is.atomic(x))
  len <- length(x)
  xL <- x[-len]
  xF <- x[-1]
  which(xL == xF)
}

## ---- Examples ----
#unit tests
test1 <- rep(c(TRUE, FALSE), 4)
expect_identical(neighboreq(test1),integer(0))

test2 <- rep(c(TRUE, TRUE), 4)
expect_identical(neighboreq(test2), c(1:7))

test3 <- c(NA, NA, NA)
expect_identical(neighboreq(test3), integer(0))

test4 <- c(NA, NA, 2,  2)
expect_equal(neighboreq(test4), 3)

test5 <- list(a = 1)
expect_error(neighboreq(test5))

expect_error(neighboreq(mean))

test6 <- as.data.frame(matrix(1, 2, 2))
expect_error(neighboreq(test6))

test7 <- c(Inf, Inf)
expect_equal(neighboreq(test7), 1)

expect_that(neighboreq(test7), is_a("integer"))
expect_that(neighboreq(test2), is_a("integer"))

#examples
neighboreq(c(Inf, Inf, -Inf))
neighboreq(c('apple', 'car', 'car', 'car', 'apple'))
neighboreq(rep(c(T,F,T), 3))
neighboreq(rep(c(2L,1L,1L), 3))
neighboreq(rep(c(1,2,2,3,1), 3))
neighboreq(rep(c(1,NA,NA,3,1), 3))
neighboreq(matrix(1,2,3))
neighboreq(list(a = 1, b =3, c ="car"))
neighboreq(rnorm(100))
neighboreq(sample(c(0,1), 100, replace = TRUE))

## ------------------------ Exercise 01.04 ----------------------------

## ---- Documentation ----
# DESCRIPTION
# sample2() generates a random sample (with replacement) consisting of
# k elements in a given vector x
#
# ARGUMENTS
# x - atomic vector
#
# RETURN VALUE
# vector of length k

## ---- Function ----

sample2 <- function(x, k){
  stopifnot(k>=0, is.atomic(x), length(x) >= 1L)
  len = length(x)
  indice <- round(runif(k, 1, len))
  x[indice]
}

## ---- Examples ----
#unit test
vec1 <- rep(c(1,2,3), 10)
expect_error(sample2(vec1))
expect_error(sample2(mean, 1))
expect_error(sample2(vec1, -1))
expect_equal(length(sample2(vec1, 10)), 10)
expect_that(sample2(vec1, 10), is_a("numeric"))
expect_that(sample2(vec1, 0), is_a("numeric"))
expect_identical(sample2(vec1, 0), numeric(0))
vec2 <- rep(c("car", "car", "apple"), 5)
expect_that(sample2(vec2, 10), is_a("character"))
vec3 <- rep(c(F, F, T), 5)
expect_that(sample2(vec3, 10), is_a("logical"))
vec4 <- rep(c(F, NA, T), 5)
expect_that(sample2(vec4, 10), is_a("logical"))
vec5 <- c(1,1,1)
expect_equal(sample2(vec5, 1), 1)
expect_error(sample2(list(a =1 ), 1))
vec6 <- c(1L, 1L, 1L)
expect_equal(sample2(vec6, 1), 1)
expect_that(sample2(vec6, 10), is_a('integer'))

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




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
# they are removed.

## ---- Function ----

is.natural<- function(x, tol = .Machine$double.eps^0.5){
  x > tol & abs(x - round(x)) < tol
}  

# function to calculate winsorized mean
winsor <- function(x, k){
  #check conditions
  stopifnot(is.finite(x), is.numeric(k)) 
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
expect_error(winsor(1:10,0))
expect_error(winsor(1,0))
expect_error(winsor(rep(1,100),0.8))
expect_error(winsor(rep(1,100),-1))
expect_error(winsor(rep(1,100),50))
expect_error(winsor(rep(1,100)))
expect_error(winsor(c(),1))
expect_error(winsor(c(1, 2, 3, Inf, Inf), 1))
expect_error(winsor(c(NA, 2, NA, Inf, Inf), 1))

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
  #check conditions
  stopifnot(is.numeric(x))
  stopifnot(!is.na(x[1]), !is.na(x[length(x)]))  
  pos <- which(is.na(x)==T)
  res <- sapply(pos, function(i) (x[i-1]+x[i+1])/2)  
  stopifnot(length(which(is.na(res)))==0)
  x[pos] <- res
  as.numeric(x)
}

## ---- Examples ----
# tests:
library(testthat)
expect_error(replacena(numeric(0)))
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
vec <- c(NA, NA, 2, 3)
expect_error(replacena(vec))
vec2 <- c(NA, 1, 2, 3)
expect_error(replacena(vec2))
vec3 <- c(1, 1, 2, NA)
expect_error(replacena(vec3))
vec4 <- c(NA, 1, 1, 2, NA)
expect_error(replacena(vec4))

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
  stopifnot(k>=0, is.atomic(x))
  stopifnot(!(length(x) == 0 & k > 0))
  len = length(x)
  indice <- round(runif(k, 1, len))
  x[indice]
}

## ---- Examples ----
#unit test
expect_error(sample2(numeric(0),1))
expect_equal(sample2(numeric(0),0), numeric(0))
expect_equal(sample2(c(),0), NULL)
expect_equal(sample2(integer(0),0), integer(0))
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
# DESCRIPTION
# merge_string_list() joins each string in a given list of character
# vectors x into a single string.
#
# ARGUMENTS
# x - list of character vectors
# sep - a character string to seperate joined strings. empty by default
#
# RETURN VALUE
# character string of joined strings from list

## ---- Function ----

merge_string_list <- function(x, sep = ''){
  stopifnot(is.list(x), is.character(sep))
  stopifnot(sapply(x, function(i){is.character(i)}))
  paste(unlist(x), collapse = sep)
}

## ---- Examples ----
x <- list(l1=c("a","b","c"), l2=c("b","d"))
x2 <- list(l1="c", l2=c("b","d"))
x3 <- list(l1=c(1,2), l2=c("b","d"))
x4 <- list(l1=c("car", "apple"), l2=c(1,2,3))
x5 <- list(l1=c(NA, "apple"), l2=c("1","2","3"))
expect_identical(merge_string_list(x2, sep=""), "cbd")
expect_error(merge_string_list(x2, 1))
expect_error(merge_string_list(x3))
expect_error(merge_string_list(x4))
expect_error(merge_string_list(mean))
expect_error(merge_string_list(c(1,2,3)))
expect_that(merge_string_list(x2, sep=""), is_a('character'))
expect_identical(merge_string_list(x5, sep=""), "NAapple123")
expect_error(merge_string_list(list()))
expect_error(merge_string_list(list()))
expect_identical(merge_string_list(list(a=character(0))), "")

## ------------------------ Exercise 01.06 ----------------------------

## ---- Documentation ----

# ... TO DO ...


## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...




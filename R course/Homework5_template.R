## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Advanced Data Analysis Software Development with R - Batch 2
## Homework 5
##
## Student:       YourSurnameHere YourNameHere
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# All the blocks should be kept as-is,
# do not modify the structure of special "## ---" comments.
# This file will be pre-processed automatically.
#
# Solutions non confirming to this very template will not be checked.
#
#
# Before submitting your homework, clear RStudio's workspace
# (e.g. by calling `rm(list=ls(all=TRUE))`) and source() (CTRL+SHIFT+S)
# this script to make sure everything is OK with your code.


## ------------------------ Exercise 05.01 ----------------------------

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
library(Rcpp)

Rcpp::sourceCpp(code='
                #include<Rcpp.h>
                using namespace Rcpp ;
                
                //[[Rcpp::export]]
                IntegerVector order_c(NumericVector x) {
                NumericVector sorted = clone(x).sort();
                return match(sorted, x);
                }
                
                //[[Rcpp::export]]
                bool comonotonic(const NumericVector x, const NumericVector y) {
                
                if (is_true(any(duplicated(x)))) stop("duplicates in vector x");
                if (is_true(any(duplicated(y)))) stop("duplicates in vector y");
                if (is_true(any(is_na(x)))) stop("NAs in vector x");
                if (is_true(any(is_na(y)))) stop("NAs is vector y");
                
                int s1 = x.size();
                int s2 = y.size();
                if (s1 != s2) stop("x y are of different length");
                
                IntegerVector sortedx = order_c(x);
                
                for (int i = 0; i < (s1-1.0); ++i) {
                if ((x[(sortedx[i]-1.0)]-x[sortedx[i]])*(y[(sortedx[i]-1.0)]-y[sortedx[i]]) < 0.0) return false;
                }
                return true;
                
                }')

## ---- Examples ----
library(testthat)

expect_error(comonotonic(1, NA))
expect_error(comonotonic(1:10, 10))
expect_error(comotonic(1:2, rep(0,2)))
expect_error(comonotonic(c(1:50, NA), 1:51))
expect_error(comonotonic(c(1:50, "1"), 1:51))
expect_identical(comonotonic(1:50, 51:100), TRUE)
expect_identical(comonotonic(1:50, 1:50), TRUE)
expect_identical(comonotonic(1:50, 100:51), FALSE)
expect_identical(comonotonic(1, 1), TRUE)
expect_identical(comonotonic(numeric(0), numeric(0)), TRUE)

## ------------------------ Exercise 05.02 ----------------------------

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

Rcpp::cppFunction('
                  NumericVector sortedmerge(const NumericVector x, const NumericVector y) {
                  
                  if(is_true(any(is_na(x)))) stop("NAs in vector x");
                  if(is_true(any(is_na(y)))) stop("NAs is vector y");
                  
                  int s1 = x.size();
                  int s2 = y.size();
                  
                  if(s1 == 0) stop("x is of length 0");
                  if(s2 == 0) stop("y is of length 0");
                  
                  NumericVector merged(s1+s2);
                  
                  bool state1 = is_true(all(diff(x)<=0)) & is_true(all(diff(y)<=0));
                  bool state2 = is_true(all(diff(x)>=0)) & is_true(all(diff(y)>=0));
                  
                  
                  if (!state1 & !state2) {
                  stop("Vectors are in different order");
                  }
                  
                  for (int i = 0, j = 0, k = 0; k < merged.size(); k++) {
                  if (i < s1 & j < s2) {
                  if ((x[i] > y[j] & state1) | (x[i] < y[j] & state2)) {
                  merged[k] = x[i];
                  ++i;
                  } else {
                  merged[k] = y[j];
                  ++j;
                  }
                  } else if(i < s1) {
                  merged[k] = x[i];
                  ++i;
                  } else {
                  merged[k] = y[j];
                  ++j;
                  }
                  }
                  return merged;
                  }
                  ')

## ---- Examples ----

# tests:
library(testthat)
x <- c(1,2,3)
y <- c(1,2,1)
expect_error(sortedmerge(x,y))
expect_error(sortedmerge(c(1,2),c("a","b")))
expect_error(soredmerge(c(1,2,NA), c(1,2)))
expect_error(sortedmerge(numeric(0), numeric(0)))
expect_error(sortedmerge(c(1,2,3), c(3,2,1)))
expect_is(sortedmerge(c(1,2,3), c(1,2,3)), "numeric")
expect_equal(sortedmerge(c(1,2,3), c(1,2,3)), c(1,1,2,2,3,3))
expect_equal(sortedmerge(c(1,1,1), c(1,2,3)), c(1,1,1,1,2,3))
expect_equal(sortedmerge(c(1), c(1,2,3)), c(1,1,2,3))
expect_equal(sortedmerge(1,1), c(1,1))

## ------------------------ Exercise 05.03 ----------------------------

## ---- Documentation ----

# I don't know how to solve this exercise. :(

## ---- Function ----

# I don't know how to solve this exercise. :(

## ---- Examples ----

# I don't know how to solve this exercise. :(




## ------------------------ Exercise 05.04 ----------------------------

## ---- Documentation ----

# ... TO DO ...



## ---- Function ----

Rcpp::cppFunction('
                  NumericVector sample2(const NumericVector x, int k){
                  if(k > x.size()){
                  stop("k is larger than x length");
                  }
                  if(k < 0){
                  stop("k is < 0");
                  }
                  NumericVector copy = Rcpp::clone(x);
                  for(int i = 0; i < k; ++i){
                  int p = (int) Rf_runif(0.0, (copy.size()-i));
                  double perm = copy[i];
                  copy[i] = copy[i+p];
                  copy[i+p] = perm;
                  }
                  NumericVector output(k);
                  for(int i=0; i<k; ++i) output[i] = copy[i];
                  return output;
                  }')


## ---- Examples ----

library(testthat)
x <- c(3,4)
expect_error(sample2(x, -10))
expect_error(sample2(x, 5))
expect_equivalent(sample2(integer(0), 0), numeric(0))
expect_equal(sample2(x, 2), c(4,3))
expect_error(sample2(c("l","k"),2))
expect_error(sample2(x, "o"))
expect_error(sample2(x))
expect_is(sample2(x,2), "numeric")
expect_equal(length(sample2(x, 2)),2)
expect_equal(sample2(rep(0,100), 100), rep(0,100))

## ------------------------ Exercise 05.05 ----------------------------

## ---- Documentation ----

# ... TO DO ...



## ---- Function ----

Rcpp::cppFunction('
                  NumericVector randperm(const NumericVector x){
                  NumericVector copy = Rcpp::clone(x);
                  
                  for(int i = 0; i < copy.size(); ++i){
                  int p = (int) Rf_runif(0.0, (copy.size()-i));
                  double perm = copy[i];
                  copy[i] = copy[i+p];
                  copy[i+p] = perm;
                  }
                  return copy;
                  }
                  ')

## ---- Examples ----
library(testthat)
expect_error(randperm("1"))
expect_error(randperm(Nlogical(0)))
expect_identical(randperm(numeric(0)), numeric(0))
expect_identical(randperm(integer(0)), numeric(0))
expect_identical(rep(0,100), rep(0,100))
expect_equal(length(randperm(1:4)),4)
expect_is(randperm(1:4),"numeric")
set.seed(123)
expect_equivalent(randperm(1:4), c(2,4,3,1))
set.seed(123)
expect_equivalent(randperm(1:10), c(3,9,6,10,4,1,2,5,8,7))
expect_is(randperm(NA),"numeric")


## ------------------------ Exercise 05.06 ----------------------------

## ---- Documentation ----

# ... TO DO ...


## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...




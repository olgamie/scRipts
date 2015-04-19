## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Advanced Data Analysis Software Development with R - Batch 2
## Homework 2
##
## Student:       Mierzwa Olga
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


## ------------------------ Exercise 02.01 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# perms() determines all permutations of elements in a given atomic vector
# x. Return a matrix with n! rows and n columns, where n is the length of x
#
# ARGUMENTS
# x - atomic vector
#
# RETURN VALUE
# matrix with n! rows and n columns, where n is the length of x

## ---- Function ----

perms <- function(x){
  stopifnot(is.atomic(x), is.vector(x))
  len <- length(x)
  if(len < 1 ) 
    return(c())
  else
    do.call(rbind, sapply(1:len, function(i) cbind(perms(x[-i]),x[i]), simplify = FALSE))
}

## ---- Examples ----
library(testthat)
expect_that(perms(c(1,2,3)), is_a("matrix"))
expect_that(perms(c(NA, 2, NA, Inf, Inf)), is_a("matrix"))
expect_equal(perms(integer(0)), c())
expect_equal(perms(logical(0)), c())
expect_equal(perms(c("dog")), as.matrix("dog"))
expect_equal(dim(perms(c(NA, 2, NA, Inf, Inf))), c(120, 5))
expect_error(perms(list(a = 1, b =2)))
expect_error(perms(matrix(0, 2,3)))
expect_equal(perms("1"),matrix("1",1,1))
expect_equal(perms(c(TRUE)),matrix(TRUE,1,1))


## ------------------------ Exercise 02.02 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# is_prime() takes a vector of positive integers x as an argument and
# returns a logical vector r such that ri is TRUE if and only of xi is prime.
#
# ARGUMENTS
# x - vector of positive integers
#
# RETURN VALUE
# logical vector r such that r_{i} is TRUE if and only of x_{i} is prime

## ---- Function ----

is_prime = function(x){
  stopifnot(is.vector(x), is.integer(x), all(x>0))
  if (length(x) == 0) {
    return(integer(0))
  }
  sapply(x, function(i) i == 2L || all(i%% 2L:ceiling(sqrt(i)) != 0))
}

## ---- Examples ----

library(testthat)
expect_equal(is_prime(integer(0)), integer(0))
expect_error(is_prime(logical(0)))
expect_error(is_prime(c(-2L)))
expect_error(is_prime(c(1, 2, 4, 6)))
expect_error(is_prime(c()))

y <- pi * c(-1:1, 10)
expect_error(is_prime(as.integer(y)))
expect_equal(is_prime(c(1L, 2L, 2L)), c(FALSE, TRUE, TRUE))
expect_equal(is_prime(c(1L)), c(FALSE))
expect_equal(is_prime(1L), c(FALSE))
expect_error(is_prime(list(a = 1L)))


## ------------------------ Exercise 02.03 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# generate a sequence consisting of randomly chosen elements in x (with replacement) such
# that Pr(SELECTj = xi) = pi (for all i = 1, . . . , k, j = 1, . . . , n).
#
# ARGUMENTS
# n - a single natural number
# x - a numeric vector of length k (for some k) with unique elements,
# p - a numeric vector of length k such that pi >= 0 and p elements sum to 1. If the elements in p don’t sum up to 1,
# when warning is shown and p is normalized
#
# RETURN VALUE
# a numeric vector of length n

## ---- Function ----


is.natural<- function(x, tol = .Machine$double.eps^0.5){
  x > tol & abs(x - round(x)) < tol
} 

gendiscrete <- function (n, x, p){
  stopifnot(length(n) == 1, is.natural(n))
  stopifnot(is.vector(x), is.numeric(x), length(unique(x)) == length(x), is.finite(x))
  stopifnot(is.vector(p), is.numeric(p), length(p) == length(x), all(p >= 0), is.finite(p))
  len <- length(x)  
  # normalize
  if(sum(p) != 1) {
    warning('vector p will be normalized')
    p <- p/sum(p)
  }
  # cal cumulative sums
  sums <- cumsum(p)
  # generate n random numbers from uniform distribution
  uni <- runif(n)  
  sapply(1:n, function(i) x[(uni[i] > c(0,sums[-len]) & uni[i] <= sums )==T])
}

## ---- Examples ----

library(testthat)
expect_error(gendiscrete(1, 2))
expect_error(gendiscrete(1, 2 , c(2,2)))
expect_error(gendiscrete(1, logical(0) , numeric(0)))
expect_error(gendiscrete(1.5, numeric(0) , numeric(0)))
expect_error(gendiscrete(0, numeric(0) , numeric(0)))
expect_error(gendiscrete(-1, numeric(0) , numeric(0)))
expect_error(gendiscrete(2, c(2,3,4) , c(2,3,4,5)))
expect_error(gendiscrete(2, c(2,3,NA) , c(2,3,4)))
expect_error(gendiscrete(2, c(2,3,1:3) , c(2,3,4)))
expect_error(gendiscrete(2, c(2,3,Inf) , c(2,3,4)))

expect_equal(length(gendiscrete(2, c(2,3,4) , c(2,3,4))), 2)
expect_equal(length(gendiscrete(6, c(2,3,4) , c(2,3,4))), 6)

## ------------------------ Exercise 02.04 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# unwind() transforms a given n × m matrix (with the dimnames attribute
# set) into a data frame
#
# ARGUMENTS
# x - matrix with n row and m columns with dimnames set
# names - column names of the resulting data frame
#
# RETURN VALUE
# data frame with nm rows and three columns. The column names of the resulting data frame are given
# via the 2nd function’s parameter.



## ---- Function ----
unwind <- function(mat, names = c("name1", "name2", "name3")){
  #verify arg types
  stopifnot(is.matrix(mat), is.character(names))
  #verify length of dataframe names
  stopifnot(length(names) == 3)
  #verify matrix dimnames
  stopifnot(!is.null(rownames(mat)), !is.null(colnames(mat)))
  nCol <- ncol(mat)
  collect <- list()
  for (i in 1:nCol){
    dataF <- as.data.frame(mat[,i])
    dataF$col <- colnames(mat)[i]
    dataF$row <- rownames(mat)
    collect[[i]] <- dataF
  }
  dataResult <- do.call(rbind, collect)
  colnames(dataResult) <- names
  rownames(dataResult) <- 1:nrow(dataResult)
  dataResult
}


## ---- Examples ----

library(testthat)
expect_error(unwind(WorldPhones, 1))
expect_error(unwind(WorldPhones, c("a", "b")))
expect_error(unwind(matrix(0, 4,5), c("a","b","c")))
expect_error(unwind(data.frame(matrix(0, 4,5)), c("a","b","c")))
A = matrix( c(2, 4, 3, 1, 5, 7), nrow=2, ncol = 3, byrow = TRUE)
colnames(A) = c("col1", "col2", "col3")
expect_error(unwind(A, c("a","b","c")))
A = matrix( c(2, 4, 3, 1, 5, 7), nrow=2, ncol = 3, byrow = TRUE)
rownames(A) = c("row1", "row2")
expect_error(unwind(A, c("a","b","c")))
expect_equal(unwind(WorldPhones, c("count", "where", "when"))[2,1], 60423)
expect_equal(unwind(WorldPhones, c("count", "where", "when"))[2,2], "N.Amer")
expect_equal(unwind(WorldPhones, c("count", "where", "when"))[2,3], "1956")
expect_equal(unwind(WorldPhones, c("count", "where", "when"))[9,2], "Europe")
expect_equal(unwind(WorldPhones, c("count", "where", "when"))[9,3], "1956")


## ------------------------ Exercise 02.05 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# join() returns a new data frame that is the result of an SQL-like LEFT / RIGHT / FULL / INNER JOIN 
# operation on x and y with respect to a common column named k
#
# ARGUMENTS
# x - a data frame
# y - a data frame
# k - a single character string, a common column name
# type - a string character, one of "left", "right", "full", "inner"
#
# RETURN VALUE
# data frame resuling from from join

## ---- Function ----

join <- function( x, y, k, type){
  stopifnot(is.data.frame(x), is.data.frame(y), is.character(k), length(k)==1, is.character(type)) 
  stopifnot(k %in% colnames(x), k %in% colnames(y))
  stopifnot(type == "left" || type == "right" || type == "full" || type == "inner")
  
  vec1 <- x[,k]
  vec2 <- y[,k]
  
  #check what elements in x present in y
  common <- (vec1 %in% vec2) 
  #check what elements in y present in x
  common2 <- (vec2 %in% vec1)
  
  #inner join
  inner <- cbind(x[common==T,],y[common2==T,which(names(y) %in% names(x) == F)])
  colnames(inner) <- c(colnames(x), colnames(y)[which(names(y) %in% names(x) == F)])
  output <- inner
  
  if (type == "left" || type == "full"){
    #left join
    colNames <- names(y)[names(y) %in% names(x)==F]
    x[,colNames] <- NA
    left <- rbind(inner, x[common!=T,])
    output <- left
    #full join 
    if (type == "full"){
      colNames2 <- names(x)[names(x) %in% names(y)==F]
      y[,colNames2] <- NA
      full <- rbind(left, y[common2==F,])
      rownames(full) <- c(1:nrow(full))
      output <- full
    }
  }
  if (type == "right"){
    #right join
    colNames <- names(x)[names(x) %in% names(y)==F]
    y[,colNames] <- NA
    right <- rbind(inner, y[common2!=T,])
    rownames(right) <- c(1:nrow(right))
    output <- right
  }
  output
}

## ---- Examples ----

# test
library(testthat)
employee <- c("John Doe","Peter Gynn","Jolie Hope")
salary <- c(21000, 23400, 26800)
startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14'))
data1 <- data.frame(employee, salary, startdate)

employee <- c("John Doe", "Rob", "Peter Gynn", "Ellen A")
city <- c("city1", "city2", "city3", "city1")
age <- c( 1,3,4,2)
data2 <- data.frame(employee, city, age)

expect_error(join(data1, data2, "salary", "left"))
expect_error(join(data1, data2, "employee", "fully"))
expect_error(join(data1, "employee", "fully"))
expect_error(join(data1, "employee", "left"))
expect_error(join(data1, as.matrix(data2), "employee", "left"))
expect_error(join(data1, data2, c(""), "left"))
expect_error(join(data1, data2, c("data2.employee"), "left"))
expect_error(join(data1, data2, "left"))
expect_that(join(data1, data2, "employee", "left"), is_a("data.frame"))
expect_that(join(data1, data2, "employee", "right"), is_a("data.frame"))
expect_that(join(data1, data2, "employee", "inner"), is_a("data.frame"))
expect_that(join(data1, data2, "employee", "full"), is_a("data.frame"))

employee <- c('John Doe','Peter Gynn','Jolie Hope')
salary <- c(21000, 23400, 26800)
startdate <- as.Date(c('2010-11-1','2008-3-25','2007-3-14'))
data1 <- data.frame(employee, salary, startdate)

employee <- c('John Doe','Rob', 'Peter Gynn', "Ellen A")
data2 <- data.frame(employee)
expect_equivalent(join(data1, data2, "employee", "left")[,1], data1[,1])


## ------------------------ Exercise 02.06 ----------------------------

## ---- Documentation ----

# ... TO DO ...


## ---- Function ----

# ... TO DO ...


## ---- Examples ----

# ... TO DO ...




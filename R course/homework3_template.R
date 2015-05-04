## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Advanced Data Analysis Software Development with R - Batch 2
## Homework 3
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


## ------------------------ Exercise 03.01 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# eventdifftime - function returns a numeric vector of length n − 1, giving the time difference (in seconds) between consecutive events.
#
# ARGUMENTS
# input a data frame with n rows (for some n) and the following columns:
# h – an integer vector with elements in {0, 1, . . . , 23},
# m – an integer vector with elements in {0, 1, . . . , 59},
# s – an integer vector with elements in {0, 1, . . . , 59}.
#
# RETURN VALUE
# numeric vector

## ---- Function ----
is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

eventdifftime <- function (data){
  stopifnot(is.data.frame(data), ncol(data)==3)
  stopifnot(all(sort(colnames(data)==c("h", "m", "s"))))
  stopifnot(all(data$h>=0 & data$h<=23))
  stopifnot(all(data$m>=0 & data$m<=59))
  stopifnot(all(data$s>=0 & data$s<=59))
  stopifnot(all(is.wholenumber(df)))
  
  data <- data[order(data$h, data$m, data$s),]
  nRow <- nrow(data)
  
  data$h[-1]*60^2+data$m[-1]*60+data$s[-1]-(data$h[-nRow]*60^2+data$m[-nRow]*60+data$s[-nRow])
}

## ---- Examples ----
library(testthat)
h <- c(13, 14, 13)
m <- c(1, 3, 2)
s <- c(1, 4, 2)
df <- data.frame(h, m, s)

expect_that(eventdifftime(df), is_a("numeric"))
expect_equal(length(eventdifftime(df)), nrow(df)-1)
expect_equal(eventdifftime(df), c(61, 3662))

h <- c(13, 14, 13)
m <- c(1, 3, 2)
s <- c(1, 61, 2)
df <- data.frame(h, m, s)
expect_error(eventdifftime(df))

h <- c(13, 14, 13)
m <- c(1, 71, 2)
s <- c(1, 5, 2)
df <- data.frame(h, m, s)
expect_error(eventdifftime(df))

h <- c(25, 14, 13)
m <- c(1, 3, 2)
s <- c(1, 5, 2)
df <- data.frame(h, m, s)
expect_error(eventdifftime(df))

h <- c(23, -1, 13)
m <- c(1, 3, 2)
s <- c(1, 5, 2)
df <- data.frame(h, m, s)
expect_error(eventdifftime(df))

h <- c(23, 2.4, 13)
m <- c(1, 3, 2)
s <- c(1, 5, 2)
df <- data.frame(h, m, s)
expect_error(eventdifftime(df))

h <- c(23, 2, 13)
m <- c(1, 3, 2)
df <- data.frame(h, m)
expect_error(eventdifftime(df))

h <- c(23, 2, 13)
m <- c(1, 3, 2)
sec <- c(1, 5, 2)
df <- data.frame(h, m, sec)
expect_error(eventdifftime(df))

## ------------------------ Exercise 03.02 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# decomposeurls decompose URLS into three parts: protocol, host, and resource.
#
# ARGUMENTS
# x - a character vector
#
# RETURN VALUE
# a data frame with length(x) rows.

## ---- Function ----

library(stringi)
decomposeurls <- function(vec){
  stopifnot(all(is.character(vec)))
  stopifnot(all(length(vec)>0))
  m <- '^(.*)://([A-Za-z0-9\\-\\.]+)(:[0-9]+)?(.*)$'
  res <- lapply(vec, function(x) {
    r <- regexec(m, x)
    df <- do.call(rbind, lapply(regmatches(x, r), `[`, c(2L, 3L, 5L)))
  })
  df <- do.call(rbind.data.frame, res)
  colnames(df) <- c("protocol", "host", "resource")
  return(df)
}

decomposeurls <- function(vec){
  stopifnot(all(is.character(vec)))
  stopifnot(all(length(vec)>0))
  
  results <- lapply(vec, function(x) {
    if(grepl(pattern ='(http[s]?|ftp)://.', x) == T){
      res <- gregexpr(pattern ='/', x)
      res <- res[[1]]
      first <- res[1]
      #check for port
      if (stri_count_regex(x, ":[0-9]") !=0){
        find_expr2 <- gregexpr(":[0-9]", x)
        sec <- find_expr2[[1]][1]-1
      }else{
        sec <- res[2]}
      #check if url ends after host
      if (length(res) == 2) {
        third <- nchar(x)
      } else {
        third <- res[3]-1
        end <- nchar(x)
      }      
      protocol <-substr(x, 1, first-2)
      host <- substr(x, sec+1, third)
      resource <- if (nchar(x) != third) substr(x, third+2, end) else ""
      c(protocol, host, resource)
      
      #not a proper url
    } else{
      protocol <- NA
      host <- NA
      resource <- NA
      c(protocol, host, resource)
    }
    
  })
  df <- do.call(rbind.data.frame, results)
  colnames(df) <- c("protocol", "host", "resource")
  return(df)
  
}

## ---- Examples ----
library(testthat)
vec <- c("http://www.aaa.pl/ala", "http://www.aaa.pl", "https://developer.mozilla.org",
         "https://developer.mozilla.org/en-US/docs/Learn/",
         "https://developer.mozilla.org/en-US/search?q=URL",
         "http://www.example.com/results?search_type=search_videos&search_query=tpb&search_sort=relevance
         &search_category=25")

vec2 <- c("https://blog.example.com/dresses/green-dresses-are-awesome/3245/", 
          "https://example.com/dresses/cocktail?gclid=ABCD", "sth else", "http://", "ftp://213.135.44.35")

expect_that(decomposeurls(vec2), is_a("data.frame"))
expect_error(decomposeurls(c(character(0))))
expect_error(decomposeurls(c(1,2,2)))
expect_equal(nrow(decomposeurls(vec2)), length(vec2))
expect_equal(nrow(decomposeurls(c("NA", "NA", "NA"))), length(c("NA", "NA", "NA")))
expect_equal(as.character(decomposeurls(vec)[1,1]), "http")
expect_equal(ncol(decomposeurls(vec)), 3)
expect_equal(as.character(decomposeurls(vec)[3,3]), "")
expect_equal(is.na(decomposeurls(vec2)[4,1]), T)
expect_equal(as.character(decomposeurls("http://www.MyWebSite.com/page-sitemap.xml/")[1,3]), 
             "page-sitemap.xml/")

## ------------------------ Exercise 03.03 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# findemails() to extract all well-formed email addresses 
#
# ARGUMENTS
# vec - a character vector.
#
# RETURN VALUE
# a character vector with exracted emails

## ---- Function ----

findemails <- function(vec){
  stopifnot(is.character(vec))  
  myText <- paste(vec, collapse = ' ')  
  results <- regmatches(myText, gregexpr( "([_+a-zA-Z_0-9-]+(\\.[_+a-zA-Z_0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))", myText),
                        invert = FALSE)
  results[[1]]
}

## ---- Examples ----
library(testthat)
expect_equal(findemails(c("bla ja+ty@blabla.comcom")), "ja+ty@blabla.comcom")
expect_equal(findemails(c("bla ANNNA_MARIA@bla100bla.comcom")), "ANNNA_MARIA@bla100bla.comcom")
expect_equal(findemails(c("bla ANNNA_MARIA@bla100bla.")), character(0))
expect_equal(findemails(c("bla email@email.com", "email@email.com")), c("email@email.com", "email@email.com"))
expect_equal(findemails(c("bla _@email.com")), c("_@email.com"))
expect_equal(findemails(c("bla 9999@email.com")), c("9999@email.com"))
expect_error(findemails(c(1,2,2)))
expect_error(findemails(c(NA)))
expect_equal(findemails(c("bla <9999@email.com>")), c("9999@email.com"))
expect_equal(findemails(c("bla <99.99@email.com>")), c("99.99@email.com"))
expect_equal(findemails(c("bla <99$99@email.com>")), c("99@email.com"))
expect_equal(findemails(c(NA, "test")), character(0))

## ------------------------ Exercise 03.04 ----------------------------

## ---- Documentation ----

# DESCRIPTION
# scrabble returns top (w.r.t. the total number of points per word) n words from dict. Total number of
# points for a given word is just the sum of the points per each letter.
#
# ARGUMENTS
# x – a named integer vector giving how may points (values) may be obtained by using corresponding
# letters (names),
# n- a single integer
# dict - a character vector, determining a set of possible words.
#
# RETURN VALUE
# a character vector

## ---- Function ----
is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

is.letter <- function(x) grepl("[[:alpha:]]", x)

scrabble <- function(x, n, dict){
  stopifnot(!is.null(names(x)), is.numeric(x), all(is.letter(names(x))))
  stopifnot(all(is.wholenumber(x)))
  stopifnot(is.wholenumber(n), n>0, length(n)==1)
  stopifnot(is.character(dict))
  stopifnot(!is.na(dict), unique(strsplit(paste(dict, collapse= ""), "")[[1]]) %in% names(x))
  
  score <- sapply(strsplit(stri_replace_all_fixed(dict, names(x), x, vectorize_all = FALSE),""),
                  function(nb) sum(as.numeric(nb)) )
  
  dict[order(-score)][1:min(n, length(dict))] 
}


## ---- Examples ----
library(testthat)
x <- c(1,3)
names(x) <- c("a", "l")
expect_equal(scrabble(x, 3, c("ala", "lala")), c("lala", "ala"))
expect_that(scrabble(x, 3, c("ala", "lala")), is_a("character"))
expect_error(scrabble(x, 3, c("abc", "aaa", "aaabccc")))
expect_error(scrabble(x, 3.4, "ala"))
expect_error(scrabble(x, 3, c(1:3)))
x <- c("one",2,3, 4)
names(x) <- c("a", "c", "b", "l")
expect_error(scrabble(x, 3, c("ala", "lala")))

x <- 1:6
names(x)  <- c("a", "ą", "b", "c", "ć", "m")
expect_equal(scrabble(x, 3, c("ćma", "ma", "ba")), c("ćma", "ma", "ba"))
expect_equal(length(scrabble(x, 6, c("ćma", "ma", "ba"))), 3)

expect_error(scrabble(x, 3, c("a", "mama", NA)))
expect_equal(scrabble(x, 3, c("a", "mama")), c("mama", "a"))

## ------------------------ Exercise 03.05 ----------------------------

# DESCRIPTION
# strcompress transforms contiguous sequence of at least two equivalent (the function is case insensitive) letters ia string into
# lo, where l – letter, o – number of occurrences.
# If a string does not consist solely of letters (Latin alphabet), return NA.
#
# ARGUMENTS
# vec - a character vector
#
# RETURN VALUE
# a transformed character vector

## ---- Function ----
library(stringi)

strcompress <- function(vec){
  stopifnot(all(is.character(vec)))
  stopifnot(all(length(vec) > 0))
  stopifnot(!is.na(vec))
  
  sapply(vec, function (l){
    if (stri_detect_regex(l, '[^A-Za-z]')){
      NA
    }else{
      all_reg <- stri_extract_all_regex(l, '(?i)(\\w)\\1+')[[1]]
      #nothing to replace
      if (length(all_reg) ==1 && is.na(all_reg)){
        l
        #sequence to replace
      }else{
        all_reg <- all_reg[order(all_reg,  decreasing = TRUE)]
        n <- as.character(nchar(all_reg))
        str <- stri_sub(all_reg,1,1)
        repl <- paste0(str, n)
        
        stri_replace_all_regex(l, all_reg, repl, vectorize_all = FALSE)
      }
    }
  })
}


## ---- Examples ----
library(testthat)
expect_that(strcompress(c("lalalaa", "Heeey", "HHhhheee", "Hey!")), is_a("character"))
expect_that(strcompress(c("Hi32", "Hey!")), is_a("logical"))
expect_equivalent(strcompress(c("lala")), "lala")
expect_equivalent(strcompress(c("laala")), "la2la")
expect_equivalent(strcompress(c("laalaaa")), "la2la3")
expect_equivalent(strcompress(c("abbccc", "xyyyz", "aaah","uffffff", "Good", "Aachen", "hey!", "wrr")),
                  c("ab2c3","xy3z","a3h","uf6","Go2d","A2chen",NA,"wr2"))

expect_error(strcompress(c(1,2,NA, "doog")))
expect_error(strcompress(c()))
expect_error(strcompress(c(NA, NA, "dog")))
expect_equivalent(strcompress(c("")), "")

## ------------------------ Exercise 03.06 ----------------------------

## ---- Documentation ----

# ... TO DO ...


## ---- Function ----

# ... TO DO ...



## ---- Examples ----

# ... TO DO ...




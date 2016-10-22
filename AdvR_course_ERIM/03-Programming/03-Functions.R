# --------------------------------------------
# Authors: Pieter Schoonees and Andreas Alfons
#          Erasmus Universiteit Rotterdam
# --------------------------------------------

## Libraries
library("euR")
library("ggplot2")

# -----------------
# Function basics
# -----------------

## Print source
sd

## First function
scale01 <- function(x) {
  res <- (x  - min(x)) / (max(x) - min(x))
  res
}
scale01(1:11)
res

## Adding argument
scale01(c(0, 1, NA, 3, 4, 5))
scale01 <- function(x, NA.RM = FALSE) {                          # value name is NA.RM, set default value equals False
  res <- (x  - min(x, na.rm = NA.RM)) / 
    (max(x, na.rm = NA.RM) - min(x, na.rm = NA.RM))
  res
}
scale01(c(0, 1, NA, 3, 4, 5))
scale01(c(0, 1, NA, 3, 4, 5), NA.RM = TRUE)

## Function elements
formals(scale01)
args(scale01)
body(scale01)
environment(scale01)

## Packaged functions
environment(mean)
environment(chstat)

## Primitive functions
sum
formals(sum)
body(sum)
environment(sum)

## Sourcing a file
source('myFunctions.R')

# ----------------------
# Arguments and Return
# ----------------------

## ----eval=FALSE----------------------------------------------------------
data("Affairs")
head(Affairs, n = 10)

## Matching arguments
f <- function(argument1, name1, name2) {
  c(a = argument1, b = name1, c = name2)
}
f(1, 2, 3) 
f(2, 3, argument1 = 1) 
f(2, arg = 1, 3) 
f(1, 2, name = 3)

## Calling mean() correctly
mean(1:10)
mean(1:10, trim = 0.05)
mean(x = 1:10)

## Suboptimal calls
mean(1:10, n = T)
mean(1:10, , FALSE)
mean(1:10, 0.05)
mean(, TRUE, x = c(1:10, NA))

## Default arguments
f <- function(a = 1, b = 2){
  c(a, b)
}
f(5)
f(b = 4)
f(10, 20)

## Using return()
f <- function(x) {
  return(x^2)
  print("Hello")
}
g <- function(x) {
  print("Hello")
  x^2
}
f(5)
g(5)

## return() early
f <- function(x) {
  if (!is.numeric(x))
    return(NA)
  x ^ 2
}
f(1:5)
f(c("a", "b"))

## Return a list
f <- function(x) {
  return(list(square = x^2, cube = x^3, status = "OK"))
}
f(5)

## Argument checking
f <- function(x) {
  if (!is.numeric(x))
    stop("Argument 'x' must be numeric.")
  x ^ 2
}
f("c")
f(2)

## Missing arguments
f <- function(a, b){
  c(missing(a), missing(b))
}
f()
f(a = 1)
f(b = 2)
f(1, 2)

## Lazy evaluation
f <- function(x) {
  10
}
f()

## Lazy evaluation: defaults
f <- function(a, b = 2 * a) {
  c(a, b)
}
f(10)

## Lazy evaluation: default based on local variables
f <- function(a, b = 2 * d) {
  d <- a^3 + 1
  c(a, b)
}
f(3)

## Lazy evaluation of conditions
f <- function(a, b) {
  a > 0 && b > 0
}
f(-1)
f(1)

## ... argument
f <- function(x, ...){
  mean(x, ...)
}
f(c(1:10, NA))
f(c(1:10, NA), na.rm = TRUE)
f(c(1:10, NA), na.mr = TRUE)

## Passing all parameters through
f <- function(x, trim = 0, na.rm = FALSE){
  mean(x, trim = trim, na.rm = na.rm)
}
f(c(1:10, NA))
f(c(1:10, NA), na.rm = TRUE)

# --------------------------
# Lexical scope
# --------------------------

## Search path
search()

## Printing local variables
scale01_copy <- function(x, na.rm = FALSE) {
  res <- (x  - min(x, na.rm = na.rm)) / 
    (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
  print(ls())
  res
}
scale01_copy(0:10)
rm(scale01_copy)

##L ocal variable examples
f <-  function() {
  x <- 1
  y <- 2
  c(x, y)
}
f()
x # Same for y
rm(f)

## Global and local variables: lookup
x <- 1
f <-  function() {
  y <- 2
  c(x, y)
}
f()
y 
rm(f, x)

## Lookup of local and global variables
x <- 1
f <-  function() {
  y <- 2
  g <-  function() {
    z <- 3
    c(x, y, z)
  }
  g()
}
f()
g
rm(f, x)

## Function lookup
f <- function(x) x + 1
g <- function() {
  f <- function(x) x * 2
  f(10)
}
g()
f(10)
rm(f, g)

## Function vs variables
f <- function(x) x / 2
g <- function() {
  f <- 10
  f(f)
}
g()
rm(f, g)

## Fresh start
x <- 1
f <- function() {
  x <- 2 * x
  x
}
f()
f()
rm(f, x)

## Global variable issues
x <- 1
f <- function() {
  x^2
}
f()
x <- 10
f()
rm(f, x)

## Special functions
1 + 2
`+`(1, 2)
x <- 1:5
x[3]
`[`(x, 3)
?`[`
?`?`
`(`

## Masking
mean <- function(x) NA
mean(1:10)
rm(mean)

## Infix operators
`%+%` <- function(x, y) paste(x, y)
"new" %+% "string"

## Assignment functions
`second<-` <- function(x, value) {
  x[2] <- value
  x
}
x <- 1:5
second(x) <- 10
x

## Pure functions
f <- function(x) {
  x$a <- 2
  x 
}
x <- list(a = 1)
f(x)
x

## Returning invisibly
f <- function(x) invisible(x)
f(10)
(f(10)) # Force printing
(x <- 2)
x <- y <- z <- 2
(x <- (y <- (z <- 2)))

## on.exit() example
in_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old))
  code
}
in_dir("~", getwd())

## switch() example
f <- function(x, what = "mean") {
  switch(what, mean = mean(x), median = median(x), sd = sd(x))
}
f(1:10) # mean
f(1:10, what = "sd") # sd

## switch alternative by passing a function
f <- function(x, what = mean) what(x)
f(1:10)
f(1:10, what = sd)

## debug examples
debugonce(scale01)
scale01(0:10)
?browser
debug(scale01)
scale01(0:10)
scale01(0:10)
undebug(scale01)


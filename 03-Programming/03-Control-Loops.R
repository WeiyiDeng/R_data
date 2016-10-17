# --------------------------------------------
# Authors: Pieter Schoonees and Andreas Alfons
#          Erasmus Universiteit Rotterdam
# --------------------------------------------

## Libraries
library("euR")
library("ggplot2")

# -----------------
# for() loops
# -----------------

## Simple for loops
for (i in 1:5)
  print(1:i)
for(letter in c("a", "e", "i", "o", "u"))
  print(letter)
for(letter in c("a", "e", "i", "o", "u"))
  letter

## Storing results
res <- numeric(5)                  # create vec with 0s
res
for (i in 1:5)
  res[i] <- i^2
res

(1:5)^2                            # much faster, avoid loops

## Creating a scree plot
data("cityweather")
set.seed(1)
maxK <- 5
error_df <- data.frame(k = 1:maxK, error = NA)          # two cols       check data frame?
error_df
for (k in 1:maxK) {
  error_df[k, 2] <- kmeans(cityweather, centers = k, 
                      nstart = 1000)$tot.withinss       # just get the errors
}
error_df
ggplot(error_df, aes(x = k, y = error)) + geom_line() + geom_point() 

## Storing the complete cluster analysis results for all k
resK <- vector("list", length = maxK)                   # create a list       check list [][]?
str(resK)
for (k in 1:maxK) { 
  resK[[k]] <- kmeans(cityweather, centers = k, nstart = 1000)
}
str(resK, max.level = 1)

## Standardizing the rows
data("crime2008")
crime.std <- crime2008
for (i in 1:nrow(crime.std))
  crime.std[i, ] <- 1000 * crime.std[i, ] / crime.std[i, 9]
tail(crime.std, 3)
crime.std <- crime.std[, -9] # remove the Population column

## Creating empty vectors
rep(NA, 5)
integer(5)
double(5)
numeric(5)
character(5)
logical(5)

## Creating empty lists
vector(mode = "list", length = 5)

## Creating empty matrices
matrix(NA, nrow = 2, ncol = 3)

## Creating empty data frames
data.frame(x = rep(NA, 3), y = rep(NA, 3))
data.frame(k = 1:5, error = NA)


# -----------------
# if() statements
# -----------------

## Simple if() example for log()
set.seed(1)
x <- rnorm(1)
if (x > 0) {
  log(x)
} else {
  NA
}
x>0

## Same example in one line
set.seed(1)
x <- rnorm(1)
if (x > 0) log(x) else NA

## Assigning the result of the loop
x <- rnorm(1)
xlog <- if (x > 0) log(x) else NA
xlog

## Dropping else
x <- rnorm(1)
if (x > 0) log(x)

## -The condition should be of length one
x <- rnorm(5)
if (x > 0) log(x) else NA

## Numbers are conditions
ind <- 0
if (ind) TRUE else FALSE                  # !!!!!!!!   0==False logically eveerthing else equals to true
ind <- 0.01
if (ind) TRUE else FALSE

## Median example 
x <- c(1, 3, 2, 7, 0, 11)
n <- length(x)
## Calculate the median
x_srt <- sort(x)
if (n %% 2) {                              # %% to check remainder
  ## Odd: Extract middle number
  x_srt[(n + 1) / 2]
} else {
  ## Even: Average two middle numbers
  mean(x_srt[c(n / 2, n / 2 + 1)])
}
## Check our result
median(x)

## Median example on different vector
x <- c(2, 5, -3, -1, 0)
n <- length(x)
## Calculate the median
x_srt <- sort(x)
if (n %% 2) {
  ## Odd: Extract middle number
  x_srt[(n + 1) / 2]
} else {
  ## Even: Average two middle numbers
  mean(x_srt[c(n / 2, n / 2 + 1)])
}
## Check our result
median(x)

## Modulo
9 %% 2

## Looping over values and counting the odd ones
values <- sample.int(100, 20)
values
nr <- 0
for (val in values){
  if (val %% 2)
    nr <- nr + 1
}
nr

sum(values %% 2)                              # faster

# -----------------
# ifelse() function
# -----------------


## Attempt at the sign() function
x <- rnorm(10)
ifelse(x > 0, 1, -1)                          # vectorized version of if() !!!
sign(x)

## Consider edge cases
x <- c(-3, 0, 4, -2, NA, 5)
ifelse(x > 0, 1, -1)
sign(x)

## Alternative to cut() using nested ifelse's
rain <- cityweather$RainDays 
rain_cat <- ifelse(rain <= 100, "dry", 
                   ifelse(rain > 190, "wet", "average"))
rain_cat
df_rain <- data.frame(rain, rain_cat)
head(df_rain, 10)

# --------------------
# while() and repeat()
# --------------------

## -While example
i <- 5
while (i > 0) {
  print(i)
  i <- i - 1
}

## Factorial using while()
n <- 10
res <- 1
while (n > 0) {
  res <- res * n
  n <- n - 1
}
res

prod(1:10)

## Asking a password
pwd <- "banana"
attempt <- ""
while (attempt != pwd){
  print("Please enter your password:")
  attempt <- readline()
}

## Repeat() with break statement
n <- 10
res <- 1
repeat {
  res <- res * n
  n <- n - 1
  if (n == 0)
    break
}
res

## Print only even numbers
n <- 11
repeat {
  n <- n - 1
  if (n == 0)
    break
  if (n %% 2) # Skip to next iteration if odd number
    next
  print(n)
}

# --------------------
# Exception handling
# --------------------

## Failing loop
numbers <- list(pi, 20:24, "five", 45:51, 1:10)
results <- vector("list", length = length(numbers))
for (i in 1:length(numbers))
  results[[i]] <- numbers[[i]]^2
str(results)

## Wrapping potentially failing statement in  try()
results <- vector("list", length = length(numbers))
for (i in 1:length(numbers))
  results[[i]] <- try(numbers[[i]]^2)

results

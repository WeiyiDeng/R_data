b <- c(1:8)

A <- matrix(b,2)

d <- A[2,4]

d

plot(b,sort(rnorm(8)))

# ------------- test function -------------
source('D:/Users/R_Data/re_myFunctions.R')

library("testthat")

test_that("getresidual", {
  a <- 17
  b <- 5
  ans <- getresidual(a,b)
  expect_identical(ans, 2)
})

test_that("AddFive", {
  x <- 24
  ans <- AddFive(x)
  expect_identical(ans, 29)
})

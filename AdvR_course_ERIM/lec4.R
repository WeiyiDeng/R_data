nrNA <- function(x){
  sum_missing <- sum(is.na(x))
  prop_missing <- sum_missing/length(x)
  return(list(sum_missing, prop_missing))
}

data("crime2008")
summary(crime2008)

nrNA(crime2008[,2])


colNA <- function(X){
#  res <- matrix(NA, nrow = 2, ncol = ncol(X))                              # check how to do with matrix ??
  res <- data.frame(sum_na = rep(NA, ncol(X)), prop_na = rep(NA, ncol(X)))
  for (i in 1:ncol(X)){
    res[i,] <- nrNA(X[,i])
    # print(res[i,])
  }
  return(res)
}
colNA(crime2008)

data("ftertiary")
colNA(ftertiary)

#
drawCurve <- function(fun, lower, upper, npoints){
  x <- seq(lower,upper,npoints)
  y <- fun(x)
  plot(x,y)
}
cubed <- function(x){
  return(x^3)
}
drawCurve(cubed,0,5,0.1)
dev.off()                                                                  # clears last plot
drawCurve(function(x) x^3, -10, 10, 0.1)                                   # simplified method of writing a func !!

  
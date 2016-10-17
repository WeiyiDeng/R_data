## Function to scale a vector to minimum zero and maximum one
scale01 <- function(x, na.rm = FALSE) {
  res <- (x  - min(x, na.rm = na.rm)) / 
    (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
  res
}
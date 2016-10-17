#' Add Five to a number
#' 
#' Takes in any parameter value and add 5 to it
#' @param x A numeric value to be added
#' @return the output after add 5
#' @export

AddFive <- function(x){
  y <- x+5
  return(y)
}

#' Get residual of two integer numbers
#'
#' Take an integer number divided by another and get the residual
#' @param a, b are integer numbers with a > b
#' @return the residual of a divided by b
#' @examples getresidual(111,11)             # gets residual 1
#' getresidual(12,3)                         # gets residual 0
#' @export

getresidual <- function (a,b) 
{
  stopifnot(a > b)
  stopifnot(a%%1==0 && b%%1==0)              # check fractional part to test if a and b are integers
  for (i in 1:a){
    c = a - b*i
    if (c <= 0){
      if (c==0){
        res <- 0
      }
      else{
        res <-  a - b*(i-1)
      }
      break
    }   
  }    
  return (res)
}
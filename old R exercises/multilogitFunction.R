multiLogit <- function (utilityVector)    # multinomial logit
{
  deno <- 0                               # 1 only if one set of parameters are set to 0 !!
  for (i in 1:length(utilityVector)) {
    deno <- deno + exp(utilityVector[i])
  }
    prob <- exp(utilityVector)/deno 
  return (prob)
}

c <- multiLogit(c(1,2,3,4,5,6))
sum(c)

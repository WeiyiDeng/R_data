# simulate two variables with correlation equals to 0.7 (use PXY = cov(X,Y)/sqrt(var(X)*var(Y)))
library(MASS)

set.seed(401716)
sigma <- matrix(c(1,0.7,0.7,1),nrow = 2)                   # set covariance matrix
mu <- c(0,2)                                               # set means
mysimu <- mvrnorm(10,mu,sigma)                              # simulate mvn

cor(mysimu[,1],mysimu[,2])
cor.test(mysimu[,1],mysimu[,2])

x <- mysimu[,1]
y <- mysimu[,2]


## write func to compute p value of correlation between simulated variables using permutation
permTest <- function(x, y, nperm_set = 1000){
  nperm <- nperm_set
  if (!is.numeric(x)||!is.numeric(y)) 
    stop("x and y must be numeric")
  if (length(x)!=length(y))
    stop("x and y must have the same length")
  obs_cor <- cor(x,y)
  cor_perm <- rep(NA,nperm)
  # set.seed(401716)
  for (k in 1:nperm){
    yPerm <- sample(y,length(y),replace=FALSE)
    cor_perm[k] <- cor(yPerm,x)
  }
  if (obs_cor >0){
    num_extreme <- sum(cor_perm>obs_cor)
  } else {
    num_extreme <- sum(cor_perm<obs_cor)
  }
  perm_p_val <- 2*num_extreme/nperm
  print(perm_p_val)
  return(list("computed correlation" = obs_cor, "permutation p-value" = perm_p_val,            
              "vector of nperm correlations" = cor_perm))                           # return ends excuting the func
}

output <- permTest(x,y)                                     # by assigning return to a variable suppress printing the returned values
cor_val <- output[[1]]
vec_nperm_cor <- output[[3]]

cor.test(x,y)                             # compare with cor.test() result

plot(density(vec_nperm_cor), 
     main = "density plot for testing correlation", xlab = "correlation", ylab = "pdf")
abline(v = cor_val, col = "red")

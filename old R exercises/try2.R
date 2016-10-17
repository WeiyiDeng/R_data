## wtf!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

rm(list = ls())

N <- 15                         # total observations
J <- 3                          # number of alternatives
T <- 5                          # number of times to choose a choice

beta <- c(1,1,1)

feature <- matrix(rnorm(N*2),N,2)              # number of variables

utility <- exp(beta[1]+beta[2]*feature[,1]+beta[3]*feature[,2])

U1 <- matrix(utility,T,J)                      # reshape vector to matrix in matlab

Usum <- apply(U1,1,sum)                        # sum over columns
Umat <- kronecker(matrix(1,1,J),Usum)          # repmat in matlab

prob_mat <- U1/Umat

choice_prob <- t(apply(prob_mat,1,cumsum))     # cummulative sum over columns 

draw_prob <- runif(T)
draw_mat <- kronecker(matrix(1,1,J),draw_prob)
choice <- apply(choice_prob < draw_mat, 1, sum)+1

v <- c(1:J)
bmat <- kronecker(matrix(1,T,1),t(v))
cmat <- kronecker(matrix(1,1,J),choice)
bchoice <- (bmat==cmat)




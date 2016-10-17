rm(list = ls())

# -----------------------------------------------
# ------------- friends 0-1 matrix --------------
# -----------------------------------------------

# matrix(runif(6), nrow=2, ncol=3)

n <- 100
temfriend <- matrix(runif(n^2), n)
friend <- matrix(0, n, n)

cutoff <- which(temfriend<0.2)             # 0.2 cut off point
friend[cutoff] <- 1
head(friend)                               
friend[upper.tri(friend,diag=TRUE)] <- 0
friend <- friend + t(friend)

# -----------------------------------------------
# ---- avg num of listens per band per week -----
# -----------------------------------------------

# seq(0, 1, by=0.2)
# seq(bd1, bd2, length=10)

k <- 10                                   # 100 bands
startt <- runif(k,-1.96,1.96)
endt <- runif(k,startt,1.96)

t <- 20                                    # 20 weeks of observations
lisp <- matrix(0, k, t)                    # lisp K*t matrix
for (i in 1:k){
  lisp[i,] <- seq(startt[i], endt[i], length=t)
}
lis <- dnorm(lisp)

# not drawing randomly within [a,b] but by sequence

Successk <- rchisq(k,2)                    # chi-square distribution df = 2
peakk <- Successk*n
listenkt <- lis*peakk                      # listen kt avg number of listens per band per week
# listenKT <- round(listenkt)


#-----------------------------------------------------
source('E:/Wei/Graduate/Programming exercise/multilogitFunction.R', echo=TRUE)

G <- 1
exterU <- matrix(runif(n*t, 0, G), nrow=n, ncol=t)

alpha <- matrix(rnorm(6*n), nrow=n, ncol=6)
parameter <- exp(alpha)
a0 <- parameter[,1]
a4 <- parameter[,5]
a5 <- parameter[,6]

slot <- 420
lisN <- matrix(0, n*k, slot)

Allu1 <- c()                               # for t = 1, slot=1
for (i in 1:n) {
  bandsu <- runif(k)
  nu1 <- append(bandsu, exterU[i,1])           
  Allu1 <- append(Allu1, nu1)
}
                
prob <- c()
choice <- matrix(0, n, slot)
for (i in 1:n) {
  p <- multiLogit(Allu1[((i-1)*(k+1)+1):((i-1)*(k+1)+k)])  
  prob <- append(prob, p)
  choice[i,1] <- which(p == max(p))                        # here only choose the max prob now
}

sum(prob[1:10])                

for (i in 1:n) {
  if (choice[i,1] != (k+1)) {
    lisN[k*(i-1)+choice[i,1],1] <- 1    
  }
}

# lisN[,1]

# Uband <- x
c <- which(friend[,1]==1)
index <- (c-1)*k

friendTotal <- 0
for (j in 1:length(c)){
  friendTotal = friendTotal + lisN[(index[j]+1):(index[j]+k)]    # ?? fix this !
}



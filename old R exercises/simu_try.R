# qnorm is the quantile function, which is just the inverse CDF
# dnorm == pdf; pnorm == cdf
# rnorm: random sampling

set.seed(401716)                       # set the seed EVERYTIME when do the simulation
r <- rnorm(50)
summary(r)

# or generate from uniform distribution with inverse cdf
set.seed(401716)
u <- runif(50)
normsample <- qnorm(u)
summary(normsample)

# pdf
dnorm(2)
# cdf
pnorm(2)
pnorm(-2)

poi <- rpois(10,8)
poi
summary(poi)

dpois(4,10)
ppois(10,10)
qpois(0.5,10)
ppois(15,10)
ppois(100,100)
qpois(0.5,100)
rpois(10,8.5)     # mu does not have to be an integer, mu can be equal to exp(x*beta)

#----------------------------------
# simulate from the linear regression model: y = beta0 + beta1*x + epsilon
# beta0 = 0.5, beta1 = 2, x~N(0,1), epsilon~N(0,2^2)
set.seed(20)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 + x*2 + e
plot(x,y)
summary(y)

# if x is binary, uses binomial distribution
set.seed(401716)
x <- rbinom(100, 1, 0.5)
x
y <- 0.5 + x*2 + e
summary(y)
plot(x,y)

# simulate from a poisson model Y ~Poisson(mu), mui = exp(xi'*beta)
# log(mu) = beta0 + beta1*x, beta0 = 0.5, beta1 = 0.3, x~N(0,1)
set.seed(1)
x <- rnorm(100)
logMu <- 0.5 + 0.3*x
mu <- exp(logMu)
y <- rpois(100,mu)
summary(y)
plot(x,y)

#------------------------------------------------------------
# draw random samples from arbitrary distributions
set.seed(401716)
sample(1:10, 4)
sample(1:10, 4, replace=TRUE)
sample(1:10)         # if does not specify n, gives permutation, eg: 1 to 10 pai'lie'zu'he

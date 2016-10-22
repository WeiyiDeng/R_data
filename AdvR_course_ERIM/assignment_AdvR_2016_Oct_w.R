# Advanced Data Analysis with R: Assignment
# Weiyi Deng
# 2016 Oct 21

rm(list = ls())

### Q1
# a
library("euR")
data("heart")
? heart

heart_data <- as.data.frame(heart)
heart_data$y[1:10]
heart_data$ycat <- as.factor(heart_data$y)

# attach(heart_data)
levels(heart_data$ycat) <- c("no", "yes")
summary(heart_data$ycat)
# 160 suffered from heart disease; 302 did not.

# b
summary(heart_data)

# library("colorspace")
# colhcl <- rainbow_hcl(5)
# plot(heart_data[,-c(1,11)], main="heart", col = colhcl[ycat])            
plot(heart_data[,-c(1,11)], main="heart", col = heart_data$ycat)                      # rm the first and last columns from data frame
# people with head disease tend to have higher systolic blood pressure, more tobacco consumption, have higher ldl, more adiposity and obesity, more type A behavior and more alcohol consumption

# c
cor(heart_data[,-c(1,11)])
# cor between obesity and adiposity is 0.717; larger than 0.7, there is concern for multicollinearity

# d
mod1 <- glm(y ~ .-ycat, family = binomial(link = "logit"), 
            data = heart_data)
summary(mod1)

mod0 <- glm(y ~ 1, family = binomial(link = "logit"), 
            data = heart_data)
anova(mod0, mod1, test="Chisq")
# difference in deviance: 123.97; difference in degrees of freedom: 9; p-value for the change in deviance: 2.2e-16

# e
pred <- predict(mod1, newdata = heart_data, type = "response")
yes_heart <- pred[heart_data$ycat=="yes"]
no_heart <- pred[heart_data$ycat=="no"]
hist(yes_heart, main = "distr of fitted response values of people with heart disease", breaks = 20)
hist(no_heart, main = "distr of fitted response values of people without heart disease", breaks = 20)
# the model tend to predict people without heart disease to have a lower probability of having the disease, 
# and people with heart disease to have a higher probability of having the disease. The true positive rate 
# is not very high, but overall the model has reasonable performance in predicting heart disease.

### Q2
# a
data("Scotch")
? Scotch
summary(Scotch)
head(Scotch)
sum(!complete.cases(Scotch))
# no missing values; data are in 0 and 1s

# b
consume_by_brand <- apply(Scotch,2,sum)
consume_by_brand
barplot(consume_by_brand, las = 2, main = "number of consumptions")
# Chivas.Regal is the most popular; the.Singleton the least popular

# c
consume_by_customer <- apply(Scotch,1,sum)
mean(consume_by_customer)
max(consume_by_customer)
min(consume_by_customer)
table(consume_by_customer)
barplot(table(consume_by_customer), xlab = "number of whiskeys consumptions", ylab = "number of customers",
        main = "histogram of consumption by customers")
# mean of whiskey consumption of cumstomers is 2.292606; min consumption is 1 and max consumption is 20

# d
# dist computes the distances between the rows of a data matrix
Jaccard_distance <- dist(t(Scotch), method = "binary")              # take transpose of Scotch
Jaccard_distance

# library(ggplot2)
# ? geom_tile

# e
library("cluster")
pm <- pam(Jaccard_distance, k = 3)
pm$medoids
table(pm$cluster)
pm$clustering
# "Johnnie.Walker.Red.Label", "Glenlivet" and "Passport" are chosen as medoids
# The cluster sizes are 6, 4 and 11 respectively

# f
pm2 <- pam(Jaccard_distance, k = 4)
pm2$medoids
table(pm2$cluster)
pm2$clustering
# prefers the one with 3 clusters; the cluster sizes are more balanced


### Q3
# write a function to compute p value of correlation between simulated variables using permutation
permTest <- function(x, y, nperm_set = 1000){
  nperm <- nperm_set
  if (!is.numeric(x)||!is.numeric(y))                      # check if inputs are numeric
    stop("x and y must be numeric")
  if (length(x)!=length(y))                                # check if inputs have the same sizes
    stop("x and y must have the same length")
  obs_cor <- cor(x,y)
  cor_perm <- rep(NA,nperm)
  set.seed(401716)
  for (k in 1:nperm){
    yPerm <- sample(y,length(y),replace=FALSE)
    cor_perm[k] <- cor(yPerm,x)
  }
  if (obs_cor >0){                                        # get number of permuted correlations more extreme than observed cor
    num_extreme <- sum(cor_perm>obs_cor)
  } else {
    num_extreme <- sum(cor_perm<obs_cor)
  }
  perm_p_val <- 2*num_extreme/nperm                       # calculate the two-sided permutation p-value
  print(perm_p_val)                                       # print out permutation p-value
  return(list("computed correlation" = obs_cor, "permutation p-value" = perm_p_val,            
              "vector of nperm correlations" = cor_perm))                         
}

# test with Prestige data
data("Prestige")
summary(Prestige)
output <- permTest(Prestige$women,Prestige$prestige)                                   
cor_val <- output[[1]]
vec_nperm_cor <- output[[3]]

perm_p_value <- output[[2]]
perm_p_value
cor.test(Prestige$women,Prestige$prestige)                             # compare with cor.test() result

plot(density(vec_nperm_cor), 
     main = "density plot for testing correlation", xlab = "correlation", ylab = "pdf")
abline(v = cor_val, col = "red")

# test with cityweather
data("cityweather")
summary(cityweather)
output <- permTest(cityweather$AvgCold,cityweather$RainDays,nperm_set = 10000)                                   
cor_val <- output[[1]]
vec_nperm_cor <- output[[3]]

perm_p_value <- output[[2]]
perm_p_value
cor.test(cityweather$AvgCold,cityweather$RainDays)                             # compare with cor.test() result

plot(density(vec_nperm_cor), 
     main = "density plot for testing correlation", xlab = "correlation", ylab = "pdf")
abline(v = cor_val, col = "red")
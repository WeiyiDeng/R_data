# can not use install.packages for local pkgs, need to install manually from packages on bottom right panel

# use ggplot ?

library("euR")
data("Affairs")
Affairs

barplot(table(Affairs$affairs))
sum(Affairs$affairs>=1)

# b <- c(-1,1,15)
# Affairs$binaffairs <- cut(Affairs$affairs, breaks=b,labels = c(0,1))

Affairs$binaffairs <- as.numeric(Affairs$affairs>=1)
# quicker way 1*(var=="x")
# or use as.numeric

mod1 <- glm(binaffairs ~ gender+age+yearsmarried+children+religiousness+education+ occupation+rating,    # can use DV ~. - affairs
            family = binomial(link = "logit"),                        # binomial check?
            data = Affairs)

summary(mod1)
confint(mod1, level = 0.99)
exp(confint(mod1, level = 0.99))

plot(mod1)
predict(mod1)
a_hat <- predict(mod1, type = "response")
binary_yhat <- 1*(a_hat>0.5)
# binary_yhat <- as.numeric(a_hat>0.5)
binary_yhat


###-----------------------------------------
### exec 1.2
###-----------------------------------------
more <- Affairs$affairs>=1
yesAffairs <- as.data.frame(Affairs[more,])

table(yesAffairs$affairs)

mod1 <- glm(affairs ~ .-binaffairs, family = poisson(link = "log"), 
            data = yesAffairs)
mod1
summary(mod1)

mod2 <- glm(affairs ~ 1, family = poisson(link = "log"), 
            data = yesAffairs)
mod2
summary(mod2)

anova(mod1, mod2, test = "Chisq")

exp(confint(mod1, level = 0.95))

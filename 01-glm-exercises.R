# --------------------------------------------
# Authors: Andreas Alfons and Pieter Schoonees
#          Erasmus University Rotterdam
# --------------------------------------------

library("euR")
library("colorspace")
library("ggplot2")
library("GGally")

##  1.1 Affairs data
data("Affairs")
?Affairs
head(Affairs)
summary(Affairs)
plot(Affairs)
ggpairs(Affairs)

# a
with(Affairs, summary(affairs))
ggplot(Affairs, aes(x = affairs)) + geom_bar()
with(Affairs, sum(affairs >= 1))
with(Affairs, sum(affairs == 0))

# b
Affairs <- within(Affairs, binaffairs <- as.numeric(affairs >= 1))
head(Affairs)
with(Affairs, table(binaffairs))

# c
fit <- glm(binaffairs ~ . - affairs, 
           family = binomial(link = "logit"),
           data = Affairs)
summary(fit)
coef(fit)

# d
ci <- confint(fit, level = 0.99)
exp(ci)

# e
plot(fit)
df <- data.frame(index = 1:nrow(Affairs), residuals = residuals(fit), 
                 occupation = Affairs$occupation, 
                 yearsmarried = Affairs$yearsmarried)
ggplot(df, aes(x = index, y = residuals)) + geom_point()
ggplot(df, aes(x = occupation, y = residuals)) + geom_point()
ggplot(df, aes(x = yearsmarried, y = residuals)) + geom_point()

# f
fvals.fit <- fitted(fit)
fvals.pred <- predict(fit, type = "response")
all(fvals.pred == fvals.fit)
df$fitted <- fvals.fit
ggplot(df, aes(x = fitted)) + geom_histogram() + 
  geom_vline(xintercept = 0.5, colour = "red")

# g
table(fvals.fit > 0.5) / nrow(Affairs)
tab <- with(Affairs, table(binaffairs))
tab / sum(tab)

# h
mypred <- cut(fvals.fit, breaks=c(0, 0.5, 1), labels = 0:1)
table(mypred)
table(mypred)/nrow(Affairs)
ctab <- table(mypred, Affairs$binaffairs)
ctab
1 - sum(diag(ctab))/nrow(Affairs)

# i
ggplot(df, aes(x = fitted)) + geom_histogram() + 
  geom_vline(xintercept = 0.5, colour = "red")
mypred2 <- cut(fvals.fit, breaks=c(0, 0.45, 1), labels = 0:1)
ctab2 <- table(actual = Affairs$binaffairs, predicted = mypred2)
ctab2
1 - sum(diag(ctab2)) / nrow(Affairs)

##  1.1 Affairs data: Poisson regression
yesAffairs <- Affairs[Affairs$binaffairs == 1, ]
head(yesAffairs)
dim(yesAffairs)

# a
with(yesAffairs, table(affairs))

# b
fit2 <- glm(affairs ~ . - binaffairs, 
            family = poisson(link = "log"), 
            data = yesAffairs)
summary(fit2)
coef(fit2)

# c
fit3 <- glm(affairs ~ 1, family = poisson(link = "log"), data = yesAffairs)
anova(fit3, fit2, test = "Chisq")

# d
exp(confint(fit2))

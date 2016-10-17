# --------------------------------------------
# Authors: Pieter Schoonees and Andreas Alfons
#          Erasmus Universiteit Rotterdam
# --------------------------------------------


## Load deathpenalty data
library("euR")
data("deathpenalty")
deathpenalty

## Fit logistic regression with interaction, get summary
mod1 <- glm(DeathPenalty ~ VictimRace*DefendantRace, 
            family = binomial(link = "logit"),                        # binomial check?
            weights = Freq, data = deathpenalty)
summary(mod1)

## Calculate p-value manually
1 - pchisq(440.84 - 418.58, df = 6 - 3)

## p-value via anova()
mod2 <- glm(DeathPenalty ~ 1, family = binomial(link = 
              "logit"), weights = Freq, data = deathpenalty)
anova(mod2, mod1, test="Chisq")

## Compare main effects and interaction model
mod3 <- update(mod2, ~ VictimRace + DefendantRace)
anova(mod3, mod1, test = "Chisq")

# ------------------------------
# Logistic regression section
# ------------------------------

## Print summary of main effects model
summary(mod3)

## Confidence interval using profile likelihood
confint(mod3, level = 0.9)
exp(confint(mod3, level = 0.9))

## Predictions
predict(mod3)
predict(mod3, type = "response")
deathpenalty <- within(deathpenalty, fitted <-
                     predict(mod3, type = "response"))

## Diagnostic plots
plot(mod3)

# ---------------------------------------
# Out-of-sample Classification Accuracy
# ---------------------------------------

## heart data
data("heart")
?heart

head(heart)
dim(heart)

## Train-test split
n <- nrow(heart)
set.seed(1)
train <- sample(x = n, size = ceiling(0.7 * n))
heart_train <- as.data.frame(heart[train, ])
heart_test <- as.data.frame(heart[-train, ])

## Train model, predict test data
trainfit <- glm(y ~ ., data = heart_train, 
                family = binomial(link = "logit"))
testpred <- predict(trainfit, newdata = heart_test, 
                    type = "response")

## Calculate test confusion matrix
thres <- 0.5
testpred_cat <- 1 * (testpred > thres)
conftest <- table(actual = heart_test$y, predicted = testpred_cat)
conftest

## Calculate train confusion matrix
trainpred <- predict(trainfit, type = "response")
trainpred_cat <- 1 * (trainpred > thres)
conftrain <- table(actual = heart_train$y,                      # prediction table !!
                   predicted = trainpred_cat)
conftrain

# ------------------------------
# Poisson regression 
# ------------------------------

## Poisson regression for PHDPublications data
data("PhDPublications")
mod1 <- glm(articles ~ ., family = poisson(link = "log"), 
            data = PhDPublications)
plot(mod1)
summary(mod1) 

## Adding an interaction
mod2 <- update(mod1, ~ . + mentor:prestige)
summary(mod2)
plot(mod2)

## Comparing model fit
anova(mod1, mod2, test = "Chisq")

## Confidence intervals
confint(mod2)

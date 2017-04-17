# ISLR Chapter 5 Lab
# Daniel J. Vera, Ph.D.

library(ISLR)     # for data
library(boot)     # for cv.glm()

# The Validation Set Approach ==============================================
set.seed(1)
# shortcut of putting 392 in first argument of sample() yields vector 1:392
# see ?sample
train <- sample(392, 196)
lm_fit <- lm(mpg ~ horsepower, 
             data = Auto, 
             subset = train)
attach(Auto)
mean((mpg - predict(lm_fit, Auto))[-train] ^ 2)

lm_fit2 <- lm(mpg ~ poly(horsepower, 2),
              data = Auto,
              subset = train)
mean((mpg - predict(lm_fit2, Auto))[-train] ^ 2)

lm_fit3 <- lm(mpg ~ poly(horsepower, 3),
              data = Auto,
              subset = train)
mean((mpg - predict(lm_fit3, Auto))[-train] ^ 2)

set.seed(2)
train <- sample(392, 196)
lm_fit <- lm(mpg ~ horsepower, 
             data = Auto, 
             subset = train)
mean((mpg - predict(lm_fit, Auto))[-train] ^ 2)

lm_fit2 <- lm(mpg ~ poly(horsepower, 2),
              data = Auto,
              subset = train)
mean((mpg - predict(lm_fit2, Auto))[-train] ^ 2)

lm_fit3 <- lm(mpg ~ poly(horsepower, 3),
              data = Auto,
              subset = train)
mean((mpg - predict(lm_fit3, Auto))[-train] ^ 2)

# LOOCV ====================================================================
# glm without family argument does linear regression.
glm_fit <- glm(mpg ~ horsepower,
               data = Auto)
coef(glm_fit)
lm_fit <- lm(mpg ~ horsepower,
               data = Auto)
coef(lm_fit)

cv_err <- cv.glm(Auto, glm_fit)
cv_err$delta

cv_error <- rep(0,5)
for (i in 1:5) {
  glm_fit <- glm(mpg ~ poly(horsepower, i),
                 data = Auto)
  cv_error[i] <- cv.glm(Auto, glm_fit)$delta[1]
}
cv_error

# k-Fold Cross-Validation ==================================================
set.seed(17)
cv_error10 <- rep(0,10)
for (i in 1:10) {
  glm_fit <- glm(mpg ~ poly(horsepower, i),
                 data = Auto)
  cv_error10[i] <- cv.glm(Auto, glm_fit, K = 10)$delta[1]
}
cv_error10

cv_error10_bias <- rep(0,10) #to see the bias-corrected version
for (i in 1:10) {
  glm_fit <- glm(mpg ~ poly(horsepower, i),
                 data = Auto)
  cv_error10_bias[i] <- cv.glm(Auto, glm_fit, K = 10)$delta[2]
}
cv_error10_bias

cv_error10 - cv_error10_bias

# The Bootstrap ============================================================
# To perform bootstrap in R entails two steps.
# First we create a function that computes the statistic of interest.
# Second we use boot() to repeatedly sample observations from data set
# with replacement.

alpha_fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y) - cov(X, Y))/(var(X) + var(Y) - 2*cov(X, Y)))
}
alpha_fn(Portfolio, 1:100)
set.seed(1)
alpha_fn(Portfolio, sample(100, 100, replace = TRUE))
boot(Portfolio, alpha_fn, R = 1000)

# Bootstrap on Linear Regression Model =====================================
boot_fn <- function(data, index) {
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}
boot_fn(Auto, 1:392)

set.seed(1)
boot_fn(Auto, sample(392, 392, replace = TRUE))
boot_fn(Auto, sample(392, 392, replace = TRUE))

boot(Auto, boot_fn, 1000)
summary(lm(mpg ~ horsepower, data = Auto))$coef

# quadratic model
boot_fn_2 <- function(data, index) {
  return(coef(lm(mpg ~ horsepower + I(horsepower ^ 2), 
                 data = data, 
                 subset = index)))
}
set.seed(1)
boot(Auto, boot_fn_2, 1000)
summary(lm(mpg ~ horsepower + I(horsepower ^ 2), data = Auto))$coef
        

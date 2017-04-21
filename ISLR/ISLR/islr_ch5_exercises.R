# ISLR Chapter 5 Exericses
# Daniel J. Vera, Ph.D.
library(ISLR)
library(boot)
library(MASS)
library(tidyverse)


# Exercise 2 ===============================================================
# (g) Create a plot that displays, for each integer value of n from 1
# to 100,000, the probability that the jth observation is in the
# bootstrap sample. Comment on what you observe.

obs <- 1:10000
probs <- rep(0, 10000)

for (i in obs) {
  probs[i] <- (1 - (1 - 1/i)^i)
}

ggplot(tibble(obs, probs), aes(obs, probs)) +
  geom_point()

ggplot(tibble(obs, probs), aes(obs, probs)) +
  geom_point() +
  coord_cartesian(ylim = c(0.6, 0.75))
# values appear to be approaching a limit at 0.632

# (h) We will now investigate numerically the probability that a bootstrap
# sample of size n = 100 contains the jth observation. Here
# j = 4. We repeatedly create bootstrap samples, and each time
# we record whether or not the fourth observation is contained in
# the bootstrap sample.

store = rep(NA, 10000)
for (i in 1:100000) {
  store[i] <- sum(sample(1:100, replace = TRUE) == 4) > 0 
}
mean(store)
# probability is 0.636313, very close to the limit found above.
# Recall lim n -> infinity (1 + x/n)^n = e^x. Then since the probability
# of jth observation not being in bootstrap sample of size n is (1 - 1/n)^n, 
# this probability converges to e^-1 or 1/e, approximately 0.3678794.
# Therefore the probability that the jth observation IS in the bootstrap
# converges to 1 - 1/e or roughly 0.6321206.

# Applied Exercises ========================================================
# 5. In Chapter 4, we used logistic regression to predict the probability of
# default using income and balance on the Default data set. We will
# now estimate the test error of this logistic regression model using the
# validation set approach. Do not forget to set a random seed before
# beginning your analysis.

# (a) Fit a logistic regression model that uses income and balance to
# predict default.
attach(Default)
set.seed(1)

glm_fit <- glm(default ~ income + balance, 
               data = Default, 
               family = "binomial")
summary(glm_fit)

# (b) Using the validation set approach, estimate the test error of this
# model. In order to do this, you must perform the following steps:

#   i. Split the sample set into a training set and a validation set.
train <- sample(10000, 5000)
Default_train <- Default[train,]
Default_test <- Default[-train,]
#  ii. Fit a multiple logistic regression model using only the training
#      observations.
glm_train <- glm(default ~ income + balance, 
               data = Default, 
               family = "binomial",
               subset = train)
# iii. Obtain a prediction of default status for each individual in
#      the validation set by computing the posterior probability of
#      default for that individual, and classifying the individual to
#      the default category if the posterior probability is greater
#      than 0.5.
glm_probs <- predict(glm_train, Default_test, type = "response")
glm_predicts <- rep("No", length(glm_probs))
glm_predicts[glm_probs > 0.5] <- "Yes"
#  iv. Compute the validation set error, which is the fraction of
#      the observations in the validation set that are misclassified.
1 - mean(glm_predicts == Default_test$default)
# (c) Repeat the process in (b) three times, using three different splits
# of the observations into a training set and a validation set. Comment
# on the results obtained.

seeds <- c(3, 7, 137)
errors <- rep(NA, 3)

for (i in 1:3) {
  set.seed(seeds[i])

glm_fit <- glm(default ~ income + balance, 
               data = Default, 
               family = "binomial")
train <- sample(10000, 5000)
Default_train <- Default[train,]
Default_test <- Default[-train,]
glm_train <- glm(default ~ income + balance, 
                 data = Default, 
                 family = "binomial",
                 subset = train)
glm_probs <- predict(glm_train, Default_test, type = "response")
glm_predicts <- rep("No", length(glm_probs))
glm_predicts[glm_probs > 0.5] <- "Yes"
errors[i] <- 1 - mean(glm_predicts == Default_test$default)
}
errors
# note the errors vary depending on the split

# (d) Now consider a logistic regression model that predicts the probability
# of default using income, balance, and a dummy variable
# for student. Estimate the test error for this model using the validation
# set approach. Comment on whether or not including a
# dummy variable for student leads to a reduction in the test error
# rate.
set.seed(1)

glm_fit <- glm(default ~ ., 
               data = Default, 
               family = "binomial")
summary(glm_fit)
train <- sample(10000, 5000)
Default_train <- Default[train,]
Default_test <- Default[-train,]
glm_train <- glm(default ~ ., 
                 data = Default, 
                 family = "binomial",
                 subset = train)
glm_probs <- predict(glm_train, Default_test, type = "response")
glm_predicts <- rep("No", length(glm_probs))
glm_predicts[glm_probs > 0.5] <- "Yes"
1 - mean(glm_predicts == Default_test$default)
# adding student does not seem to reduce error. We note the p-value

# 6. We continue to consider the use of a logistic regression model to
# predict the probability of default using income and balance on the
# Default data set. In particular, we will now compute estimates for
# the standard errors of the income and balance logistic regression 
# coefficients in two different ways: (1) using the bootstrap, and (2) 
# using the standard formula for computing the standard errors in the glm()
# function. Do not forget to set a random seed before beginning your
# analysis.
# (a) Using the summary() and glm() functions, determine the estimated
# standard errors for the coefficients associated with income
# and balance in a multiple logistic regression model that uses
# both predictors.
set.seed(1)

glm_fit <- glm(default ~ income + balance, 
               data = Default, 
               family = "binomial")
summary(glm_fit)

# (b) Write a function, boot.fn(), that takes as input the Default data
# set as well as an index of the observations, and that outputs
# the coefficient estimates for income and balance in the multiple
# logistic regression model.

boot_fn <- function(data, index) {
  fit <- glm(default ~ income + balance, 
             data = data, 
             family = "binomial",
             subset = index)
  return(coef(fit))
}
# (c) Use the boot() function together with your boot.fn() function to
# estimate the standard errors of the logistic regression coefficients
# for income and balance.
boot(Default, boot_fn, 1000)
# (d) Comment on the estimated standard errors obtained using the
# glm() function and using your bootstrap function.

# The bootstrap errors and standard errors from the logistic regression
# are fairly close.

# 7. In Sections 5.3.2 and 5.3.3, we saw that the cv.glm() function can be
# used in order to compute the LOOCV test error estimate. Alternatively,
# one could compute those quantities using just the glm() and
# predict.glm() functions, and a for loop. You will now take this approach
# in order to compute the LOOCV error for a simple logistic
# regression model on the Weekly data set. Recall that in the context
# of classification problems, the LOOCV error is given in (5.4).
# (a) Fit a logistic regression model that predicts Direction using Lag1
# and Lag2.
attach(Weekly)
glm_fit <- glm(Direction ~ Lag1 + Lag2,
               data = Weekly,
               family = "binomial")
summary(glm_fit)
# (b) Fit a logistic regressionmodel that predicts Direction using Lag1
# and Lag2 using all but the first observation.
glm_fit1 <- glm(Direction ~ Lag1 + Lag2,
               data = Weekly[-1,],
               family = "binomial")
summary(glm_fit1)
# (c) Use the model from (b) to predict the direction of the first 
# observation. You can do this by predicting that the first observation
# will go up if P(Direction="Up"|Lag1, Lag2) > 0.5. Was this observation
# correctly classified?
prob <- predict(glm_fit1, Weekly[1,], type = "response")
first_obs_prediction <- ifelse(prob > 0.5, "Up", "Down")
first_obs_prediction == Weekly[1,]$Direction

# Since above is false, the first observation was misclassified.

# (d) Write a for loop from i = 1 to i = n, where n is the number of
# observations in the data set, that performs each of the following
# steps:
#   i. Fit a logistic regression model using all but the ith observation
#      to predict Direction using Lag1 and Lag2.
#  ii. Compute the posterior probability of the market moving up
#      for the ith observation.
# iii. Use the posterior probability for the ith observation in order
#      to predict whether or not the market moves up.
#  iv. Determine whether or not an error was made in predicting
#      the direction for the ith observation. If an error was made,
#      then indicate this as a 1, and otherwise indicate it as a 0.

cls_errors <- rep(1, dim(Weekly)[1])

for (i in 1:dim(Weekly)[1]) {
  glm_fit <- glm(Direction ~ Lag1 + Lag2,
                  data = Weekly[-i,],
                  family = "binomial")
  probability <- predict(glm_fit, Weekly[i,], type = "response")
  prediction <- ifelse(probability > 0.5, "Up", "Down")
  cls_errors[i] <- ifelse(prediction == Weekly[i,]$Direction, 0, 1)
}
cls_errors


# (e) Take the average of the n numbers obtained in (d)iv in order to
# obtain the LOOCV estimate for the test error. Comment on the
# results.
mean(cls_errors)
# LOOCV error of 44.99541%, seems high.

# 8. We will now perform cross-validation on a simulated data set.
# (a) Generate a simulated data set as follows:
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)
# In this data set, what is n and what is p? Write out the model
# used to generate the data in equation form.
# n = 100, p = 2
# Y = X - 2X^2 + epsilon

# (b) Create a scatterplot of X against Y . Comment on what you find.
ggplot(tibble(x, y), aes(x, y)) + geom_point()
# data seems VERY much to follow a parabala :) 
ggplot(tibble(x, y), aes(x, y)) + geom_point() + geom_smooth(se = FALSE)
# (c) Set a random seed, and then compute the LOOCV errors that
# result from fitting the following four models using least squares
# Note you may find it helpful to use the data.frame() function
# to create a single data set containing both X and Y:
set.seed(1)
#   i. Y = β0 + β1X + epsilon
df <- data.frame(x, y)
fit_glm1 <- glm(y ~ x, data = df)
cv.glm(df, fit_glm1)$delta[1]
#  ii. Y = β0 + β1X + β2X2 + epsilon
fit_glm2 <- glm(y ~ poly(x, 2), data = df)
cv.glm(df, fit_glm2)$delta[1]
# iii. Y = β0 + β1X + β2X2 + β3X3 + epsilon
fit_glm3 <- glm(y ~ poly(x, 3), data = df)
cv.glm(df, fit_glm3)$delta[1]
# iv. Y = β0 + β1X + β2X2 + β3X3 + β4X4 + epsilon.
fit_glm4 <- glm(y ~ poly(x, 4), data = df)
cv.glm(df, fit_glm4)$delta[1]

# (d) Repeat (c) using another random seed, and report your results.
# Are your results the same as what you got in (c)? Why?
set.seed(137)
#   i. Y = β0 + β1X + epsilon
df <- data.frame(x, y)
fit_glm1 <- glm(y ~ x, data = df)
cv.glm(df, fit_glm1)$delta[1]
#  ii. Y = β0 + β1X + β2X2 + epsilon
fit_glm2 <- glm(y ~ poly(x, 2), data = df)
cv.glm(df, fit_glm2)$delta[1]
# iii. Y = β0 + β1X + β2X2 + β3X3 + epsilon
fit_glm3 <- glm(y ~ poly(x, 3), data = df)
cv.glm(df, fit_glm3)$delta[1]
# iv. Y = β0 + β1X + β2X2 + β3X3 + β4X4 + epsilon.
fit_glm4 <- glm(y ~ poly(x, 4), data = df)
cv.glm(df, fit_glm4)$delta[1]

# They are identical since LOOCV holds out 1 observation each time so 
# we have the same training sets.

# (e) Which of the models in (c) had the smallest LOOCV error? Is
# this what you expected? Explain your answer.
# The degree 2 and yes this was expected since the data is simulated from
# a degree 2 polynomial.

# (f) Comment on the statistical significance of the coefficient estimates
# that results from fitting each of the models in (c) using
# least squares. Do these results agree with the conclusions drawn
# based on the cross-validation results?
summary(fit_glm4)

# The p-values associated with the degree 2 is the lowest showing that it is
# the most significant. Also, higher degree terms show higher p-values and
# hence less signficance. This agrees with the conclusions drawn from the
# CV results which had the lowest error estiamte at degree 2.

# 9. We will now consider the Boston housing data set, from the MASS
# library.
# (a) Based on this data set, provide an estimate for the population
# mean of medv. Call this estimate μ_hat.
attach(Boston)
mu_hat <- mean(medv)
# (b) Provide an estimate of the standard error of μ_hat. Interpret this
# result.
# Hint: We can compute the standard error of the sample mean by
# dividing the sample standard deviation by the square root of the
# number of observations.
se_hat <- sd(medv)/sqrt(dim(Boston)[1])
# (c) Now estimate the standard error of μ_hat using the bootstrap. How
# does this compare to your answer from (b)?
set.seed(1)
boot_mean <- function(data, index) {
  return(mean(data[index]))
}

boot(medv, boot_mean, 1000)

# the standard error from (b) was 0.409 and from the bootstrap was
# 0.412, fairly close.

# (d) Based on your bootstrap estimate from (c), provide a 95% confidence
# interval for the mean of medv. Compare it to the results
# obtained using t.test(Boston$medv).
# Hint: You can approximate a 95% confidence interval using the
# formula [μ_hat − 2SE(μ_hat), μ_hat + 2SE(μ_hat)].
ci <- c(mu_hat - 2 * se_hat, mu_hat + 2 * se_hat)
t.test(Boston$medv)

# confidence interval from t.test function is [21.73, 23.34] compared to
# our calculation of [21.72, 23.35].

# (e) Based on this data set, provide an estimate, μ_hat_med, for the median
# value of medv in the population.
mu_hat_med <- median(Boston$medv)
# (f) We now would like to estimate the standard error of ˆμmed. Unfortunately,
# there is no simple formula for computing the standard
# error of the median. Instead, estimate the standard error of the
#median using the bootstrap. Comment on your findings.
set.seed(1)
boot_median <- function(data, index) {
  return(median(data[index]))
}

boot(medv, boot_median, 1000)

# estimated median value of 21.2, same as calculated from the data set,
# and standard error is 0.3801

# (g) Based on this data set, provide an estimate for the tenth percentile
#of medv in Boston suburbs. Call this quantity μ_hat_0.1. 
# (You can use the quantile() function.)
decile <- quantile(Boston$medv, 0.1)

# (h) Use the bootstrap to estimate the standard error of μ_hat_0.1. Comment
# on your findings.
set.seed(1)
boot_decile <- function(data, index) {
  return(quantile(data[index], 0.1))
}

boot(medv, boot_decile, 1000)

# bootstrap gives estimate of 12.75, again equal to the estimate from data 
# set with a standard error of 0.505.
# ISLR Chapter 4 Exercises
# Daniel J. Vera, Ph.D.

library(class)    # for KNN
library(ISLR)     # for data
library(MASS)     # for LDA
library(tidyverse)
library(GGally)   # added for cooler scatterplot matrix

# Applied Exercise 10 ======================================================
# This question should be answered using the Weekly data set, which
# is part of the ISLR package. This data is similar in nature to the
# Smarket data from this chapter’s lab, except that it contains 1,089
# weekly returns for 21 years, from the beginning of 1990 to the end of
# 2010.
 
# (a) Produce some numerical and graphical summaries of the Weekly
# data. Do there appear to be any patterns?
summary(Weekly)
cor(Weekly[ ,-9])
ggscatmat(Weekly, color = "Direction")
ggscatmat(Weekly, columns = 2:9, color = "Direction")

Weekly %>% mutate(row = row_number()) %>%
  ggplot(aes(x = row, y = Volume)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# correlations are all basically zero, except for year and volume which
# shows an increase in volume over time.

# (b) Use the full data set to perform a logistic regression with
# Direction as the response and the five lag variables plus Volume
# as predictors. Use the summary function to print the results. Do
# any of the predictors appear to be statistically significant? If so,
# which ones?

glm_fit_wk <- glm(Direction ~ 
                    Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                  data = Weekly, 
                  family = binomial)
summary(glm_fit_wk)

# only Lag2 shows any real significance.

# (c) Compute the confusion matrix and overall fraction of correct
# predictions. Explain what the confusion matrix is telling you
# about the types of mistakes made by logistic regression.
glm_probs_wk = predict(glm_fit_wk, type = "response")
glm_pred_wk = rep("Down", length(glm_probs_wk)) 
glm_pred_wk[glm_probs_wk > 0.5] <- "Up"

table(glm_pred_wk, Weekly$Direction)
mean(glm_pred_wk == Weekly$Direction)

# It appears as though 56.1% of the responses are predicted correctly.
# Training error rate = 43.89%, overly optimistic.
# There are only 54 out of 484 down days accurately predicted but 
# 557 out of 605 of up days are predicted. So its right only ~ 11%
# of time when market is down but ~ 92% when up. 

# (d) Now fit the logistic regression model using a training data period
# from 1990 to 2008, with Lag2 as the only predictor. Compute the
# confusion matrix and the overall fraction of correct predictions
# for the held out data (that is, the data from 2009 and 2010).
train <- (Weekly$Year < 2009)
Weekly_train <- Weekly[train,]
Weekly_test <- Weekly[!train,]
Direction_train <- Weekly_train$Direction
Direction_test <- Weekly_test$Direction

logistic_wkly <- glm(Direction ~ Lag2, 
                     data = Weekly_train, 
                     family = binomial)
# or
logistic_wkly2 <- glm(Direction ~ Lag2, 
                     data = Weekly, 
                     family = binomial,
                     subset = train)

summary(logistic_wkly)

logistic_probs <- predict(logistic_wkly, Weekly_test, type = "response")
logistic_pred = rep("Down", length(Direction_test))
logistic_pred[logistic_probs > 0.5] <- "Up"
table(logistic_pred, Direction_test)
mean(logistic_pred == Direction_test)

# (e) Repeat (d) using LDA.
lda_wkly <- lda(Direction ~ Lag2, data = Weekly, subset = train)
lda_wkly
plot(lda_wkly)
lda_probs <- predict(lda_wkly, Weekly_test)
table(lda_probs$class, Direction_test)
mean(lda_probs$class == Direction_test)

# (f) Repeat (d) using QDA.
qda_wkly <- qda(Direction ~ Lag2, data = Weekly, subset = train)
qda_wkly
qda_pred <- predict(qda_wkly, Weekly_test)
table(qda_pred$class, Direction_test)
mean(qda_pred$class == Direction_test)

# (g) Repeat (d) using KNN with K = 1.
train_X <- as.matrix(Weekly$Lag2[train])
test_X <- as.matrix(Weekly$Lag2[!train])

set.seed(1)
knn_pred <- knn(train_X, test_X, Direction_train, k = 1)
table(knn_pred, Direction_test)
mean(knn_pred == Direction_test)

# (h) Which of these methods appears to provide the best results on
# this data?
# Logistic and LDA are pretty close. QDA is not very good nor is 1NN,
# which is the worst.

# (i) Experiment with different combinations of predictors, including
# possible transformations and interactions, for each of the
# methods. Report the variables, method, and associated confusion
# matrix that appears to provide the best results on the held
# out data. Note that you should also experiment with values for
# K in the KNN classifier.

# Different Logistic Models
logistic_wkly3 <- glm(Direction ~ Lag2:Lag1, 
                     data = Weekly_train, 
                     family = binomial)
summary(logistic_wkly3)

logistic_probs3 <- predict(logistic_wkly3, Weekly_test, type = "response")
logistic_pred3 = rep("Down", length(Direction_test))
logistic_pred3[logistic_probs3 > 0.5] <- "Up"
table(logistic_pred3, Direction_test)
mean(logistic_pred3 == Direction_test)

# Different LDA Models
lda_wkly2 <- lda(Direction ~ Lag2:Lag1,
                 data = Weekly, 
                 subset = train)
lda_wkly2
plot(lda_wkly)
lda_probs2 <- predict(lda_wkly2, Weekly_test)
table(lda_probs2$class, Direction_test)
mean(lda_probs2$class == Direction_test)

# Different QDA Models
qda_wkly2 <- qda(Direction ~ Lag2 + sqrt(abs(Lag2)),
                 data = Weekly,
                 subset = train)
qda_wkly2
qda_pred2 <- predict(qda_wkly2, Weekly_test)
table(qda_pred2$class, Direction_test)
mean(qda_pred2$class == Direction_test)
# Different KNN Models
train_X <- as.matrix(Weekly$Lag2[train])
test_X <- as.matrix(Weekly$Lag2[!train])

set.seed(1)
knn_pred3 <- knn(train_X, test_X, Direction_train, k = 3)
table(knn_pred3, Direction_test)
mean(knn_pred3 == Direction_test)

train_X <- as.matrix(Weekly$Lag2[train])
test_X <- as.matrix(Weekly$Lag2[!train])

set.seed(1)
knn_pred100 <- knn(train_X, test_X, Direction_train, k = 100)
table(knn_pred100, Direction_test)
mean(knn_pred100 == Direction_test)

# Applied Exercise 11 ======================================================
# In this problem, you will develop a model to predict whether a given
# car gets high or low gas mileage based on the Auto data set.
Auto <- as_tibble(ISLR::Auto)
# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains
# a value above its median, and a 0 if mpg contains a value below
# its median. You can compute the median using the median()
# function. Note you may find it helpful to use the data.frame()
# function to create a single data set containing both mpg01 and
# the other Auto variables.
Auto_mpg01 <- Auto %>% 
  mutate(mpg01 = ifelse(mpg > median(mpg), 1, 0))

# (b) Explore the data graphically in order to investigate the association
# between mpg01 and the other features. Which of the other
# features seem most likely to be useful in predicting mpg01? Scatterplots
# and boxplots may be useful tools to answer this question.
# Describe your findings.
summary(Auto_mpg01)
ggscatmat(as.data.frame(Auto_mpg01))

Auto_mpg01 %>% 
  ggplot(aes(cut_number(mpg01, 2), displacement)) + 
  geom_boxplot()

Auto_mpg01 %>% 
  ggplot(aes(cut_number(mpg01, 2), horsepower)) + 
  geom_boxplot()

Auto_mpg01 %>% 
  ggplot(aes(cut_number(mpg01, 2), weight)) + 
  geom_boxplot()

Auto_mpg01 %>% 
  ggplot(aes(cut_number(mpg01, 2), acceleration)) + 
  geom_boxplot()

ggplot(Auto_mpg01) +
  geom_count(aes(cut_number(mpg01, 2), cylinders))

ggplot(Auto_mpg01) +
  geom_boxplot(aes(cut_number(mpg01, 2), cylinders))

Auto_mpg01 %>% 
  ggplot(aes(cut_number(mpg01, 2), mpg)) + 
  geom_boxplot()

# to predict mpg01, can use mpg obviously. there seems to be
# neg corr with cylnders and displacement, horsepower, and weight but
# positive correlation with acceleration.

# (c) Split the data into a training set and a test set.
train <- (Auto_mpg01$year %% 2 == 0)
train_auto <- Auto_mpg01[train,]
test_auto <- Auto_mpg01[!train,]
# (d) Perform LDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in
# (b). What is the test error of the model obtained?

# LDA Models
lda_auto <- lda(mpg01 ~ cylinders + displacement + horsepower + weight, 
                data = Auto_mpg01,
                subset = train)
lda_auto
lda_auto_pred <- predict(lda_auto, test_auto)
table(lda_auto_pred$class, test_auto$mpg01)
mean(lda_auto_pred$class == test_auto$mpg01)

# test error rate is 
1 - mean(lda_auto_pred$class == test_auto$mpg01)

# (e) Perform QDA on the training data in order to predict mpg01
# using the variables that seemed most associated with mpg01 in
# (b). What is the test error of the model obtained?
qda_auto <- qda(mpg01 ~ cylinders + displacement + horsepower + weight, 
                data = Auto_mpg01,
                subset = train)
qda_auto
qda_auto_pred <- predict(qda_auto, test_auto)
table(qda_auto_pred$class, test_auto$mpg01)
mean(qda_auto_pred$class == test_auto$mpg01)

# test error rate is 
1 - mean(qda_auto_pred$class == test_auto$mpg01)
# (f) Perform logistic regression on the training data in order to predict
# mpg01 using the variables that seemed most associated with
# mpg01 in (b). What is the test error of the model obtained?
logistic_auto <- glm(mpg01 ~ cylinders + displacement + horsepower + weight,
                     data = Auto_mpg01,
                     subset = train,
                     family = binomial)
summary(logistic_auto)

logistic_probs_auto <- predict(logistic_auto, test_auto, type = "response")
logistic_pred_auto <- rep(0,length(test_auto$mpg01))
logistic_pred_auto[logistic_probs_auto > 0.5] <- 1

table(logistic_pred_auto, test_auto$mpg01)
mean(logistic_pred_auto == test_auto$mpg01)
1 - mean(logistic_pred_auto == test_auto$mpg01)

# (g) Perform KNN on the training data, with several values of K, in
# order to predict mpg01. Use only the variables that seemed most
# associated with mpg01 in (b). What test errors do you obtain?
# Which value of K seems to perform the best on this data set?
train_auto_X <- cbind(
  Auto_mpg01$cylinders, 
  Auto_mpg01$displacement, 
  Auto_mpg01$horsepower,
  Auto_mpg01$weight
)[train,]

test_auto_X <- cbind(
  Auto_mpg01$cylinders, 
  Auto_mpg01$displacement, 
  Auto_mpg01$horsepower,
  Auto_mpg01$weight
)[!train,]

set.seed(1)
knn_auto1 <- knn(train_auto_X, test_auto_X, train_auto$mpg01, k = 1)
table(knn_auto1, test_auto$mpg01)
mean(knn_auto1 == test_auto$mpg01)
#test error
1 - mean(knn_auto1 == test_auto$mpg01)

knn_auto10 <- knn(train_auto_X, test_auto_X, train_auto$mpg01, k = 10)
table(knn_auto10, test_auto$mpg01)
mean(knn_auto10 == test_auto$mpg01)
#test error
1 - mean(knn_auto10 == test_auto$mpg01)

knn_auto3 <- knn(train_auto_X, test_auto_X, train_auto$mpg01, k = 3)
table(knn_auto3, test_auto$mpg01)
mean(knn_auto3 == test_auto$mpg01)
#test error
1 - mean(knn_auto3 == test_auto$mpg01)

knn_auto100 <- knn(train_auto_X, test_auto_X, train_auto$mpg01, k = 100)
table(knn_auto100, test_auto$mpg01)
mean(knn_auto100 == test_auto$mpg01)
#test error
1 - mean(knn_auto100 == test_auto$mpg01)

knn_auto5 <- knn(train_auto_X, test_auto_X, train_auto$mpg01, k = 5)
table(knn_auto5, test_auto$mpg01)
mean(knn_auto5 == test_auto$mpg01)
#test error
1 - mean(knn_auto5 == test_auto$mpg01)

# Applied Exercise 12 ======================================================
# This problem involves writing functions.
# (a) Write a function, Power(), that prints out the result of raising 2
# to the 3rd power. In other words, your function should compute
# 2^3 and print out the results.
# Hint: Recall that x^a raises x to the power a. Use the print()
# function to output the result.

Power <- function(x, a) {
  print(x^a)
}

Power(2,3)

# (b) Create a new function, Power2(), that allows you to pass any
# two numbers, x and a, and prints out the value of x^a. You can
# do this by beginning your function with the line
#   > Power2 = function(x,a){
# You should be able to call your function by entering, for instance,
#   > Power2(3,8)
# on the command line. This should output the value of 3^8, namely,
# 6,561.

# see (a)

# (c) Using the Power2() function that you just wrote, compute 10^3,
# 8^17, and 131^3.
Power(10,3)
Power(8,17)
Power(131,3)

# (d) Now create a new function, Power3(), that actually returns the
# result x^a as an R object, rather than simply printing it to the
# screen. That is, if you store the value x^a in an object called
# result within your function, then you can simply return() this
# result, using the following line:
#   > return (result)
# The line above should be the last line in your function, before
# the } symbol.
Power3 <- function(x, a) {
  result <- x^a
  return(result)
}

Power3(10,3)

# (e) Now using the Power3() function, create a plot of f(x) = x^2.
# The x-axis should display a range of integers from 1 to 10, and
# the y-axis should display x^2. Label the axes appropriately, and
# use an appropriate title for the figure. Consider displaying either
# the x-axis, the y-axis, or both on the log-scale. You can do this
# by using log=‘‘x’’, log=‘‘y’’, or log=‘‘xy’’ as arguments to
# the plot() function.
x_axis_pts <- 1:10

ggplot(tibble(x = x_axis_pts, y = Power3(x_axis_pts,2)), aes(x,y)) + 
  geom_point() +
  labs(title = "f(x) = x^2", xlab = "x", ylab = "x^2")

ggplot(tibble(x = log(x_axis_pts), y = log(Power3(x_axis_pts,2))), aes(x,y)) + 
  geom_point() +
  labs(title = "Log of x^2", 
       xlab = "Log of x", 
       ylab = "Log of x^2")

# (f) Create a function, PlotPower(), that allows you to create a plot
# of x against x^a for a fixed a and for a range of values of x. For
# instance, if you call
#   > PlotPower (1:10,3)
# then a plot should be created with an x-axis taking on values
# 1, 2,..., 10, and a y-axis taking on values 1^3, 2^3,..., 10^3.
PlotPower <- function(x, a) {
  ggplot(tibble(x, y = Power3(x,a)), aes(x,y)) + 
    geom_point() +
    labs(title = "f(x)", xlab = "x", ylab = "y")
}

PlotPower(1:10, 3)
PlotPower(-5:5, 3)
PlotPower(-7:7, 4)
# Applied Exercise 13 ======================================================
# Using the Boston data set, fit classification models in order to predict
# whether a given suburb has a crime rate above or below the median.
# Explore logistic regression, LDA, and KNN models using various subsets
# of the predictors. Describe your findings.

Boston_tb <- as_tibble(Boston)

Boston_tb <- Boston_tb %>%
  mutate(crim01 = ifelse(crim > median(crim), 1, 0))

cor(Boston_tb)
ggscatmat(as.data.frame(Boston_tb))

# need to add factor to seperate 'high crime' (crim01 = 1) vs
# 'low crime' (crim01 = 0)

Boston_tb_f <- Boston_tb %>%
  mutate(crim_gp = 
           as.factor(
             ifelse(crim < median(crim), "below median", "above median")
             )
         ) %>%
  select(crim_gp, everything())

Boston_tb_f <- as.data.frame(Boston_tb_f)
ggscatmat(Boston_tb_f)
ggscatmat(Boston_tb_f, color = "crim_gp")
# will give NA when use color since the crim01 and factors are 
# 1-1 correspondence. Remember it calculates correlation by factor

# Logistic Regression
train <- 1:(length(Boston_tb$crim)/2)
test <- (length(Boston_tb$crim)/2 + 1):length(Boston_tb$crim)

Boston_train <- Boston_tb[train,]
Boston_test <- Boston_tb[test,]

glm_crim <- glm(crim01 ~ . -crim, 
                data = Boston_tb,
                family = binomial,
                subset = train)
summary(glm_crim)
# note that chas and tax have no significance

crim_probs <- predict(glm_crim, Boston_test, type = "response")
crim_pred <- rep(0, length(test))
crim_pred[crim_probs > .5] <- 1 

table(crim_pred, Boston_test$crim01)
mean(crim_pred == Boston_test$crim01)
1 - mean(crim_pred == Boston_test$crim01) # test error

glm2_crim <- glm(crim01 ~ . -crim - chas - tax, 
                 data = Boston_tb,
                 family = binomial,
                 subset = train)
summary(glm2_crim)

crim2_probs <- predict(glm2_crim, Boston_test, type = "response")
crim2_pred <- rep(0, length(test))
crim2_pred[crim2_probs > .5] <- 1 

table(crim2_pred, Boston_test$crim01)
mean(crim2_pred == Boston_test$crim01)
1 - mean(crim2_pred == Boston_test$crim01) # test error

# LDA
lda_crim <- lda(crim01 ~ . -crim, data = Boston_tb, subset = train)
lda_crim_preds <- predict(lda_crim, Boston_test)
table(lda_crim_preds$class, Boston_test$crim01)
mean(lda_crim_preds$class == Boston_test$crim01)
1 - mean(lda_crim_preds$class == Boston_test$crim01) # test error

lda2_crim <- lda(crim01 ~ . -crim - chas - tax, 
                 data = Boston_tb, 
                 subset = train)
lda2_crim_preds <- predict(lda2_crim, Boston_test)
table(lda2_crim_preds$class, Boston_test$crim01)
mean(lda2_crim_preds$class == Boston_test$crim01)
1 - mean(lda2_crim_preds$class == Boston_test$crim01) # test error

# LDA
attach(Boston_tb)
train_xb <- cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio,
                  black, lstat, medv)[train,]
test_xb <- cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio,
                 black, lstat, medv)[test,]  

train_crim01 <- crim01[train]

set.seed(1)
knn_pred_bos <- knn(train_xb, test_xb, train_crim01, k = 1)
table(knn_pred_bos, Boston_test$crim01)
1 - mean(knn_pred_bos == Boston_test$crim01) # test error k =1

knn10_pred_bos <- knn(train_xb, test_xb, train_crim01, k = 10)
table(knn10_pred_bos, Boston_test$crim01)
1 - mean(knn10_pred_bos == Boston_test$crim01) 

knn_pred_bos <- knn(train_xb, test_xb, train_crim01, k = 1)
table(knn_pred_bos, Boston_test$crim01)
1 - mean(knn_pred_bos == Boston_test$crim01) # test error k =1

knn100_pred_bos <- knn(train_xb, test_xb, train_crim01, k = 100)
table(knn100_pred_bos, Boston_test$crim01)
1 - mean(knn100_pred_bos == Boston_test$crim01) 

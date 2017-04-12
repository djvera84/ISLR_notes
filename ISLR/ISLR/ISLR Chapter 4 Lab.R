# ISLR Chapter 4 Lab
# Daniel J. Vera, Ph.D.

library(class)    # for KNN
library(ISLR)     # for data
library(MASS)     # for LDA
library(tidyverse)
library(GGally)   # added for cooler scatterplot matrix, see line 11 and 12

summary(Smarket)
pairs(Smarket)
ggscatmat(Smarket, columns = 2:9, color = "Direction")
pairs(Smarket, col = Smarket$Direction)
cor(Smarket[ ,-9])
ggscatmat(Smarket, color = "Direction")

Smarket %>% mutate(row = row_number()) %>%
  ggplot(aes(x = row, y = Volume)) + 
  geom_point() + 
  geom_smooth(se = FALSE)

# Logistic Regression
#====================================================================
glm_fit_sm = glm(Direction ~ 
                   Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                 data = Smarket,
                 family = binomial)
summary(glm_fit_sm)
coef(glm_fit_sm)
summary(glm_fit_sm)$coef
summary(glm_fit_sm)$coef[, 4]

glm_probs = predict(glm_fit_sm, type = "response")
glm_probs[1:10]

contrasts(Smarket$Direction)

glm_pred = rep("Down", 1250)      #create vector of 1250 'Down' elements
glm_pred[glm_probs > 0.5] = "Up"  #transforms 'Down' to 'Up' if prob>.5

# confusion matrix
table(glm_pred, Smarket$Direction)
mean(glm_pred == Smarket$Direction)

train = (Smarket$Year < 2005)
Smarket_2005 = Smarket[!train,]
dim(Smarket_2005)
Direction_2005 = Smarket$Direction[!train]

glm_fit2 = glm(Direction ~
                Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket,
              family = binomial,
              subset = train)
glm_probs2 = predict(glm_fit2, Smarket_2005, type = "response")
glm_pred2 = rep("Down", 252)
glm_pred2[glm_probs2 > 0.5] = "Up"
table(glm_pred2, Direction_2005)
mean(glm_pred2 == Direction_2005)
mean(glm_pred2 != Direction_2005)

glm_fit3 = glm(Direction ~ Lag1 + Lag2, 
               data = Smarket,
               family = "binomial",
               subset = train)
glm_probs3 = predict(glm_fit3, Smarket_2005, type = "response")
glm_pred3 = rep("Down", 252)
glm_pred3[glm_probs3 > 0.5] = "Up"
table(glm_pred3, Direction_2005)
mean(glm_pred3 == Direction_2005)

predict(glm_fit3,
        newdata = data.frame(Lag1 = c(1.2, 1.5),
                             Lag2 = c(1.1, -0.8)),
        type = "response")

# LDA
#====================================================================
lda_fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda_fit
plot(lda_fit)
lda_pred = predict(lda_fit, Smarket_2005)
names(lda_pred)
lda_class = lda_pred$class
table(lda_class, Direction_2005)
mean(lda_class == Direction_2005)

sum(lda_pred$posterior[,1] >= 0.5)
sum(lda_pred$posterior[,1] < 0.5)

lda_pred$posterior[1:20, 1]
lda_class[1:20]

sum(lda_pred$posterior[,1] > 0.9)

# QDA
#====================================================================
qda_fit = qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda_fit

qda_class = predict(qda_fit, Smarket_2005)$class
table(qda_class, Direction_2005)
mean(qda_class == Direction_2005)

# KNN
#====================================================================
train_X = cbind(Smarket$Lag1, Smarket$Lag2)[train,]
test_X = cbind(Smarket$Lag1, Smarket$Lag2)[!train,]
train_Direction = Smarket$Direction[train]

set.seed(1)
# If several observations are tied, R randomly breaks tie so we set
# seed to reproduce results in future

knn_predict = knn(train_X, test_X, train_Direction, k = 1)
table(knn_predict, Direction_2005)

knn_predict = knn(train_X, test_X, train_Direction, k = 3)
table(knn_predict, Direction_2005)
mean(knn_predict == Direction_2005)

# KNN for CARAVAN data set
#====================================================================
dim(Caravan)
attach(Caravan)
summary(Purchase)

standardized_X = scale(Caravan[, -86])
test = 1:1000
train_X = standardized_X[-test,]
test_X = standardized_X[test,]
train_Y = Purchase[-test]
test_Y = Purchase[test]
set.seed(1)

knn_predict = knn(train_X, test_X, train_Y, k = 1)
mean(test_Y != knn_predict)
mean(test_Y != "No")

table(knn_predict, test_Y)

knn_predict3 = knn(train_X, test_X, train_Y, k = 3)
table(knn_predict3, test_Y)

knn_predict5 = knn(train_X, test_X, train_Y, k = 5)
table(knn_predict5, test_Y)

# compare to logistic regression

glm_fit_caravan = glm(Purchase ~ ., 
                      data = Caravan, 
                      family = binomial,
                      subset = -test)
glm_probs_caravan = predict(glm_fit_caravan, 
                            Caravan[test,], 
                            type = "response")
glm_pred_caravan = rep("No", 1000)
glm_pred_caravan[glm_probs_caravan > 0.5] = "Yes"
table(glm_pred_caravan, test_Y)

# reset to lower probability threshold
glm_pred_caravan = rep("No", 1000)
glm_pred_caravan[glm_probs_caravan > 0.25] = "Yes"
table(glm_pred_caravan, test_Y)

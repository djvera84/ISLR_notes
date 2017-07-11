# ISLR Chapter 6 Lab 1
# Daniel J. Vera, Ph.D.

library(ISLR)
library(leaps)      #for best subset selection
library(tidyverse)

# Best Subset Selection ---------------------------------------------------
fix(Hitters)
names(Hitters)
sum(is.na(Hitters$Salary))
mean(is.na(Hitters$Salary))

dim(Hitters)
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

regfit_full = regsubsets(Salary ~ ., Hitters)
summary(regfit_full)

regfit_full2 = regsubsets(Salary~., data = Hitters, nvmax = 19)
reg_summary = summary(regfit_full2)
names(reg_summary)

reg_summary$rsq
par(mfrow=c(2,2))
plot(reg_summary$rss, xlab = "Number of Variables", ylab = "RSS",
     type = "l")

plot(reg_summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted Rsq", type = "l")

# Adjusted R^2
which.max(reg_summary$adjr2)
points(11, reg_summary$adjr2[11], col = "red", cex = 2, pch = 20)

# C_p
plot(reg_summary$cp, xlab = "Number of Variables", ylab = "Cp",
     type = "l")
which.min(reg_summary$cp)
points(10, reg_summary$cp[10], col = "red", cex = 2, pch = 20)

# AIC
plot(reg_summary$bic, xlab = "Number of Variables", ylab = "BIC",
     type = "l")
points(6, reg_summary$bic[6], col = "red", cex = 2, pch = 20)

plot(regfit_full2, scale = "r2")
plot(regfit_full2, scale = "adjr2")
plot(regfit_full2, scale = "Cp")
plot(regfit_full2, scale = "bic")
coef(regfit_full2, 6)

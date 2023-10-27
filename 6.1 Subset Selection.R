#We wish to predict a baseball player's Salary on the basis of many stats associated with performance
#in that previous year

#The is.na() function can be used to identify the missing observations.

library(ISLR2)
library(ggplot2)
library(MASS)
View(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))



library(leaps)
#regsubsets() performs best subset selection by identifying the best model that contains a given
#number of predictors
#best is quantified using RSS
regfit.full <- regsubsets(Salary ~ ., Hitters)
summary(regfit.full)


regfit.full <- regsubsets(Salary ~ ., Hitters,
                          nvmax = 19)
reg.summary <- summary(regfit.full)

names(reg.summary)
reg.summary$rsq

par(mfrow = c(2,2))
plot(reg.summary$rss, xlab = "Number of Variables",
     ylab = "RSS")
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted Rsq")

which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2,
       pch = 20)


plot(reg.summary$cp, xlab = "Number of Variables",
     ylab =  "Cp")
which.min(reg.summary$cp)
points(10, reg.summary$adjr2[11], col = "red", cex = 2,
       pch = 20)
points(10, reg.summary$cp[10], col = "red", cex = 2,
       pch = 20)
which.min(reg.summary$bic)
plot(reg.summary$bic, xlab = "Number of Vars",
     ylab = "BIC")
points(6, reg.summary$bic[6], col = "red", cex = 2,
       pch = 20)

plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")

coef(regfit.full, 6)

regfit.fwd <- regsubsets(Salary ~ ., data = Hitters,
                         nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary ~ ., data = Hitters,
                         nvmax = 19, method = "backward")
summary(regfit.bwd)


coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)


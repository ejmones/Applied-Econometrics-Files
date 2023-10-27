

rm(list=ls())  # Clear w

library(rddtools)
library(rdd)
Rdd <- read.csv("C:/Users/em4kn/OneDrive/Documents/Summer Semester 2023/EC320 Econometrics/Personal R Files/Rdd_data(1).csv")

Rdd_rdd <- rdd_data(x=x, y=y, data = Rdd, cutpoint = 0)

OLSmodel <- lm(y ~ x, data = Rdd)
OLSmodel


#let's plot our data!
plot(x = Rdd$x,
     y = Rdd$y,
     main = "Scatterplot",
     xlab = "x",
     ylab = "y",
     pch = 20,
     ylim = c(-6, 8),
     xlim = c(-1, 1),
     cex.main = 0.8)

# add horizontal dashed line for 0
abline(v = 0, lty = 2, col = "darkred")

# add the estimated regression line
abline(Rdd, 
       lwd = 1.8, 
       col = "steelblue")

reg_para = rdd_reg_lm(rdd_object = Rdd_rdd)
summary(reg_para)
plot(reg_para)

# print robust coefficient summary
coeftest(reg_para, vcov. = vcovHC, type = "HC1")



#run a nonparametric local linear regression with IK optimal bandwidth
reg_nonpara = rdd_reg_np(rdd_object = Rdd_rdd)
summary(reg_nonpara)
plot(reg_nonpara)

dens_test(rdd_object = Rdd_rdd, plot = TRUE)


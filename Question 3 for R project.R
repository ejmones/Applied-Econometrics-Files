

rm(list=ls())  # Clear w

library(quantreg)
library(wooldridge)
library(AER)
library(dplyr)
library(ggplot2)
library(lmtest)
library(broom)
library(car)
Hospital_data <- read.csv("C:/Users/em4kn/OneDrive/Documents/Summer Semester 2023/EC320 Econometrics/Personal R Files/Hospital_data(1).csv")

OLSmodel <- lm(log(patient_revenue) ~ log(ER_visits) + log(surgeries) + log(beds) + occupancy,
               data = Hospital_data)
summary(OLSmodel)

LADmodel <- rq(log(patient_revenue) ~ log(ER_visits) + log(surgeries) + log(beds) + occupancy,
               data = Hospital_data)
summary(LADmodel)

multiLADmodel1 <- rq(log(patient_revenue) ~ log(ER_visits) + log(surgeries) + log(beds) + occupancy,
                   data = Hospital_data,
                   tau = 0.5)
summary(multiLADmodel1)
multiLADmodel5 <- rq(log(patient_revenue) ~ log(ER_visits) + log(surgeries) + log(beds) + occupancy,
                    data = Hospital_data,
                    tau = 0.9)
summary(multiLADmodel5)
summary(multiLADmodel)
tidy(multiLADmodel1)

anova_result <- anova(multiLADmodel1, multiLADmodel5)
anova_result




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


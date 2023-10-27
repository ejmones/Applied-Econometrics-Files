rm(list=ls())  # Clear w



data(charity,package = "wooldridge")
library(wooldridge)
library(dplyr)
library(ggplot2)
library(AER)
library(glm2)
library(mfx)
library(margins)

#Estimate a probit model for respond, using resplast, weekslast, propresp, mailsyear,
#and avggift as explanatory variables. Discuss whether the explanatory variable mailsyear
#is statistically significant. Clearly indicate the test statistic you make use of and the
#rejection rule you apply. In your answer discuss what interpretation you can give to the
#parameter estimate on mailsyear

###Probit Regression

# estimate the simple probit model
  
respondprobit <- glm(respond ~ resplast + weekslast + propresp + mailsyear + avggift,
                     family = binomial(link = "probit"),
                     data = charity)
# Computes the coefficient of each parameter
coeftest(respondprobit, vcov = vcovHC, type = "HC1")

# compute 95% confidence interval for coefficients in probit model
confint(respondprobit)

#average partial effect
probit2 <- probitmfx(respond ~ resplast + weekslast + propresp + mailsyear + avggift,
                     atmean = FALSE,
                     data = charity)
probit2$mfxest


respondmod1 <- lm(respond ~ resplast + weekslast + propresp + mailsyear + avggift, data = charity)
coeftest(respondmod1, vcov. = vcovHC)

#tobit MLE (vi)
gifttobit <- tobit(gift ~ resplast + weekslast + propresp + mailsyear + avggift,
                 data = charity)
summary(gifttobit)


#gives APE (used Bing AI to help modify code to calculate APE)
mean(pnorm(gifttobit$linear.predictors / gifttobit$scale) )
APE <- function(x) mean(pnorm(x / gifttobit$scale))
APE(gifttobit$linear.predictors - gifttobit$coefficients[1]) #resplast
APE(gifttobit$linear.predictors - gifttobit$coefficients[2]) #weekslast
APE(gifttobit$linear.predictors - gifttobit$coefficients[3]) #propresp
APE(gifttobit$linear.predictors - gifttobit$coefficients[4]) #mailyear
APE(gifttobit$linear.predictors - gifttobit$coefficients[5]) #avggift


giftOLS <- lm(gift ~ resplast + weekslast + propresp + mailsyear + avggift,
                   data = charity)
coeftest(giftOLS, vcov. = vcovHC)


###
# estimate the simple probit model
respondprobit <- glm(respond ~ resplast + weekslast + propresp + mailsyear + avggift,
                     family = binomial(link = "probit"),
                     data = charity)
# Computes the coefficient of each parameter
coeftest(respondprobit, vcov = vcovHC, type = "HC1")


#tobit MLE (vi)
gifttobit <- tobit(respond ~ resplast + weekslast + propresp + mailsyear + avggift,
                   data = charity)
summary(gifttobit)

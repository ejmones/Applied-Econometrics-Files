

rm(list=ls())  # Clear w

library(wooldridge)
library(readxl)
library(dplyr)
library(ggplot2)

dfof2017 = 

#count(charity, resplast)


#Estimate a linear probability model for respond, using resplast, weekslast, propresp,
#mailsyear, and avggift as explanatory variables. Carefully interpret the estimated
#coefficient on resplast and discuss whether the effect is statistically significant.


head(charity)
summary(charity)

# convert 'respond' to numeric
charity$respond <- as.numeric(charity$respond) - 1

#simple linear probability model
respondmod1 <- lm(respond ~ resplast, data = charity)
respondmod1

#let's plot our data!
plot(x = charity$resplast,
     y = charity$respond,
     main = "Scatterplot",
     xlab = "resplast",
     ylab = "Respond",
     pch = 20,
     ylim = c(-0.4, 1.4),
     xlim = c(0, 10),
     cex.main = 0.8)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Did Respond")
text(2.5, -0.1, cex= 0.8, "Didn't Respond")

# add the estimated regression line
abline(respondmod1, 
       lwd = 1.8, 
       col = "steelblue")

# print robust coefficient summary
coeftest(respondmod1, vcov. = vcovHC, type = "HC1")



# estimate the model
respondmod1 <- lm(respond ~ resplast + weekslast + propresp + mailsyear + giftlast + avggift, data = charity)
coeftest(respondmod1, vcov. = vcovHC)

# add model for more  
abline(respondmod1,
       lwd = 1.8,
       col = "red")





#Estimate a probit model for respond, using resplast, weekslast, propresp, mailsyear,
#and avggift as explanatory variables. Discuss whether the explanatory variable mailsyear
#is statistically significant. Clearly indicate the test statistic you make use of and the
#rejection rule you apply. In your answer discuss what interpretation you can give to the
#parameter estimate on mailsyear

###Probit Regression

# estimate the simple probit model
respondprobit1 <- glm(respond ~ resplast,
                      family = binomial(link = "probit"),
                      data = charity)
respondprobit2 <- glm(respond ~ resplast + weekslast + propresp + mailsyear + giftlast + avggift, data = charity)
coeftest(respondprobit1, vcov. = vcovHC)


# plot data
plot(x = charity$resplast,
     y = charity$respond,
     main = "Probit Model of the Probability of Response, Given Response Last Time",
     xlab = "Resplast",
     ylab = "Respond",
     pch = 20,
     ylim = c(-0.4, 1.4),
     cex.main = 0.85)

# add horizontal dashed lines and text
abline(h = 1, lty = 2, col = "darkred")
abline(h = 0, lty = 2, col = "darkred")
text(2.5, 0.9, cex = 0.8, "Mortgage denied")
text(2.5, -0.1, cex= 0.8, "Mortgage approved")

# add estimated regression line
x <- seq(0, 3, 0.01)
y <- predict(respondprobit1, list(resplast = x), type = "response")

  lines(x, y, lwd = 1.5, col = "steelblue")

# 1. compute predictions for P/I ratio = 0.3, 0.4
predictions <- predict(respondprobit1, 
                       newdata = data.frame("resplast" = c(0.3, 0.4)),
                       type = "response")

# 2. Compute difference in probabilities
diff(predictions)

respondprobit2 <- glm(respond ~ resplast + weekslast + propresp + mailsyear + avggift, data = charity)


coeftest(respondprobit2, vcov. = vcovHC, type = "HC1")

# 1. compute predictions for P/I ratio = 0.3
predictions <- predict(respondprobit2, 
                       newdata = data.frame("black" = c("no", "yes"), 
                                            "resplast" = c(0.3, 0.3)),
                       type = "response")

# 2. compute difference in probabilities
diff(predictions)


















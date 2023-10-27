# load the package AER and the STAR dataset
library(AER)
data(STAR)

# get an overview
head(STAR, 2)

dim(STAR)

names(STAR)

# drop NA recordings for the first observation and print to the console
STAR[1, !is.na(STAR[1, ])]


# compute differences Estimates for each grades
fmk <- lm(I(readk + mathk) ~ stark, data = STAR)
fm1 <- lm(I(read1 + math1) ~ star1, data = STAR)
fm2 <- lm(I(read2 + math2) ~ star2, data = STAR)
fm3 <- lm(I(read3 + math3) ~ star3, data = STAR)

# obtain coefficient matrix using robust standard errors
coeftest(fmk, vcov = vcovHC, type= "HC1")
coeftest(fm1, vcov = vcovHC, type= "HC1")
coeftest(fm2, vcov = vcovHC, type= "HC1")
coeftest(fm3, vcov = vcovHC, type= "HC1")


# compute robust standard errors for each model and gather them in a list
rob_se_1 <- list(sqrt(diag(vcovHC(fmk, type = "HC1"))),
                 sqrt(diag(vcovHC(fm1, type = "HC1"))),
                 sqrt(diag(vcovHC(fm2, type = "HC1"))),
                 sqrt(diag(vcovHC(fm3, type = "HC1"))))


library(stargazer)



stargazer(fmk,fm1,fm2,fm3,
          title = "Project STAR: Differences Estimates",
          header = FALSE, 
          type = "latex",
          model.numbers = F,
          omit.table.layout = "n",
          digits = 3, 
          column.labels = c("K", "1", "2", "3"),
          dep.var.caption  = "Dependent Variable: Grade",
          dep.var.labels.include = FALSE,
          se = rob_se_1)


# load packages 'dplyr' and 'tidyr' for data wrangling functionalities
library(dplyr)
library(tidyr)

# generate subset with kindergarten data
STARK <- STAR %>% 
  transmute(gender,
            ethnicity,
            stark,
            readk,
            mathk,
            lunchk,
            experiencek,
            schoolidk) %>% 
  mutate(black = ifelse(ethnicity == "afam", 1, 0),
         race = ifelse(ethnicity == "afam" | ethnicity == "cauc", 1, 0),
         boy = ifelse(gender == "male", 1, 0))

# estimate the models 
gradeK1 <- lm(I(mathk + readk) ~ stark + experiencek, 
              data = STARK)

gradeK2 <- lm(I(mathk + readk) ~ stark + experiencek + schoolidk, 
              data = STARK)

gradeK3 <- lm(I(mathk + readk) ~ stark + experiencek + boy + lunchk 
              + black + race + schoolidk, 
              data = STARK)

# obtain robust inference on the significance of coefficients
coeftest(gradeK1, vcov. = vcovHC, type = "HC1")
coeftest(gradeK2, vcov. = vcovHC, type = "HC1")[1:4, ]
coeftest(gradeK3, vcov. = vcovHC, type = "HC1")[1:7, ]
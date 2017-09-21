# Alexander Durbin
# STAT5514 Homework 4
# Dual Regression

library(tidyverse)

raw <- read.table('data.txt', header = TRUE)
head(raw)

lmfit <- lm(Lab ~ Field, data = raw)
ob0 <- coefficients(lmfit)[1]
ob1 <- coefficients(lmfit)[2]

resid <- residuals(lmfit)
resid2 <- resid^2

glmfit <- glm(resid2 ~ raw$Field, family = Gamma(link = "log"))

w = rep(0, nrow(raw))

for(i in 1:nrow(raw)){
  
  w[i] = 1 / exp(ob0 + ob1 * raw$Field[i])
  
}


raw$Lab <- sqrt(w) * raw$Lab
raw$Field <- sqrt(w) * raw$Field

lmfit_new <- lm(Lab ~ Field, data = raw)

# so assuming i am computing this correctly keep wrap this in a while loop 
# after initial estimates old, new
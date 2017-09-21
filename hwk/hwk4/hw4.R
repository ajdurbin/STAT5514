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

if(all(coefficients(lmfit) - coefficients(lmfit_new)) < 1E-5) print('alex')

# so assuming i am computing this correctly keep wrap this in a while loop 
# after initial estimates old, new


# new start ---------------------------------------------------------------

raw <- read.table('data.txt', header = TRUE)
y <- raw$Lab
x <- raw$Field
f1 <- lm(y ~ x)
bo <- coefficients(f1)
r2 <- residuals(f1)^2
f2 <- glm(r2 ~ x, family = Gamma(link = 'log'), maxit = 100)
w = rep(0, nrow(raw))
w <- sapply(x, function(g) return(1 / exp(coefficients(f1)[1] + coefficients(f1)[2] * g)))
y <- sqrt(w) * y
x <- sqrt(w) * x
bn <- coefficients(lm(y ~ x))

while(all(abs(bn - bo) > 1E-5)){
  
  f1 <- lm(y ~ x)
  bo <- coefficients(f1)
  r2 <- residuals(f1)^2
  f2 <- glm(r2 ~ x, family = Gamma(link = 'log'), maxit = 100)
  w = rep(0, nrow(raw))
  w <- sapply(x, function(g) return(1 / exp(coefficients(f1)[1] + coefficients(f1)[2] * g)))
  y <- sqrt(w) * y
  x <- sqrt(w) * x
  bn <- coefficients(lm(y ~ x))
  
}

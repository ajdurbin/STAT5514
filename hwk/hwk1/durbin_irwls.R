# Alexander Durbin
# Homework 1 part 2
# Iterative reweighted least squares for logistic regression

rm(list = ls())
options(stringsAsFactors = FALSE)

library(tidyverse)

raw <- read.table('kyphosis.txt', header = TRUE)

x <- raw %>%
    select(Age, Number, Start) %>% 
    mutate(Intercept = 1) %>% 
    select(Intercept, Age, Number, Start)

y <- raw %>% 
    select(Kyphosis) %>% 
    mutate(Indicator = ifelse(Kyphosis == 'present', 1, 0)) %>% 
    select(Indicator)



initial_fit <- lm(y$Indicator ~ x$Age + x$Number + x$Start)
beta_0 <- coefficients(initial_fit)

flag <- TRUE
iter <- 0

while(flag){
    
    # this gives the multiplications for the coefficients and the variables
    eta <- t(apply(x, 1, function(g) g*beta_0))
    
    # this sums up across the rows
    eta <- apply(eta, 1, sum)
    
    w <- exp(eta) / (1 + exp(eta))^2
    mu <- exp(eta) / (1 + exp(eta))
    
    W <- diag(w)
    
    beta_new <- beta_0 + solve( t(as.matrix(x)) %*% W %*% as.matrix(x)) %*% t(as.matrix(x)) %*% (as.matrix(y) - mu)
    
    # getting warning need to test on ALL the coefficients
    if( abs(beta_new - beta_0) < 0.00001){
        
        flag <- FALSE
        
    } else{
        
        beta_0 <- beta_new
        iter <- iter + 1
        
    }
    
}

beta_new
iter

r_says <- glm(y$Indicator ~ x$Age + x$Number + x$Start, family = "binomial")
summary(r_says)
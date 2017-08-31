# Alexander Durbin
# Homework 1 part 1
# Newton-Raphson method for logistic regression

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
    tmp <- t(apply(x, 1, function(g) g*beta_0))
    
    # this sums up across the rows
    tmp <- apply(tmp, 1, sum)
    
    # this is the actual exponential terms now
    h <- exp(tmp) / (1 + exp(tmp))
    
    # component multiplications for h
    comps <- data.frame(h,
                        x1h = x$Age * h * (1-h),
                        x2h = x$Number * h * (1-h),
                        x3h = x$Start * h * (1-h), 
                        x1sqh = x$Age^2 * h * (1-h),
                        x2sqh = x$Number^2 * h * (1-h),
                        x3sqh = x$Start^2 * h * (1-h),
                        x1x2h = x$Age * x$Number * h * (1-h), 
                        x1x3h = x$Age * x$Start * h * (1-h),
                        x2x3h = x$Number * x$Start * h * (1-h))
    
    hes = matrix(data = NA, nrow = 4, ncol = 4)
    hes[1, 1] = -sum(comps$h * (1 - comps$h))
    hes[2, 2] = -sum(comps$x1sqh)
    hes[3, 3] = -sum(comps$x2sqh)
    hes[4, 4] = -sum(comps$x3sqh)
    
    hes[1, 2] = -sum(comps$x1h)
    hes[2, 1] = hes[1, 2]
    hes[1, 3] = -sum(comps$x2h)
    hes[3, 1] = hes[1, 3]
    hes[1, 4] = -sum(comps$x3h)
    hes[4, 1] = hes[1, 4]
    
    hes[2, 3] = -sum(comps$x1x2h)
    hes[3, 2] = hes[2, 3]
    hes[2, 4] = -sum(comps$x1x3h)
    hes[4, 2] = hes[2, 4]
    hes[3, 4] = -sum(comps$x2x3h)
    hes[4, 3] = hes[3, 4]
    
    u <- c(sum(y$Indicator - h), 
           sum(x$Age * (y$Indicator - h)),
           sum(x$Number * (y$Indicator - h)),
           sum(x$Start * (y$Indicator - h)))
    
    beta_new <- beta_0 - solve(hes) %*% u
    
    # getting warning need to test on ALL the coefficients
    if( abs(beta_new - beta_0) < 0.00001){
        
        flag <- FALSE
        
    } else{
        
        beta_0 <- beta_new
        iter <- iter + 1
        
    }
    
}

(beta_new)
(iter)

# now to test this using the actual data

r_says <- glm(y$Indicator ~ x$Age + x$Number + x$Start, family = "binomial")
summary(r_says)

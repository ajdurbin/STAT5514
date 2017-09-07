rm(list = ls())

set.seed(12345)

library(tidyverse)
library(MASS)
library(galts)
library(quantreg)
library(tibble)


# to cut down on repetition
rf <- function(dep, ind, arg){
  
  if (arg == 'lm'){
    
    lmfit <- lm(dep ~ ind)
    
  } else if (arg == 'rlm'){
    
    lmfit <- rlm(dep ~ ind)
    
  } else if (arg == 'lad'){
    
    lmfit <- rq(dep ~ ind)
    
  } else {
    
    lmfit <- ga.lts(dep ~ ind, lower = -1000, upper = 1000)

  }
  
  my_list <- list(b0 = coefficients(lmfit)[1], b1 = coefficients(lmfit)[2])
  return(my_list)
  
}


b0 <- 1
b1 <- 2

b0_s <- tibble(l1 = rep(0, 100),
               l2 = rep(0, 100),
               l3 = rep(0, 100),
               r1 = rep(0, 100),
               r2 = rep(0, 100),
               r3 = rep(0, 100),
               d1 = rep(0, 100),
               d2 = rep(0, 100),
               d3 = rep(0, 100),
               t1 = rep(0, 100),
               t2 = rep(0, 100),
               t3 = rep(0, 100))

b1_s <- tibble(l1 = rep(0, 100),
               l2 = rep(0, 100),
               l3 = rep(0, 100),
               r1 = rep(0, 100),
               r2 = rep(0, 100),
               r3 = rep(0, 100),
               d1 = rep(0, 100),
               d2 = rep(0, 100),
               d3 = rep(0, 100),
               t1 = rep(0, 100),
               t2 = rep(0, 100),
               t3 = rep(0, 100))


for (i in 1:100){
  
  raw <- tibble(x = rnorm(100, 0, 1), 
                e1 = rnorm(100, 0, 2),
                e2 = rlnorm(100, 0, 2),
                e3 = rcauchy(100, 0 ,2),
                y1 = b0 + b1 * x + e1,
                y2 = b0 + b1 * x + e2,
                y3 = b0 + b1 * x + e3)
  
  
  # lse
  fit <- rf(raw$y1, raw$x, 'lm')
  b0_s$l1[i] <- fit$b0
  b1_s$l1[i] <- fit$b1
  
  fit <- rf(raw$y2, raw$x, 'lm')
  b0_s$l2[i] <- fit$b0
  b1_s$l2[i] <- fit$b1
  
  fit <- rf(raw$y3, raw$x, 'lm')
  b0_s$l3[i] <- fit$b0
  b1_s$l3[i] <- fit$b1
  
  # robust
  fit <- rf(raw$y1, raw$x, 'rlm')
  b0_s$r1[i] <- fit$b0
  b1_s$r1[i] <- fit$b1
  
  fit <- rf(raw$y2, raw$x, 'rlm')
  b0_s$r2[i] <- fit$b0
  b1_s$r2[i] <- fit$b1
  
  fit <- rf(raw$y3, raw$x, 'rlm')
  b0_s$r3[i] <- fit$b0
  b1_s$r3[i] <- fit$b1
  
  # lad
  fit <- rf(raw$y1, raw$x, 'lad')
  b0_s$d1[i] <- fit$b0
  b1_s$d1[i] <- fit$b1
  
  fit <- rf(raw$y2, raw$x, 'lad')
  b0_s$d2[i] <- fit$b0
  b1_s$d2[i] <- fit$b1
  
  fit <- rf(raw$y3, raw$x, 'lad')
  b0_s$d3[i] <- fit$b0
  b1_s$d3[i] <- fit$b1
  
  # least trimmed
  fit <- rf(raw$y1, raw$x, 'lts')
  b0_s$t1[i] <- fit$b0
  b1_s$t1[i] <- fit$b1
  
  fit <- rf(raw$y2, raw$x, 'lts')
  b0_s$t2[i] <- fit$b0
  b1_s$t2[i] <- fit$b1
  
  fit <- rf(raw$y3, raw$x, 'lts')
  b0_s$t3[i] <- fit$b0
  b1_s$t3[i] <- fit$b1
  
}

b0_s_m <- colMeans(b0_s)
b1_s_m <- colMeans(b1_s)
b0_s_v <- sapply(b0_s, var)
b1_s_v <- sapply(b1_s, var)
b0_s_b <- b0 - b0_s_m
b1_s_b <- b1 - b1_s_m
b0_mse <- b0_s_b^2 + b0_s_v
b1_mse <- b1_s_b^2 + b1_s_v
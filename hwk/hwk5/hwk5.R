# Alexander Durbin
# STAT5514 Homework 5

# rm(list = ls())
# options(stringsAsFactors = FALSE)
# library(minpack.lm)


# data formatting ---------------------------------------------------------


# formatting
# raw <- readxl::read_excel("Herbicide.xls")
# raw <- read.csv("Herbicide.csv")
# colnames(raw) <- NULL
# raw <- raw[2:8, 2:5]
# d1 <- raw[, c(2, 1)]
# colnames(d1) <- c('y', 'x')
# d2 <- raw[, c(4,3)]
# colnames(d2) <- c('y', 'x')
# d1$y <- as.numeric(d1$y)
# d2$y <- as.numeric(d2$y)
# d1$x <- as.numeric(d1$x)
# d2$x <- as.numeric(d2$x)

# filtering and formatting of data after exporting the xlsx to csv
# raw <- read.csv("Herbicide.csv")
# colnames(raw) <- NULL
# raw <- raw[2:nrow(raw), ]
# raw <- raw[, 2:ncol(raw)]
# # obs <- raw[, 1:2]
# obs <- raw[, 3:4]
# x <- as.numeric(obs[, 1])
# y <- as.numeric(obs[, 2])


# function declarations ---------------------------------------------------


# functions for D matrix and residuals
# ass_fun <- function(data, old){
#   val <- old[1] * (old[2] * data^old[3]) / (1 + old[2] * data^old[3])
#   return(val)
# }
# 
# alpha_fun <- function(data, old){
#   val <- (old[2] * data ^old[3]) / (1 + old[2] * data^old[3])
#   return(val)
# }
# 
# beta_fun <- function(data, old){
#   val <- (old[1] * data^old[3]) / (1 + old[2] * data^old[3])^2
#   return(val)
# }
# 
# gamma_fun <- function(data, old){
#   val <- (old[1] * old[2] * data^old[3] * log(data)) / (1 + old[2] * data^old[3])^2
#   return(val)
# }
# 
# 
# # gauss-newton algorithm --------------------------------------------------
# 
# 
# # algorithm
# travel <- 1
# iter <- 1
# tol <- 1e-5
# maxiter <- 1000
# new <- c(97, 0.1, -2.2)
# 
# while(travel > tol && iter < maxiter){
#   
#   old <- new
#   r <- y - ass_fun(x, old)
#   sigma_sq <- sum(r^2) / (nrow(obs1) - 3)
#   v <- diag(sigma_sq, nrow(obs1))
#   d <- matrix(data = NA, nrow = nrow(obs1), ncol = 3)
#   d[, 1] <- alpha_fun(x, old)
#   d[, 2] <- beta_fun(x, old)
#   d[, 3] <- gamma_fun(x, old)
#   new <- old + solve(t(d) %*% solve(v) %*% d) %*% t(d) %*% solve(v) %*% r
#   travel <- sum(abs(new - old))
#   iter <- iter + 1
#   
# }



# using nls ---------------------------------------------------------------

# rm(list = ls())
# options(stringsAsFactors = FALSE)
# library(minpack.lm)
# 
# raw <- read.csv("Herbicide.csv")
# colnames(raw) <- NULL
# raw <- raw[2:nrow(raw), ]
# raw <- raw[, 2:ncol(raw)]
# # obs <- raw[, 1:2]
# obs <- raw[, 3:4]
# x <- as.numeric(obs[, 1])
# y <- as.numeric(obs[, 2])
# 
# nlfit <- nls(y ~ (p1 * p2 * x^p3) / (1 + p2 * x^p3), 
#              start = c(p1 = 100, p2 = 1, p3 = -1))
# 
# plot(x,y)
# lines(x, predict(nlfit), lty = 2, col = "red", lwd = 3)
# 
# nlslmfit <- nlsLM(y ~ (p1 * p2 * x^p3) / (1 + p2 * x^p3), 
#                   start = c(p1 = 10, p2 = 0, p3 = -1))
# 
# plot(x,y)
# lines(x, predict(nlslmfit), lty = 2, col = "red", lwd = 3)




# bootstrapping ci --------------------------------------------------------


rm(list = ls())
options(stringsAsFactors = FALSE)
library(minpack.lm)
library(nls2)
library(mgcv)

raw <- read.csv("Herbicide.csv")
colnames(raw) <- NULL
raw <- raw[2:nrow(raw), ]
raw <- raw[, 2:ncol(raw)]
obs <- raw[, 1:2]
# obs <- raw[, 3:4]
x <- as.numeric(obs[, 1])
y <- as.numeric(obs[, 2])


b_coef <- matrix(data = NA, ncol = 3, nrow = 10000)
ss_res <- rep(0, 10000)

for(i in 1:10000){
  
  boot <- sample(1:7, 7, replace = TRUE)
  x_star <- x[boot]
  y_star <- y[boot]
  
  try({
    
    nlfit <- nls2(y_star ~ (p1 * p2 * x_star^p3) / (1 + p2 * x_star^p3), 
                start = c(p1 = 100, p2 = 1, p3 = -1),
                control = nls.control(maxiter = 1e3,
                                     minFactor = .Machine$double.eps,
                                     tol = 1e-5),
                algorithm = "default")
  }, silent = TRUE)
  
  if(class(nlfit) == 'try-error'){
    b_coef[i, ] <- NA
  } else {
    b_coef[i, ] <- coefficients(nlfit)
    ss_res[i] <- sum(residuals(nlfit)^2)
  }
  

}

dim(uniquecombs(b_coef))
length(unique(ss_res))
b_coef <- unique(b_coef)
ss_res <- unique(ss_res)
summary(ss_res)

# compare best estimate from all 7 data points to mean of bootstrapped sample
best <- nls2(y ~ (p1 * p2 * x^p3) / (1 + p2 * x^p3), 
              start = c(p1 = 100, p2 = 1, p3 = -1),
              control = nls.control(maxiter = 1e3,
                                    minFactor = .Machine$double.eps,
                                    tol = 1e-5),
              algorithm = "default")
coefficients(best)
c(mean(b_coef[, 1]), mean(b_coef[, 2]), mean(b_coef[, 3]))

# plot
plot(x, y)
lines(x, predict(best), lty = 2, col = "red", lwd = 3)

# ci
quantile(b_coef[, 1], probs = c(0.025, 0.975))
quantile(b_coef[, 2], probs = c(0.025, 0.975))
quantile(b_coef[, 3], probs = c(0.025, 0.975))

# nlstools ----------------------------------------------------------------


# rm(list = ls())
# options(stringsAsFactors = FALSE)
# library(minpack.lm)
# library(nlstools)
# 
# 
# raw <- read.csv("Herbicide.csv")
# colnames(raw) <- NULL
# raw <- raw[2:nrow(raw), ]
# raw <- raw[, 2:ncol(raw)]
# # obs <- raw[, 1:2]
# obs <- raw[, 3:4]
# x <- as.numeric(obs[, 1])
# y <- as.numeric(obs[, 2])
# 
# fit <- nls(y ~ (p1 * p2 * x^p3) / (1 + p2 * x^p3), 
#            start = c(p1 = 100, p2 = 1, p3 = -1),
#            control = nls.control(maxiter = 1e5,
#                                  minFactor = .Machine$double.eps,
#                                  tol = 1e-1))
# 
# boot <- nlsBoot(fit, niter = 1000)


# problem 1 ---------------------------------------------------------------


rm(list = ls())
options(stringsAsFactors = FALSE)
library(minpack.lm)
library(nls2)
library(mgcv)
library(robustbase)

raw <- read.csv("EffRange.csv")
raw <- raw[, 2:ncol(raw)]
obs <- raw[, 1:2]
colnames(obs) <- c('y', 'x')


b_coef <- matrix(data = NA, ncol = 3, nrow = 10000)
ss_res <- rep(0, 10000)

for(i in 1:10000){
  
  boot <- sample(1:7, 7, replace = TRUE)
  boot_sample <- obs[boot, ]
  
  try({
    
    nlfit <- nlrob(formula = y ~ a / (1 + b * exp(-k * x)), 
                  start = list(a = 40, b = 400, k = 0.1),
                  data = boot_sample,
                  control = nls.control(maxiter = 1e3,
                                    minFactor = .Machine$double.eps,
                                    tol = 1e-5))
  }, silent = TRUE)
  
  if(class(nlfit) == 'try-error'){
    b_coef[i, ] <- NA
  } else {
    b_coef[i, ] <- coefficients(nlfit)
    ss_res[i] <- sum(residuals(nlfit)^2)
  }
  
  
}

dim(uniquecombs(b_coef))
length(unique(ss_res))
b_coef <- unique(b_coef)
ss_res <- unique(ss_res)
summary(ss_res)

# compare best estimate from all 7 data points to mean of bootstrapped sample
best <- nlrob(formula = y ~ a / (1 + b * exp(-k * x)),
             start = list(a = 40, b = 400, k = 0.1),
             data = obs)
coefficients(best)
c(mean(b_coef[, 1]), mean(b_coef[, 2]), mean(b_coef[, 3]))

# plots
plot(obs$x, obs$y)
lines(obs$x, predict(best), lty = 2, col = "red", lwd = 3)

# ci
quantile(b_coef[, 1], probs = c(0.025, 0.975))
quantile(b_coef[, 2], probs = c(0.025, 0.975))
quantile(b_coef[, 3], probs = c(0.025, 0.975))
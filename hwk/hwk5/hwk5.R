# Alexander Durbin
# STAT5514 Homework 5

rm(list = ls())
options(stringsAsFactors = FALSE)
library(minpack.lm)


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
raw <- read.csv("Herbicide.csv")
colnames(raw) <- NULL
raw <- raw[2:nrow(raw), ]
raw <- raw[, 2:ncol(raw)]
# obs <- raw[, 1:2]
obs <- raw[, 3:4]
x <- as.numeric(obs[, 1])
y <- as.numeric(obs[, 2])


# function declarations ---------------------------------------------------


# functions for D matrix and residuals
ass_fun <- function(data, old){
  val <- old[1] * (old[2] * data^old[3]) / (1 + old[2] * data^old[3])
  return(val)
}

alpha_fun <- function(data, old){
  val <- (old[2] * data ^old[3]) / (1 + old[2] * data^old[3])
  return(val)
}

beta_fun <- function(data, old){
  val <- (old[1] * data^old[3]) / (1 + old[2] * data^old[3])^2
  return(val)
}

gamma_fun <- function(data, old){
  val <- (old[1] * old[2] * data^old[3] * log(data)) / (1 + old[2] * data^old[3])^2
  return(val)
}


# gauss-newton algorithm --------------------------------------------------


# algorithm
travel <- 1
iter <- 1
tol <- 1e-5
maxiter <- 1000
new <- c(97, 0.1, -2.2)

while(travel > tol && iter < maxiter){
  
  old <- new
  r <- y - ass_fun(x, old)
  sigma_sq <- sum(r^2) / (nrow(obs1) - 3)
  v <- diag(sigma_sq, nrow(obs1))
  d <- matrix(data = NA, nrow = nrow(obs1), ncol = 3)
  d[, 1] <- alpha_fun(x, old)
  d[, 2] <- beta_fun(x, old)
  d[, 3] <- gamma_fun(x, old)
  new <- old + solve(t(d) %*% solve(v) %*% d) %*% t(d) %*% solve(v) %*% r
  travel <- sum(abs(new - old))
  iter <- iter + 1
  
}



# using nls ---------------------------------------------------------------


nlfit <- nls(y ~ (p1 * p2 * x^p3) / (1 + p2 * x^p3), 
             start = c(p1 = 100, p2 = 1, p3 = -1))

plot(x,y)
lines(x, predict(nlfit), lty = 2, col = "red", lwd = 3)

nlslmfit <- nlsLM(y ~ (p1 * p2 * x^p3) / (1 + p2 * x^p3), 
                  start = c(p1 = 10, p2 = 0, p3 = -1))

plot(x,y)
lines(x, predict(nlslmfit), lty = 2, col = "red", lwd = 3)

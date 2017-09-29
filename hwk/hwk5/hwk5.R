# Alexander Durbin
# STAT5514 Homework 5

rm(list = ls())

# formatting
raw <- readxl::read_excel("Herbicide.xls")
colnames(raw) <- NULL
raw <- raw[2:8, 2:5]
d1 <- raw[, c(2, 1)]
colnames(d1) <- c('y', 'x')
d2 <- raw[, c(4,3)]
colnames(d2) <- c('y', 'x')
d1$y <- as.numeric(d1$y)
d2$y <- as.numeric(d2$y)
d1$x <- as.numeric(d1$x)
d2$x <- as.numeric(d2$x)


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
  val <- (old[1] * data ^old[3]) / (1 + old[2] * data ^old[3])^2
  return(val)
}

gamma_fun <- function(data, old){
  val <- (old[1] * old[2] * data * log(data)) / (1 + old[2] * data^old[3])^2
}


# algorithm
travel <- 1
iter <- 1
tol <- 1e-5
maxiter <- 1000
new <- coefficients(lm(y ~ x + I(x^2), data = d1))

while(travel > tol & iter < maxiter){
  
  old <- new
  r <- d1$y - ass_fun(d1$x, old)
  sigma_sq <- r^2 / (nrow(d1) - 3)
  v <- diag(sigma_sq)
  d <- matrix(data = NA, nrow = nrow(d1), ncol = 3)
  d[, 1] <- alpha_fun(d1$x, old)
  d[, 2] <- beta_fun(d1$x, old)
  d[, 3] <- gamma_fun(d1$x, old)
  new <- old + solve(t(d) %*% solve(v) %*% d) %*% t(d) %*% solve(v) %*% r
  travel <- sum(abs(new - old))
  iter <- iter + 1
  
}
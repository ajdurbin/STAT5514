# Alexander Durbin
# Homework 1
# Newton-Raphson method for logistic regression

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

# this gives the multiplications for the coefficients and the variables
tmp <- t(apply(x, 1, function(g) g*beta_0))

# this sums up across the rows
tmp <- apply(tmp, 1, sum)

# this is the actual exponential terms now
h <- exp(tmp) / (1 + exp(tmp))

# component multiplications for h and u
# assume this is correct for now
comps <- data.frame(h,
                    x1h = x$Age * H * (1-H),
                    x2h = x$Number * H * (1-H),
                    x3h = x$Start * H * (1-H), 
                    x1sqh = x$Age^2 * H * (1-H),
                    x2sqh = x$Number^2 * H * (1-H),
                    x3sqh = x$Start^2 * H * (1-H),
                    x1x2h = x$Age * x$Number * H * (1-H), 
                    x1x3h = x$Age * x$Start * H * (1-H),
                    x2x3h = x$Number * x$Start * H * (1-H))

hes = matrix(data = NA, nrow = 4, ncol = 4)
hes[1, 1] = -sum(comps$h * (1 - comps$h))
hes[2, 2] = -sum(comps$x1sqh)
hes[3, 3] = -sum(comps$x2sqh)
hes[4, 4] = -sum(comps$x3sqh)
# Alexander Durbin
# Homework 1
# Newton-Raphson method for logistic regression

options(stringsAsFactors = FALSE)

library(tidyverse)

raw <- read.table('kyphosis.txt', header = TRUE)

X <- raw %>%
    select(Age, Number, Start) %>% 
    mutate(Intercept = 1) %>% 
    select(Intercept, Age, Number, Start)

Y <- raw %>% 
    select(Kyphosis) %>% 
    mutate(Indicator = ifelse(Kyphosis == 'present', 1, 0)) %>% 
    select(Indicator)

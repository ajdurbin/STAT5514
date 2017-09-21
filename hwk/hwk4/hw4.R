# Alexander Durbin
# STAT5514 Homework 4
# Dual Regression

library(tidyverse)

raw <- read.table('data.txt', header = TRUE)
head(raw)

lmfit <- lm(Lab ~ Field, data = raw)
ob0 <- coefficients(lmfit)
ob1 <- coefficients(lmfit)


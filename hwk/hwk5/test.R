# fit some sort of log function
raw <- read.csv("EffRange.csv")
raw <- raw[, 2:ncol(raw)]
obs <- raw[, 1:2]
colnames(obs) <- c('y', 'x')
plot(obs$x, obs$y)

pts <- seq(min(obs$x), max(obs$x), by = 0.01)
nlfit <- nls2(formula = y ~ a * x / (b + x),
              data = obs)
a <- coefficients(nlfit)[1]
b <- coefficients(nlfit)[2]
preds <- a * pts / (b + pts)
plot(obs$x, obs$y)
lines(pts, preds)

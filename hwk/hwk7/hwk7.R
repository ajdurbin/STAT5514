library(tidyverse)

set.seed(1127)

# data
x <- runif(n = 1000, min = 0, max = 3*pi/2)
e <- rnorm(n = 1000, mean = 0, sd = 0.1)
y <- sin(x + 0.1) + e
df <- data.frame(x, e, y)

# knot locations
K <- 30
k <- 1:K
knots <- quantile(x, probs = (k+1)/(K+2))

# linear spline X matrix
X <- cbind(1, x)
for(i in k){
  X <- cbind(X, (x - knots[i]) * as.numeric(x > knots[i]))
}

# build D
D <- diag(c(0, 0, rep(1, K)))

# set smoothing parameter
lambda <- 0

# solve for beta hat
beta_hat <- solve(t(X) %*% X + lambda * D, t(X) %*% y)

# get estimates
y_hat <- X %*% beta_hat
df$y_hat <- y_hat

# plot
ggplot(data = df) +
  geom_point(mapping = aes(x = x, y = y), alpha = 0.3, show.legend = FALSE) +
  geom_vline(xintercept = knots, alpha = 0.3) +
  stat_function(mapping = aes(color = "Truth"), fun = function(x) {sin(x + 0.1)}, lwd = 1) +
  geom_line(mapping = aes(x = x, y = y_hat, color = "Fitted")) +
  scale_color_manual(name = "Fitted", values = c("red", "blue")) +
  ggtitle(paste0("Fitted spline with ", K, " knots \nand smoothing parameter ", lambda))

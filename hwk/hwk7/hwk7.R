rm(list = ls())
library(tidyverse)

prediction_storage <- list("0" = NULL)
knot_storage <- list("0" = NULL)

x <- runif(n = 1000, min = 0, max = 3*pi/2)
e <- rnorm(n = 1000, mean = 0, sd = 0.1)
y <- sin(x + 0.1) + e
df <- data.frame(x, e, y)

for(num_knots in c(5, 10, 20, 30)){
    for(l in c(0, 10, 15, 20, 50, 1000)){
        
        # get knot locations
        K <- num_knots
        k <- 1:K
        knots <- quantile(x, probs = (k+1)/(K+2))
        
        # make linear spline matrix
        X <- cbind(1, x)
        for(i in k){
            X <- cbind(X, (x - knots[i]) * as.numeric(x > knots[i]))
        }
        
        # make D
        D <- diag(c(0, 0, rep(1, K)))
        
        # set smoothing parameter
        lambda <- l
        
        # get beta hat
        beta_hat <- solve(t(X) %*% X + lambda * D, t(X) %*% y)
        
        # get estimates
        y_hat <- X %*% beta_hat
        
        
        prediction_storage[[as.character(num_knots)]][[as.character(l)]] <- y_hat
        
    }
    knot_storage[[as.character(num_knots)]] <- knots
}

my_unpack <- function(my_list, which_k){
    
    my_list <- my_list[[which_k]]
    pckg <- my_list[[1]]
    for(i in 2:length(my_list)){
        tmp <- my_list[[i]]
        pckg <- cbind(pckg, tmp)
    }
    return(pckg)
}

test_preds <- my_unpack(prediction_storage, 2)

# plot
ggplot(data = df) +
    geom_point(mapping = aes(x = x, y = y), alpha = 0.3, show.legend = FALSE) +
    geom_vline(xintercept = knot_storage[[2]], alpha = 0.3) +
    stat_function(fun = function(x) {sin(x + 0.1)}, lwd = 1, color = "red") +
    geom_line(mapping = aes(x = x, y = test_preds[, 1], color = "0")) +
    geom_line(mapping = aes(x = x, y = test_preds[, 2], color = "10")) +
    geom_line(mapping = aes(x = x, y = test_preds[, 3], color = "15")) +
    geom_line(mapping = aes(x = x, y = test_preds[, 4], color = "20")) +
    geom_line(mapping = aes(x = x, y = test_preds[, 5], color = "50")) +
    geom_line(mapping = aes(x = x, y = test_preds[, 6], color = "1000")) +
    scale_color_discrete() +
    ggtitle("Linear spline with 5 Knots And Various Lambda")

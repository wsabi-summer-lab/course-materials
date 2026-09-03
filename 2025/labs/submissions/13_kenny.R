library(ggplot2)
library(tidyverse)

set.seed(13)

setwd("/Users/kennywatts/Documents/GitHub/summer-lab/2025/labs/data")

putts_train = read_csv("13_putts-train.csv.gz")

head(putts_train)

# 13.1 Mean across All Golfers

mean_all_golfers <- mean(putts_train$X)
mean_all_golfers

# 13.2 Mean for Each Golfer

mean_each_golfer <- putts_train$X
mean_each_golfer

# 13.3

k <- nrow(putts_train)

c <- mean_all_golfers * (1-mean_all_golfers)

putts_train$x_tilda <- putts_train$X/((c/putts_train$N)^0.5)

mean_x_tilda <- mean(putts_train$x_tilda)

tilda_sos <- sum((putts_train$x_tilda-mean_x_tilda)^2)

putts_train$theta_i_hat <- mean_x_tilda + (1 - (k-1)/tilda_sos)*(putts_train$x_tilda - mean_x_tilda)

putts_train$p_i <- (c/putts_train$N)^0.5 * putts_train$theta_i_hat

putts_train$p_i

# 13.4 

putts_train$H <- round(putts_train$X * putts_train$N)

putts_train$double_tilda <- asin(((putts_train$H + 3/8)/(putts_train$N + 3/4))^0.5)

putts_train$nu_i_squared <- 1/(4 * putts_train$N)

putts_train$new_xi_tilda <- putts_train$double_tilda/(putts_train$nu_i_squared^0.5)

new_x_tilda_sos <- sum((putts_train$new_xi_tilda - mean(putts_train$new_xi_tilda))^2)

putts_train$theta_i_tilda <- mean(putts_train$new_xi_tilda) + (1 - (k-1)/new_x_tilda_sos) * (putts_train$new_xi_tilda - mean(putts_train$new_xi_tilda))

putts_train$p_i_em2 <- sin((putts_train$nu_i_squared^0.5) * putts_train$theta_i_tilda)^2

# 13.5

sample_variance <- (1/(k-1)) * sum((putts_train$X - mean(putts_train$X))^2)

tau_hat_squared <- sample_variance - c * mean(1/putts_train$N)

putts_train$p_i_eb1 <- mean(putts_train$X) + (tau_hat_squared/(tau_hat_squared + (c/putts_train$N))) * (putts_train$X - mean(putts_train$X))

# 13.6

new_mu <- mean(putts_train$double_tilda)

sample_variance_double_tilda <- (1/(k-1)) * sum((putts_train$double_tilda - new_mu)^2)

mean_nu_squared <- mean(putts_train$nu_i_squared)

tau_squared <- sample_variance_double_tilda - mean_nu_squared

putts_train$theta_i_eb2 <- new_mu + (tau_squared/(tau_squared + putts_train$nu_i_squared)) * (putts_train$double_tilda - new_mu)

putts_train$p_i_eb2 <- (sin(putts_train$theta_i_eb2))^2

# Out of sample loss

putts_test = read_csv("13_putts-test.csv.gz")

head(putts_test)

mse_overall_mean <- mean((putts_test$X - mean_all_golfers)^2)
mse_overall_mean

mse_mle <- mean((putts_test$X - mean_each_golfer)^2)
mse_mle

mse_em1 <- mean((putts_test$X - putts_train$p_i)^2)
mse_em1

mse_em2 <- mean((putts_test$X - putts_train$p_i_em2)^2)
mse_em2

mse_eb1 <- mean((putts_test$X - putts_train$p_i_eb1)^2)
mse_eb1

mse_eb2 <- mean((putts_test$X - putts_train$p_i_eb2)^2)
mse_eb2

# Best is eb2, worst is player level mle

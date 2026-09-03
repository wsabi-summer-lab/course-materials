########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "readr"))

# load packages
library(dplyr)
library(readr)

#################
### LOAD DATA ###
#################

# run from the repository root
putt_data = read_csv("2026/lectures/data/03_first-putts.csv.gz", show_col_types = FALSE) |>
    transmute(
        player_first_name = `Player First Name`,
        player_last_name = `Player Last Name`,
        round = Round,
        shot = Shot,
        distance = `Distance to Pin` / 12,
        putt_made = as.integer(`Hole out`)
    ) |>
    filter(!is.na(distance), !is.na(putt_made), distance > 0) |>
    mutate(log_distance = log(distance))

#####################################
### LINEAR REGRESSION PUTT MODELS ###
#####################################

# simple linear regression
model_1 = lm(putt_made ~ distance, data = putt_data)
summary(model_1)

# transformed linear regression
model_2 = lm(
    putt_made ~ log_distance,
    data = putt_data
)
summary(model_2)

#######################################
### LOGISTIC REGRESSION PUTT MODELS ###
#######################################

# simple logistic regression on log distance
model_3 = glm(putt_made ~ log_distance, data = putt_data, family = "binomial")
summary(model_3)

# fitted probabilities with uncertainty on a grid
grid = data.frame(
    distance = seq(min(putt_data$distance), max(putt_data$distance), length.out = 200)
)
grid$log_distance = log(grid$distance)

pred = predict(model_3, newdata = grid, type = "link", se.fit = TRUE)

grid$eta_hat = pred$fit
grid$se_eta = pred$se.fit

grid$p_hat = plogis(grid$eta_hat)
grid$p_low = plogis(grid$eta_hat - 1.96 * grid$se_eta)
grid$p_high = plogis(grid$eta_hat + 1.96 * grid$se_eta)

# cubic logistic regression on log distance
model_4 = glm(
    putt_made ~ log_distance + I(log_distance^2) + I(log_distance^3),
    data = putt_data,
    family = "binomial"
)
summary(model_4)

#############
### SETUP ###
#############

rm(list=ls())

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

####################
### PARK EFFECTS ###
####################

# load data
park_data = read_csv("../data/05_park-effects.csv.gz")
names(park_data)

## 5.1.1
mean_runs_per_ballpark <- park_data %>% 
  group_by(PARK) %>% 
  summarise(mean_runs = mean(INN_RUNS))

set.seed(3)
n <- dim(park_data)[1]
train_indices <- sample(1:n, size = floor(0.8 * n))

# convert x variables to categorical
park_data$OT_YR <- as.factor(park_data$OT_YR)
park_data$DT_YR <- as.factor(park_data$DT_YR)
park_data$PARK  <- as.factor(park_data$PARK)

train <- park_data[train_indices, ]
test <- park_data[-train_indices, ]
  
## 5.1.3 model
multi_model <- lm(INN_RUNS ~ OT_YR + DT_YR + PARK + 0, data = train)
summary(multi_model)

# predict mean number of runs per ballpark
test <- test %>%
  mutate(exp_runs = predict(multi_model, newdata = test, type = "response"))

# mean predicted runs for each ballpark
exp_mean_runs_per_ballpark <- test %>% 
  group_by(PARK) %>% 
  summarise(exp_mean_runs = mean(exp_runs))

runs_per_ballpark <- full_join(mean_runs_per_ballpark, exp_mean_runs_per_ballpark, by = "PARK")
names(runs_per_ballpark)

runs_long <- runs_per_ballpark %>%
  pivot_longer(cols = c(mean_runs, exp_mean_runs),
               names_to = "type",
               values_to = "runs")

ggplot(runs_long, aes(x = reorder(PARK, -runs), y = runs, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Actual vs Expected Mean Runs per Ballpark",
       x = "Ballpark",
       y = "Mean Runs",
       fill = "Run Type") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

names(park_data)

# naive: mean runs per park
naive_estimates <- train %>% 
  group_by(PARK) %>% 
  summarise(naive_mean = mean(INN_RUNS))
test <- left_join(test, naive_estimates, by = "PARK")

names(test)

# MSE
mse_naive <- mean((test$INN_RUNS - test$naive_mean)^2)
mse_better <- mean((test$INN_RUNS - test$exp_runs)^2) # lower MSE

## find which park effects differ the most between the two estimates
runs_per_ballpark <- runs_per_ballpark %>%
  mutate(diff = mean_runs - exp_mean_runs) %>%
  arrange(desc(diff))
# visualize these differences
ggplot(runs_per_ballpark, aes(x = reorder(PARK, -diff), y = diff)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Difference in Actual vs Expected Mean Runs per Ballpark",
       x = "Ballpark",
       y = "Difference (Actual - Expected)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# ## below: beginings of attempt to do matrix method
# # X variable: make a matrix with columns being team names and park names. 
# # for each row, enter 1 if OT, DT, or park matches; 0 otherwise.
# # desired result: coefficients for each park. control variables: team names?
# teams_and_parks <- sort(unique(c(park_data$PARK, park_data$OT_YR, park_data$DT_YR)))
# length(teams_and_parks)
# 
# X <- matrix(0, nrow = nrow(park_data), ncol = length(teams_and_parks))
# colnames(X) <- c(paste0("B", teams_and_parks))
# X
# dim(X)
# 
# # Loop through each half inning to fill in the matrix
# for (i in 1:nrow(park_data)) {
#   row <- park_data[i,]
#   ot <- row$OT_YR
#   dt <- row$DT_YR
#   park <- row$PARK
#   
#   X[i, paste0("B", ot)] <- 1
#   X[i, paste0("B", dt)] <- 1
#   X[i, park] <- 1
#   
# }


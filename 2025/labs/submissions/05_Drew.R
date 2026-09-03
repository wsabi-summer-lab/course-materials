#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

####################
### PARK EFFECTS ###
####################

# load data
park_data = read_csv("../data/05_park-effects.csv.gz")
# find mean of each the mean runs scored in a half-inning at each PARK
# group by park and calculate mean runs scored
park_effects1 = park_data %>%
  group_by(PARK) %>%
  summarise(mean_runs = mean(INN_RUNS)) %>%
  arrange(desc(mean_runs))
# print  mean_runs
print(park_effects1)

park_data2 = park_data %>%
  mutate(
    o = factor(OT_YR),
    d = factor(DT_YR),
    p = factor(PARK)
  )


# Fit the linear model
adj_linear <-
  lm(
    INN_RUNS ~ o + d + p + 0,
    data = park_data2
  )
# Get the coefficients
park_effects2 = as.data.frame(coef(adj_linear))
# Rename the column
colnames(park_effects2) <- c("mean_runs")
# Add the PARK names
park_effects2$PARK <- rownames(park_effects2)
# Arrange by mean_runs
park_effects2 <- park_effects2 %>%
  filter(substr(PARK,1,1)=="p") %>%
  mutate(PARK = substring(PARK, 2)) %>% 
  arrange(desc(mean_runs))
# Print the adjusted park effects
print(park_effects2)

park_data2$predictions = predict(adj_linear, park_data2)

combined_estimates = left_join(park_effects1, park_effects2, by = "PARK", suffix = c("_binned", "_model"))

#find mean of column INN_RUNS in park_data
mean_runs = mean(park_data$INN_RUNS)
print(mean_runs)
# add 0.0.5227096 to each mean_runs_model in combined_estimates
combined_estimates$mean_runs_binned <- combined_estimates$mean_runs_binned - mean_runs
# plot # combined_estimates$mean_runs_binned vs combined_estimates$mean_runs_model for each park
ggplot(combined_estimates, aes(x = mean_runs_binned, y = mean_runs_model)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(
    title = "Park Effects: Binned vs Model Estimates",
    x = "Mean Runs Scored (Binned)",
    y = "Mean Runs Scored (Model)"
  ) +
  theme_minimal()

#TESTT TWO ESTMATES USING OUT OF SAMPLE PREDICTATIVE PERFORMANCE
set.seed(2025)
n=nrow(park_data2)
train_index = sample(1:n,size = floor(.8*n))
train = slice(park_data2,train_index)
test = slice(park_data2,-train_index)

adj_linear_train <-
  lm(
    INN_RUNS ~ o + d + p + 0,
    data = train
  )
predictions_test = predict(adj_linear_train, test)

residuals = test$INN_RUNS - predictions_test

SSR = sum(residuals^2)
SSR

train_naive_means <- 
  train %>%
  group_by(PARK) %>%
  summarize(park_mean_train = mean(INN_RUNS, na.rm = TRUE)) %>%
  ungroup()

overall_train_mean <- mean(train$INN_RUNS, na.rm = TRUE)
test_naive_preds <- 
  test %>%
  left_join(train_naive_means, by = "PARK") %>%
  mutate(
    park_mean_train = if_else(is.na(park_mean_train), overall_train_mean, park_mean_train),
    residual_naive  = INN_RUNS - park_mean_train
  )
SSR_naive <- sum(test_naive_preds$residual_naive^2)

combined_estimates= combined_estimates %>%
  mutate(
   difference= mean_runs_model - mean_runs_binned,
   abs_difference = abs(difference)
  )

biggest_difference = combined_estimates %>%
  arrange(desc(abs_difference))
  
  # plot in bar chart the abs_difference vs PARK
  ggplot(biggest_difference, aes(x = reorder(PARK, abs_difference), y = abs_difference)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Biggest Differences in Park Effects",
    x = "Park",
    y = "Absolute Difference"
  ) +
  theme_minimal()


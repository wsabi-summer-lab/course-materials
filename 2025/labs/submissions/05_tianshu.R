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

park_data %>% group_by(PARK) %>%
  summarise(mean_runs = mean(INN_RUNS, na.rm = TRUE)) %>%
  mutate(park_factor = mean_runs - mean(park_data$INN_RUNS, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(PARK, park_factor), y = park_factor)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Naïve Park Factor",
       x = "Park",
       y = "Park Factor") +
  theme_minimal()

parks = park_data %>%
  select(PARK) %>%
  unique()

teams = park_data %>%
  select(OT_YR) %>%
  unique() %>%
  rename(TEAM = OT_YR) %>%
  arrange(TEAM)

# Build a linear model: park_factor + off_rating - def_rating
n_games = nrow(park_data)
X = matrix(0, nrow = n_games, ncol = 120)
for (i in 1:n_games) {
  park_id = which(parks$PARK == park_data$PARK[i])
  off_id = which(teams$TEAM == park_data$OT_YR[i])
  def_id = which(teams$TEAM == park_data$DT_YR[i])
  X[i, park_id] = 1
  X[i, off_id + 30] = 1
  X[i, def_id + 30] = -1
}
X = X[, 1:(ncol(X)-3)]
model = lm(INN_RUNS ~ X + 0, data = park_data)
summary(model)

# Naïve park factor
naive_pf <- park_data %>%
  group_by(PARK) %>%
  summarise(mean_runs = mean(INN_RUNS, na.rm = TRUE)) %>%
  mutate(park_factor = mean_runs - mean(park_data$INN_RUNS, na.rm = TRUE),
         type = "Naïve") %>%
  select(PARK, park_factor, type)

# Model-based park factor
model_pf <- parks %>%
  mutate(park_factor = model$coefficients[1:30] - mean(park_data$INN_RUNS, na.rm = TRUE),
         type = "Model-Based") %>%
  select(PARK, park_factor, type)

# Combine the data
combined_pf <- bind_rows(naive_pf, model_pf)

# Plot
ggplot(combined_pf, aes(x = reorder(PARK, park_factor), y = park_factor, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(title = "Comparison of Park Factors",
       x = "Park",
       y = "Park Factor",
       fill = "Method") +
  theme_minimal()
#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(4)

##############
### PART 1 ###
##############

# load data
field_goals = read_csv("../data/03_field-goals.csv.gz")

# Task 1:
# - Inspect the field-goal dataset
# - Compute basic summaries for the response and explanatory variables
# - Make a plot of field-goal outcome against yardline
# - Describe how make probability appears to change with distance
summary(field_goals)
ggplot(data=field_goals)+
  geom_point(aes(x=ydl, y=fg_made), alpha=.2)+
  geom_smooth(aes(x=ydl, y=fg_made), color="hotpink")
# Task 2:
# - Fit at least 3 competing models for field-goal success probability
# - Include at least one linear regression and at least one logistic regression
# - Consider whether kicker quality should enter the model
# - Write down each model clearly
# 80/20 train-test split
# 80/20 train-test split
train_idx <- sample(
  seq_len(nrow(field_goals)),
  size = 0.8 * nrow(field_goals)
)

train <- field_goals[train_idx, ]
test  <- field_goals[-train_idx, ]

# Model A: Linear Probability Model
mod_lin <- lm(fg_made ~ ydl, data = train)

# Model B: Logistic Regression
mod_log <- glm(fg_made ~ ydl,
               family = binomial,
               data = train)

# Model C: Logistic Regression + Kicker Quality
mod_log_kq <- glm(fg_made ~ ydl + kq,
                  family = binomial,
                  data = train)

# Test-set predictions
test$pred_lin <- predict(mod_lin,
                         newdata = test)

test$pred_log <- predict(mod_log,
                         newdata = test,
                         type = "response")

test$pred_log_kq <- predict(mod_log_kq,
                            newdata = test,
                            type = "response")

# Prediction grid for plotting
pred_grid <- data.frame(
  ydl = seq(min(field_goals$ydl),
            max(field_goals$ydl),
            length.out = 200),
  kq = mean(field_goals$kq, na.rm = TRUE)
)

pred_grid$linear <- predict(mod_lin,
                            newdata = pred_grid)

pred_grid$logistic <- predict(mod_log,
                              newdata = pred_grid,
                              type = "response")

pred_grid$logistic_kq <- predict(mod_log_kq,
                                 newdata = pred_grid,
                                 type = "response")

plot_df <- pred_grid %>%
  pivot_longer(
    cols = c(linear, logistic, logistic_kq),
    names_to = "model",
    values_to = "prob"
  )

# Plot fitted curves
ggplot(train) +
  geom_point(aes(x = ydl, y = fg_made),
             alpha = .1) +
  geom_line(data = plot_df,
            aes(x = ydl,
                y = prob,
                color = model),
            linewidth = 1.2) +
  labs(
    title = "Field Goal Success Models",
    x = "Yard Line Distance",
    y = "Probability of Made Field Goal",
    color = "Model"
  ) +
  geom_smooth(aes(x=ydl,y=fg_made), color="hotpink")
  theme_minimal()

# Log loss function
log_loss <- function(y, p) {
  p <- pmax(pmin(p, 1 - 1e-15), 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

# Compare models on test set
logloss_results <- data.frame(
  Model = c("Linear", "Logistic", "Logistic + KQ"),
  Test_LogLoss = c(
    log_loss(test$fg_made, test$pred_lin),
    log_loss(test$fg_made, test$pred_log),
    log_loss(test$fg_made, test$pred_log_kq)
  )
) %>%
  arrange(Test_LogLoss)

print(logloss_results)
# Task 3:
# - Compare the models using out-of-sample predictive performance
# - Use log loss as the main metric
# - If using cross-validation, report mean test log loss and its standard error across folds
# - Select a preferred model and explain why

# Task 4:
# - Report coefficient estimates, standard errors, and 95% confidence intervals for the selected logistic model
# - Interpret the selected model's coefficients on the log-odds scale
# - When useful, exponentiate coefficients and interpret them as odds ratios
coef_table <- summary(mod_log)$coefficients
ci <- confint(mod_log)

results <- data.frame(
  Estimate = coef(mod_log),
  Std_Error = coef_table[, "Std. Error"],
  CI_Lower = ci[, 1],
  CI_Upper = ci[, 2],
  Odds_Ratio = exp(coef(mod_log)),
  OR_Lower = exp(ci[, 1]),
  OR_Upper = exp(ci[, 2])
)

print(round(results, 4))
# Task 5:
# - Plot the selected model's predicted make probability as a function of yardline
# - Add a 95% confidence ribbon for the fitted probability
# - Bin the data by yardline and compare fitted probabilities to observed make rates
# - Comment on where the model fits well and where it misses
pred_grid <- data.frame(
  ydl = seq(min(field_goals$ydl),
            max(field_goals$ydl),
            length.out = 200)
)

pred_link <- predict(
  mod_log,
  newdata = pred_grid,
  type = "link",
  se.fit = TRUE
)

pred_grid$fit <- plogis(pred_link$fit)
pred_grid$lower <- plogis(pred_link$fit - 1.96 * pred_link$se.fit)
pred_grid$upper <- plogis(pred_link$fit + 1.96 * pred_link$se.fit)

# Observed make rates by 5-yard bins
obs_rates <- field_goals %>%
  mutate(yard_bin = cut_width(ydl, width = 5)) %>%
  group_by(yard_bin) %>%
  summarize(
    mean_ydl = mean(ydl),
    make_rate = mean(fg_made),
    n = n(),
    .groups = "drop"
  )

ggplot() +
  geom_ribbon(
    data = pred_grid,
    aes(x = ydl,
        ymin = lower,
        ymax = upper),
    alpha = 0.2
  ) +
  geom_line(
    data = pred_grid,
    aes(x = ydl,
        y = fit),
    linewidth = 1.2
  ) +
  geom_point(
    data = obs_rates,
    aes(x = mean_ydl,
        y = make_rate,
        size = n)
  ) +
  labs(
    title = "Logistic Model Fit for Field Goal Success",
    x = "Yard Line Distance",
    y = "Probability of Making Field Goal",
    size = "Attempts"
  ) +
  theme_minimal()
##############
### PART 2 ###
##############

# load data
ncaab_results = read_csv("../data/03_ncaab-results.csv.gz")
ncaab_team_info = read_csv("../data/03_ncaab-teams.csv.gz")

# Task 1:
# - Filter the NCAA results to the 2023-2024 season
# - Recode the data into a Bradley-Terry model dataset
# - Make sure you can identify the home team, away team, and binary game outcome
# - State the identifiability convention you will use for team ratings
ncaa_results_clean<- ncaab_results|>
  filter(Season==2024)
teams_vec <- sort(unique(c(ncaa_results_clean$WTeamID,
                           ncaa_results_clean$LTeamID)))

X <- data.frame(
  home = as.numeric(ncaa_results_clean$WLoc != "N"),
  matrix(
    0,
    nrow = nrow(ncaa_results_clean),
    ncol = length(teams_vec)
  )
)

colnames(X)[-1] <- teams_vec

# Response = did the home team win?
y <- ifelse(ncaa_results_clean$WLoc == "A", 0, 1)

for(j in seq_len(nrow(ncaa_results_clean))) {
  
  if(ncaa_results_clean$WLoc[j] == "A") {
    
    # losing team is home
    home_team <- as.character(ncaa_results_clean$LTeamID[j])
    away_team <- as.character(ncaa_results_clean$WTeamID[j])
    
  } else {
    
    # winner is home (or arbitrary "team 1" for neutral)
    home_team <- as.character(ncaa_results_clean$WTeamID[j])
    away_team <- as.character(ncaa_results_clean$LTeamID[j])
    
  }
  
  X[j, home_team] <-  1
  X[j, away_team] <- -1
}

# Bradley-Terry model with home-court advantage
bt_model <- glm(
  y ~ . - 1,
  data = X,
  family = binomial
)

# Extract coefficients from Bradley-Terry model
coef_table <- summary(bt_model)$coefficients

# Identify team coefficient rows (exclude home intercept)
team_rows <- setdiff(rownames(coef_table), "home")

# Build ratings dataframe
rating_df <- data.frame(
  TeamID = as.numeric(gsub("[^0-9]", "", team_rows)),
  Rating = coef_table[team_rows, "Estimate"],
  SE = coef_table[team_rows, "Std. Error"]
)

# Center ratings so average team = 0
rating_df <- rating_df %>%
  mutate(
    Rating = Rating - mean(Rating),
    Lower = Rating - 1.96 * SE,
    Upper = Rating + 1.96 * SE
  )

# Join team names (CORRECT lookup table)
rating_df <- rating_df %>%
  left_join(
    ncaab_team_info %>% select(TeamID, TeamName),
    by = "TeamID"
  ) %>%
  arrange(desc(Rating))

# Strongest teams
rating_df %>%
  select(TeamName, Rating, Lower, Upper) %>%
  head(10)

# Sample 30 evenly spaced teams across ranking
sample_idx <- unique(round(seq(1, nrow(rating_df), length.out = 30)))

rating_sample <- rating_df[sample_idx, ]

# Plot
ggplot(rating_sample,
       aes(x = reorder(TeamName, Rating),
           y = Rating, 
           color=TeamName)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.15) +
  coord_flip() +
  labs(
    title = "Bradley-Terry Ratings (30 Evenly Spaced Teams)",
    x = NULL,
    y = "Centered Rating"
  ) +
  theme_minimal()
# Task 2:
# - Fit a Bradley-Terry logistic regression model
# - Include a home-court advantage term
# - Join team names back onto the fitted coefficients so the ratings are interpretable
# - Explain what a larger team rating means


# Task 3:
# - Visualize the fitted team ratings
# - Add uncertainty intervals for the ratings or for rating differences
# - Explain why rating differences are often more meaningful than raw levels
# - Identify the strongest teams under your fitted model

# Task 4:
# - Choose one or more team comparisons and compute win probabilities from the fitted model
# - For at least one matchup, quantify uncertainty in the predicted probability
# - Make sure your probability calculation matches your identifiability convention

# Task 5:
# - For the Purdue vs UConn national-title game, set beta_0 = 0 for a neutral site
# - Report the estimated win probability for each team
# - Compute an approximate 95% confidence interval for the win probability
# - Convert the point estimate and both confidence-interval endpoints into moneyline prices
# - Briefly explain that this interval reflects uncertainty in the fitted probability, not certainty about one game outcome
# Extract teams
purdue <- rating_df %>% filter(TeamName == "Purdue")
uconn  <- rating_df %>% filter(TeamName == "Connecticut")

# Rating difference (neutral site => beta_0 = 0)
delta <- purdue$Rating - uconn$Rating

# Win probability (Purdue)
p_hat <- plogis(delta)

# Approx SE of difference (independent approximation)
se_delta <- sqrt(purdue$SE^2 + uconn$SE^2)

# CI on log-odds scale
lower_logit <- delta - 1.96 * se_delta
upper_logit <- delta + 1.96 * se_delta

# Convert to probability scale
p_lower <- plogis(lower_logit)
p_upper <- plogis(upper_logit)

# Moneyline conversion
to_moneyline <- function(p) {
  ifelse(
    p >= 0.5,
    -100 * (p / (1 - p)),
    100 * ((1 - p) / p)
  )
}

point_ml <- to_moneyline(p_hat)
lower_ml <- to_moneyline(p_lower)
upper_ml <- to_moneyline(p_upper)

# Final output
data.frame(
  Matchup = "Purdue vs UConn (Neutral Site)",
  Purdue_Prob = p_hat,
  UConn_Prob = 1 - p_hat,
  CI_Lower = p_lower,
  CI_Upper = p_upper,
  Moneyline = point_ml,
  ML_Lower = lower_ml,
  ML_Upper = upper_ml
)


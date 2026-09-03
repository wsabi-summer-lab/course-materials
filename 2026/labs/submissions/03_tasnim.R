#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)



# set seed
set.seed(4)

# helper function for log loss
log_loss = function(y, p) {
  eps = 1e-15
  p = pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

##############
### PART 1 ###
##############

# load data
field_goals <- read_csv("~/Desktop/03_field-goals.csv.gz")

# Task 1:
# - Inspect the field-goal dataset
glimpse(field_goals)
head(field_goals)

# - Compute basic summaries for the response and explanatory variables
field_goal_summary = field_goals %>%
  summarize(
    n = n(),
    make_rate = mean(fg_made),
    mean_ydl = mean(ydl),
    sd_ydl = sd(ydl),
    min_ydl = min(ydl),
    max_ydl = max(ydl),
    mean_kq = mean(kq),
    sd_kq = sd(kq),
    min_kq = min(kq),
    max_kq = max(kq)
  )

field_goal_summary

field_goals %>%
  summarize(
    n_kickers = n_distinct(kicker),
    n_teams = n_distinct(posteam),
    first_season = min(season),
    last_season = max(season)
  )

# - Make a plot of field-goal outcome against yardline
# Since fg_made is only 0 or 1, jitter helps us see overlapping points.
ggplot(field_goals, aes(x = ydl, y = fg_made)) +
  geom_jitter(height = 0.04, alpha = 0.15) +
  geom_smooth(method = "loess", se = FALSE)

# - Describe how make probability appears to change with distance
# The fitted smooth should usually slope downward: longer attempts are harder,
# so the make probability appears to decrease as ydl increases.

# Task 2:
# - Fit at least 3 competing models for field-goal success probability
# - Include at least one linear regression and at least one logistic regression
# - Consider whether kicker quality should enter the model
# - Write down each model clearly

# Model 1: Linear probability model using only yardline
fg_lm_ydl = lm(fg_made ~ ydl, data = field_goals)

# Model 2: Logistic regression using only yardline
fg_logit_ydl = glm(
  fg_made ~ ydl,
  data = field_goals,
  family = binomial
)

# Model 3: Logistic regression using yardline and kicker quality
fg_logit_ydl_kq = glm(
  fg_made ~ ydl + kq,
  data = field_goals,
  family = binomial()
)

# Model 4: More flexible logistic regression using a quadratic yardline term
# and kicker quality
fg_logit_quad_kq = glm(
  fg_made ~ ydl + I(ydl^2) + kq,
  data = field_goals,
  family = binomial 
)

summary(fg_lm_ydl)
summary(fg_logit_ydl)
summary(fg_logit_ydl_kq)
summary(fg_logit_quad_kq)

# Task 3:
# - Compare the models using out-of-sample predictive performance
# - Use log loss as the main metric
# - If using cross-validation, report mean test log loss and its standard error across folds
# - Select a preferred model and explain why

# 5-fold cross-validation
K = 5
field_goals = field_goals %>%
  mutate(fold = sample(rep(1:K, length.out = n())))

cv_results = map_dfr(1:K, function(k) {
  train = field_goals %>% filter(fold != k)
  test = field_goals %>% filter(fold == k)

  m1 = lm(fg_made ~ ydl, data = train)
  m2 = glm(fg_made ~ ydl, data = train, family = binomial())
  m3 = glm(fg_made ~ ydl + kq, data = train, family = binomial())
  m4 = glm(fg_made ~ ydl + I(ydl^2) + kq, data = train, family = binomial())

  tibble(
    fold = k,
    lm_ydl = log_loss(test$fg_made, predict(m1, newdata = test)),
    logit_ydl = log_loss(test$fg_made, predict(m2, newdata = test, type = "response")),
    logit_ydl_kq = log_loss(test$fg_made, predict(m3, newdata = test, type = "response")),
    logit_quad_kq = log_loss(test$fg_made, predict(m4, newdata = test, type = "response"))
  )
})

cv_summary = cv_results %>%
  pivot_longer(-fold, names_to = "model", values_to = "log_loss") %>%
  group_by(model) %>%
  summarize(
    mean_test_log_loss = mean(log_loss),
    se_test_log_loss = sd(log_loss) / sqrt(n()),
    .groups = "drop"
  ) %>%
  arrange(mean_test_log_loss)

cv_summary

# Preferred model: choose the model with the smallest mean test log loss.
# In most runs this will be one of the logistic models, because the response is binary
# and logistic regression keeps predicted probabilities between 0 and 1.
selected_fg_model = fg_logit_quad_kq

# Task 4:
# - Report coefficient estimates, standard errors, and 95% confidence intervals for the selected logistic model
# - Interpret the selected model's coefficients on the log-odds scale
# - When useful, exponentiate coefficients and interpret them as odds ratios

coef_table = summary(selected_fg_model)$coefficients
coef_table

confint_selected = confint.default(selected_fg_model)
confint_selected

odds_ratios = exp(cbind(
  estimate = coef(selected_fg_model),
  confint_selected
))
odds_ratios


# ydl: holding other terms fixed, this is the estimated change in
# log-odds of making the field goal for a one-unit increase in ydl.
# I(ydl^2): because it's a quadratic the distance can bend.
# kq : holding ydl fixed, better kicker quality changes the log-odds of success.
# exp(coefficient) gives the multiplicative change in odds for a one-unit increase.

# Task 5:
# - Plot the selected model's predicted make probability as a function of yardline
# - Add a 95% confidence ribbon for the fitted probability
# - Bin the data by yardline and compare fitted probabilities to observed make rates
# - Comment on where the model fits well and where it misses

fg_grid = tibble(
  ydl = seq(min(field_goals$ydl), max(field_goals$ydl), length.out = 200),
  kq = mean(field_goals$kq)
)

fg_pred_link = predict(
  selected_fg_model,
  newdata = fg_grid,
  type = "link",
  se.fit = TRUE
)

fg_grid = fg_grid %>%
  mutate(
    fit_link = fg_pred_link$fit,
    se_link = fg_pred_link$se.fit,
    prob = plogis(fit_link),
    lwr = plogis(fit_link - 1.96 * se_link),
    upr = plogis(fit_link + 1.96 * se_link)
  )

fg_binned = field_goals %>%
  mutate(ydl_bin = cut(ydl, breaks = seq(min(ydl), max(ydl) + 5, by = 5), include.lowest = TRUE)) %>%
  group_by(ydl_bin) %>%
  summarize(
    mean_ydl = mean(ydl),
    observed_make_rate = mean(fg_made),
    n = n(),
    .groups = "drop"
  )

fg_binned

ggplot() +
  geom_ribbon(
    data = fg_grid,
    aes(x = ydl, ymin = lwr, ymax = upr),
    alpha = 0.2
  ) +
  geom_line(
    data = fg_grid,
    aes(x = ydl, y = prob),
    linewidth = 1
  ) +
  geom_point(
    data = fg_binned,
    aes(x = mean_ydl, y = observed_make_rate, size = n),
    alpha = 0.7
  ) +
  labs(
    title = "Selected logistic model: predicted vs observed make probability",
    x = "Yardline / distance measure",
    y = "Make probability",
    size = "Bin size"
  )


# The model fits well where the binned observed make rates are close to the fitted curve.


##############
### PART 2 ###
##############

# load data
ncaab_results <- read_csv("~/Desktop/03_ncaab-results.csv.gz")
ncaab_team_info <- read_csv("~/Desktop/03_ncaab-teams.csv.gz")

# Task 1:
# - Filter the NCAA results to the 2023-2024 season
# - Recode the data into a Bradley-Terry model dataset
# - Make sure you can identify the home team, away team, and binary game outcome
# - State the identifiability convention you will use for team ratings

ncaab_2024 = ncaab_results %>%
  filter(Season == 2024)

bt_data = ncaab_2024 %>%
  mutate(
    home_team = case_when(
      WLoc == "H" ~ WTeamID,
      WLoc == "A" ~ LTeamID,
      WLoc == "N" ~ WTeamID
    ), 
    away_team = case_when(
      WLoc == "H" ~ LTeamID,
      WLoc == "A" ~ WTeamID,
      WLoc == "N" ~ LTeamID
    ),
    home_win = case_when(
      WLoc == "H" ~ 1,
      WLoc == "A" ~ 0,
      WLoc == "N" ~ 1
    ),
    home_court = if_else(WLoc == "N", 0, 1)
  ) %>%
  select(Season, DayNum, home_team, away_team, home_win, home_court, WTeamID, LTeamID, WScore, LScore, WLoc)

head(bt_data)



team_ids = sort(unique(c(bt_data$home_team, bt_data$away_team)))
reference_team = team_ids[1]
non_reference_teams = team_ids[team_ids != reference_team]

reference_team

# Task 2:
# - Fit a Bradley-Terry logistic regression model
# - Include a home-court advantage term
# - Join team names back onto the fitted coefficients so the ratings are interpretable
# - Explain what a larger team rating means

# Build a Bradley-Terry design matrix manually.
# For each game: logit(P(home team wins)) = home_court_advantage + rating_home - rating_away.
X = matrix(0, nrow = nrow(bt_data), ncol = length(non_reference_teams))
colnames(X) = paste0("team_", non_reference_teams)

for (j in seq_along(non_reference_teams)) {
  team = non_reference_teams[j]
  X[, j] = if_else(bt_data$home_team == team, 1, 0) -
    if_else(bt_data$away_team == team, 1, 0)
}

bt_model_df = bind_cols(
  bt_data %>% select(home_win, home_court),
  as_tibble(X)
)

bt_model = glm(
  home_win ~ .,
  data = bt_model_df,
  family = binomial()
)

summary(bt_model)

bt_coef = coef(bt_model)

team_ratings = tibble(
  TeamID = team_ids,
  rating = if_else(
    team_ids == reference_team,
    0,
    bt_coef[paste0("team_", team_ids)]
  )
) %>%
  left_join(ncaab_team_info, by = "TeamID") %>%
  arrange(desc(rating))

team_ratings

home_court_advantage = bt_coef["home_court"]
home_court_advantage
exp(home_court_advantage)


# if Team A's rating is higher than Team B's rating, Team A has a higher predicted
# chance of beating Team B

# Task 3:
# - Visualize the fitted team ratings
# - Add uncertainty intervals for the ratings or for rating differences
# - Explain why rating differences are often more meaningful than raw levels
# - Identify the strongest teams under your fitted model

# Approximate standard errors for ratings relative to the reference team.
coef_se = summary(bt_model)$coefficients[, "Std. Error"]

team_ratings_with_se = team_ratings %>%
  mutate(
    se = if_else(
      TeamID == reference_team,
      0,
      coef_se[paste0("team_", TeamID)]
    ),
    lwr = rating - 1.96 * se,
    upr = rating + 1.96 * se
  )

strongest_teams = team_ratings_with_se %>%
  slice_max(rating, n = 20)

strongest_teams

ggplot(strongest_teams, aes(x = reorder(TeamName, rating), y = rating)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  coord_flip() +
  labs(
    title = "Top fitted Bradley-Terry team ratings, 2023-2024 season",
    x = "Team",
    y = "Rating relative to reference team"
  )



# Task 4:
# - Choose one or more team comparisons and compute win probabilities from the fitted model
# - For at least one matchup, quantify uncertainty in the predicted probability
# - Make sure your probability calculation matches your identifiability convention

get_rating = function(team_id) {
  if (team_id == reference_team) {
    return(0)
  }
  bt_coef[paste0("team_", team_id)]
}

get_rating_se = function(team_id) {
  if (team_id == reference_team) {
    return(0)
  }
  coef_se[paste0("team_", team_id)]
}

neutral_probability = function(team_a, team_b) {
  # probability that team_a beats team_b on a neutral court
  eta = get_rating(team_a) - get_rating(team_b)
  plogis(eta)
}

# Example comparison: strongest team vs second strongest team on a neutral court
team_a = strongest_teams$TeamID[1]
team_b = strongest_teams$TeamID[2]

p_a_beats_b = neutral_probability(team_a, team_b)
p_a_beats_b

# Approximate uncertainty for the rating difference using the model covariance matrix.
rating_difference_ci = function(team_a, team_b) {
  v = rep(0, length(coef(bt_model)))
  names(v) = names(coef(bt_model))

  if (team_a != reference_team) {
    v[paste0("team_", team_a)] = 1
  }
  if (team_b != reference_team) {
    v[paste0("team_", team_b)] = v[paste0("team_", team_b)] - 1
  }

  eta = sum(v * coef(bt_model))
  se_eta = sqrt(as.numeric(t(v) %*% vcov(bt_model) %*% v))

  tibble(
    team_a = team_a,
    team_b = team_b,
    eta = eta,
    se_eta = se_eta,
    prob = plogis(eta),
    prob_lwr = plogis(eta - 1.96 * se_eta),
    prob_upr = plogis(eta + 1.96 * se_eta)
  )
}

rating_difference_ci(team_a, team_b)

# Task 5:
# - For the Purdue vs UConn national-title game, set beta_0 = 0 for a neutral site
# - Report the estimated win probability for each team
# - Compute an approximate 95% confidence interval for the win probability
# - Convert the point estimate and both confidence-interval endpoints into moneyline prices
# - Briefly explain that this interval reflects uncertainty in the fitted probability, not certainty about one game outcome

purdue_id = ncaab_team_info %>% filter(TeamName == "Purdue") %>% pull(TeamID)
nuconn_id = ncaab_team_info %>% filter(TeamName == "Connecticut") %>% pull(TeamID)

purdue_id
nuconn_id

purdue_uconn_ci = rating_difference_ci(purdue_id, uconn_id) %>%
  mutate(
    purdue_win_prob = prob,
    uconn_win_prob = 1 - prob,
    purdue_prob_lwr = prob_lwr,
    purdue_prob_upr = prob_upr,
    uconn_prob_lwr = 1 - prob_upr,
    uconn_prob_upr = 1 - prob_lwr
  )

moneyline = function(p) {
  if_else(
    p >= 0.5,
    -100 * p / (1 - p),
    100 * (1 - p) / p
  )
}

purdue_uconn_moneyline = purdue_uconn_ci %>%
  transmute(
    matchup = "Purdue vs UConn, neutral site",
    purdue_win_prob,
    purdue_prob_lwr,
    purdue_prob_upr,
    purdue_moneyline = moneyline(purdue_win_prob),
    purdue_moneyline_lwr = moneyline(purdue_prob_lwr),
    purdue_moneyline_upr = moneyline(purdue_prob_upr),
    uconn_win_prob,
    uconn_prob_lwr,
    uconn_prob_upr,
    uconn_moneyline = moneyline(uconn_win_prob),
    uconn_moneyline_lwr = moneyline(uconn_prob_lwr),
    uconn_moneyline_upr = moneyline(uconn_prob_upr)
  )

purdue_uconn_moneyline


# must fall inside the interval. A single game is completely random with an outcome that could go either way so the interval doesn't tell us 
# the outcome of a game instead it describes uncertainty for the model.

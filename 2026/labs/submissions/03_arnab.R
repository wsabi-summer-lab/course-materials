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
summary(field_goals$fg_made)
summary(field_goals$ydl)
summary(field_goals$kq)

# - Make a plot of field-goal outcome against yardline
ggplot(field_goals, aes(x = ydl, y = fg_made)) +
  geom_jitter(width = 0.3, height = 0.04, alpha = 0.35, size = 1.5) +
  labs(
    title = "Field Goal Outcome by Yard Line",
    x = "Yard line",
    y = "Field goal made"
  ) +
  theme_minimal()
# - Describe how make probability appears to change with distance
#There are more misses as distance increases , until around the 40, where there are limited attempts in general. I assume thats dependent on the kicker, and whether the coach chooses to send them out.

# Task 2:
ggplot(field_goals, aes(x = ydl, y = fg_made)) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2
  ) +
  labs(
    title = "Average Field Goal Success Rate by Yard Line",
    x = "Yard Line",
    y = "Success Rate"
  ) +
  ylim(0, 1) +
  theme_minimal()
# - Fit at least 3 competing models for field-goal success probability
# - Include at least one linear regression and at least one logistic regression
lm_model = lm(fg_made ~ ydl, data = field_goals)

log_model = glm(fg_made ~ ydl, data = field_goals, family = 'binomial')
log_model2 = glm(fg_made ~ ydl + kq, data = field_goals, family = 'binomial')

field_goals$fit_lm <- fitted(lm_model)
field_goals$fit_log <- fitted(log_model)
field_goals$fit_log2 <- fitted(log_model2)


ggplot(field_goals, aes(x = ydl, y = fg_made)) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 2,
    alpha = 0.8
  ) +
  geom_line(aes(y = fit_lm, color = "Linear"), linewidth = 1) +
  geom_line(aes(y = fit_log, color = "Logistic"), linewidth = 1) +
  geom_line(aes(y = fit_log2, color = "Logistic + Kicker Quality"), linewidth = 1) +
  scale_color_manual(
    name = "Model",
    values = c(
      "Linear" = "red",
      "Logistic" = "orange",
      "Logistic + Kicker Quality" = "blue"
    )
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Field Goal Success Rate by Yard Line",
    x = "Yard Line",
    y = "Success Rate"
  ) +
  theme_minimal()
# - Consider whether kicker quality should enter the model
# - Write down each model clearly
#Linear model was using fg_made ~ ydl, the first logistic regression used fg_made ~ ydl, while the second one added kq.


# Task 3:
# - Compare the models using out-of-sample predictive performance
# - Use log loss as the main metric
# - If using cross-validation, report mean test log loss and its standard error across folds
# - Select a preferred model and explain why

fit_lm_prob <- pmin(pmax(field_goals$fit_lm, 1e-15), 1 - 1e-15)
fit_log_prob <- pmin(pmax(field_goals$fit_log, 1e-15), 1 - 1e-15)
fit_log2_prob <- pmin(pmax(field_goals$fit_log2, 1e-15), 1 - 1e-15)

log_loss <- function(y, p) {
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

ll_lm <- log_loss(field_goals$fg_made, fit_lm_prob)
ll_log <- log_loss(field_goals$fg_made, fit_log_prob)
ll_log2 <- log_loss(field_goals$fg_made, fit_log2_prob)

print(ll_lm)
print(ll_log)
print(ll_log2)

#Using the lowest log loss, I chose the 2nd logistic regression model, which added kicker quality, to use.

# Task 4:
# - Report coefficient estimates, standard errors, and 95% confidence intervals for the selected logistic model
# - Interpret the selected model's coefficients on the log-odds scale
# - When useful, exponentiate coefficients and interpret them as odds ratios
summary(log_model2)
confint(log_model2)
#Coefficients: Intercept 4.084451, ydl = -0.105949, kq = 0.276526
#Standard Error: Intercept = 0.087508, ydl = 0.003188, kq = 0.053498
#95% confidence intervals
# #              2.5 %      97.5 %
#   (Intercept)  3.9150509  4.25813319
# ydl         -0.1122534 -0.09975375
# kq           0.1721175  0.38185379

#Looking at log odds, an increase in yardline leads to a decrease in probability that the field goal is made, while kicker quality increases the probability by more than that. 
#Odds ratios: ydl: 0.899, kq = 1.319
#For every yard further, the odds must be multiplied by .899, which would decrease the odds. Using the same idea, an increase in a unit of kicker quality would increase the odds of a make. 

# Task 5:
# - Plot the selected model's predicted make probability as a function of yardline
# - Add a 95% confidence ribbon for the fitted probability
# - Bin the data by yardline and compare fitted probabilities to observed make rates
# - Comment on where the model fits well and where it misses

pred <- predict(log_model2,
                type = "link",
                se.fit = TRUE)
field_goals$eta <- pred$fit
field_goals$eta_se <- pred$se.fit

field_goals <- field_goals %>%
  mutate(
    fit_prob = plogis(eta),
    lower = plogis(eta - 1.96 * eta_se),
    upper = plogis(eta + 1.96 * eta_se)
  )

yard_summary <- field_goals %>%
  group_by(ydl) %>%
  summarize(
    observed_rate = mean(fg_made),
    fitted_prob = mean(fit_prob),
    lower = mean(lower),
    upper = mean(upper),
    .groups = "drop"
  )


ggplot(yard_summary, aes(x = ydl)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2,
    fill = "steelblue"
  ) +
  geom_line(
    aes(y = fitted_prob),
    color = "blue",
    linewidth = 1.2
  ) +
  geom_point(
    aes(y = observed_rate),
    color = "black",
    size = 2
  ) +
  labs(
    title = "Logistic Model Fit vs Observed Make Rates",
    x = "Yard Line",
    y = "Probability of Making Field Goal"
  ) +
  coord_cartesian(ylim = c(0,1)) +
  theme_minimal()

#The model does well in the beginning, but is worse at the end, possibly due to decision making being influenced by kicker quality.

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
ncaab_2023 <- ncaab_results %>%
  filter(Season == 2023)

games <- ncaab_2023 %>%
  left_join(
    ncaab_team_info %>%
      select(TeamID, TeamName),
    by = c("WTeamID" = "TeamID")
  ) %>%
  rename(WTeam = TeamName)
games <- games %>%
  left_join(
    ncaab_team_info %>%
      select(TeamID, TeamName),
    by = c("LTeamID" = "TeamID")
  ) %>%
  rename(LTeam = TeamName)

games_model <- ncaab_2023 %>%
  mutate(
    HomeTeamID = case_when(
      WLoc == "H" ~ WTeamID,
      WLoc == "A" ~ LTeamID,
      TRUE ~ WTeamID      # neutral site
    ),
    AwayTeamID = case_when(
      WLoc == "H" ~ LTeamID,
      WLoc == "A" ~ WTeamID,
      TRUE ~ LTeamID
    ),
    Margin = case_when(
      WLoc == "H" ~ WScore - LScore,
      WLoc == "A" ~ LScore - WScore,
      TRUE ~ WScore - LScore
    )
  ) %>%
  select(HomeTeamID, AwayTeamID, Margin)

#Will drop one team

# Task 2:
# - Fit a Bradley-Terry logistic regression model
# - Include a home-court advantage term
# - Join team names back onto the fitted coefficients so the ratings are interpretable
# - Explain what a larger team rating means

games_model$HomeWin <- as.integer(games_model$Margin > 0)

teams <- sort(unique(c(games_model$HomeTeamID,
                       games_model$AwayTeamID)))

p <- length(teams)
n <- nrow(games_model)

X <- matrix(0, nrow = n, ncol = p)
colnames(X) <- paste0("T", teams)

for(i in 1:n){
  
  home_col <- match(games_model$HomeTeamID[i], teams)
  away_col <- match(games_model$AwayTeamID[i], teams)
  
  X[i, home_col] <-  1
  X[i, away_col] <- -1
}
X <- X[, -1]
X <- cbind(HomeAdv = 1, X)


bt_model = glm(HomeWin ~ X + 0, data = games_model, family = "binomial")

games_model$fitbt <- fitted(bt_model)

games_model <- games_model %>%
  select(-any_of(c("HomeTeam", "AwayTeam"))) %>%
  left_join(
    ncaab_team_info %>%
      select(TeamID, TeamName),
    by = c("HomeTeamID" = "TeamID")
  ) %>%
  rename(HomeTeam = TeamName) %>%
  left_join(
    ncaab_team_info %>%
      select(TeamID, TeamName),
    by = c("AwayTeamID" = "TeamID")
  ) %>%
  rename(AwayTeam = TeamName) %>%
  select(HomeTeam, AwayTeam, Margin, fitbt)

summary(bt_model)

ratings <- data.frame(
  term = names(coef(bt_model)),
  rating = coef(bt_model)
)

ratings <- ratings %>%
  filter(term != "(Intercept)",
         term != "XHomeAdv") %>%
  mutate(
    TeamID = as.numeric(sub("XT", "", term))
  ) %>%
  left_join(
    ncaab_team_info %>%
      select(TeamID, TeamName),
    by = "TeamID"
  ) %>%
  arrange(desc(rating))

#LeMoyne was dropped, so these power ratings measure the teams ability relative to them. 

# Task 3:
# - Visualize the fitted team ratings
# - Add uncertainty intervals for the ratings or for rating differences
# - Explain why rating differences are often more meaningful than raw levels
# - Identify the strongest teams under your fitted model

coefs <- summary(bt_model)$coefficients

ratings <- data.frame(
  term = rownames(coefs),
  rating = coefs[, "Estimate"],
  se = coefs[, "Std. Error"]
) %>%
  filter(term != "(Intercept)",
         term != "XHomeAdv") %>%
  filter(!is.na(rating)) %>%
  mutate(
    TeamID = as.numeric(sub("XT", "", term)),
    lower = rating - 1.96 * se,
    upper = rating + 1.96 * se
  ) %>%
  left_join(
    ncaab_team_info %>% select(TeamID, TeamName),
    by = "TeamID"
  ) %>%
  arrange(desc(rating))

top_ratings <- ratings %>%
  slice_max(rating, n = 25)

ggplot(top_ratings,
       aes(x = reorder(TeamName, rating), y = rating)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  coord_flip() +
  labs(
    title = "Top 25 Fitted Team Ratings",
    x = "Team",
    y = "Estimated Team Rating"
  ) +
  theme_minimal()

#Rating differences are better because it can be compared to a certain team, providing more relative meaning.

#Bama, Kansas, Houston, Purdue are the best in 2023, which lines up given they were the 1 seeds for the tournament.

# Task 4:
# - Choose one or more team comparisons and compute win probabilities from the fitted model
# - For at least one matchup, quantify uncertainty in the predicted probability
# - Make sure your probability calculation matches your identifiability convention
games_model$fitbt[3667]
x0 <- model.matrix(bt_model)[3667, ]

b <- coef(bt_model)
keep <- !is.na(b)

eta_hat <- sum(x0[keep] * b[keep])

V <- vcov(bt_model)[keep, keep]

se_eta <- sqrt(
  t(x0[keep]) %*% V %*% x0[keep]
)

data.frame(
  FitProb = plogis(eta_hat),
  Lower95 = plogis(eta_hat - 1.96 * se_eta),
  Upper95 = plogis(eta_hat + 1.96 * se_eta)
)


# Task 5:
# - For the Purdue vs UConn national-title game, set beta_0 = 0 for a neutral site
# - Report the estimated win probability for each team
# - Compute an approximate 95% confidence interval for the win probability
# - Convert the point estimate and both confidence-interval endpoints into moneyline prices
# - Briefly explain that this interval reflects uncertainty in the fitted probability, not certainty about one game outcome

purdue_id <- ncaab_team_info$TeamID[ncaab_team_info$TeamName == "Purdue"]
uconn_id  <- ncaab_team_info$TeamID[ncaab_team_info$TeamName == "Connecticut"]

purdue_id
uconn_id


b <- coef(bt_model)

get_rating <- function(team_id) {
  term <- paste0("XT", team_id)
  
  if (term %in% names(b)) {
    return(b[term])
  } else {
    return(0) 
  }
}

beta_purdue <- get_rating(purdue_id)
beta_uconn  <- get_rating(uconn_id)
beta_0 <- 0

eta_purdue <- beta_0 + beta_purdue - beta_uconn
p_purdue <- plogis(eta_purdue)

p_uconn <- 1 - p_purdue

print(p_purdue)
print(p_uconn)

V <- vcov(bt_model)

purdue_term <- paste0("XT", purdue_id)
uconn_term  <- paste0("XT", uconn_id)

se_eta <- sqrt(
  V[purdue_term, purdue_term] +
    V[uconn_term, uconn_term] -
    2 * V[purdue_term, uconn_term]
)
lower_eta <- eta_purdue - 1.96 * se_eta
upper_eta <- eta_purdue + 1.96 * se_eta

lower_p <- plogis(lower_eta)
upper_p <- plogis(upper_eta)

print(lower_p)
print(upper_p)

prob_to_moneyline <- function(p) {
  ifelse(
    p >= 0.5,
    -100 * p / (1 - p),
    100 * (1 - p) / p
  )
}


data.frame(
  PurdueWinProb = p_purdue,
  PurdueMoneyline = prob_to_moneyline(p_purdue),
  Lower95Prob = lower_p,
  Lower95Moneyline = prob_to_moneyline(lower_p),
  Upper95Prob = upper_p,
  Upper95Moneyline = prob_to_moneyline(upper_p)
)

data.frame(
  UConnWinProb = p_uconn,
  UConnMoneyline = prob_to_moneyline(p_uconn),
  Lower95Prob = 1 - upper_p,
  Lower95Moneyline = prob_to_moneyline(1 - upper_p),
  Upper95Prob = 1 - lower_p,
  Upper95Moneyline = prob_to_moneyline(1 - lower_p)
)

#This interval is quite large and shows that there is not much certainty regarding the game outcome given the true probability could favor either side. 
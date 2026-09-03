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
field_goals = read_csv("C:/Users/sundw/Downloads/03_field-goals.csv")

# Bin kicks by ydl and compute make probability per bin
fg_binned <- field_goals |>
  mutate(ydl_bin = cut(ydl, breaks = seq(0, 50, by = 1),
                       include.lowest = TRUE)) |>
  group_by(ydl_bin) |>
  summarise(
    attempts  = n(),
    makes     = sum(fg_made),
    make_prob = mean(fg_made),
    mid_ydl   = mean(ydl),
    .groups   = "drop"
  )

# Preview
print(fg_binned)

# Plot
ggplot(fg_binned, aes(x = mid_ydl, y = make_prob)) +
  geom_point(aes(size = attempts), colour = "steelblue", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_size_continuous(name = "# attempts") +
  labs(
    title = "FG Make Probability by Yard Line",
    x     = "Yards from End Zone (bin midpoint)",
    y     = "Make Probability"
  ) +
  theme_minimal(base_size = 13)

# Linear Probability Model
lm_model <- lm(fg_made ~ ydl, data = field_goals)
summary(lm_model)

# Add fitted line to the binned chart
ggplot(fg_binned, aes(x = mid_ydl, y = make_prob)) +
  geom_point(aes(size = attempts), colour = "steelblue", alpha = 0.8) +
  geom_smooth(data = field_goals, aes(x = ydl, y = fg_made),
              method = "lm", se = TRUE,
              colour = "firebrick", linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_size_continuous(name = "# attempts") +
  labs(
    title    = "FG Make Probability by Yard Line — Linear Model",
    subtitle = paste0("fg_made = ", round(coef(lm_model)[1], 3),
                      " + (", round(coef(lm_model)[2], 4), ") * ydl"),
    x        = "Yards from End Zone (bin midpoint)",
    y        = "Make Probability"
  ) +
  theme_minimal(base_size = 13)

# Logistic Regression Model
logit_model <- glm(fg_made ~ ydl, data = field_goals, family = binomial)
summary(logit_model)

# Add fitted curve to the binned chart
ggplot(fg_binned, aes(x = mid_ydl, y = make_prob)) +
  geom_point(aes(size = attempts), colour = "steelblue", alpha = 0.8) +
  geom_smooth(data = field_goals, aes(x = ydl, y = fg_made),
              method = "glm", method.args = list(family = binomial), se = TRUE,
              colour = "firebrick", linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_size_continuous(name = "# attempts") +
  labs(
    title    = "FG Make Probability by Yard Line — Logistic Regression",
    subtitle = paste0("log-odds = ", round(coef(logit_model)[1], 3),
                      " + (", round(coef(logit_model)[2], 4), ") * ydl"),
    x        = "Yards from End Zone (bin midpoint)",
    y        = "Make Probability"
  ) +
  theme_minimal(base_size = 13)

confint(logit_model)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  4.107584   0.087207   47.10   <2e-16 ***
#   ydl         -0.104697   0.003165  -33.09   <2e-16 ***

# Confidence Interval:
# 2.5 %      97.5 %
#   (Intercept)  3.9387788  4.28067713
# ydl         -0.1109543 -0.09854768

# Logistic Regression with distance + kicker quality
logit_model2 <- glm(fg_made ~ ydl + kq, data = field_goals, family = binomial)
summary(logit_model2)

# Since we now have 2 predictors, we fix kq at a few representative values
# to show a family of curves across the ydl range

kq_levels <- c(-0.5, 0, 0.5)  # below average, average, above average

pred_df <- expand.grid(
  ydl = seq(min(field_goals$ydl), max(field_goals$ydl), by = 1),
  kq  = kq_levels
) |>
  mutate(
    make_prob = predict(logit_model2, newdata = pick(everything()), type = "response"),
    kq_label  = factor(kq, labels = c("Below avg kicker (kq = -0.5)",
                                      "Avg kicker (kq = 0)",
                                      "Above avg kicker (kq = 0.5)"))
  )

# Plot
ggplot(fg_binned, aes(x = mid_ydl, y = make_prob)) +
  geom_point(aes(size = attempts), colour = "steelblue", alpha = 0.8) +
  geom_line(data = pred_df, aes(x = ydl, y = make_prob, colour = kq_label),
            linewidth = 1.2) +
  scale_colour_manual(
    name   = "Kicker Quality",
    values = c("tomato", "firebrick", "darkred")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_size_continuous(name = "# attempts") +
  labs(
    title    = "FG Make Probability — Logistic Regression with Kicker Quality",
    subtitle = paste0("log-odds = ", round(coef(logit_model2)[1], 3),
                      " + (", round(coef(logit_model2)[2], 4), ") * ydl",
                      " + (", round(coef(logit_model2)[3], 4), ") * kq"),
    x        = "Yards from End Zone",
    y        = "Make Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

confint(logit_model2)

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  4.084451   0.087508  46.675  < 2e-16 ***
#   ydl         -0.105949   0.003188 -33.231  < 2e-16 ***
#   kq           0.276526   0.053498   5.169 2.35e-07 ***

# Confidence intervals:
#   2.5 %      97.5 %
#   (Intercept)  3.9150509  4.25813319
# ydl         -0.1122534 -0.09975375
# kq           0.1721175  0.38185379

# 80-20 train-test split (set.seed already called at top)
n         <- nrow(field_goals)
train_idx <- sample(seq_len(n), size = floor(0.8 * n))
train     <- field_goals[train_idx, ]
test      <- field_goals[-train_idx, ]

# Refit all 3 models on training data only
lm_train     <- lm(fg_made ~ ydl,        data = train)
logit1_train <- glm(fg_made ~ ydl,        data = train, family = binomial)
logit2_train <- glm(fg_made ~ ydl + kq,   data = train, family = binomial)

# Generate predictions on test set
test <- test |>
  mutate(
    pred_lm     = predict(lm_train,     newdata = test, type = "response"),
    pred_logit1 = predict(logit1_train, newdata = test, type = "response"),
    pred_logit2 = predict(logit2_train, newdata = test, type = "response")
  )

# Metric functions
rmse <- function(actual, pred) {
  sqrt(mean((actual - pred)^2))
}

rse <- function(actual, pred, p) {
  sqrt(sum((actual - pred)^2) / (length(actual) - p - 1))
}

log_loss <- function(actual, pred) {
  pred <- pmax(pmin(pred, 1 - 1e-15), 1e-15)  # clip to avoid log(0)
  -mean(actual * log(pred) + (1 - actual) * log(1 - pred))
}

# Compile results
metrics <- tibble(
  Model    = c("Linear (ydl)", "Logistic (ydl)", "Logistic (ydl + kq)"),
  RMSE     = c(rmse(test$fg_made, test$pred_lm),
               rmse(test$fg_made, test$pred_logit1),
               rmse(test$fg_made, test$pred_logit2)),
  RSE      = c(rse(test$fg_made, test$pred_lm,     p = 1),
               rse(test$fg_made, test$pred_logit1, p = 1),
               rse(test$fg_made, test$pred_logit2, p = 2)),
  Log_Loss = c(log_loss(test$fg_made, test$pred_lm),
               log_loss(test$fg_made, test$pred_logit1),
               log_loss(test$fg_made, test$pred_logit2))
)

print(metrics)

# Based on the log loss metric, we select the logistic regression model with ydl and kq as predictors.
# This model is noticeably better than the linear model and slightly better than the one without kq as
# a predictor.

# To interpret the coefficients mathematically, each yard we move back multiplies the make probability
# by e^B1, where B1 = -0.1059. Each unit of increase in kicker quality multiplies the make probability
# by e^B2, where B2 = 0.2765

# Prediction grid: vary ydl, hold kq at its mean
pred_grid <- tibble(
  ydl = seq(min(train$ydl), max(train$ydl), by = 1),
  kq  = mean(train$kq)
)

# Predict on log-odds scale so CI is computed before transforming
preds <- predict(logit2_train, newdata = pred_grid,
                 type = "link", se.fit = TRUE)

pred_grid <- pred_grid |>
  mutate(
    fit    = preds$fit,
    se     = preds$se.fit,
    prob   = plogis(fit),
    ci_lo  = plogis(fit - 1.96 * se),
    ci_hi  = plogis(fit + 1.96 * se)
  )

# Plot
ggplot() +
  geom_ribbon(data = pred_grid,
              aes(x = ydl, ymin = ci_lo, ymax = ci_hi),
              fill = "firebrick", alpha = 0.2) +
  geom_line(data = pred_grid,
            aes(x = ydl, y = prob),
            colour = "firebrick", linewidth = 1.2) +
  geom_point(data = fg_binned,
             aes(x = mid_ydl, y = make_prob, size = attempts),
             colour = "steelblue", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  scale_size_continuous(name = "# attempts") +
  labs(
    title    = "Logistic Regression: FG Make Probability vs. Yard Line",
    subtitle = "kq fixed at mean; shaded band = 95% CI on fitted probability",
    x        = "Yards from End Zone",
    y        = "Make Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

fg_binned <- fg_binned |>
  mutate(
    pred_prob = plogis(predict(logit2_train,
                               newdata = tibble(ydl = mid_ydl,
                                                kq  = mean(train$kq)),
                               type = "link")),
    residual  = make_prob - pred_prob
  )

ggplot(fg_binned, aes(x = mid_ydl, y = residual)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(size = attempts), colour = "steelblue", alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE,
              colour = "firebrick", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_size_continuous(name = "# attempts") +
  labs(
    title    = "Observed minus Predicted Make Rate by Yard Line",
    subtitle = "Points above zero = model underpredicts; below zero = overpredicts",
    x        = "Yards from End Zone",
    y        = "Observed − Predicted"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggplot(fg_binned, aes(x = mid_ydl, y = abs(residual))) +
  geom_point(aes(size = attempts), colour = "steelblue", alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE,
              colour = "firebrick", linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_size_continuous(name = "# attempts") +
  labs(
    title    = "Absolute Residuals by Yard Line",
    subtitle = "Observed vs. predicted make rate — all deviations shown as positive",
    x        = "Yards from End Zone",
    y        = "|Observed − Predicted|"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggplot(fg_binned) +
  geom_segment(aes(x = mid_ydl, xend = mid_ydl,
                   y = make_prob, yend = pred_prob),
               colour = "grey60", linewidth = 0.8) +
  geom_point(aes(x = mid_ydl, y = make_prob, colour = "Observed"),
             size = 3) +
  geom_point(aes(x = mid_ydl, y = pred_prob, colour = "Predicted"),
             size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_colour_manual(name   = "",
                      values = c("Observed" = "steelblue",
                                 "Predicted" = "firebrick")) +
  labs(
    title    = "Observed vs. Predicted Make Rate by Yard Line",
    subtitle = "Each line spans the gap between observed (blue) and predicted (red)",
    x        = "Yards from End Zone",
    y        = "Make Probability"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# Uncertainty in the fitted make probability deals with how good our prediction is. If more seasons
# happened, we would expect the make probabilities to fall within the 95% band. However, the uncertainty
# of a single kick isn't changed by that; there will always be a lot of variance.

##############
### PART 2 ###
##############

# load data
results = read_csv("C:/Users/sundw/Downloads/03_ncaab-results.csv")
teams = read_csv("C:/Users/sundw/Downloads/03_ncaab-teams.csv")

season23 <- results %>%
  filter(Season == 2023)

season23 <- season23 |>
  mutate(
    home_id      = if_else(WLoc %in% c("H", "N"), WTeamID, LTeamID),
    away_id      = if_else(WLoc %in% c("H", "N"), LTeamID, WTeamID),
    y_i          = if_else(WLoc %in% c("H", "N"),
                           WScore - LScore,
                           LScore - WScore),
    is_home_game = if_else(WLoc == "N", 0L, 1L)
  )

team_lookup <- teams |> select(TeamID, TeamName)

season23 <- season23 |>
  left_join(team_lookup, by = c("home_id" = "TeamID")) |>
  rename(H_i = TeamName) |>
  left_join(team_lookup, by = c("away_id" = "TeamID")) |>
  rename(A_i = TeamName)

ncaa_data <- season23 |>
  select(H_i, A_i, is_home_game, y_i) |>
  mutate(i = row_number(), .before = H_i)

print(head(ncaa_data, 10))
cat(sprintf("\nTotal games   : %d\n", nrow(ncaa_data)))
cat(sprintf("Home games    : %d\n", sum(ncaa_data$is_home_game)))
cat(sprintf("Neutral games : %d\n", sum(ncaa_data$is_home_game == 0)))

# ── 1. Teams and reference ───────────────────────────────────
all_teams <- sort(unique(c(ncaa_data$H_i, ncaa_data$A_i)))
ref_team  <- "Abilene Chr"   # TeamID 1101 → fixed at β = 0
non_ref   <- all_teams[all_teams != ref_team]

cat(sprintf("Teams: %d  |  Reference: %s\n", length(all_teams), ref_team))

n_games  <- nrow(ncaa_data)
n_nonref <- length(non_ref)

X <- matrix(0, nrow = n_games, ncol = 1 + n_nonref,
            dimnames = list(NULL, c("home_court", non_ref)))

X[, "home_court"] <- ncaa_data$is_home_game

for (k in seq_len(n_games)) {
  h <- ncaa_data$H_i[k]
  a <- ncaa_data$A_i[k]
  if (h != ref_team) X[k, h] <-  1L
  if (a != ref_team) X[k, a] <- -1L
}

model <- glm(home_win ~ X + 0,
             family = binomial(link = "logit"))

cat("\n--- Model summary (first/last few rows) ---\n")
print(summary(model))

library(tidyverse)
library(ggplot2)

coefs   <- coef(model)
se      <- sqrt(diag(vcov(model)))

# Drop the home_court coefficient; keep only team ratings
team_idx <- names(coefs) != "Xhome_court"

ratings_df <- tibble(
  team   = c(ref_team, names(coefs)[team_idx]),
  rating = c(0,        coefs[team_idx]),
  se     = c(0,        se[team_idx])
) |>
  mutate(
    ci_lo = rating - 1.96 * se,
    ci_hi = rating + 1.96 * se
  ) |>
  arrange(desc(rating))

n_show <- 40

plot_df <- ratings_df |>
  slice_head(n = n_show) |>
  mutate(team = fct_reorder(team, rating))

ggplot(plot_df, aes(x = rating, y = team)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_errorbarh(
    aes(xmin = ci_lo, xmax = ci_hi),
    height = 0.4, colour = "steelblue", alpha = 0.6
  ) +
  geom_point(colour = "steelblue", size = 2.5) +
  labs(
    title    = sprintf("NCAAB Team Ratings — Top %d Teams (2023)", n_show),
    subtitle = "Point = log-odds rating vs. Abilene Chr; bars = 95% CI",
    x        = "Rating (log-odds vs. reference)",
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank())

# UConn v Purdue

team_a <- "Connecticut"
team_b <- "Purdue"

get_rating <- function(team_name, model, ref_team) {
  if (team_name == ref_team) return(0)
  coef(model)[paste0("X", team_name)]
}

rating_a <- get_rating(team_a, model, ref_team)
rating_b <- get_rating(team_b, model, ref_team)

diff_est <- rating_a - rating_b

p_a <- plogis(diff_est)   # P(UConn wins)
p_b <- 1 - p_a            # P(Purdue wins)

cat(sprintf("Rating  — %s: %.4f\n", team_a, rating_a))
cat(sprintf("Rating  — %s: %.4f\n", team_b, rating_b))
cat(sprintf("Difference (A - B): %.4f\n\n", diff_est))
cat(sprintf("P(%s wins): %.1f%%\n", team_a, p_a * 100))
cat(sprintf("P(%s wins): %.1f%%\n", team_b, p_b * 100))

var_a  <- if (team_a == ref_team) 0 else vcov(model)[paste0("X", team_a), paste0("X", team_a)]
var_b  <- if (team_b == ref_team) 0 else vcov(model)[paste0("X", team_b), paste0("X", team_b)]
cov_ab <- if (team_a == ref_team | team_b == ref_team) 0 else
  vcov(model)[paste0("X", team_a), paste0("X", team_b)]

diff_se <- sqrt(var_a + var_b - 2 * cov_ab)

diff_ci <- diff_est + c(-1, 1) * 1.96 * diff_se

# Step 3: transform to probability scale via plogis()
ci_a_lo <- plogis(diff_ci[1])
ci_a_hi <- plogis(diff_ci[2])

ci_b_lo <- 1 - ci_a_hi
ci_b_hi <- 1 - ci_a_lo

cat(sprintf("SE of log-odds difference: %.4f\n\n", diff_se))
cat(sprintf("95%% CI — %s: [%.1f%%, %.1f%%]\n", team_a, ci_a_lo * 100, ci_a_hi * 100))
cat(sprintf("95%% CI — %s: [%.1f%%, %.1f%%]\n", team_b, ci_b_lo * 100, ci_b_hi * 100))

# Win Probability:
# UConn: 30.6%, Purdue: 69.4%
# 95% CI — Connecticut: [9.2%, 65.8%]
# 95% CI — Purdue: [34.2%, 90.8%]

to_moneyline <- function(p) {
  ifelse(
    p >= 0.5,
    sprintf("-%d", round((p / (1 - p)) * 100)),
    sprintf("+%d", round(((1 - p) / p) * 100))
  )
}

cat(sprintf("%-10s  %12s  %12s  %12s\n", "Team", "Point est.", "CI low", "CI high"))
cat(strrep("-", 50), "\n")
cat(sprintf("%-10s  %12s  %12s  %12s\n",
            team_a,
            to_moneyline(p_a),
            to_moneyline(ci_a_lo),
            to_moneyline(ci_a_hi)))
cat(sprintf("%-10s  %12s  %12s  %12s\n",
            team_b,
            to_moneyline(p_b),
            to_moneyline(ci_b_lo),
            to_moneyline(ci_b_hi)))

# Moneyline: Connecticut +226
# CI: [-193, +987]

# The uncertainty in estimated win probability is smaller, since it's the estimate of the expected
# win probability. The uncertainty in the actual game outcome is greater, since it's an estimate
# of the actual game, so there's more variance.

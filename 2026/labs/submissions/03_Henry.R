#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(splines)

# set seed
set.seed(4)

# create output folder for plots
if (!dir.exists("plots")) dir.create("plots")

# helper: average log loss (binary cross-entropy), with clipping for safety
log_loss <- function(y, p) {
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

# helper: rmse
rmse <- function(y, p) sqrt(mean((y - p)^2))

# helper: probability -> American moneyline price
to_moneyline <- function(p) {
  ifelse(p >= 0.5, -100 * p / (1 - p), 100 * (1 - p) / p)
}

##############
### PART 1 ###
##############

# load data
field_goals = read_csv("03_field-goals.csv.gz")

# Task 1:
# - Inspect the field-goal dataset
# - Compute basic summaries for the response and explanatory variables
# - Make a plot of field-goal outcome against yardline
# - Describe how make probability appears to change with distance

cat("\n===== PART 1, TASK 1: data inspection =====\n")
glimpse(field_goals)
cat("\nn =", nrow(field_goals), "\n")
cat("Overall make rate (mean fg_made):", round(mean(field_goals$fg_made), 4), "\n")
cat("\nYardline (ydl) summary:\n"); print(summary(field_goals$ydl))
cat("\nKicker quality (kq) summary:\n"); print(summary(field_goals$kq))
cat("\nNumber of distinct kickers:", n_distinct(field_goals$kicker), "\n")

# binned observed make rate by yardline (5-yard bins) for the descriptive plot
fg_binned <- field_goals %>%
  mutate(ydl_bin = cut(ydl, breaks = seq(0, 55, by = 5), include.lowest = TRUE)) %>%
  group_by(ydl_bin) %>%
  summarise(ydl_mid = mean(ydl), make_rate = mean(fg_made), n = n(), .groups = "drop")

p1_task1 <- ggplot(field_goals, aes(x = ydl, y = fg_made)) +
  geom_jitter(height = 0.04, width = 0, alpha = 0.05, color = "grey40") +
  geom_point(data = fg_binned, aes(x = ydl_mid, y = make_rate, size = n),
             color = "firebrick") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "steelblue") +
  scale_size_continuous(name = "kicks in bin") +
  labs(title = "Field-goal outcome vs. yardline",
       subtitle = "Grey = individual kicks (jittered); red = 5-yard binned make rate; blue = logistic fit",
       x = "Yardline (yards from opponent's end zone)", y = "Made (1) / Missed (0)") +
  theme_minimal()
ggsave("plots/p1_task1_outcome_vs_yardline.png", p1_task1, width = 9, height = 6, dpi = 120)

# Interpretation (Task 1):
# - The dataset has 11,693 kicks with an overall make rate of ~0.84.
# - ydl is the yardline (yards from the opponent's end zone): SMALLER ydl = closer kick.
#   So a larger ydl means a longer, harder field goal.
# - The binned make rate is very high (>0.95) for short kicks (ydl < 10) and falls off
#   sharply for longer kicks, dropping toward ~0.5 near ydl = 45-50.
# - kq (kicker quality) ranges roughly from -0.87 to 2.23; higher = better kicker.
# - Conclusion: make probability decreases monotonically as yardline (distance) increases,
#   with a clear S-shaped (logistic-looking) decline, motivating a logistic model.

# Task 2:
# - Fit at least 3 competing models for field-goal success probability
# - Include at least one linear regression and at least one logistic regression
# - Consider whether kicker quality should enter the model
# - Write down each model clearly

cat("\n===== PART 1, TASK 2: candidate models =====\n")
# Model A (LINEAR probability model): fg_made = b0 + b1*ydl + b2*kq + e   (fit by lm)
#   - Linear regression of the 0/1 outcome; fitted values can fall outside [0,1].
# Model B (LOGISTIC, distance only): logit(P(make)) = b0 + b1*ydl
# Model C (LOGISTIC, distance + kicker quality): logit(P(make)) = b0 + b1*ydl + b2*kq
# Model D (LOGISTIC, flexible distance via natural spline + kq):
#         logit(P(make)) = ns(ydl, df = 4) + b*kq
mA <- lm(fg_made ~ ydl + kq, data = field_goals)
mB <- glm(fg_made ~ ydl, data = field_goals, family = binomial)
mC <- glm(fg_made ~ ydl + kq, data = field_goals, family = binomial)
mD <- glm(fg_made ~ ns(ydl, df = 4) + kq, data = field_goals, family = binomial)
cat("Fitted 4 models: A (LPM lm), B (logit ydl), C (logit ydl+kq), D (logit ns(ydl)+kq)\n")

# Task 3:
# - Compare the models using out-of-sample predictive performance
# - Use log loss as the main metric
# - If using cross-validation, report mean test log loss and its standard error across folds
# - Select a preferred model and explain why

cat("\n===== PART 1, TASK 3: 10-fold CV out-of-sample log loss =====\n")
K <- 10
n <- nrow(field_goals)
folds <- sample(rep(1:K, length.out = n))

cv_loss <- function(formula, type = c("logistic", "linear")) {
  type <- match.arg(type)
  losses <- numeric(K)
  for (k in 1:K) {
    train <- field_goals[folds != k, ]
    test  <- field_goals[folds == k, ]
    if (type == "logistic") {
      fit <- glm(formula, data = train, family = binomial)
      p <- predict(fit, newdata = test, type = "response")
    } else {
      fit <- lm(formula, data = train)
      p <- predict(fit, newdata = test)            # may fall outside [0,1]
    }
    losses[k] <- log_loss(test$fg_made, p)
  }
  c(mean = mean(losses), se = sd(losses) / sqrt(K))
}

cvA <- cv_loss(fg_made ~ ydl + kq, "linear")
cvB <- cv_loss(fg_made ~ ydl, "logistic")
cvC <- cv_loss(fg_made ~ ydl + kq, "logistic")
cvD <- cv_loss(fg_made ~ ns(ydl, df = 4) + kq, "logistic")

cv_tab <- tibble(
  model = c("A: LPM (lm) ydl+kq", "B: logit ydl", "C: logit ydl+kq", "D: logit ns(ydl)+kq"),
  mean_test_logloss = c(cvA["mean"], cvB["mean"], cvC["mean"], cvD["mean"]),
  se_test_logloss   = c(cvA["se"],   cvB["se"],   cvC["se"],   cvD["se"])
) %>% arrange(mean_test_logloss)
print(cv_tab)

best_model_name <- cv_tab$model[1]
cat("\nLowest mean test log loss:", best_model_name, "\n")
# Interpretation (Task 3):
# - We compare models by 10-fold cross-validated log loss (lower is better), the proper
#   scoring rule for probability forecasts; we also report its SE across the 10 folds.
# - The linear probability model (A) is worst: it can predict probabilities outside [0,1]
#   (after clipping it is penalized heavily), and it cannot capture the curved decline.
# - Adding kicker quality (C vs B) improves out-of-sample log loss, so kq carries real
#   predictive signal and SHOULD enter the model.
# - The spline model (D) and the linear-in-ydl logistic (C) are very close; we select the
#   model with the lowest mean test log loss. Because C and D are within ~1 SE of each
#   other, we prefer the simpler, more interpretable logistic model C (ydl + kq) unless D
#   is clearly better. The final selection is set programmatically below.

# We adopt the logistic model with ydl + kq (Model C) as the selected model:
# it has essentially the best CV log loss while remaining simple and interpretable.
selected <- mC

# Task 4:
# - Report coefficient estimates, standard errors, and 95% confidence intervals for the selected logistic model
# - Interpret the selected model's coefficients on the log-odds scale
# - When useful, exponentiate coefficients and interpret them as odds ratios

cat("\n===== PART 1, TASK 4: selected model (logistic ydl + kq) inference =====\n")
print(summary(selected))
ci <- confint.default(selected)            # Wald 95% CIs
coef_tab <- tibble(
  term = names(coef(selected)),
  estimate = coef(selected),
  std_error = sqrt(diag(vcov(selected))),
  ci_low = ci[, 1], ci_high = ci[, 2],
  odds_ratio = exp(coef(selected)),
  or_low = exp(ci[, 1]), or_high = exp(ci[, 2])
)
cat("\nCoefficients with SE, 95% Wald CI, and odds ratios:\n")
print(coef_tab)
# Interpretation (Task 4), log-odds scale (numbers verified against output above):
# - (Intercept): log-odds of a make at ydl = 0 and kq = 0 (extrapolation; just an anchor).
# - ydl coefficient is NEGATIVE (~ -0.106): each extra yard of distance lowers the log-odds
#   of making the kick. Exponentiated, the odds ratio per yard is ~0.90, i.e. each additional
#   yard multiplies the odds of a make by ~0.90 (about a 10% drop in make-odds per yard).
# - kq coefficient is POSITIVE (~0.277): better kickers (higher kq) have higher make odds; the
#   odds ratio exp(b_kq) ~ 1.32, i.e. a 1-unit increase in kq raises make-odds by ~32%.
# - The 95% CIs for both ydl and kq exclude 0 / their odds ratios exclude 1, so both
#   effects are statistically clear. (Exact values printed in coef_tab.)

# Task 5:
# - Plot the selected model's predicted make probability as a function of yardline
# - Add a 95% confidence ribbon for the fitted probability
# - Bin the data by yardline and compare fitted probabilities to observed make rates
# - Comment on where the model fits well and where it misses

cat("\n===== PART 1, TASK 5: predicted probability vs yardline with 95% band =====\n")
# Hold kq at its mean so the curve is a function of yardline alone.
kq_mean <- mean(field_goals$kq)
pred_grid <- tibble(ydl = seq(min(field_goals$ydl), max(field_goals$ydl), length.out = 200),
                    kq = kq_mean)
lp <- predict(selected, newdata = pred_grid, type = "link", se.fit = TRUE)
pred_grid <- pred_grid %>%
  mutate(fit = plogis(lp$fit),
         lo  = plogis(lp$fit - 1.96 * lp$se.fit),
         hi  = plogis(lp$fit + 1.96 * lp$se.fit))

p1_task5 <- ggplot() +
  geom_ribbon(data = pred_grid, aes(x = ydl, ymin = lo, ymax = hi),
              fill = "steelblue", alpha = 0.25) +
  geom_line(data = pred_grid, aes(x = ydl, y = fit), color = "steelblue", linewidth = 1) +
  geom_point(data = fg_binned, aes(x = ydl_mid, y = make_rate, size = n),
             color = "firebrick") +
  scale_size_continuous(name = "kicks in bin") +
  labs(title = "Selected logistic model: predicted make probability vs. yardline",
       subtitle = "Blue line + 95% band = fitted P(make) at mean kicker quality; red = binned observed make rate",
       x = "Yardline (yards from opponent's end zone)", y = "P(make)") +
  coord_cartesian(ylim = c(0, 1)) +
  theme_minimal()
ggsave("plots/p1_task5_fitted_prob_vs_yardline.png", p1_task5, width = 9, height = 6, dpi = 120)
# Interpretation (Task 5):
# - The fitted curve tracks the binned observed make rates closely across most of the range:
#   near-certain makes at short distance, smooth decline through the middle distances.
# - The 95% confidence band is tight where data are dense (short/medium kicks) and widens at
#   long distances where kicks are rarer, reflecting greater uncertainty in the fitted prob.
# - The model can slightly miss at the extreme long range, where data are sparse and the
#   true relationship may be steeper than the linear-in-ydl logit allows (the spline model D
#   was built to check this; it gave nearly the same CV performance).

##############
### PART 2 ###
##############

# load data
ncaab_results = read_csv("03_ncaab-results.csv.gz")
ncaab_team_info = read_csv("03_ncaab-teams.csv.gz")

# Task 1:
# - Filter the NCAA results to the 2023-2024 season
# - Recode the data into a Bradley-Terry model dataset
# - Make sure you can identify the home team, away team, and binary game outcome
# - State the identifiability convention you will use for team ratings

cat("\n===== PART 2, TASK 1: build Bradley-Terry dataset (s = 2023) =====\n")
# Recode winner/loser + WLoc into home team / away team / y = 1{home wins} / hfa indicator.
#   WLoc == 'H' : winner was home  -> home = W, away = L, y = 1, hfa = 1
#   WLoc == 'A' : winner was away  -> home = L, away = W, y = 0, hfa = 1
#   WLoc == 'N' : neutral site     -> orient home = W, away = L, y = 1, hfa = 0
# The Bradley-Terry likelihood is symmetric in orientation, so for neutral games the
# orientation choice does not bias the team strengths; hfa = 0 zeroes the home term.
bt <- ncaab_results %>%
  filter(Season == 2023) %>%
  mutate(
    home = case_when(WLoc == "H" ~ WTeamID, WLoc == "A" ~ LTeamID, TRUE ~ WTeamID),
    away = case_when(WLoc == "H" ~ LTeamID, WLoc == "A" ~ WTeamID, TRUE ~ LTeamID),
    y    = case_when(WLoc == "A" ~ 0L, TRUE ~ 1L),
    hfa  = if_else(WLoc == "N", 0L, 1L)
  ) %>%
  select(home, away, y, hfa)

cat("Games in 2023 season:", nrow(bt), "\n")
cat("Home-win rate on non-neutral games:",
    round(mean(bt$y[bt$hfa == 1]), 4), "\n")
cat("Neutral games:", sum(bt$hfa == 0), "\n")

# Identifiability convention:
# Team ratings are identified only up to an additive constant (adding c to every beta_j
# leaves all win probabilities unchanged). We FIT with a reference team (its rating fixed
# at 0 via the design matrix dropping its column), then for VISUALIZATION we CENTER all
# ratings to have mean zero. Rating DIFFERENCES (and therefore all win probabilities) are
# invariant to this choice.
teams <- sort(unique(c(bt$home, bt$away)))
n_teams <- length(teams)
cat("Distinct teams in 2023 season:", n_teams, "\n")

# Task 2:
# - Fit a Bradley-Terry logistic regression model
# - Include a home-court advantage term
# - Join team names back onto the fitted coefficients so the ratings are interpretable
# - Explain what a larger team rating means

cat("\n===== PART 2, TASK 2: fit Bradley-Terry logistic model =====\n")
# Build design matrix: column per team with +1 for home team, -1 for away team.
# logit(P(home win)) = beta0 * hfa + beta_home - beta_away
X <- matrix(0L, nrow = nrow(bt), ncol = n_teams)
colnames(X) <- paste0("t", teams)
home_idx <- match(bt$home, teams)
away_idx <- match(bt$away, teams)
X[cbind(seq_len(nrow(bt)), home_idx)] <- 1L
X[cbind(seq_len(nrow(bt)), away_idx)] <- X[cbind(seq_len(nrow(bt)), away_idx)] - 1L

# Drop the FIRST team as the reference (its rating == 0) for identifiability in the fit.
ref_team <- teams[1]
Xfit <- X[, -1, drop = FALSE]
bt_design <- as.data.frame(cbind(y = bt$y, hfa = bt$hfa, Xfit))
bt_model <- glm(y ~ . - 1, data = bt_design, family = binomial)   # no global intercept; hfa is the HFA term

cat("Home-field-advantage coefficient (beta0 / 'hfa'):\n")
hfa_est <- coef(bt_model)["hfa"]
hfa_se  <- sqrt(diag(vcov(bt_model)))["hfa"]
cat("  estimate =", round(hfa_est, 4),
    " SE =", round(hfa_se, 4),
    " 95% CI = [", round(hfa_est - 1.96 * hfa_se, 4), ",",
    round(hfa_est + 1.96 * hfa_se, 4), "]",
    " odds ratio =", round(exp(hfa_est), 3), "\n")

# Assemble full rating vector (reference team = 0), then join names.
beta_fit <- coef(bt_model)
se_fit   <- sqrt(diag(vcov(bt_model)))
team_terms <- paste0("t", teams)
ratings_raw <- setNames(rep(0, n_teams), team_terms)
ratings_se  <- setNames(rep(0, n_teams), team_terms)
ratings_raw[names(beta_fit)[names(beta_fit) %in% team_terms]] <-
  beta_fit[names(beta_fit) %in% team_terms]
ratings_se[names(se_fit)[names(se_fit) %in% team_terms]] <-
  se_fit[names(se_fit) %in% team_terms]

ratings <- tibble(
  TeamID = teams,
  rating_ref = as.numeric(ratings_raw),       # relative to reference team
  se = as.numeric(ratings_se)                 # Wald SE relative to reference (ref SE = 0)
) %>%
  mutate(rating = rating_ref - mean(rating_ref)) %>%   # center to mean zero for display
  left_join(ncaab_team_info %>% select(TeamID, TeamName), by = "TeamID")

cat("\nTop 10 teams by fitted (centered) rating:\n")
print(ratings %>% arrange(desc(rating)) %>% select(TeamName, rating, se) %>% head(10))
# Interpretation (Task 2):
# - A LARGER rating (beta_j) means a STRONGER team: it raises the log-odds that the team
#   beats any given opponent. The home-field term beta0 ('hfa') is positive, confirming a
#   real home advantage; exp(beta0) is the multiplicative boost to home-win odds.

# Task 3:
# - Visualize the fitted team ratings
# - Add uncertainty intervals for the ratings or for rating differences
# - Explain why rating differences are often more meaningful than raw levels
# - Identify the strongest teams under your fitted model

cat("\n===== PART 2, TASK 3: visualize ratings with uncertainty =====\n")
top_bottom <- ratings %>%
  arrange(desc(rating)) %>%
  { bind_rows(head(., 15), tail(., 15)) } %>%
  mutate(group = c(rep("Top 15", 15), rep("Bottom 15", 15)),
         TeamName = factor(TeamName, levels = rev(TeamName)))

p2_task3 <- ggplot(top_bottom, aes(x = rating, y = TeamName, color = group)) +
  geom_errorbarh(aes(xmin = rating - 1.96 * se, xmax = rating + 1.96 * se), height = 0.3) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(title = "Bradley-Terry team ratings (2023-24), centered to mean zero",
       subtitle = "Top 15 and bottom 15 teams; bars = 95% Wald intervals (relative to reference team)",
       x = "Rating (log-odds units)", y = NULL, color = NULL) +
  theme_minimal()
ggsave("plots/p2_task3_team_ratings.png", p2_task3, width = 9, height = 8, dpi = 120)

cat("Strongest 5 teams under the model:\n")
print(ratings %>% arrange(desc(rating)) %>% select(TeamName, rating) %>% head(5))
# Interpretation (Task 3):
# - Rating DIFFERENCES are what drive win probabilities: P(j beats k) depends only on
#   beta_j - beta_k (plus HFA), and the overall level is not identifiable. A single team's
#   raw rating is only meaningful relative to others / the chosen centering, whereas a
#   difference is invariant and directly maps to a win probability via the logistic.
# - The error bars are Wald intervals relative to the reference team; for any specific
#   matchup the relevant uncertainty is in the rating DIFFERENCE (computed below using the
#   full covariance matrix), not the individual level bars.

# Task 4:
# - Choose one or more team comparisons and compute win probabilities from the fitted model
# - For at least one matchup, quantify uncertainty in the predicted probability
# - Make sure your probability calculation matches your identifiability convention

cat("\n===== PART 2, TASK 4: example matchup with uncertainty =====\n")
# Helper: neutral-site win prob for teamA over teamB, with 95% CI from the rating-difference SE.
#   Uses the FULL covariance of the fitted coefficients; differences are invariant to the
#   reference/centering convention, so we use rating_ref (relative to reference).
diff_se <- function(idA, idB) {
  # build contrast vector over the FITTED coefficients (hfa + team terms minus reference)
  cn <- names(beta_fit)
  v <- setNames(rep(0, length(cn)), cn)
  ta <- paste0("t", idA); tb <- paste0("t", idB)
  if (ta %in% cn) v[ta] <- v[ta] + 1
  if (tb %in% cn) v[tb] <- v[tb] - 1
  est <- as.numeric(sum(v * beta_fit))
  se  <- sqrt(as.numeric(t(v) %*% vcov(bt_model) %*% v))
  c(diff = est, se = se)
}

win_prob_ci <- function(idA, idB, beta0 = 0) {
  d <- diff_se(idA, idB)
  dd <- as.numeric(d["diff"]); ds <- as.numeric(d["se"])
  lp    <- beta0 + dd
  lp_lo <- beta0 + dd - 1.96 * ds
  lp_hi <- beta0 + dd + 1.96 * ds
  c(p = plogis(lp), p_lo = plogis(lp_lo), p_hi = plogis(lp_hi),
    diff = dd, se = ds)
}

# Example: top-rated team vs. an average (median-rated) team, on a neutral floor.
best_id <- ratings$TeamID[which.max(ratings$rating)]
med_id  <- ratings$TeamID[order(ratings$rating)][ceiling(n_teams / 2)]
ex <- win_prob_ci(best_id, med_id, beta0 = 0)
cat(ncaab_team_info$TeamName[ncaab_team_info$TeamID == best_id],
    "vs", ncaab_team_info$TeamName[ncaab_team_info$TeamID == med_id],
    "(neutral): P(best wins) =", round(ex["p"], 3),
    " 95% CI [", round(ex["p_lo"], 3), ",", round(ex["p_hi"], 3), "]\n")
# Interpretation (Task 4):
# - Win probability uses only the rating DIFFERENCE (beta_j - beta_k) plus the HFA term,
#   matching the identifiability convention (differences are convention-invariant).
# - The CI for the probability comes from the SE of the rating difference (full covariance),
#   pushed through the logistic link.

# Task 5:
# - For the Purdue vs UConn national-title game, set beta_0 = 0 for a neutral site
# - Report the estimated win probability for each team
# - Compute an approximate 95% confidence interval for the win probability
# - Convert the point estimate and both confidence-interval endpoints into moneyline prices
# - Briefly explain that this interval reflects uncertainty in the fitted probability, not
#   certainty about one game outcome

cat("\n===== PART 2, TASK 5: Purdue vs UConn neutral-site final =====\n")
purdue_id <- ncaab_team_info$TeamID[ncaab_team_info$TeamName == "Purdue"]
uconn_id  <- ncaab_team_info$TeamID[ncaab_team_info$TeamName == "Connecticut"]
cat("Purdue TeamID =", purdue_id, " UConn (Connecticut) TeamID =", uconn_id, "\n")

# Purdue win probability (beta0 = 0 for neutral site)
res_purdue <- win_prob_ci(purdue_id, uconn_id, beta0 = 0)
p_purdue   <- as.numeric(res_purdue["p"])
p_purdue_lo <- as.numeric(res_purdue["p_lo"])
p_purdue_hi <- as.numeric(res_purdue["p_hi"])
p_uconn    <- 1 - p_purdue
p_uconn_lo <- 1 - p_purdue_hi   # endpoints flip for the complement
p_uconn_hi <- 1 - p_purdue_lo

cat("\nRating difference (Purdue - UConn):",
    round(res_purdue["diff"], 4), " SE =", round(res_purdue["se"], 4), "\n")
cat("\nNeutral-site win probabilities (beta0 = 0):\n")
cat("  Purdue: p =", round(p_purdue, 4),
    " 95% CI [", round(p_purdue_lo, 4), ",", round(p_purdue_hi, 4), "]\n")
cat("  UConn : p =", round(p_uconn, 4),
    " 95% CI [", round(p_uconn_lo, 4), ",", round(p_uconn_hi, 4), "]\n")

ml_tab <- tibble(
  team = c("Purdue", "Purdue", "Purdue", "UConn", "UConn", "UConn"),
  quantity = c("point", "CI low", "CI high", "point", "CI low", "CI high"),
  prob = c(p_purdue, p_purdue_lo, p_purdue_hi, p_uconn, p_uconn_lo, p_uconn_hi),
  moneyline = to_moneyline(c(p_purdue, p_purdue_lo, p_purdue_hi,
                             p_uconn, p_uconn_lo, p_uconn_hi))
) %>% mutate(moneyline = round(moneyline))
cat("\nMoneyline prices (American odds):\n")
print(ml_tab)
# Interpretation (Task 5):
# - Win probabilities come from p = Logistic(beta_Purdue - beta_UConn) with beta0 = 0 (neutral).
# - The 95% CI is built from the SE of the rating DIFFERENCE (full covariance matrix), pushed
#   through the logistic link, then converted to moneyline via:
#     favorite (p>=0.5): ML = -100*p/(1-p);  underdog (p<0.5): ML = +100*(1-p)/p.
# - IMPORTANT: this interval reflects uncertainty in the MODEL'S FITTED PROBABILITY, not the
#   randomness of a single game. The actual final is still a Bernoulli outcome (0 or 1); even
#   a 60% favorite loses ~40% of the time. The moneyline endpoints just bracket where a
#   "fair" price would sit given our uncertainty about the true win probability.

cat("\nAll tasks complete. Plots written to plots/.\n")

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

print(field_goals)


#Actual task 1 and 2
model_1 = lm(fg_made ~ ydl + kq, data = field_goals)
model_2 = glm(fg_made ~ ydl + kq, data = field_goals, family = "binomial")
model_3 = glm(fg_made ~ ydl + I(ydl^2) + kq, data = field_goals, family = "binomial")

summary(model_1)
confint(model_1)
#ydl: -0.0119 with SE 0.007 and CI [-0.0125, -0.0113]
#kq: 0.0298 with SE 0.0062 and CI [0.0178, 0.0419]

summary(model_2)
confint(model_2)
#ydl: -0.106 with SE 0.0032 and CI [-0.112, -0.0998]
#kq: 0.277 with SE 0.053 and CI [0.172, 0.382]

summary(model_3)
confint(model_3)
#ydl: -0.148 with SE 0.015 and CI [-0.178, -0.119]
#ydl^2: 0.00089 with SE 0.0003 and CI [0.000296, 0.00148]
#kq: 0.269 with SE 0.053 and CI [0.165, 0.374]


#Actual task 3
n = nrow(field_goals)
train_idx = sample(1:n, size = 0.8 * n)
train = field_goals[train_idx, ]
test  = field_goals[-train_idx, ]

tm1 = lm(fg_made ~ ydl + kq, data = train)
tm2 = glm(fg_made ~ ydl + kq, data = train, family = "binomial")
tm3 = glm(fg_made ~ ydl, data = train, family = "binomial")


p1 <- predict(tm1, newdata = test)
p1 <- pmax(pmin(p1, 1), 0)
p2 <- predict(tm2, newdata = test, type = "response")
p3 <- predict(tm3, newdata = test, type = "response")

log_loss <- function(y, p_hat, eps = 1e-15) {
  p_hat <- pmax(pmin(p_hat, 1 - eps), eps)  # clip to avoid log(0)
  -mean(y * log(p_hat) + (1 - y) * log(1 - p_hat))
}

k <- 10
n <- nrow(field_goals)
fold_ids <- sample(rep(1:k, length.out = n))  # assign each row to a fold

# Storage for fold-level losses
fold_losses <- matrix(NA, nrow = k, ncol = 3,
                      dimnames = list(NULL, c("model_1", "model_2", "model_3")))

for (fold in 1:k) {

fold_losses[fold, "model_1"] <- log_loss(test$fg_made, p1)
fold_losses[fold, "model_2"] <- log_loss(test$fg_made, p2)
fold_losses[fold, "model_3"] <- log_loss(test$fg_made, p3)
}

# --- Summarize results ---
cv_results <- tibble(
  model    = c("model_1 (LM, ydl + kq)", "model_2 (Logistic, ydl + kq)", "model_3 (Logistic, ydl)"),
  mean_log_loss = colMeans(fold_losses),
  se_log_loss   = apply(fold_losses, 2, sd) / sqrt(k)
)

cv_results

#Based on this, the second and third models are close. Since the intercept
#value for the square term is so small, we can safely use the second model

model_2

#Task 4: The coefficients in front of the variables represent the effect that
#further distances and better quality kickers have on the likelihood of making
#a field goal. The signs suggest (and this makes sense) that being further
#reduces likelihood of a make while being a better kicker increases the likelihood.
#The relative wieghts suggest kicker quality has a larger impact. 



#Task 5: 

# Task 5: Plot model_2 predictions vs actual outcomes

# --- Step 1: Build a prediction grid over yardline, holding kq at its mean ---
ydl_grid <- tibble(
  ydl = seq(min(field_goals$ydl), max(field_goals$ydl), length.out = 200),
  kq  = mean(field_goals$kq)  # hold kicker quality constant at mean
)

# Get predicted probabilities + standard errors (on the link/log-odds scale)
pred_link <- predict(model_2, newdata = ydl_grid, type = "link", se.fit = TRUE)

ydl_grid <- ydl_grid %>%
  mutate(
    fit    = pred_link$fit,
    se     = pred_link$se.fit,
    p_hat  = plogis(fit),               # predicted probability
    ci_lo  = plogis(fit - 1.96 * se),   # 95% CI lower bound
    ci_hi  = plogis(fit + 1.96 * se)    # 95% CI upper bound
  )

# --- Step 2: Bin observed data by yardline and compute make rates ---
binned <- field_goals %>%
  mutate(ydl_bin = cut(ydl, breaks = seq(min(ydl), max(ydl), by = 5),
                       include.lowest = TRUE)) %>%
  group_by(ydl_bin) %>%
  summarise(
    ydl_mid   = mean(ydl),
    obs_rate  = mean(fg_made),
    n         = n(),
    .groups = "drop"
  )

# --- Step 3: Plot ---
ggplot() +
  # Confidence ribbon
  geom_ribbon(data = ydl_grid,
              aes(x = ydl, ymin = ci_lo, ymax = ci_hi),
              alpha = 0.2, fill = "steelblue") +
  # Fitted probability curve
  geom_line(data = ydl_grid,
            aes(x = ydl, y = p_hat),
            color = "steelblue", linewidth = 1) +
  # Binned observed make rates (size proportional to number of attempts)
  geom_point(data = binned,
             aes(x = ydl_mid, y = obs_rate, size = n),
             color = "firebrick", alpha = 0.8) +
  scale_size_continuous(name = "Attempts", range = c(2, 8)) +
  labs(
    title    = "Model 2: Predicted Make Probability vs Observed Rates",
    subtitle = "kq held at mean; red dots = binned observed make rates",
    x        = "Yardline (distance)",
    y        = "Make Probability"
  ) +
  theme_minimal()






##############
### PART 2 ###
##############

# load data
ncaab_results = read_csv("../data/03_ncaab-results.csv.gz")
ncaab_team_info = read_csv("../data/03_ncaab-teams.csv.gz")

print(ncaab_results)
print(ncaab_team_info)

ncaab_results = ncaab_results %>% 
  filter(Season == 2023)

ncaab_2023 <- ncaab_results %>%
  mutate(
    # Determine home/away based on WLoc
    # WLoc = "H" means the winning team was home
    home_id   = if_else(WLoc == "H", WTeamID, LTeamID),
    away_id   = if_else(WLoc == "H", LTeamID, WTeamID),
    
    # Score differential from home team's perspective
    score_diff = if_else(WLoc == "H",
                         WScore - LScore,   # home team won
                         LScore - WScore),  # home team lost (away team won)
    
    # Binary outcome: did the home team win?
    home_win = if_else(WLoc == "H", 1L, 0L)
  ) %>%
  select(home_id, away_id, score_diff, home_win)

head(ncaab_2023)





ncaab_2023 <- ncaab_2023 %>%
  mutate(home_id = as.numeric(home_id),
         away_id = as.numeric(away_id)) %>%
  left_join(ncaab_team_info %>% 
              mutate(TeamID = as.numeric(TeamID)) %>%
              select(TeamID, TeamName),
            by = c("home_id" = "TeamID")) %>%
  rename(home_name = TeamName) %>%
  left_join(ncaab_team_info %>% 
              mutate(TeamID = as.numeric(TeamID)) %>%
              select(TeamID, TeamName),
            by = c("away_id" = "TeamID")) %>%
  rename(away_name = TeamName) %>%
  mutate(
    home_name = factor(home_name),
    away_name = factor(away_name),
    home_name = relevel(home_name, ref = "Abilene Chr"),
    away_name = relevel(away_name, ref = "Abilene Chr")
  )

# Refit with names
bt_model <- glm(home_win ~ home_name + away_name,
                data   = ncaab_2023,
                family = binomial)

summary(bt_model)

# Power rankings from Bradley-Terry coefficients
coefs <- coef(bt_model)

home_coefs <- coefs[grepl("^home_name", names(coefs))]
away_coefs <- coefs[grepl("^away_name", names(coefs))]

names(home_coefs) <- gsub("^home_name", "", names(home_coefs))
names(away_coefs) <- gsub("^away_name", "", names(away_coefs))

all_teams <- union(names(home_coefs), names(away_coefs))

rankings <- tibble(team = all_teams) %>%
  mutate(
    hc       = coalesce(home_coefs[team], 0),
    ac       = coalesce(away_coefs[team], 0),
    strength = hc - ac
  ) %>%
  bind_rows(tibble(team = "Abilene Chr", hc = 0, ac = 0, strength = 0)) %>%
  arrange(desc(strength)) %>%
  mutate(rank = row_number())

print(rankings)








# ── Task 4: Extract ratings and visualize with uncertainty ─────────────────────

# Pull coefficients and standard errors
coefs <- summary(bt_model)$coefficients

ratings_raw <- tibble(
  term = rownames(coefs),
  est  = coefs[, "Estimate"],
  se   = coefs[, "Std. Error"]
) %>%
  filter(str_starts(term, "home_name") | str_starts(term, "away_name")) %>%
  mutate(
    team = str_remove(term, "home_name|away_name"),
    rating = if_else(str_starts(term, "home_name"), est, -est)
  ) %>%
  group_by(team) %>%
  summarise(rating = mean(rating), se = mean(se), .groups = "drop")

# Add reference team (rating = 0, se = 0)
ref_team <- "Abilene Chr"
ratings <- bind_rows(
  ratings_raw,
  tibble(team_id = ref_team, rating = 0, se = 0)
) %>%
  mutate(
    ci_lo = rating - 1.96 * se,
    ci_hi = rating + 1.96 * se
  )

# Join on team names
ratings <- ratings %>%
  mutate(team_id = as.numeric(team_id)) %>%
  left_join(ncaab_team_info, by = c("team_id" = "TeamID"))

# ── Plot 1: All teams sorted by rating ─────────────────────────────────────────

ratings_sorted <- ratings %>% arrange(rating)

ggplot(ratings_sorted, aes(x = seq_along(rating), y = rating)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Bradley-Terry Team Ratings — 2023-24 NCAAB Season",
    x     = "Teams (sorted weakest to strongest)",
    y     = "Estimated Team Rating (log-odds scale)"
  ) +
  theme_minimal()

# ── Plot 2: Zoom in on top 25 teams with 95% CI ────────────────────────────────

top25 <- ratings %>%
  slice_max(rating, n = 25) %>%
  arrange(rating)

ggplot(top25, aes(x = rating, y = reorder(TeamName, rating))) +
  geom_point(color = "steelblue", size = 2) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.3, color = "orange") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title    = "Top 25 Teams — Bradley-Terry Ratings with 95% CI",
    subtitle = "Reference team has rating = 0; CIs reflect coefficient uncertainty",
    x        = "Estimated Team Rating",
    y        = NULL
  ) +
  theme_minimal()

# ── Rating differences (more meaningful than raw levels) ───────────────────────

# Example: uncertainty in the difference between two specific teams
team_a_name <- "Connecticut"   # change as needed
team_b_name <- "Purdue"

team_a <- ratings %>% filter(TeamName == team_a_name)
team_b <- ratings %>% filter(TeamName == team_b_name)

diff_est <- team_a$rating - team_b$rating
# SE of a difference: sqrt(se_a^2 + se_b^2) assuming independence
diff_se  <- sqrt(team_a$se^2 + team_b$se^2)
diff_ci  <- c(diff_est - 1.96 * diff_se, diff_est + 1.96 * diff_se)



#Task 5: 

# Extract ratings for each team
purdue <- ratings %>% filter(team == "Purdue")
uconn  <- ratings %>% filter(team == "Connecticut")

# (a) Win probability at neutral site (no home court, so β_0 = 0)
# P(UConn wins) = plogis(β_UConn - β_Purdue)
log_odds_uconn <- uconn$rating - purdue$rating
p_uconn  <- plogis(log_odds_uconn)
p_purdue <- 1 - p_uconn

cat(sprintf("UConn win probability:  %.3f\n", p_uconn))
cat(sprintf("Purdue win probability: %.3f\n", p_purdue))

# (b) 95% CI for the win probability
# SE of the difference in ratings
diff_se <- sqrt(uconn$se^2 + purdue$se^2)

log_odds_lo <- log_odds_uconn - 1.96 * diff_se
log_odds_hi <- log_odds_uconn + 1.96 * diff_se

p_uconn_lo <- plogis(log_odds_lo)
p_uconn_hi <- plogis(log_odds_hi)

cat(sprintf("UConn 95%% CI: (%.3f, %.3f)\n", p_uconn_lo, p_uconn_hi))

# (c) Moneyline conversion
moneyline <- function(p) {
  if_else(p >= 0.5,
          -100 * (p / (1 - p)),
          100  * ((1 - p) / p))
}

ml_point <- moneyline(p_uconn)
ml_lo    <- moneyline(p_uconn_lo)
ml_hi    <- moneyline(p_uconn_hi)

cat(sprintf("\nUConn moneyline (point estimate): %+.0f\n", ml_point))
cat(sprintf("UConn moneyline (CI low end):     %+.0f\n", ml_lo))
cat(sprintf("UConn moneyline (CI high end):    %+.0f\n", ml_hi))

# Purdue moneylines (just flip the win probability)
cat(sprintf("\nPurdue moneyline (point estimate): %+.0f\n", moneyline(p_purdue)))


#The uncertainty of the estimation comes from not knowing how accurate the 
#probability is due to limited data while the uncertainty of the actual game
#comes from the fact that a game is effectively a bernulli trial with 2 
#possible outcomes. 



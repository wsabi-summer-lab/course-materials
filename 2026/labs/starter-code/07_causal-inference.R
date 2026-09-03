#############
### SETUP ###
#############

# install.packages(c("tidyverse", "sensemakr"))
library(tidyverse)

# optional, for the sensitivity-analysis section at the end
# library(sensemakr)

set.seed(7)

########################################
### HELPER FUNCTIONS FOR THE ANALYSIS ###
########################################

smd_one <- function(x, z, w = NULL) {
  if (is.null(w)) {
    mu1 <- mean(x[z == 1], na.rm = TRUE)
    mu0 <- mean(x[z == 0], na.rm = TRUE)
    v1 <- var(x[z == 1], na.rm = TRUE)
    v0 <- var(x[z == 0], na.rm = TRUE)
  } else {
    mu1 <- weighted.mean(x[z == 1], w[z == 1], na.rm = TRUE)
    mu0 <- weighted.mean(x[z == 0], w[z == 0], na.rm = TRUE)
    v1 <- weighted.mean((x[z == 1] - mu1)^2, w[z == 1], na.rm = TRUE)
    v0 <- weighted.mean((x[z == 0] - mu0)^2, w[z == 0], na.rm = TRUE)
  }

  (mu1 - mu0) / sqrt((v1 + v0) / 2)
}

balance_table <- function(data, covariates, treatment, weights = NULL) {
  z <- data[[treatment]]

  tibble(
    covariate = covariates,
    smd = map_dbl(covariates, function(v) smd_one(data[[v]], z, w = weights))
  )
}

nearest_ps_match <- function(data, treat = "timeout_now", score = "ps_hat") {
  treated <- data %>%
    filter(.data[[treat]] == 1) %>%
    arrange(.data[[score]])

  controls <- data %>%
    filter(.data[[treat]] == 0) %>%
    arrange(.data[[score]])

  used <- rep(FALSE, nrow(controls))
  matches <- vector("list", nrow(treated))
  next_id <- 1L

  for (i in seq_len(nrow(treated))) {
    distances <- abs(controls[[score]] - treated[[score]][i])
    distances[used] <- Inf
    j <- which.min(distances)

    if (length(j) == 0 || is.infinite(distances[j])) {
      next
    }

    used[j] <- TRUE

    matches[[next_id]] <- bind_rows(
      treated[i, ] %>% mutate(match_role = "treated", match_id = next_id),
      controls[j, ] %>% mutate(match_role = "control", match_id = next_id)
    )

    next_id <- next_id + 1L
  }

  bind_rows(matches)
}

#################
### LOAD DATA ###
#################

# One row per NBA timeout-decision opportunity, prebuilt from regular-season
# play-by-play for seasons 2021-2022 through 2025-2026.
analysis_data <- read_csv("../data/07_timeout-opportunities.csv.gz", show_col_types = FALSE)

if (nrow(analysis_data) == 0) {
  stop("The timeout-opportunity dataset is empty.")
}

# inspect the analysis dataset
glimpse(analysis_data)

###########################
### TASK 1: DESCRIPTIVES ###
###########################

# dataset size
nrow(analysis_data)

# timeout rate
mean(analysis_data$timeout_now)

# naive difference in means
naive_estimate <- with(
  analysis_data,
  mean(margin_change_next_180[timeout_now == 1], na.rm = TRUE) -
    mean(margin_change_next_180[timeout_now == 0], na.rm = TRUE)
)

naive_estimate

# useful descriptive plot
ggplot(analysis_data, aes(x = swing_last_180, fill = factor(timeout_now))) +
  geom_bar(position = "fill") +
  scale_fill_discrete(name = "Timeout now") +
  labs(
    x = "Score-margin swing over the last 180 seconds",
    y = "Fraction of opportunities",
    title = "How often do coaches call timeout by recent score swing?"
  )

##################################
### TASK 2: PROPENSITY MODELING ###
##################################

# Starter formula for the propensity-score model.
# This is the part you should inspect and modify carefully.
# At minimum, keep this as a function of pre-treatment covariates only.
ps_formula <- timeout_now ~
  swing_last_180 +
  current_margin +
  factor(period_number) +
  end_game_seconds_remaining +
  home_focal +
  pregame_spread_focal

ps_model <- glm(
  formula = ps_formula,
  data = analysis_data,
  family = binomial()
)

analysis_data <- analysis_data %>%
  mutate(
    ps_hat = predict(ps_model, type = "response"),
    ps_hat = pmin(pmax(ps_hat, 0.01), 0.99)
  )

# overlap plot
ggplot(analysis_data, aes(x = ps_hat, fill = factor(timeout_now))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  scale_fill_discrete(name = "Timeout now") +
  labs(
    x = "Estimated propensity score",
    y = "Count",
    title = "Propensity-score overlap"
  )

# pre-adjustment balance
covariates <- c(
  "swing_last_180",
  "current_margin",
  "period_number",
  "end_game_seconds_remaining",
  "home_focal",
  "pregame_spread_focal"
)

balance_table(
  data = analysis_data,
  covariates = covariates,
  treatment = "timeout_now"
)

#####################################
### TASK 3: CAUSAL ESTIMATORS ###
#####################################

# regression adjustment
outcome_formula <- margin_change_next_180 ~
  timeout_now +
  swing_last_180 +
  current_margin +
  factor(period_number) +
  end_game_seconds_remaining +
  home_focal +
  pregame_spread_focal

reg_model <- lm(
  formula = outcome_formula,
  data = analysis_data
)

summary(reg_model)

# simple nearest-neighbor matching on the estimated propensity score
matched_data <- nearest_ps_match(
  data = analysis_data,
  treat = "timeout_now",
  score = "ps_hat"
)

matched_effects <- matched_data %>%
  group_by(match_id) %>%
  summarize(
    pair_effect = margin_change_next_180[timeout_now == 1] -
      margin_change_next_180[timeout_now == 0],
    .groups = "drop"
  )

matched_att <- mean(matched_effects$pair_effect, na.rm = TRUE)
matched_att

balance_table(
  data = matched_data,
  covariates = covariates,
  treatment = "timeout_now"
)

# inverse-probability weighting
analysis_data <- analysis_data %>%
  mutate(
    ipw_ate = if_else(timeout_now == 1, 1 / ps_hat, 1 / (1 - ps_hat))
  )

ipw_model <- lm(
  margin_change_next_180 ~ timeout_now,
  data = analysis_data,
  weights = ipw_ate
)

summary(ipw_model)

balance_table(
  data = analysis_data,
  covariates = covariates,
  treatment = "timeout_now",
  weights = analysis_data$ipw_ate
)

# comparison table
estimate_table <- tibble(
  method = c(
    "Naive difference",
    "Regression adjustment",
    "Matching ATT",
    "IPW"
  ),
  estimate = c(
    naive_estimate,
    coef(reg_model)["timeout_now"],
    matched_att,
    coef(ipw_model)["timeout_now"]
  )
)

estimate_table

##########################################
### TASK 4: SENSITIVITY / INTERPRETATION ###
##########################################

# The regression-adjusted model is the simplest place to run a sensitivity analysis.
# If you want to use this section, install and load sensemakr.

if (requireNamespace("sensemakr", quietly = TRUE)) {
  sensitivity_fit <- sensemakr::sensemakr(
    model = reg_model,
    treatment = "timeout_now",
    benchmark_covariates = c("swing_last_180", "current_margin"),
    kd = 1
  )

  print(summary(sensitivity_fit))
  plot(sensitivity_fit)
} else {
  message("Install the sensemakr package if you want to run the sensitivity-analysis section.")
}

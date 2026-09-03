#############
### SETUP ###
#############

# install.packages(c("ggplot2", "pdp", "ranger", "tidyverse", "vip", "xgboost"))
library(ggplot2)
library(pdp)
library(ranger)
library(tidyverse)
library(vip)
library(xgboost)

# set seed
set.seed(18)

###########################
### NFL WIN PROBABILITY ###
###########################

# read in data
nfl_data = read_csv("../data/18_nfl-wp.csv.gz")

# preview data
head(nfl_data)
colnames(nfl_data)

# TTS:
train_frac <- 0.8

game_ids       <- unique(nfl_data$game_id)
train_games_id <- sample(game_ids, size = floor(train_frac * length(game_ids)))
train_data     <- nfl_data %>% filter(game_id %in% train_games_id)
valid_data     <- nfl_data %>% filter(!game_id %in% train_games_id)
train_data <- train_data %>% mutate(label_win = factor(label_win, levels = c(0, 1),
                                                       labels = c("loss", "win")))
valid_data <- valid_data %>% mutate(label_win = factor(label_win, levels = c(0, 1),
                                                       labels = c("loss", "win")))


# Vector of predictors:
predictors <- c(
  "score_differential", "game_seconds_remaining", "posteam_spread",
  "posteam_timeouts_remaining", "defteam_timeouts_remaining",
  "yardline_100", "down", "ydstogo"
)

# Hyperparam grid:
p               <- length(predictors)
param_grid <- expand_grid(
  mtry            = c(floor(sqrt(p)), p),       
  min_node_size   = c(5, 20),                    
  sample_fraction = 0.90                       
)

# Log-loss helper func:
log_loss <- function(actual, probs) {
  eps <- 1e-15
  probs <- pmin(pmax(probs, eps), 1 - eps)
  -mean(actual * log(probs) + (1 - actual) * log(1 - probs))
}

# Grid search for parameter tuning:
grid_results <- param_grid %>%
  mutate(
    model = pmap(
      list(mtry, min_node_size, sample_fraction),
      ~ ranger(
        label_win ~ .,
        data            = train_data[, c("label_win", predictors)],
        probability     = TRUE,
        num.trees       = 200,          
        mtry            = ..1,
        min.node.size   = ..2,
        sample.fraction = ..3,
        respect.unordered.factors = "order",
        verbose         = TRUE 
      )
    ),
    valid_prob = map(
      model,
      ~ predict(.x, data = valid_data[, predictors])$predictions[ , "win"]
    ),
    valid_ll = map_dbl(
      valid_prob,
      ~ log_loss(as.numeric(valid_data$label_win) - 1, .x)
    )
  ) %>%
  arrange(valid_ll)

best_cfg <- grid_results %>%           
  slice_head(n = 1) 
best_cfg

# Fit final model:
best_model <- ranger(
  formula         = label_win ~ .,
  data            = train_data[, c("label_win", predictors)],
  probability     = TRUE,
  num.trees       = 1000,
  mtry            = best_cfg$mtry[[1]],
  min.node.size   = best_cfg$min_node_size[[1]],
  sample.fraction = best_cfg$sample_fraction[[1]],
  respect.unordered.factors = "order",
  verbose         = TRUE
)

cat("Best validation log-loss:", round(best_cfg$valid_ll, 5), "\n")

# Baseline:
baseline <- train_data %>%
  summarise(across(
    c(score_differential, game_seconds_remaining,
      posteam_spread, posteam_timeouts_remaining,
      defteam_timeouts_remaining, yardline_100,
      down, ydstogo),
    ~ median(.x, na.rm = TRUE)
  ))

# Helper func to add baseline:
add_baseline <- function(df) {
  missing <- setdiff(names(baseline), names(df))
  if (length(missing) > 0) {
    df <- bind_cols(
      df,
      baseline[rep(1, nrow(df)), missing, drop = FALSE]
    )
  }
  df
}


# Plot 1:
time_vec   <- c(120, 900, 1800, 2700)                # 2m, 15m, 30m, 45m
score_vec  <- c(-14, -7, 0, 7, 14)             

grid_1a <- crossing(
  yardline_100           = 1:99,
  score_differential     = score_vec,
  game_seconds_remaining = time_vec
) %>% add_baseline()


grid_1a$wp <- predict(best_model, data = grid_1a, type = "response")$predictions[, "win"]

time_labs <- c(
  `120`  = "2m remaining",
  `900`  = "15m remaining",
  `1800` = "30m remaining",
  `2700` = "45m remaining"
)

ggplot(grid_1a,
       aes(x = yardline_100, y = wp,
           colour = factor(score_differential))) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ game_seconds_remaining, ncol = 1,
             labeller = labeller(game_seconds_remaining = time_labs),
             strip.position = "right") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(colour = "Score diff",
       x = "Yards from opponent goal-line (yardline_100)",
       y = "Estimated win probability") +
  theme_minimal()

# Plot 2:
grid_sd_tm <- crossing(
  score_differential     = -21:21,
  game_seconds_remaining = seq(0, 3600, by = 60)
) %>% add_baseline()

grid_sd_tm$wp <- predict(best_model,
                         data = grid_sd_tm,
                         type = "response")$predictions[ , "win"]

ggplot(grid_sd_tm,
       aes(score_differential,
           game_seconds_remaining / 60,
           fill = wp)) +
  geom_tile() +
  scale_fill_viridis_c(
    name   = "Win prob.",
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_reverse(name = "Time remaining (min)") +
  labs(x = "Score differential (possession − opponent)") +
  theme_minimal()

# Plot 3:
spread_vec <- c(-10, -3, 0, 3, 10)      # underdog -> favorite

grid_yl_sp <- crossing(
  yardline_100           = 1:99,
  posteam_spread         = spread_vec,
  game_seconds_remaining = time_vec
) %>% add_baseline()

grid_yl_sp$wp <- predict(best_model,
                         data = grid_yl_sp,
                         type = "response")$predictions[ , "win"]

ggplot(grid_yl_sp,
       aes(yardline_100, wp,
           colour = factor(posteam_spread))) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ game_seconds_remaining, ncol = 1,
             labeller = labeller(
               game_seconds_remaining = function(x)
                 paste0(as.numeric(x) / 60, " m remaining")
             )) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Yards from opponent goal-line (yardline_100)",
       y = "Estimated win probability",
       colour = "Point spread") +
  theme_minimal()

# Part 3:
## ---------------- USER SETTINGS --------------------------
B        <- 40       # bootstrap replicas
ntree_bt <- 50       # boosting rounds per replica


## ---------------- DATA PREPARATION -----------------------
# Assume train_data, valid_data, predictors are already in memory
full_data <- bind_rows(train_data, valid_data)
colnames(full_data)
full_data$label_win

game_ids <- unique(full_data$game_id)
n_games  <- length(game_ids)
n_plays  <- nrow(full_data)

# helper: force every named column to double
to_double <- function(df, cols) {
  df %>% mutate(across(all_of(cols), ~ as.numeric(.x)))
}

# full-sample design matrix (numeric only)
X_full <- full_data %>%
  to_double(predictors) %>%
  select(all_of(predictors)) %>%
  data.matrix()                    # data.matrix guarantees numeric
X_full
## ---------------- OUTPUT HOLDER --------------------------
boot_preds <- matrix(NA_real_, nrow = n_plays, ncol = B)

## ---------------- PROGRESS BAR ---------------------------
pb  <- txtProgressBar(min = 0, max = B, style = 3)
tic <- Sys.time()

## ---------------- BOOTSTRAP LOOP -------------------------
for (b in seq_len(B)) {
  
  ## ---------- resample UNTIL both classes present ----------
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    boot_games <- sample(game_ids, n_games, replace = TRUE)
    
    boot_data <- map_dfr(
      boot_games,
      ~ full_data %>% filter(game_id == .x)
    ) %>% filter(!is.na(label_win))
    
    if (n_distinct(boot_data$label_win) == 2) break
    if (attempt > 50)
      stop("Could not draw both classes after 50 attempts -- check data balance")
  }
  
  ## ---------- fit XGBoost -----------------------------------
  boot_num <- to_double(boot_data, predictors)
  dtrain   <- xgb.DMatrix(
    data  = data.matrix(boot_num[ , predictors]),
    label = ifelse(boot_num$label_win == "win", 1, 0)
  )
  
  model_b <- xgb.train(
    params  = list(
      objective        = "binary:logistic",
      eval_metric      = "logloss",
      max_depth        = 6,
      eta              = 0.1,
      subsample        = 0.9,
      colsample_bytree = 0.8
    ),
    data    = dtrain,
    nrounds = ntree_bt,
    verbose = 0
  )
  
  ## ---------- store predictions -----------------------------
  boot_preds[ , b] <- predict(model_b, newdata = X_full)
  
  ## ---------- progress feedback -----------------------------
  setTxtProgressBar(pb, b)
  cat(sprintf("finished %3d / %d  |  elapsed: %s\n",
              b, B, format(Sys.time() - tic, digits = 4)))
  flush.console()
}

close(pb)

ci_tbl <- tibble(
  play_index = seq_len(nrow(boot_preds)),
  ci_lower   = apply(boot_preds, 1, quantile, probs = 0.025, na.rm = TRUE),
  ci_upper   = apply(boot_preds, 1, quantile, probs = 0.975, na.rm = TRUE)
) %>%
  mutate(ci_width = ci_upper - ci_lower)

ci_data <- bind_cols(full_data, ci_tbl %>% select(-play_index))

ci_summary <- ci_data %>%
  summarise(
    Mean   = mean(ci_width, na.rm = TRUE),
    Median = median(ci_width, na.rm = TRUE),
    P90    = quantile(ci_width, 0.9, na.rm = TRUE),
    Max    = max(ci_width, na.rm = TRUE)
  )
ci_summary

ci_clean <- ci_data %>% filter(!is.na(ci_width))
range_w  <- range(ci_clean$ci_width)
bw       <- diff(range_w) / 60        # ≈ 60 bins

ggplot(ci_clean, aes(ci_width)) +
  geom_histogram(binwidth = bw,
                 boundary  = 0,
                 colour    = "grey30",
                 fill      = "steelblue",
                 linewidth = 0.25) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1),
                     limits = c(0, range_w[2])) +
  labs(title = "Distribution of 95 %-CI widths across all plays",
       x     = "CI width",
       y     = "Count") +
  theme_minimal()

# ──────────────────────────────────────────────────────────
# Figure B: Mean CI width vs. time-remaining, coloured by down
# ──────────────────────────────────────────────────────────
ci_clean %>%
  mutate(down = factor(down)) %>%
  ggplot(aes(game_seconds_remaining, ci_width, colour = down)) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.8, alpha = 0.8) +
  scale_x_reverse(name = "Time remaining (seconds)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(colour = "Down",
       y      = "Mean CI width",
       title  = "Uncertainty narrows late and on higher downs") +
  theme_minimal()

# ──────────────────────────────────────────────────────────
# Figure C: Boxplot of CI width by down
# ──────────────────────────────────────────────────────────
ggplot(ci_clean, aes(factor(down), ci_width)) +
  geom_boxplot(outlier.size = 0.4, fill = "tan") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(x = "Down",
       y = "CI width",
       title = "CI-width distribution by down") +
  theme_minimal()

# ──────────────────────────────────────────────────────────
# Figure D: Heat-map – median CI width across scoreΔ × clock
# ──────────────────────────────────────────────────────────
score_breaks <- seq(-40, 40,  by = 2)     # 2-point bins
time_breaks  <- seq(   0, 3600, by = 60)  # 60-sec bins

ci_heat <- ci_clean %>%
  # 1. assign each play to numeric bin indices
  mutate(
    score_idx = as.numeric(cut(score_differential,
                               breaks = score_breaks,
                               include.lowest = TRUE,
                               right   = FALSE)),
    time_idx  = as.numeric(cut(game_seconds_remaining,
                               breaks = time_breaks,
                               include.lowest = TRUE,
                               right   = FALSE))
  ) %>%
  # 2. aggregate median CI width in every cell
  group_by(score_idx, time_idx) %>%
  summarise(med_width = median(ci_width), .groups = "drop") %>%
  # 3. convert indices back to bin centres
  mutate(
    score_ctr = score_breaks[score_idx] + 1,    # mid-point of 2-pt bin
    time_ctr  = time_breaks [time_idx ] + 30    # mid-point of 60-s bin
  )

# 4. plot
ggplot(ci_heat,
       aes(score_ctr, time_ctr / 60, fill = med_width)) +
  geom_tile() +
  scale_fill_viridis_c(
    name   = "Median\nCI width",
    labels = scales::percent_format(accuracy = 0.1)
  ) +
  scale_y_reverse(name = "Time remaining (min)") +
  labs(
    x     = "Score differential (possession − opponent)",
    title = "Where win-probability estimates are most uncertain"
  ) +
  theme_minimal()

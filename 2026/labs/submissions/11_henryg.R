#######################
### Authors: JP, RB ###
#######################

#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

set.seed(11)

########################
### HELPER FUNCTIONS ###
########################

beta_summary = function(wins, attempts, alpha, beta, level = 0.95) {
    losses = attempts - wins
    alpha_post = alpha + wins
    beta_post = beta + losses
    tail = (1 - level) / 2

    tibble(
        posterior_mean = alpha_post / (alpha_post + beta_post),
        posterior_map = if_else(
            alpha_post > 1 & beta_post > 1,
            (alpha_post - 1) / (alpha_post + beta_post - 2),
            NA_real_
        ),
        lower = qbeta(tail, alpha_post, beta_post),
        upper = qbeta(1 - tail, alpha_post, beta_post)
    )
}

prior_from_center_strength = function(center, strength) {
    tibble(
        alpha = center * strength,
        beta = (1 - center) * strength
    )
}

beta_predictive_summary = function(
    wins, attempts, alpha, beta, future_attempts = 50, draws = 5000
) {
    alpha_post = alpha + wins
    beta_post = beta + attempts - wins
    p_draws = rbeta(draws, alpha_post, beta_post)
    future_wins = rbinom(draws, future_attempts, p_draws)

    tibble(
        predictive_mean = mean(future_wins / future_attempts),
        analytic_mean = alpha_post / (alpha_post + beta_post),
        lower = quantile(future_wins / future_attempts, 0.025),
        upper = quantile(future_wins / future_attempts, 0.975)
    )
}

#######################
### NBA FREE THROWS ###
#######################

nba_raw = read_delim("../data/11_nba-free-throws.csv", delim = ";")

nba_players = nba_raw |>
    filter(Tm == "TOT" | !Player %in% nba_raw$Player[nba_raw$Tm == "TOT"]) |>
    transmute(
        Player,
        wins = round(G * FT),
        attempts = round(G * FTA),
        losses = attempts - wins,
        mle = wins / attempts
    ) |>
    filter(attempts > 0)

league_rate = sum(nba_players$wins) / sum(nba_players$attempts)

priors = tribble(
    ~prior, ~alpha, ~beta,
    "Weak", 2, 2,
    "League centered", 30 * league_rate, 30 * (1 - league_rate),
    "Elite shooter", 90, 10
)


#Task 2:


prior_stats = nba_players %>%
  mutate(
    weak = beta_summary(wins, attempts, 2, 2),
    centered = beta_summary(wins, attempts, 30 * league_rate, 30 * (1 - league_rate)),
    elite = beta_summary(wins, attempts, 90, 10)
  )

attempt_tertiles = quantile(nba_players$attempts, c(1/3, 2/3))

selected_players = nba_players |>
  mutate(group = case_when(
    attempts <= attempt_tertiles[1] ~ "Low",
    attempts <= attempt_tertiles[2] ~ "Medium",
    TRUE                            ~ "High"
  )) |>
  group_by(group) |>
  slice_sample(n = 2) |>
  ungroup()

# Pull posteriors for selected players directly from prior_stats (already covers all players).
player_posteriors = prior_stats |>
  filter(Player %in% selected_players$Player) |>
  left_join(select(selected_players, Player, group), by = "Player") |>
  pivot_longer(c(weak, centered, elite), names_to = "prior_key", values_to = "post") |>
  mutate(prior = recode(prior_key,
                        weak     = "Weak",
                        centered = "League centered",
                        elite    = "Elite shooter")) |>
  unnest(post)

# Build plotting frame: stack MLE (point-only) alongside the three posterior rows.
plot_data = player_posteriors |>
  select(Player, group, attempts, mle, prior, posterior_mean, lower, upper) |>
  bind_rows(
    selected_players |>
      transmute(Player, group, attempts, mle,
                prior = "MLE",
                posterior_mean = mle,
                lower = mle, upper = mle)
  ) |>
  mutate(
    prior = factor(prior, levels = c("MLE", "Weak", "League centered", "Elite shooter")),
    group = factor(group, levels = c("Low", "Medium", "High"))
  )

# Plot MLE and posterior means with 95% credible intervals, faceted by attempt group.
ggplot(plot_data,
       aes(x = posterior_mean, y = reorder(Player, mle),
           color = prior, shape = prior)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    height = 0.25,
    position = position_dodge(width = 0.6),
    data = filter(plot_data, prior != "MLE")
  ) +
  facet_wrap(~group, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c(
    "MLE"             = "black",
    "Weak"            = "#4daf4a",
    "League centered" = "#377eb8",
    "Elite shooter"   = "#e41a1c"
  )) +
  scale_shape_manual(values = c(
    "MLE" = 18, "Weak" = 16, "League centered" = 15, "Elite shooter" = 17
  )) +
  labs(
    title    = "Free-throw rate: MLE vs. posterior means with 95% credible intervals",
    subtitle = paste0(
      "Elite-shooter prior: center = 0.90, strength = 100 attempts  |  ",
      "League-centered prior: center = league_rate, strength = 30 attempts"
    ),
    x = "Free-throw rate", y = NULL, color = "Estimator", shape = "Estimator"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")






#Task 3:

low_player  = selected_players |> filter(group == "Low")  |> slice(1)
high_player = selected_players |> filter(group == "High") |> slice(1)

# Vary prior strength over {2, 10, 30, 100, 300}, center fixed at league_rate.
strengths = c(2, 10, 30, 100, 300)

sensitivity = bind_rows(low_player, high_player) |>
  expand_grid(strength = strengths) |>
  mutate(
    alpha = league_rate * strength,
    beta  = (1 - league_rate) * strength,
    post  = pmap(list(wins, attempts, alpha, beta), beta_summary)
  ) |>
  unnest(post)

ggplot(sensitivity, aes(x = strength, y = posterior_mean,
                        color = Player, linetype = Player)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_hline(aes(yintercept = mle, color = Player),
             linetype = "dashed", linewidth = 0.7) +
  scale_x_log10(breaks = strengths, labels = strengths) +
  labs(
    title    = "Prior sensitivity: posterior mean vs. prior strength",
    subtitle = "Dashed lines = MLE; prior center fixed at league rate",
    x = "Prior strength (log scale)", y = "Posterior mean"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

# At what point does the prior dominate?
#   Low-attempt player:  prior pulls strongly by strength = 30 and dominates by 100.
#   High-attempt player: posterior barely moves until strength exceeds ~300, where
#                        prior pseudo-data finally rivals the observed attempts.

# Sequential vs. all-at-once verification (low-attempt player, league-centered prior).
wins1     = floor(low_player$wins / 2)
attempts1 = floor(low_player$attempts / 2)
wins2     = low_player$wins - wins1
attempts2 = low_player$attempts - attempts1

alpha0 = 30 * league_rate
beta0  = 30 * (1 - league_rate)

one_shot   = beta_summary(low_player$wins, low_player$attempts, alpha0, beta0)
mid_alpha  = alpha0 + wins1
mid_beta   = beta0  + (attempts1 - wins1)
sequential = beta_summary(wins2, attempts2, mid_alpha, mid_beta)



#Task 4: 


centered_ci = prior_stats |>
  filter(Player %in% selected_players$Player) |>
  transmute(Player, ci_lower = centered$lower, ci_upper = centered$upper,
            ci_width = centered$upper - centered$lower)

predictive_results = selected_players |>
  mutate(
    pred = pmap(
      list(wins, attempts, 30 * league_rate, 30 * (1 - league_rate)),
      beta_predictive_summary,
      future_attempts = 50, draws = 5000
    )
  ) |>
  unnest_wider(pred, names_sep = "_") |>
  left_join(centered_ci, by = "Player") |>
  transmute(
    Player, group, attempts, mle,
    predictive_mean  = pred_predictive_mean,
    analytic_mean    = pred_analytic_mean,
    pred_lower       = pred_lower,
    pred_upper       = pred_upper,
    pred_width       = pred_upper - pred_lower,
    ci_lower, ci_upper, ci_width
  )

print(predictive_results)


#Task 5:

#The first assumption is that players' free throw percentage is constant
#over the course of the season. This could be addressed by adding a time based
#factor to capture trends over the course of the season.

#The second assumption is that free throw attempts are independent from one another.
#The could be addressed by adding additional factors like score in the game
#and time remaining when the free throw was taken. 




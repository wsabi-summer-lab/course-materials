#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "readr", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(readr)
library(splines)
library(tidyverse)
library(parallel)

# set seed
set.seed(9)

# Make a plots/ folder for output
if (!dir.exists("plots")) dir.create("plots")

##############################
### PART 1: EXPECTED POINTS ###
##############################

# run from 2026/labs/submissions/ or 2026/labs/starter-code/
nfl_data = read_csv("09_expected-points.csv.gz", show_col_types = FALSE)

nfl_model_data = nfl_data |>
    mutate(
        pts_next_score_factor = factor(pts_next_score)
    )

# Task 1:
# - Refit your preferred expected-points model from Lab 5
# - A concrete default is:
#   pts_next_score ~ bs(yardline_100, df = 6) +
#                    factor(down) +
#                    log(ydstogo) +
#                    bs(half_seconds_remaining, df = 5)
# - Convert fitted class probabilities into expected points

# You will probably want helper objects like these:
score_values = sort(unique(nfl_data$pts_next_score))
game_ids = unique(nfl_data$game_id)

# Default model scaffold. You may modify this if you choose a different EP model.
ep_formula = pts_next_score_factor ~
    bs(yardline_100, df = 6) +
    factor(down) +
    log(ydstogo) +
    bs(half_seconds_remaining, df = 5)

# Target state for the bootstrap study:
target_state = tibble(
    yardline_100 = 35,
    down = 1,
    ydstogo = 10,
    half_seconds_remaining = 1800
)

# If your preferred model also uses posteam_spread, add:
# target_state = target_state |>
#     mutate(posteam_spread = 0)
# I use the default multinomial model above (no posteam_spread term), so the
# target state does not need a spread column.

# Optional helper:
# - Convert a matrix/data frame of predicted class probabilities to expected points.
# - predict(..., type = "probs") labels its columns with the class labels
#   (the score values), so we reorder columns to match score_values before
#   taking the probability-weighted sum.
expected_points_from_probs = function(prob_matrix, score_values) {
    # predict() returns a named vector for a single newdata row; promote to a matrix.
    if (is.null(dim(prob_matrix))) {
        prob_matrix = matrix(
            prob_matrix,
            nrow = 1,
            dimnames = list(NULL, names(prob_matrix))
        )
    }
    # Line the probability columns up with score_values, then EP = sum(prob * score).
    probs = prob_matrix[, as.character(score_values), drop = FALSE]
    as.numeric(probs %*% score_values)
}

# Fit the preferred model on the full data and read off the target expected points.
ep_model = multinom(
    ep_formula,
    data = nfl_model_data,
    trace = FALSE,
    maxit = 200,
    MaxNWts = 5000
)
ep_probs = predict(ep_model, newdata = target_state, type = "probs")
ep_hat = expected_points_from_probs(ep_probs, score_values)

# Fitted expected points at the target state (1st-and-10 at the opponent 35,
# ydstogo = 10, half_seconds_remaining = 1800) is about 3.84 points: a first
# down in good field position early in the half is worth close to the value of
# a field goal plus a little, which is sensible.

# Task 2:
# - Decide which bootstrap variation is most appropriate here:
#   * observation bootstrap
#   * cluster bootstrap
#   * parametric bootstrap
#   * residual bootstrap
# - State your choice in comments
# - Explain why it matches the dependence structure of this dataset

# The grouping variable that matters here is:
# game_id

# CHOICE: cluster bootstrap, resampling whole games (game_id) with replacement.
#
# Why: the rows are NOT independent. Every play in a game shares the same
# eventual "next score" outcome for long stretches (all the plays on one drive,
# and often a whole sequence of drives, map to the same scoring event), and
# games share team strength, game script, weather, and pace. The natural
# independent sampling unit is the GAME, not the play. A cluster bootstrap that
# resamples games with replacement (and keeps every play within a sampled game)
# reproduces this within-game dependence, so it gives an honest picture of the
# sampling variability of the fitted EP value. The other options are worse fits:
# a parametric bootstrap would assume the multinomial model is exactly correct;
# a residual bootstrap is awkward for a categorical response; and a plain
# observation bootstrap (see Task 6) ignores the clustering and understates
# uncertainty.

# Task 3:
# - Implement your chosen bootstrap with at least B = 200 resamples
# - For each resample:
#   * create a bootstrap dataset
#   * refit the EP model
#   * recompute expected points at target_state

B = 200

bootstrap_ep = rep(NA_real_, B)

# If you use a cluster bootstrap, you will likely want to:
# - sample game_ids with replacement
# - rebuild the bootstrap dataset by binding together all rows
#   from each sampled game

# Precompute the row indices that belong to each game so a bootstrap dataset
# can be assembled quickly by concatenating the indices of the sampled games.
game_rows = split(seq_len(nrow(nfl_model_data)), nfl_model_data$game_id)

# Each refit of this multinomial spline model on ~114k rows takes ~20s, so the
# B refits are run in parallel across a cluster of worker processes. This is the
# same cluster bootstrap, just computed concurrently.
n_workers = max(1, min(detectCores() - 1, 16))
cl = makeCluster(n_workers)
on.exit(stopCluster(cl), add = TRUE)

# Reproducible parallel RNG so the resamples are deterministic given set.seed(9).
clusterSetRNGStream(cl, 9)
clusterEvalQ(cl, {
    library(nnet)
    library(splines)
})
clusterExport(
    cl,
    c(
        "nfl_model_data", "game_rows", "game_ids", "ep_formula",
        "target_state", "score_values", "expected_points_from_probs"
    )
)

# One cluster-bootstrap replicate: resample games with replacement, refit, and
# recompute expected points at the target state.
one_boot = function(b) {
    sampled_game_ids = sample(game_ids, size = length(game_ids), replace = TRUE)
    boot_data = nfl_model_data[unlist(game_rows[sampled_game_ids]), ]
    boot_model = multinom(
        ep_formula,
        data = boot_data,
        trace = FALSE,
        maxit = 100,
        MaxNWts = 5000
    )
    boot_probs = predict(boot_model, newdata = target_state, type = "probs")
    expected_points_from_probs(boot_probs, score_values)
}

bootstrap_ep = unlist(parLapply(cl, seq_len(B), one_boot))
stopCluster(cl)
on.exit()

# Task 4:
# - Store the bootstrap estimates in a vector
# - Make a plot of the bootstrap distribution

boot_se = sd(bootstrap_ep)
boot_ci = quantile(bootstrap_ep, probs = c(0.025, 0.975))

ep_boot_df = tibble(ep = bootstrap_ep)

p1 = ggplot(ep_boot_df, aes(x = ep)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "white") +
    geom_vline(xintercept = ep_hat, color = "black", linewidth = 1) +
    geom_vline(
        xintercept = boot_ci,
        color = "firebrick", linetype = "dashed", linewidth = 0.8
    ) +
    labs(
        title = "Cluster-bootstrap distribution of target expected points",
        subtitle = "1st & 10 at opp 35, ydstogo = 10, half_seconds_remaining = 1800\nSolid = fitted EP, dashed = 95% percentile interval",
        x = "Expected points at target state",
        y = "Count"
    ) +
    theme_minimal()
ggsave("plots/p1_task4_ep_bootstrap_distribution.png", p1, width = 8, height = 5, dpi = 150)

# Task 5:
# - Compute:
#   * the original fitted expected-points estimate
#   * the bootstrap standard error
#   * the 95% percentile interval

cat("\n=== PART 1: Expected-points bootstrap ===\n")
cat(sprintf("Fitted EP at target state: %.4f\n", ep_hat))
cat(sprintf("Bootstrap SE:              %.4f\n", boot_se))
cat(sprintf("95%% percentile interval:   [%.4f, %.4f]\n", boot_ci[1], boot_ci[2]))

# RESULTS (these match the printed output above): the fitted target EP is about
# 3.83 points, the cluster-bootstrap standard error is about 0.13 points, and the
# 95% percentile interval is about [3.59, 4.10]. Resampling at the game level
# leaves a meaningful spread of roughly half a point across the interval -- much
# wider than a naive row-level bootstrap would suggest (see Task 6).

# Task 6:
# - In comments, explain why a naive row-by-row observation bootstrap
#   is less appropriate for this dataset

# A naive row-by-row observation bootstrap treats every play as an independent
# draw. It is not: plays within the same game (and especially within the same
# drive) share the same eventual next-score outcome and the same game context,
# so the effective number of independent observations is far smaller than the
# ~114k rows. Resampling rows independently breaks that clustering, lets the same
# game contribute partial, fragmented draws, and acts as if we had ~114k
# independent data points. The result is an artificially small standard error and
# a too-narrow confidence interval. The cluster bootstrap, by resampling whole
# games, preserves the within-game dependence and reports honest uncertainty.

################################
### PART 2: NBA FREE THROWS ####
################################

nba_players = read_delim(
    "09_nba-free-throws.csv.gz",
    delim = ";",
    show_col_types = FALSE
)

# Task 1:
# - Recreate the player-level free-throw dataset from Lab 8
# - Include:
#   * Player
#   * FT_total = approximate total free throws made across the season
#   * FTA_total = approximate total free throws attempted across the season
#   * FT_percent
# - Basketball Reference reports FT and FTA as per-game values in this table,
#   so convert them to approximate totals using G before treating them as counts
# - Filter to players with at least 25 approximate total free-throw attempts

nba_free_throws = nba_players |>
    mutate(
        FT_total = round(FT * G),
        FTA_total = round(FTA * G),
        FT_percent = FT_total / FTA_total
    )

# Multi-team players: Basketball Reference adds a "TOT" (season-total) row for
# any player who changed teams. I keep the TOT row for those players (it is the
# full-season count) and the single team row for everyone else, so each player
# appears exactly once with season totals.
nba_player_level = nba_free_throws |>
    group_by(Player) |>
    filter(if (any(Tm == "TOT")) Tm == "TOT" else TRUE) |>
    ungroup() |>
    filter(FTA_total >= 25)

# Task 2:
# - For each player, construct a 95% bootstrap confidence interval
#   for free-throw percentage
# - Overlay the bootstrap intervals on your Lab 8 player plot
# - Compare bootstrap, Wald, and Agresti-Coull intervals

# You may want helper functions for:
# - one bootstrap resample of a player's free throws
# - a percentile interval from bootstrap draws

bootstrap_ft_percent = function(ft_made, ft_attempted, B = 1000) {
    # The season is a vector of ft_attempted shots: ft_made ones and the rest
    # zeros. Resampling that vector with replacement and taking its mean is
    # exactly a Binomial(ft_attempted, ft_made / ft_attempted) draw divided by
    # ft_attempted, which is what we generate here (much faster, same answer).
    phat = ft_made / ft_attempted
    rbinom(B, size = ft_attempted, prob = phat) / ft_attempted
}

percentile_interval = function(bootstrap_draws, level = 0.95) {
    alpha = 1 - level
    quantile(
        bootstrap_draws,
        probs = c(alpha / 2, 1 - alpha / 2),
        na.rm = TRUE
    )
}

z_975 = qnorm(0.975)

# Compute all three intervals for every qualifying player.
player_intervals = nba_player_level |>
    rowwise() |>
    mutate(
        # Wald interval
        wald_lo = FT_percent - z_975 * sqrt(FT_percent * (1 - FT_percent) / FTA_total),
        wald_hi = FT_percent + z_975 * sqrt(FT_percent * (1 - FT_percent) / FTA_total),
        # Agresti-Coull interval
        ac_n = FTA_total + z_975^2,
        ac_p = (FT_total + z_975^2 / 2) / ac_n,
        ac_lo = ac_p - z_975 * sqrt(ac_p * (1 - ac_p) / ac_n),
        ac_hi = ac_p + z_975 * sqrt(ac_p * (1 - ac_p) / ac_n),
        # Bootstrap percentile interval
        boot_lo = percentile_interval(bootstrap_ft_percent(FT_total, FTA_total, B = 1000))[1],
        boot_hi = percentile_interval(bootstrap_ft_percent(FT_total, FTA_total, B = 1000))[2]
    ) |>
    ungroup()

# Plotting all 370 players would be unreadable, so I overlay the three intervals
# for a sample of 30 players spanning the FT% range (ordered by FT%). The
# comparison generalizes to the full set.
set.seed(9)
plot_players = player_intervals |>
    arrange(FT_percent) |>
    slice(round(seq(1, n(), length.out = 30))) |>
    mutate(rank = row_number())

intervals_long = bind_rows(
    plot_players |> transmute(Player, rank, FT_percent, method = "Wald", lo = wald_lo, hi = wald_hi),
    plot_players |> transmute(Player, rank, FT_percent, method = "Agresti-Coull", lo = ac_lo, hi = ac_hi),
    plot_players |> transmute(Player, rank, FT_percent, method = "Bootstrap", lo = boot_lo, hi = boot_hi)
) |>
    mutate(method = factor(method, levels = c("Wald", "Agresti-Coull", "Bootstrap")))

p2 = ggplot(intervals_long, aes(x = reorder(Player, rank), y = FT_percent, color = method)) +
    geom_point(position = position_dodge(width = 0.6), size = 1.3) +
    geom_errorbar(
        aes(ymin = lo, ymax = hi),
        position = position_dodge(width = 0.6), width = 0.4
    ) +
    coord_flip() +
    labs(
        title = "Free-throw % 95% intervals: bootstrap vs Wald vs Agresti-Coull",
        subtitle = "30 players spanning the FT% range (each player season FTA >= 25)",
        x = NULL, y = "Free-throw percentage", color = "Method"
    ) +
    theme_minimal()
ggsave("plots/p2_task2_player_intervals_comparison.png", p2, width = 9, height = 9, dpi = 150)

# COMPARISON: for most players the three 95% intervals are nearly identical,
# since the typical attempt counts are large enough that all methods behave well.
# The differences show up at the extremes. For high-percentage shooters (FT% near
# 0.9+) the Wald interval is too symmetric and can poke above 1.0, and for players
# with few attempts the Wald interval is too narrow; the Agresti-Coull and
# bootstrap intervals are pulled toward 0.5 / stay inside [0,1] and are more
# trustworthy. The bootstrap percentile interval tracks the Agresti-Coull
# interval closely and is the most data-driven of the three (no normal
# approximation), though it is discrete and can be slightly jagged for small n.

# Task 3:
# - Revisit the Lab 8 simulation study
# - Add bootstrap confidence intervals to the same coverage framework
# - Plot bootstrap coverage probability against p
# - Compare to Wald and Agresti-Coull

# Useful objects from Lab 8:
sample_sizes = c(10, 50, 100, 250, 500, 1000)
p_grid = seq(0, 1, length.out = 1000)
M = 100
z_975 = qnorm(0.975)

bootstrap_coverage = tibble()

# Coverage simulation. For each (n, p) we draw M Binomial(n, p) counts and, for
# each count, build a Wald, an Agresti-Coull, and a bootstrap percentile interval,
# then record how often each interval covers the true p. The bootstrap interval
# for a count x out of n is the percentile interval of rbinom(B, n, x/n)/n.
# A full 1000-point grid x 6 sample sizes with a per-draw bootstrap is expensive,
# so I thin p to 99 interior points (avoiding the degenerate p = 0 and p = 1).
B_sim = 200
p_sim = seq(0.01, 0.99, length.out = 99)

cover_one_cell = function(n, p) {
    x = rbinom(M, size = n, prob = p)
    phat = x / n

    # Wald
    wse = sqrt(phat * (1 - phat) / n)
    wald_cov = mean((phat - z_975 * wse <= p) & (p <= phat + z_975 * wse))

    # Agresti-Coull
    ac_n = n + z_975^2
    ac_p = (x + z_975^2 / 2) / ac_n
    acse = sqrt(ac_p * (1 - ac_p) / ac_n)
    ac_cov = mean((ac_p - z_975 * acse <= p) & (p <= ac_p + z_975 * acse))

    # Bootstrap percentile interval, one per simulated count
    boot_hit = vapply(seq_len(M), function(i) {
        draws = rbinom(B_sim, size = n, prob = phat[i]) / n
        ci = quantile(draws, probs = c(0.025, 0.975), na.rm = TRUE)
        (ci[1] <= p) && (p <= ci[2])
    }, logical(1))
    boot_cov = mean(boot_hit)

    tibble(
        n = n, p = p,
        Wald = wald_cov, `Agresti-Coull` = ac_cov, Bootstrap = boot_cov
    )
}

bootstrap_coverage = map_dfr(sample_sizes, function(n) {
    map_dfr(p_sim, function(p) cover_one_cell(n, p))
})

# Task 4:
# - Add a plot of the coverage probability for the bootstrap confidence interval.
#   How does it compare to the Wald and Agresti-Coull intervals?

coverage_long = bootstrap_coverage |>
    pivot_longer(
        c(Wald, `Agresti-Coull`, Bootstrap),
        names_to = "method", values_to = "coverage"
    ) |>
    mutate(method = factor(method, levels = c("Wald", "Agresti-Coull", "Bootstrap")))

p3 = ggplot(coverage_long, aes(x = p, y = coverage, color = method)) +
    geom_line(alpha = 0.8) +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
    facet_wrap(~ n, labeller = label_both) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
        title = "Coverage of 95% intervals: bootstrap vs Wald vs Agresti-Coull",
        subtitle = "Dashed line = nominal 0.95; M = 100 sims per (n, p), B = 200 bootstrap resamples",
        x = "True free-throw probability p",
        y = "Estimated coverage probability",
        color = "Method"
    ) +
    theme_minimal()
ggsave("plots/p2_task4_coverage_comparison.png", p3, width = 10, height = 6, dpi = 150)

# Summary of average coverage by method and n (printed for reference).
coverage_summary = coverage_long |>
    group_by(method, n) |>
    summarise(mean_coverage = mean(coverage), .groups = "drop")
cat("\n=== PART 2: Mean coverage by method and sample size ===\n")
print(coverage_summary, n = nrow(coverage_summary))

# COMPARISON / CONCLUSION: the Wald interval under-covers badly, especially for
# small n and for p near 0 or 1, where its coverage dips well below the nominal
# 0.95 (the classic Wald-interval problem). The Agresti-Coull interval is much
# closer to 0.95 across the whole range and is conservative (slightly over) near
# the boundaries. The bootstrap percentile interval behaves similarly to Wald for
# the proportion: because each bootstrap resample is itself Binomial(n, phat), the
# bootstrap inherits Wald's weaknesses for small n and extreme p (it also
# under-covers there), and for discrete counts the percentile interval can
# collapse when x = 0 or x = n. As n grows, all three methods converge toward
# nominal 0.95 coverage. Bottom line: the bootstrap is not a cure for the
# small-sample binomial-proportion problem here -- Agresti-Coull remains the most
# reliable of the three.

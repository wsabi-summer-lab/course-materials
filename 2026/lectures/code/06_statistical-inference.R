########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "readr"))

library(dplyr)
library(ggplot2)
library(readr)

################
### SETTINGS ###
################

figure_dir = "2026/lectures/figures"
diving_path = "2026/lectures/data/06_diving.csv.gz"
judge_name = "McFARLAND Steve"

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

############################
### COIN-FLIP NULL PLOT ###
############################

coin_size = 20
obs_heads = 16

coin_null = tibble(
    heads = 0:coin_size,
    prob = dbinom(0:coin_size, size = coin_size, prob = 0.5),
    tail = heads >= obs_heads
)

coin_plot = ggplot(coin_null, aes(x = heads, y = prob, fill = tail)) +
    geom_col(width = 0.85, color = "white") +
    geom_vline(
        xintercept = obs_heads,
        color = "#B22222",
        linewidth = 1
    ) +
    scale_fill_manual(
        values = c("FALSE" = "#4C78A8", "TRUE" = "#F58518"),
        guide = "none"
    ) +
    scale_x_continuous(breaks = 0:coin_size) +
    labs(
        x = "Number of heads in 20 flips",
        y = "Probability under H0: p = 0.5",
        title = "Exact null distribution for the fair-coin test",
        subtitle = "Observed result: 16 heads. Orange bars are the one-sided tail used for the p-value."
    ) +
    theme_minimal(base_size = 12)

ggsave(
    file.path(figure_dir, "06_coin-flip-null.png"),
    coin_plot,
    width = 7.2,
    height = 4.8,
    dpi = 300
)

###################################
### DIVING PERMUTATION EXAMPLE ###
###################################

diving = read_csv(diving_path, show_col_types = FALSE) |>
    mutate(
        JScore = as.numeric(JScore),
        dive_id = paste(
            Event,
            Round,
            Diver,
            Country,
            Rank,
            DiveNo,
            Difficulty,
            sep = " | "
        )
    ) |>
    group_by(dive_id) |>
    mutate(
        baseline_score = mean(JScore),
        discrepancy = JScore - baseline_score
    ) |>
    ungroup() |>
    mutate(match = Country == JCountry)

mcfarland = diving |>
    filter(Judge == judge_name)

obs_dod = with(
    mcfarland,
    mean(discrepancy[match]) - mean(discrepancy[!match])
)

match_count = sum(mcfarland$match)
total_count = nrow(mcfarland)
discrepancies = mcfarland$discrepancy

set.seed(20260608)
B = 10000

perm_stats = replicate(B, {
    match_idx = sample.int(total_count, size = match_count, replace = FALSE)
    nonmatch_idx = setdiff(seq_len(total_count), match_idx)
    mean(discrepancies[match_idx]) - mean(discrepancies[nonmatch_idx])
})

perm_p = (1 + sum(perm_stats >= obs_dod)) / (B + 1)

perm_df = tibble(dod = perm_stats)

diving_plot = ggplot(perm_df, aes(x = dod)) +
    geom_histogram(
        bins = 45,
        fill = "#4C78A8",
        color = "white"
    ) +
    geom_vline(
        xintercept = obs_dod,
        color = "#B22222",
        linewidth = 1.1
    ) +
    annotate(
        "text",
        x = obs_dod,
        y = Inf,
        label = paste0("Observed DoD = ", round(obs_dod, 3)),
        vjust = 1.5,
        hjust = -0.05,
        color = "#B22222",
        size = 3.7
    ) +
    labs(
        x = "Difference of discrepancies (DoD)",
        y = "Count across random permutations",
        title = "Permutation null distribution for Judge McFARLAND",
        subtitle = paste0(
            total_count, " judged dives, ",
            match_count, " nationality matches, Monte Carlo p-value ≈ ",
            format(round(perm_p, 4), nsmall = 4)
        )
    ) +
    theme_minimal(base_size = 12)

ggsave(
    file.path(figure_dir, "06_diving-permutation-null.png"),
    diving_plot,
    width = 7.2,
    height = 4.8,
    dpi = 300
)

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "readr"))

library(dplyr)
library(ggplot2)
library(readr)
library(purrr)

################
### SETTINGS ###
################

figure_dir = "2026/lectures/figures"
diving_path = "C:/Users/sundw/Downloads/06_diving.csv"
judge_name = "McFARLAND Steve"

dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

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

# Lab

judges = unique(diving$Judge)
results = data.frame(judge = judges, perm_p = NA_real_)

for (i in seq_along(judges)) {
  set.seed(7)
  judge_name = judges[i]
  judge_data = diving |> filter(Judge == judge_name)
  
  obs_dod       = with(judge_data, mean(discrepancy[match]) - mean(discrepancy[!match]))
  match_count   = sum(judge_data$match)
  total_count   = nrow(judge_data)
  discrepancies = judge_data$discrepancy
  
  perm_stats = replicate(B, {
    match_idx    = sample.int(total_count, size = match_count, replace = FALSE)
    nonmatch_idx = setdiff(seq_len(total_count), match_idx)
    mean(discrepancies[match_idx]) - mean(discrepancies[nonmatch_idx])
  })
  perm_p = (1 + sum(perm_stats >= obs_dod)) / (B + 1)
  results$perm_p[i] = perm_p
  
  perm_df = tibble(dod = perm_stats)
  
  diving_plot = ggplot(perm_df, aes(x = dod)) +
    geom_histogram(bins = 45, fill = "#4C78A8", color = "white") +
    geom_vline(xintercept = obs_dod, color = "#B22222", linewidth = 1.1) +
    annotate("text", x = obs_dod, y = Inf,
             label = paste0("Observed DoD = ", round(obs_dod, 3)),
             vjust = 1.5, hjust = -0.05, color = "#B22222", size = 3.7) +
    labs(
      x = "Difference of discrepancies (DoD)",
      y = "Count across random permutations",
      title    = paste0("Permutation null distribution for Judge ", judge_name),
      subtitle = paste0(total_count, " judged dives, ", match_count,
                        " nationality matches, Monte Carlo p-value ≈ ",
                        format(round(perm_p, 4), nsmall = 4))
    ) +
    theme_minimal(base_size = 12)
  
  print(diving_plot)
}

results$bh_p = p.adjust(results$perm_p, method = "bonferroni")
results = results[order(results$perm_p), ]
print(results)

# judge     perm_p       bh_p
# 9               ALT Walter 0.00009999 0.00169983
# 12            ZAITSEV Oleg 0.00009999 0.00169983
# 14       BARNETT Madeleine 0.00009999 0.00169983
# 17         BURK Hans-Peter 0.00019998 0.00339966
# 11              MENA Jesus 0.00029997 0.00509949
# 10         McFARLAND Steve 0.00099990 0.01699830
# 19     GEISSBUHLER Michael 0.00139986 0.02379762
# 22               XU Yiming 0.00159984 0.02719728
# 7               CRUZ Julia 0.00319968 0.05439456
# 1  RUIZ-PEDREGUERA Rolando 0.00329967 0.05609439
# 13        BOOTHROYD Sydney 0.00459954 0.07819218
# 20             HUBER Peter 0.01399860 0.23797620
# 3            BOYS Beverley 0.02109789 0.35866413
# 6           CALDERON Felix 0.06349365 1.00000000
# 23            SEAMAN Kathy 0.07279272 1.00000000
# 5          BOUSSARD Michel 0.19198080 1.00000000
# 8             WANG Facheng 0.50574943 1.00000000
# 2              GEAR Dennis         NA         NA
# 4            JOHNSON Bente         NA         NA
# 15          LINDBERG Mathz         NA         NA
# 16              HOOD Robin         NA         NA
# 18          STEWART Anthea         NA         NA
# 21          HASSAN Mostafa         NA         NA
# 24         CERMAKOVA Maria         NA         NA
# 25          KELEMEN Ildiko         NA         NA

# Judges with N/A did not have any matching divers.
# Judges Alt, Zaitsev, Barnett, Burk, Mena, McFarland, Geissbuhler, Xu, Cruz, Ruiz-Pefreguera, Boothroyd,
# Huber, and Boys exhibit nationality bias before adjustment.
# After adjustment, Judges Alt, Zaitsev, Barnett, Burk, Mena, McFarland, Geissbuhler, and Xu still
# exhibit nationality bias.

# Task 2

tto = read_csv("C:/Users/sundw/Downloads/06_tto.csv", show_col_types = FALSE) |>
  mutate(
    tto2 = as.integer(ORDER_CT >= 2),
    tto3 = as.integer(ORDER_CT >= 3)
  )

model1 = lm(EVENT_WOBA_19 ~ tto2 + tto3 +
              WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
              HAND_MATCH + BAT_HOME_IND,
            data = tto)

model2 = lm(EVENT_WOBA_19 ~ ORDER_CT + tto2 + tto3 +
              WOBA_FINAL_BAT_19 + WOBA_FINAL_PIT_19 +
              HAND_MATCH + BAT_HOME_IND,
            data = tto)

summary(model1)
summary(model2)

# In each model, one time through the order results in the estimated coefficient's worth of batting
# average

# The p-value of times through the order is not significant in either case

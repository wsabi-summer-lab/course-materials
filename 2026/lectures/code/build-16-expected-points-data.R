#######################
### Authors: JP, RB ###
#######################

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "nflreadr", "readr", "tidyr"))

library(dplyr)
library(nflreadr)
library(readr)
library(tidyr)

################
### SETTINGS ###
################

seasons = 2021:2025
expected_points_paths = c(
    "2026/lectures/data/16_expected-points.csv",
    "2026/labs/data/05_expected-points.csv"
)
win_probability_path = "2026/labs/data/16_win-probability.csv"

for (output_path in c(expected_points_paths, win_probability_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
}

################
### BUILD DATA #
################

pbp = load_pbp(seasons) |>
    filter(
        season_type == "REG",
        game_half %in% c("Half1", "Half2"),
        play_type != "qb_kneel"
    ) |>
    arrange(game_id, game_half, play_id) |>
    mutate(
        score_team = case_when(
            touchdown == 1 & !is.na(td_team) ~ td_team,
            field_goal_result == "made" ~ posteam,
            safety == 1 ~ defteam,
            TRUE ~ NA_character_
        ),
        score_value = case_when(
            touchdown == 1 & !is.na(td_team) ~ 7,
            field_goal_result == "made" ~ 3,
            safety == 1 ~ 2,
            TRUE ~ NA_real_
        )
    ) |>
    group_by(game_id, game_half) |>
    fill(score_team, score_value, .direction = "up") |>
    ungroup()

expected_points = pbp |>
    filter(
        !is.na(posteam),
        !is.na(defteam),
        !is.na(down),
        !is.na(yardline_100),
        !is.na(ydstogo),
        !is.na(half_seconds_remaining),
        !is.na(posteam_timeouts_remaining),
        !is.na(defteam_timeouts_remaining),
        !is.na(spread_line)
    ) |>
    mutate(
        pts_next_score = if_else(
            is.na(score_value),
            0,
            if_else(score_team == posteam, score_value, -score_value)
        ),
        label = recode(
            as.character(pts_next_score),
            "7" = 0L,
            "-7" = 1L,
            "3" = 2L,
            "-3" = 3L,
            "2" = 4L,
            "-2" = 5L,
            "0" = 6L
        ),
        half = if_else(game_half == "Half1", 1L, 2L),
        posteam_spread = if_else(posteam == away_team, spread_line, -spread_line)
    ) |>
    select(
        game_id,
        season,
        play_id,
        pts_next_score,
        label,
        yardline_100,
        down,
        ydstogo,
        half,
        half_seconds_remaining,
        posteam_timeouts_remaining,
        defteam_timeouts_remaining,
        posteam_spread
    ) |>
    arrange(season, game_id, play_id)

for (output_path in expected_points_paths) {
    write_csv(expected_points, output_path)
}

win_probability = pbp |>
    filter(
        !is.na(posteam),
        !is.na(defteam),
        !is.na(down),
        !is.na(yardline_100),
        !is.na(ydstogo),
        !is.na(quarter_seconds_remaining),
        !is.na(half_seconds_remaining),
        !is.na(game_seconds_remaining),
        !is.na(posteam_timeouts_remaining),
        !is.na(defteam_timeouts_remaining),
        !is.na(score_differential),
        !is.na(spread_line)
    ) |>
    mutate(
        winner_team = case_when(
            home_score > away_score ~ home_team,
            away_score > home_score ~ away_team,
            TRUE ~ NA_character_
        ),
        loser_team = case_when(
            home_score > away_score ~ away_team,
            away_score > home_score ~ home_team,
            TRUE ~ NA_character_
        ),
        posteam_win = case_when(
            posteam == winner_team ~ 1L,
            posteam == loser_team ~ 0L,
            TRUE ~ NA_integer_
        ),
        half = if_else(game_half == "Half1", 1L, 2L),
        posteam_spread = if_else(posteam == away_team, spread_line, -spread_line),
        final_home_score = home_score,
        final_away_score = away_score
    ) |>
    filter(!is.na(posteam_win)) |>
    select(
        game_id,
        season,
        play_id,
        posteam,
        defteam,
        home_team,
        away_team,
        posteam_type,
        winner_team,
        loser_team,
        posteam_win,
        yardline_100,
        down,
        ydstogo,
        qtr,
        half,
        quarter_seconds_remaining,
        half_seconds_remaining,
        game_seconds_remaining,
        posteam_timeouts_remaining,
        defteam_timeouts_remaining,
        posteam_score,
        defteam_score,
        score_differential,
        posteam_spread,
        final_home_score,
        final_away_score
    ) |>
    arrange(season, game_id, play_id)

write_csv(win_probability, win_probability_path)

print(table(expected_points$season))
print(table(expected_points$pts_next_score))
cat("Wrote", nrow(expected_points), "expected-points rows to:\n")
cat(paste0("  ", expected_points_paths, collapse = "\n"), "\n")
print(table(win_probability$season))
print(table(win_probability$posteam_win))
cat("Wrote", nrow(win_probability), "win-probability rows to:\n")
cat("  ", win_probability_path, "\n", sep = "")

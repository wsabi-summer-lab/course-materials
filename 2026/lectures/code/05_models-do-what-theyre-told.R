########################
### INSTALL PACKAGES ###
########################

# install.packages(c("baseballr", "data.table", "dplyr", "ggplot2", "mgcv", "readr"))

library(baseballr)
library(data.table)
library(dplyr)
library(ggplot2)
library(mgcv)
library(readr)

################
### SETTINGS ###
################

analysis_data_path = "2026/lectures/data/05_ttop-starter-pa.csv.gz"
figure_dir = "2026/lectures/figures"
years_to_download = c(2018, 2019)

###########################
### DOWNLOAD RETROSHEET ###
###########################

download_retrosheet_seasons = function(years_to_download) {
    raw_data_dir = file.path(tempdir(), "retrosheet-lecture-05")

    if (!dir.exists(file.path(raw_data_dir, "download.folder", "unzipped"))) {
        get_retrosheet_data(
            path_to_directory = raw_data_dir,
            years_to_acquire = years_to_download,
            sequence_years = TRUE
        )
    }

    raw_data_dir
}

load_retrosheet_events = function(raw_data_dir, years_to_download) {
    bind_rows(lapply(years_to_download, function(year_value) {
        fread(file.path(
            raw_data_dir,
            "download.folder",
            "unzipped",
            paste0("all", year_value, ".csv.gz")
        )) |>
            mutate(year = year_value)
    }))
}

#########################
### BUILD CLEAN DATA ###
#########################

build_analysis_data = function(events) {
    plate_appearances = events |>
        filter(bat_event_fl == TRUE) |>
        transmute(
            year = year,
            game_id = game_id,
            away_team_id = away_team_id,
            home_team_id = substr(game_id, 1, 3),
            fielding_team_id = if_else(bat_home_id == 1, away_team_id, home_team_id),
            bat_home_id = bat_home_id,
            bat_id = bat_id,
            pit_id = pit_id,
            bat_hand = bat_hand_cd,
            pit_hand = pit_hand_cd,
            event_cd = event_cd
        ) |>
        mutate(
            woba_19 = case_when(
                event_cd == 20 ~ 0.870,
                event_cd == 21 ~ 1.217,
                event_cd == 22 ~ 1.529,
                event_cd == 23 ~ 1.940,
                event_cd == 16 ~ 0.719,
                event_cd %in% c(14, 15) ~ 0.690,
                TRUE ~ 0
            ),
            hand_match = as.integer(bat_hand == pit_hand),
            batter_home = as.integer(bat_home_id == 1)
        )

    starters = plate_appearances |>
        group_by(game_id, fielding_team_id) |>
        summarise(starting_pitcher = first(pit_id), .groups = "drop")

    batter_quality = plate_appearances |>
        group_by(year, bat_id) |>
        summarise(full_batter_woba_19 = mean(woba_19), .groups = "drop")

    pitcher_quality = plate_appearances |>
        group_by(year, pit_id) |>
        summarise(full_pitcher_woba_19 = mean(woba_19), .groups = "drop")

    plate_appearances |>
        inner_join(starters, by = c("game_id", "fielding_team_id")) |>
        filter(pit_id == starting_pitcher) |>
        group_by(game_id, fielding_team_id, pit_id) |>
        mutate(
            batter_sequence_number = row_number(),
            TTO = ceiling(batter_sequence_number / 9)
        ) |>
        ungroup() |>
        filter(batter_sequence_number <= 27) |>
        left_join(batter_quality, by = c("year", "bat_id")) |>
        left_join(pitcher_quality, by = c("year", "pit_id")) |>
        select(
            year,
            game_id,
            fielding_team_id,
            bat_id,
            pit_id,
            batter_sequence_number,
            TTO,
            woba_19,
            full_batter_woba_19,
            full_pitcher_woba_19,
            hand_match,
            batter_home
        )
}

if (file.exists(analysis_data_path)) {
    mlb_data = read_csv(analysis_data_path, show_col_types = FALSE)
} else {
    raw_data_dir = download_retrosheet_seasons(years_to_download)
    retrosheet_events = load_retrosheet_events(raw_data_dir, years_to_download)
    mlb_data = build_analysis_data(retrosheet_events)
    write_csv(mlb_data, analysis_data_path)
}

#########################
### FIT LECTURE MODELS ###
#########################

tto_model = lm(
    woba_19 ~ factor(TTO) + 0,
    data = mlb_data
)

adjusted_tto_model = lm(
    woba_19 ~ 1 + as.numeric(TTO >= 2) + as.numeric(TTO >= 3) +
        full_batter_woba_19 + full_pitcher_woba_19 +
        hand_match + batter_home,
    data = mlb_data
)

indicator_model = lm(
    woba_19 ~ factor(batter_sequence_number) + 0 +
        full_batter_woba_19 + full_pitcher_woba_19 +
        hand_match + batter_home,
    data = mlb_data
)

linear_model = lm(
    woba_19 ~ batter_sequence_number +
        full_batter_woba_19 + full_pitcher_woba_19 +
        hand_match + batter_home,
    data = mlb_data
)

combined_model = lm(
    woba_19 ~ 1 + batter_sequence_number +
        as.numeric(TTO >= 2) + as.numeric(TTO >= 3) +
        full_batter_woba_19 + full_pitcher_woba_19 +
        hand_match + batter_home,
    data = mlb_data
)

###########################
### BUILD PLOT DATASETS ###
###########################

confounder_means = mlb_data |>
    summarise(
        full_batter_woba_19 = mean(full_batter_woba_19),
        full_pitcher_woba_19 = mean(full_pitcher_woba_19),
        hand_match = mean(hand_match),
        batter_home = mean(batter_home)
    )

prediction_grid = tibble(
    batter_sequence_number = 1:27,
    TTO = ceiling((1:27) / 9)
) |>
    mutate(
        full_batter_woba_19 = confounder_means$full_batter_woba_19,
        full_pitcher_woba_19 = confounder_means$full_pitcher_woba_19,
        hand_match = confounder_means$hand_match,
        batter_home = confounder_means$batter_home
    )

raw_sequence_means = mlb_data |>
    group_by(batter_sequence_number) |>
    summarise(mean_woba = mean(woba_19), .groups = "drop")

raw_tto_means = mlb_data |>
    group_by(TTO) |>
    summarise(mean_woba = mean(woba_19), .groups = "drop") |>
    mutate(
        start_x = c(1, 10, 19),
        end_x = c(9, 18, 27)
    )

adjusted_predictions = prediction_grid |>
    mutate(
        predicted_woba = predict(adjusted_tto_model, newdata = prediction_grid)
    )

adjusted_tto_means = adjusted_predictions |>
    group_by(TTO) |>
    summarise(mean_woba = mean(predicted_woba), .groups = "drop") |>
    mutate(
        start_x = c(1, 10, 19),
        end_x = c(9, 18, 27)
    )

indicator_predictions = prediction_grid |>
    mutate(
        predicted_woba = predict(indicator_model, newdata = prediction_grid)
    )

linear_predictions = prediction_grid |>
    mutate(
        predicted_woba = predict(linear_model, newdata = prediction_grid)
    )

spline_model = gam(
    predicted_woba ~ s(batter_sequence_number, k = 5),
    data = indicator_predictions,
    method = "REML"
)

spline_predictions = prediction_grid |>
    mutate(
        predicted_woba = predict(spline_model, newdata = prediction_grid)
    )

combined_predictions = prediction_grid |>
    mutate(
        predicted_woba = predict(combined_model, newdata = prediction_grid)
    )

#######################
### MAKE THE PLOTS ###
#######################

common_theme = theme_minimal(base_size = 12) +
    theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 16)
    )

observed_plot = ggplot(raw_sequence_means, aes(x = batter_sequence_number, y = mean_woba)) +
    geom_point(size = 2.7, color = "black") +
    labs(
        x = "batter sequence number",
        y = "observed mean wOBA"
    ) +
    common_theme

trajectory_plot = ggplot(raw_sequence_means, aes(x = batter_sequence_number, y = mean_woba)) +
    geom_point(size = 2.7, color = "black") +
    geom_segment(
        data = raw_tto_means,
        aes(x = start_x, xend = end_x, y = mean_woba, yend = mean_woba),
        inherit.aes = FALSE,
        linewidth = 0.7,
        color = "black"
    ) +
    labs(
        x = "batter sequence number",
        y = "observed mean wOBA"
    ) +
    common_theme

models_plot = ggplot(indicator_predictions, aes(x = batter_sequence_number, y = predicted_woba)) +
    geom_point(size = 2.4, color = "black") +
    geom_segment(
        data = adjusted_tto_means,
        aes(x = start_x, xend = end_x, y = mean_woba, yend = mean_woba),
        inherit.aes = FALSE,
        linewidth = 0.7,
        color = "grey40"
    ) +
    geom_line(
        data = linear_predictions,
        aes(y = predicted_woba),
        linewidth = 0.8,
        color = "blue"
    ) +
    geom_line(
        data = spline_predictions,
        aes(y = predicted_woba),
        linewidth = 0.9,
        color = "red"
    ) +
    labs(
        x = "batter sequence number",
        y = "predicted wOBA",
        title = "handedness match, batter at home,\naverage pitcher and batter"
    ) +
    common_theme

combined_plot = ggplot(combined_predictions, aes(x = batter_sequence_number, y = predicted_woba)) +
    geom_segment(
        data = adjusted_tto_means,
        aes(x = start_x, xend = end_x, y = mean_woba, yend = mean_woba),
        inherit.aes = FALSE,
        linewidth = 0.7,
        color = "grey40"
    ) +
    geom_line(linewidth = 0.9, color = "black") +
    geom_point(size = 1.8, color = "black") +
    labs(
        x = "batter sequence number",
        y = "predicted wOBA",
        title = "handedness match, batter at home,\naverage pitcher and batter"
    ) +
    common_theme

########################
### SAVE OUTPUTS ###
########################

ggsave(
    file.path(figure_dir, "05_ttop-observed.png"),
    observed_plot,
    width = 7,
    height = 4.8,
    dpi = 300
)

ggsave(
    file.path(figure_dir, "05_ttop-trajectory.png"),
    trajectory_plot,
    width = 7,
    height = 4.8,
    dpi = 300
)

ggsave(
    file.path(figure_dir, "05_ttop-models.png"),
    models_plot,
    width = 7,
    height = 4.8,
    dpi = 300
)

ggsave(
    file.path(figure_dir, "05_ttop-combined.png"),
    combined_plot,
    width = 7,
    height = 4.8,
    dpi = 300
)

########################
### REPORT SUMMARY ###
########################

print(
    mlb_data |>
        summarise(plate_appearances = n())
)

print(
    raw_tto_means |>
        select(TTO, mean_woba)
)

print(summary(adjusted_tto_model))
print(summary(combined_model))

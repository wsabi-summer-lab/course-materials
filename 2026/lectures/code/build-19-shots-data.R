###################
### Authors: JP ###
###################

########################
### INSTALL PACKAGES ###
########################

# install.packages("data.table")

library(data.table)

################
### SETTINGS ###
################

xgplus_root = path.expand(Sys.getenv("XGPLUS_ROOT", "~/wsabi/lab/projects/xg-plus"))
merged_dir = file.path(xgplus_root, "data", "merged_data")

lecture_data_path = "2026/lectures/data/19_shots.csv"
lab_data_path = "2026/labs/data/19_shots.csv"

season = "2024-2025"
competition = "pl"

###################
### LOAD SHOTS ####
###################

source_files = list.files(
    merged_dir,
    pattern = paste0("^train_", competition, "_", season, "_[0-9]+\\.csv$"),
    full.names = TRUE
)

if (length(source_files) == 0) {
    stop("No merged xg-plus files found in: ", merged_dir)
}

context_cols = c(
    "game",
    "date",
    "home_id",
    "home_name",
    "away_id",
    "away_name",
    "competition",
    "season",
    "pitch_id",
    "pitch_length",
    "pitch_width",
    "attack",
    "attack_merged",
    "period",
    "periodGameClockTime",
    "videoTimeMs",
    "attack_team_id",
    "player_name",
    "player_id",
    "frameNum"
)

feature_cols = c(
    "r",
    "theta",
    "z",
    "speed",
    "GK_r",
    "GK_theta",
    "openGoal",
    paste0("DefDist", 0:4),
    paste0("DefAngle", 0:4),
    paste0("OffDist", 0:4),
    paste0("OffAngle", 0:4)
)

target_cols = c("is_shot", "is_goal")
select_cols = c(feature_cols, target_cols, context_cols)

as_bool = function(x) {
    if (is.logical(x)) {
        return(x)
    }
    if (is.numeric(x)) {
        return(!is.na(x) & x != 0)
    }
    tolower(as.character(x)) %in% c("true", "t", "1", "yes", "y")
}

read_shot_rows = function(path) {
    dat = fread(path, select = select_cols, showProgress = FALSE)
    dat[as_bool(is_shot)]
}

shots = rbindlist(lapply(source_files, read_shot_rows), fill = TRUE)

if (nrow(shots) == 0) {
    stop("No shot rows found in merged data.")
}

###########################
### DERIVED CLASS FIELDS ##
###########################

shots[, goal := as.integer(as_bool(is_goal))]
shots[, is_shot := as.integer(as_bool(is_shot))]
shots[, is_goal := goal]

shots[, period := as.integer(as.numeric(period))]
shots[, periodGameClockTime := as.numeric(periodGameClockTime)]
shots[, match_minute := 45 * period + periodGameClockTime / 60]

shots[, shooting_team := fifelse(
    as.character(attack_team_id) == as.character(home_id),
    home_name,
    away_name
)]
shots[, opponent := fifelse(
    as.character(attack_team_id) == as.character(home_id),
    away_name,
    home_name
)]

# xg-plus stores attacking-frame polar coordinates relative to the goal being
# attacked. These fields put every shot onto a common attacking pitch with the
# goal at the top. shot_x runs across the pitch; shot_y runs toward the goal.
shots[, distance_to_goal := r]
shots[, signed_angle_to_goal := theta]
shots[, abs_angle_to_goal := abs(theta)]
shots[, depth_from_goal := r * cos(theta)]
shots[, lateral_from_center := r * sin(theta)]
shots[, shot_x := 34 + lateral_from_center]
shots[, shot_y := 105 - depth_from_goal]

# The upstream xg-plus extraction starts attacks at the final-third boundary,
# which creates a visible stack of shots at y ~= 70m. Raw bins show the pile-up
# through 70.25m, after which counts return to the local baseline.
shots = shots[shot_y > 70.25]

shots[, ball_height := pmax(z, 0)]
shots[, ball_speed := pmax(speed, 0)]
shots[, goalkeeper_distance := GK_r]
shots[, goalkeeper_signed_angle := GK_theta]
shots[, goalkeeper_abs_angle := abs(GK_theta)]
shots[, open_goal_share := openGoal]
shots[, nearest_defender_distance := DefDist0]
shots[, nearest_defender_angle := DefAngle0]
shots[, nearest_teammate_distance := OffDist0]
shots[, nearest_teammate_angle := OffAngle0]

setorder(shots, date, game, period, periodGameClockTime, frameNum)
shots[, shot_id := .I]

#####################
### GAME SPLITS #####
#####################

game_splits = unique(shots[, .(game, date)])
setorder(game_splits, date, game)
game_splits[, game_index := .I]
game_splits[, split := fcase(
    game_index <= floor(0.70 * .N), "train",
    game_index <= floor(0.85 * .N), "validation",
    default = "test"
)]

shots = merge(
    shots,
    game_splits[, .(game, split)],
    by = "game",
    all.x = TRUE,
    sort = FALSE
)
setorder(shots, shot_id)

##################
### WRITE DATA ###
##################

output_cols = c(
    "shot_id",
    "split",
    "competition",
    "season",
    "game",
    "date",
    "home_name",
    "away_name",
    "shooting_team",
    "opponent",
    "attack_team_id",
    "player_id",
    "player_name",
    "period",
    "match_minute",
    "periodGameClockTime",
    "frameNum",
    "pitch_length",
    "pitch_width",
    "shot_x",
    "shot_y",
    "distance_to_goal",
    "signed_angle_to_goal",
    "abs_angle_to_goal",
    "depth_from_goal",
    "lateral_from_center",
    "ball_height",
    "ball_speed",
    "goalkeeper_distance",
    "goalkeeper_signed_angle",
    "goalkeeper_abs_angle",
    "open_goal_share",
    "nearest_defender_distance",
    "nearest_defender_angle",
    "nearest_teammate_distance",
    "nearest_teammate_angle",
    paste0("DefDist", 0:4),
    paste0("DefAngle", 0:4),
    paste0("OffDist", 0:4),
    paste0("OffAngle", 0:4),
    "goal"
)

dir.create(dirname(lecture_data_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(lab_data_path), recursive = TRUE, showWarnings = FALSE)

out = shots[, ..output_cols]
fwrite(out, lecture_data_path)
fwrite(out, lab_data_path)

print(out[, .(shots = .N, goals = sum(goal)), by = split])
cat("Wrote", lecture_data_path, "and", lab_data_path, "\n")

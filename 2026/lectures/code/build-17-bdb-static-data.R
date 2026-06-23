#######################
### Authors: JP, RB ###
#######################

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("arrow", "dplyr", "readr", "tidyr"))

library(arrow)
library(dplyr)
library(readr)
library(tidyr)

################
### SETTINGS ###
################

bdb_dir = "/Users/Jonathan/wsabi/lab/projects/nfl-workload/data/bdb-2025"
output_dir = "2026/lectures/data"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

play_output_path = file.path(output_dir, "17_bdb-static-plays.csv")
snap_output_path = file.path(output_dir, "17_bdb-static-snap.csv")
feature_output_path = file.path(output_dir, "17_bdb-static-features.csv")

weeks = 1:9

################
### HELPERS ####
################

clock_seconds = function(x) {
    x_chr = as.character(x)
    parts = strsplit(x_chr, ":", fixed = TRUE)
    vapply(
        parts,
        function(p) {
            p = as.numeric(p)
            if (length(p) == 3) {
                60 * p[1] + p[2]
            } else if (length(p) == 2) {
                60 * p[1] + p[2]
            } else {
                NA_real_
            }
        },
        numeric(1)
    )
}

as_num = function(x) suppressWarnings(as.numeric(x))
as_int = function(x) suppressWarnings(as.integer(x))

unknown_if_missing = function(x) {
    if_else(is.na(x) | x == "NA", "UNKNOWN", x)
}

standardize_angle = function(angle, play_direction) {
    angle = as_num(angle)
    out = if_else(play_direction == "left", angle + 180, angle)
    if_else(out >= 360, out - 360, out)
}

nearest_distance = function(x, y, other_x, other_y) {
    if (length(other_x) == 0) {
        return(rep(NA_real_, length(x)))
    }
    vapply(
        seq_along(x),
        function(i) min(sqrt((x[i] - other_x)^2 + (y[i] - other_y)^2), na.rm = TRUE),
        numeric(1)
    )
}

################
### LOAD DATA ##
################

games = read_parquet(file.path(bdb_dir, "games.parquet")) |>
    select(
        gameId,
        season,
        week,
        gameDate,
        homeTeamAbbr,
        visitorTeamAbbr,
        homeFinalScore,
        visitorFinalScore
    )

plays = read_parquet(file.path(bdb_dir, "plays.parquet")) |>
    left_join(games, by = "gameId") |>
    mutate(
        gameClock = as.character(gameClock),
        gameClockSeconds = clock_seconds(gameClock),
        playClockAtSnap = as_num(playClockAtSnap),
        possessionScore = case_when(
            possessionTeam == homeTeamAbbr ~ preSnapHomeScore,
            possessionTeam == visitorTeamAbbr ~ preSnapVisitorScore,
            TRUE ~ NA_integer_
        ),
        defensiveScore = case_when(
            defensiveTeam == homeTeamAbbr ~ preSnapHomeScore,
            defensiveTeam == visitorTeamAbbr ~ preSnapVisitorScore,
            TRUE ~ NA_integer_
        ),
        scoreDifferential = possessionScore - defensiveScore,
        offenseFormation = unknown_if_missing(offenseFormation),
        receiverAlignment = unknown_if_missing(receiverAlignment)
    ) |>
    select(
        gameId,
        playId,
        season,
        week,
        gameDate,
        homeTeamAbbr,
        visitorTeamAbbr,
        possessionTeam,
        defensiveTeam,
        quarter,
        down,
        yardsToGo,
        yardlineSide,
        yardlineNumber,
        absoluteYardlineNumber,
        gameClock,
        gameClockSeconds,
        playClockAtSnap,
        preSnapHomeScore,
        preSnapVisitorScore,
        possessionScore,
        defensiveScore,
        scoreDifferential,
        expectedPoints,
        offenseFormation,
        receiverAlignment,
        isDropback,
        playAction,
        pff_runPassOption,
        passResult,
        rushLocationType,
        yardsGained,
        playDescription
    )

player_play = read_parquet(file.path(bdb_dir, "player_play.parquet")) |>
    select(
        gameId,
        playId,
        nflId,
        teamAbbr,
        inMotionAtBallSnap,
        shiftSinceLineset,
        motionSinceLineset
    )

players = read_parquet(file.path(bdb_dir, "players.parquet")) |>
    select(nflId, position, displayName, height, weight)

###########################
### SNAP-FRAME PLAY SET ###
###########################

static_play_keys = player_play |>
    inner_join(
        plays |> select(gameId, playId, possessionTeam),
        by = c("gameId", "playId")
    ) |>
    filter(teamAbbr == possessionTeam) |>
    mutate(
        inMotionAtBallSnap = coalesce(inMotionAtBallSnap == "TRUE", FALSE),
        shiftSinceLineset = coalesce(shiftSinceLineset == "TRUE", FALSE),
        motionSinceLineset = coalesce(motionSinceLineset == "TRUE", FALSE)
    ) |>
    group_by(gameId, playId) |>
    summarise(
        anyPreSnapMotion = any(inMotionAtBallSnap | shiftSinceLineset | motionSinceLineset),
        nOffenseRows = n(),
        .groups = "drop"
    )

static_plays = plays |>
    inner_join(static_play_keys, by = c("gameId", "playId")) |>
    mutate(
        isDropback = as.integer(isDropback),
        split = case_when(
            week <= 6 ~ "train",
            week == 7 ~ "validation",
            TRUE ~ "test"
        )
    ) |>
    arrange(week, gameId, playId)

########################
### SNAP TRACKING DATA #
########################

snap_tracking = lapply(
    weeks,
    function(w) {
        tracking_path = file.path(bdb_dir, paste0("tracking_week_", w, ".parquet"))
        open_dataset(tracking_path) |>
            filter(frameType == "SNAP") |>
            select(
                gameId,
                playId,
                nflId,
                displayName,
                frameId,
                frameType,
                jerseyNumber,
                club,
                playDirection,
                x,
                y,
                s,
                a,
                dis,
                o,
                dir,
                event
            ) |>
            collect()
    }
) |>
    bind_rows() |>
    inner_join(
        static_plays |>
            select(
                gameId,
                playId,
                season,
                week,
                possessionTeam,
                defensiveTeam,
                absoluteYardlineNumber,
                yardsToGo,
                isDropback,
                split
            ),
        by = c("gameId", "playId")
    ) |>
    mutate(
        nflIdInt = as_int(nflId),
        jerseyNumber = as_int(jerseyNumber),
        orientation = standardize_angle(o, playDirection),
        direction = standardize_angle(dir, playDirection),
        xStd = if_else(playDirection == "left", 120 - x, x),
        yStd = if_else(playDirection == "left", 160 / 3 - y, y),
        lineOfScrimmage = if_else(
            playDirection == "left",
            120 - absoluteYardlineNumber,
            as.numeric(absoluteYardlineNumber)
        ),
        firstDownX = pmin(lineOfScrimmage + yardsToGo, 110),
        xFromLos = xStd - lineOfScrimmage,
        side = case_when(
            club == "football" ~ "football",
            club == possessionTeam ~ "offense",
            club == defensiveTeam ~ "defense",
            TRUE ~ "other"
        )
    ) |>
    left_join(
        players |>
            rename(
                nflIdInt = nflId,
                playerName = displayName
            ),
        by = "nflIdInt"
    ) |>
    mutate(
        displayName = coalesce(playerName, displayName),
        position = if_else(side == "football", "BALL", position)
    ) |>
    select(
        gameId,
        playId,
        season,
        week,
        split,
        isDropback,
        nflId = nflIdInt,
        displayName,
        position,
        jerseyNumber,
        club,
        side,
        frameId,
        frameType,
        playDirection,
        x,
        y,
        xStd,
        yStd,
        xFromLos,
        lineOfScrimmage,
        firstDownX,
        s,
        a,
        dis,
        orientation,
        direction,
        event
    ) |>
    arrange(week, gameId, playId, side, nflId)

########################
### MODELING FEATURES ##
########################

feature_by_play = snap_tracking |>
    filter(side %in% c("offense", "defense")) |>
    group_by(gameId, playId) |>
    summarise(
        offenseWidth = diff(range(yStd[side == "offense"], na.rm = TRUE)),
        defenseWidth = diff(range(yStd[side == "defense"], na.rm = TRUE)),
        offenseDepth = diff(range(xFromLos[side == "offense"], na.rm = TRUE)),
        defenseDepth = diff(range(xFromLos[side == "defense"], na.rm = TRUE)),
        meanOffenseBackfieldDepth = mean(-xFromLos[side == "offense"], na.rm = TRUE),
        meanDefenderDepth = mean(xFromLos[side == "defense"], na.rm = TRUE),
        meanOffenseSpeed = mean(s[side == "offense"], na.rm = TRUE),
        meanDefenseSpeed = mean(s[side == "defense"], na.rm = TRUE),
        boxDefenders = sum(
            side == "defense" &
                xFromLos >= -1 &
                xFromLos <= 5 &
                yStd >= (160 / 6 - 5) &
                yStd <= (160 / 6 + 5),
            na.rm = TRUE
        ),
        .groups = "drop"
    )

nearest_defenders = snap_tracking |>
    filter(side %in% c("offense", "defense")) |>
    group_by(gameId, playId) |>
    summarise(
        meanNearestDefender = mean(
            nearest_distance(
                xStd[side == "offense"],
                yStd[side == "offense"],
                xStd[side == "defense"],
                yStd[side == "defense"]
            ),
            na.rm = TRUE
        ),
        minNearestDefender = min(
            nearest_distance(
                xStd[side == "offense"],
                yStd[side == "offense"],
                xStd[side == "defense"],
                yStd[side == "defense"]
            ),
            na.rm = TRUE
        ),
        .groups = "drop"
    )

static_features = static_plays |>
    mutate(
        yardsToEndzone = 110 - absoluteYardlineNumber
    ) |>
    left_join(feature_by_play, by = c("gameId", "playId")) |>
    left_join(nearest_defenders, by = c("gameId", "playId")) |>
    select(
        gameId,
        playId,
        season,
        week,
        split,
        isDropback,
        possessionTeam,
        defensiveTeam,
        quarter,
        down,
        yardsToGo,
        yardsToEndzone,
        gameClockSeconds,
        playClockAtSnap,
        scoreDifferential,
        expectedPoints,
        offenseFormation,
        receiverAlignment,
        offenseWidth,
        defenseWidth,
        offenseDepth,
        defenseDepth,
        meanOffenseBackfieldDepth,
        meanDefenderDepth,
        meanOffenseSpeed,
        meanDefenseSpeed,
        boxDefenders,
        meanNearestDefender,
        minNearestDefender
    ) |>
    arrange(week, gameId, playId)

################
### WRITE DATA #
################

write_csv(static_plays, play_output_path)
write_csv(snap_tracking, snap_output_path)
write_csv(static_features, feature_output_path)

cat("Wrote", nrow(static_plays), "snap-frame plays to:\n")
cat("  ", play_output_path, "\n", sep = "")
cat("Wrote", nrow(snap_tracking), "snap tracking rows to:\n")
cat("  ", snap_output_path, "\n", sep = "")
cat("Wrote", nrow(static_features), "modeling feature rows to:\n")
cat("  ", feature_output_path, "\n", sep = "")
cat("\nIncluded plays by pre-snap motion flag:\n")
print(static_plays |> count(anyPreSnapMotion))
cat("\nDropback counts by split:\n")
print(static_features |> count(split, isDropback))

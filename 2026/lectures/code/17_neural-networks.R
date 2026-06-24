#######################
### Authors: JP, RB ###
#######################

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "gridExtra", "nnet", "readr", "scales", "tidyr", "xgboost"))

library(dplyr)
library(ggplot2)
library(gridExtra)
library(nnet)
library(readr)
library(scales)
library(tidyr)
library(xgboost)

################
### SETTINGS ###
################

set.seed(17)

feature_path = "2026/lectures/data/17_bdb-static-features.csv"
snap_path = "2026/lectures/data/17_bdb-static-snap.csv"
figure_dir = "2026/lectures/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

theme_week = theme_minimal(base_size = 12) +
    theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot"
    )

activation_grid = tibble(z = seq(-5, 5, length.out = 601)) |>
    mutate(
        `ReLU\nphi(z) = max(0, z)` = pmax(0, z),
        `Leaky ReLU\nphi(z) = max(0.1z, z)` = if_else(z >= 0, z, 0.1 * z),
        `tanh\nphi(z) = tanh(z)` = tanh(z),
        `Sigmoid output\ng(z) = 1 / (1 + exp(-z))` = 1 / (1 + exp(-z))
    ) |>
    pivot_longer(
        cols = -z,
        names_to = "activation",
        values_to = "value"
    ) |>
    mutate(
        activation = factor(
            activation,
            levels = c(
                "ReLU\nphi(z) = max(0, z)",
                "Leaky ReLU\nphi(z) = max(0.1z, z)",
                "tanh\nphi(z) = tanh(z)",
                "Sigmoid output\ng(z) = 1 / (1 + exp(-z))"
            )
        )
    )

activation_plot = ggplot(activation_grid, aes(z, value, color = activation)) +
    geom_hline(yintercept = 0, color = "gray78", linewidth = 0.35) +
    geom_vline(xintercept = 0, color = "gray78", linewidth = 0.35) +
    geom_line(linewidth = 1.05, show.legend = FALSE) +
    facet_wrap(vars(activation), scales = "free_y", ncol = 2) +
    scale_color_manual(values = c("#277DA1", "#D95D39", "#4D908E", "#6A4C93")) +
    labs(
        title = "Common Nonlinear Transformations in Neural Networks",
        subtitle = "Hidden layers use phi; binary output uses g",
        x = "Input z",
        y = "Transformed value"
    ) +
    theme_week +
    theme(
        legend.position = "none",
        strip.text = element_text(face = "bold", lineheight = 0.95),
        panel.grid.minor = element_blank()
    )

ggsave(
    file.path(figure_dir, "17_activation-functions.png"),
    activation_plot,
    width = 7.6,
    height = 4.9,
    dpi = 300,
    bg = "white"
)

log_loss = function(y, p) {
    p = pmin(pmax(p, 1e-6), 1 - 1e-6)
    -mean(y * log(p) + (1 - y) * log(1 - p))
}

accuracy = function(y, p) mean(as.integer(p >= 0.5) == y)

nfl_field_layers = function(yardmin = 0, yardmax = 120) {
    field_width = 160 / 3
    yard_lines = tibble(x = seq(0, 120, by = 5)) |>
        filter(x >= yardmin, x <= yardmax)
    major_lines = yard_lines |> filter(x %% 10 == 0)
    minor_lines = yard_lines |> filter(x %% 10 != 0)
    hash_marks = expand_grid(
        x = seq(10, 110, by = 1),
        y = c(70.75 / 3, (160 - 70.75) / 3)
    ) |>
        filter(x >= yardmin, x <= yardmax)
    yard_numbers = tibble(
        x = seq(20, 100, by = 10),
        label = c("10", "20", "30", "40", "50", "40", "30", "20", "10")
    ) |>
        filter(x >= yardmin, x <= yardmax)
    end_zones = tibble(xmin = c(0, 110), xmax = c(10, 120)) |>
        mutate(
            xmin = pmax(xmin, yardmin),
            xmax = pmin(xmax, yardmax)
        ) |>
        filter(xmin < xmax)

    list(
        annotate(
            "rect",
            xmin = yardmin,
            xmax = yardmax,
            ymin = 0,
            ymax = field_width,
            fill = "forestgreen",
            color = "white",
            linewidth = 0.45
        ),
        geom_rect(
            data = end_zones,
            aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = field_width),
            inherit.aes = FALSE,
            fill = "#145A32",
            alpha = 0.65,
            color = NA
        ),
        geom_segment(
            data = minor_lines,
            aes(x = x, xend = x, y = 0, yend = field_width),
            inherit.aes = FALSE,
            color = "white",
            alpha = 0.28,
            linewidth = 0.28
        ),
        geom_segment(
            data = major_lines,
            aes(x = x, xend = x, y = 0, yend = field_width),
            inherit.aes = FALSE,
            color = "white",
            alpha = 0.62,
            linewidth = 0.55
        ),
        geom_segment(
            data = hash_marks,
            aes(x = x, xend = x, y = y - 0.42, yend = y + 0.42),
            inherit.aes = FALSE,
            color = "white",
            alpha = 0.62,
            linewidth = 0.36
        ),
        geom_text(
            data = yard_numbers,
            aes(x = x, y = 5.5, label = label),
            inherit.aes = FALSE,
            color = "white",
            alpha = 0.55,
            size = 3.4,
            fontface = "bold"
        ),
        geom_text(
            data = yard_numbers,
            aes(x = x, y = field_width - 5.5, label = label),
            inherit.aes = FALSE,
            color = "white",
            alpha = 0.55,
            size = 3.4,
            fontface = "bold"
        )
    )
}

theme_field = theme_void(base_size = 12) +
    theme(
        legend.position = "top",
        legend.justification = "left",
        plot.title.position = "plot",
        panel.background = element_rect(fill = "forestgreen", color = "forestgreen"),
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        legend.key.size = grid::unit(0.32, "inches"),
        plot.title = element_text(face = "bold", color = "gray12"),
        plot.subtitle = element_text(color = "gray28", margin = margin(b = 6)),
        plot.margin = margin(8, 10, 8, 10)
    )

################
### LOAD DATA ##
################

features = read_csv(feature_path, show_col_types = FALSE) |>
    mutate(
        isDropback = as.integer(isDropback),
        split = factor(split, levels = c("train", "validation", "test")),
        down = factor(down),
        offenseFormation = factor(offenseFormation),
        receiverAlignment = factor(receiverAlignment)
    )

snap = read_csv(snap_path, show_col_types = FALSE) |>
    mutate(
        isDropback = as.integer(isDropback),
        side = factor(side, levels = c("offense", "defense", "football"))
    )

train = features |> filter(split == "train")
valid = features |> filter(split == "validation")
test = features |> filter(split == "test")

#############################
### SNAPSHOT FIELD FIGURE ###
#############################

example_key = features |>
    filter(
        split == "test",
        offenseFormation == "SHOTGUN",
        receiverAlignment == "2x2",
        isDropback == 1
    ) |>
    slice(1) |>
    select(gameId, playId)

example_play = features |>
    semi_join(example_key, by = c("gameId", "playId")) |>
    slice(1)

example_snap = snap |>
    semi_join(example_key, by = c("gameId", "playId")) |>
    mutate(
        sideLabel = recode(
            as.character(side),
            offense = "Offense",
            defense = "Defense",
            football = "Football"
        )
    )

line_data = example_snap |>
    distinct(lineOfScrimmage, firstDownX)

field_window = c(0, 120)

movement_segments = example_snap |>
    filter(side %in% c("offense", "defense"), !is.na(direction), !is.na(s), s > 0.25) |>
    mutate(
        xEnd = xStd + pmin(s, 3) * 0.55 * cos((90 - direction) * pi / 180),
        yEnd = yStd + pmin(s, 3) * 0.55 * sin((90 - direction) * pi / 180)
    )

player_labels = example_snap |>
    filter(sideLabel %in% c("Offense", "Defense"), !is.na(jerseyNumber)) |>
    mutate(jerseyLabel = as.character(jerseyNumber))

static_snap_plot = ggplot(example_snap, aes(xStd, yStd)) +
    nfl_field_layers(field_window[1], field_window[2]) +
    geom_segment(
        data = movement_segments,
        aes(
            x = xStd,
            y = yStd,
            xend = xEnd,
            yend = yEnd
        ),
        inherit.aes = FALSE,
        color = "white",
        linewidth = 0.45,
        alpha = 0.65,
        arrow = grid::arrow(length = grid::unit(0.06, "inches"), type = "closed"),
        na.rm = TRUE
    ) +
    geom_segment(
        data = line_data,
        aes(x = lineOfScrimmage, xend = lineOfScrimmage, y = 0, yend = 160 / 3),
        inherit.aes = FALSE,
        color = "blue",
        linewidth = 1.15
    ) +
    geom_segment(
        data = line_data,
        aes(x = firstDownX, xend = firstDownX, y = 0, yend = 160 / 3),
        inherit.aes = FALSE,
        color = "yellow",
        linewidth = 1.05
    ) +
    geom_point(
        aes(fill = sideLabel, color = sideLabel, size = sideLabel, shape = sideLabel),
        alpha = 0.82,
        stroke = 0.75
    ) +
    geom_text(
        data = player_labels,
        aes(label = jerseyLabel),
        color = "white",
        vjust = 0.36,
        size = 2.7,
        fontface = "bold"
    ) +
    scale_fill_manual(
        values = c(Offense = "dodgerblue", Defense = "red", Football = "brown"),
        breaks = c("Offense", "Defense", "Football")
    ) +
    scale_color_manual(
        values = c(Offense = "dodgerblue", Defense = "red", Football = "brown"),
        breaks = c("Offense", "Defense", "Football")
    ) +
    scale_size_manual(
        values = c(Offense = 4.5, Defense = 4.5, Football = 3.1),
        breaks = c("Offense", "Defense", "Football")
    ) +
    scale_shape_manual(
        values = c(Offense = 21, Defense = 21, Football = 16),
        breaks = c("Offense", "Defense", "Football")
    ) +
    coord_fixed(xlim = field_window, ylim = c(0, 160 / 3), expand = FALSE) +
    labs(
        title = "Alignment Frozen at the Snap",
        subtitle = paste0(
            example_play$possessionTeam,
            " vs. ",
            example_play$defensiveTeam,
            ", ",
            example_play$offenseFormation,
            " ",
            example_play$receiverAlignment,
            ", ",
            if_else(example_play$isDropback == 1, "dropback", "run")
        ),
        x = NULL,
        y = NULL,
        fill = NULL
    ) +
    guides(
        fill = guide_legend(
            override.aes = list(
                shape = c(21, 21, 16),
                fill = c("dodgerblue", "red", "brown"),
                color = c("dodgerblue", "red", "brown"),
                size = c(4.5, 4.5, 3.1),
                alpha = 1
            )
        ),
        size = "none",
        shape = "none",
        color = "none"
    ) +
    theme_field

ggsave(
    file.path(figure_dir, "17_static-snap-example.png"),
    static_snap_plot,
    width = 10,
    height = 4,
    dpi = 300,
    bg = "white"
)

#####################################
### FORMATION DROPBACK RATE FIGURE ##
#####################################

formation_rates = features |>
    count(offenseFormation, receiverAlignment, isDropback) |>
    group_by(offenseFormation, receiverAlignment) |>
    mutate(
        n_total = sum(n),
        dropback_rate = sum(n * isDropback) / n_total
    ) |>
    ungroup() |>
    distinct(offenseFormation, receiverAlignment, n_total, dropback_rate) |>
    filter(n_total >= 25)

formation_order = features |>
    count(offenseFormation, sort = TRUE) |>
    pull(offenseFormation)

alignment_order = features |>
    count(receiverAlignment, sort = TRUE) |>
    pull(receiverAlignment)

formation_rates = formation_rates |>
    mutate(
        offenseFormation = factor(offenseFormation, levels = rev(formation_order)),
        receiverAlignment = factor(receiverAlignment, levels = alignment_order)
    )

formation_plot = ggplot(
    formation_rates,
    aes(receiverAlignment, offenseFormation, fill = dropback_rate)
) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(
        aes(label = paste0(percent(dropback_rate, accuracy = 1), "\n", "n=", n_total)),
        size = 3.2,
        lineheight = 0.9,
        color = "gray15"
    ) +
    scale_fill_gradient2(
        low = "#4D908E",
        mid = "#F3E8C9",
        high = "#D95D39",
        midpoint = mean(features$isDropback),
        labels = percent_format(accuracy = 1)
    ) +
    labs(
        title = "Dropback Rate From Snap-Frame Offensive Sets",
        subtitle = "BDB 2025 Weeks 1-9, all plays frozen at the snap",
        x = "Receiver alignment",
        y = "Offensive formation",
        fill = "Dropback rate"
    ) +
    theme_week +
    theme(panel.grid = element_blank())

ggsave(
    file.path(figure_dir, "17_static-formation-dropback-rates.png"),
    formation_plot,
    width = 8.4,
    height = 5.2,
    dpi = 300,
    bg = "white"
)

#############################
### FIELD TENSOR EXAMPLE ####
#############################

cell_size = 2

tensor_cells = example_snap |>
    filter(side %in% c("offense", "defense")) |>
    mutate(
        channel = if_else(side == "offense", "Offense location channel", "Defense location channel"),
        xCell = floor(xFromLos / cell_size) * cell_size + cell_size / 2,
        yCell = floor(yStd / cell_size) * cell_size + cell_size / 2
    ) |>
    count(channel, xCell, yCell, name = "players")

grid_cells = expand_grid(
    channel = c("Offense location channel", "Defense location channel"),
    xCell = seq(-16 + cell_size / 2, 22 - cell_size / 2, by = cell_size),
    yCell = seq(cell_size / 2, 160 / 3 - cell_size / 2, by = cell_size)
) |>
    left_join(tensor_cells, by = c("channel", "xCell", "yCell")) |>
    mutate(
        channel = factor(
            channel,
            levels = c("Offense location channel", "Defense location channel")
        ),
        players = replace_na(players, 0L)
    )

tensor_plot = ggplot(grid_cells, aes(xCell, yCell, fill = players)) +
    geom_tile(color = "#D9DED8", linewidth = 0.2) +
    geom_vline(xintercept = 0, color = "#2457A6", linewidth = 0.9) +
    geom_vline(
        xintercept = example_play$yardsToGo,
        color = "#D29A13",
        linewidth = 0.8,
        linetype = "22"
    ) +
    facet_wrap(~ channel, ncol = 2) +
    coord_fixed(xlim = c(-16, 22), ylim = c(0, 160 / 3), expand = FALSE) +
    scale_fill_gradient(
        low = "white",
        high = "#276FBF",
        breaks = c(0, 1),
        limits = c(0, max(1, max(grid_cells$players)))
    ) +
    labs(
        title = "Rasterizing a Football Play for a CNN",
        subtitle = "Each channel is a coarse grid over the same snap frame",
        x = "Yards from line of scrimmage",
        y = "Field width",
        fill = "Players"
    ) +
    theme_week +
    theme(
        legend.position = "right",
        panel.grid = element_blank()
    )

ggsave(
    file.path(figure_dir, "17_field-tensor-example.png"),
    tensor_plot,
    width = 9.2,
    height = 4.8,
    dpi = 300,
    bg = "white"
)

########################
### MODEL COMPARISON ###
########################

model_vars = c(
    "down",
    "yardsToGo",
    "yardsToEndzone",
    "gameClockSeconds",
    "playClockAtSnap",
    "scoreDifferential",
    "expectedPoints",
    "offenseFormation",
    "receiverAlignment",
    "offenseWidth",
    "defenseWidth",
    "offenseDepth",
    "defenseDepth",
    "meanOffenseBackfieldDepth",
    "meanDefenderDepth",
    "meanOffenseSpeed",
    "meanDefenseSpeed",
    "boxDefenders",
    "meanNearestDefender",
    "minNearestDefender"
)

model_data = features |>
    select(split, isDropback, all_of(model_vars)) |>
    drop_na()

train_m = model_data |> filter(split == "train")
valid_m = model_data |> filter(split == "validation")
test_m = model_data |> filter(split == "test")

design_formula = as.formula(
    paste("~", paste(model_vars, collapse = " + "))
)

design_all = model.matrix(design_formula, model_data)[, -1, drop = FALSE]
num_cols = vapply(as.data.frame(design_all), is.numeric, logical(1))
train_idx = model_data$split == "train"

center = colMeans(design_all[train_idx, num_cols, drop = FALSE])
scale_vals = apply(design_all[train_idx, num_cols, drop = FALSE], 2, sd)
scale_vals[scale_vals == 0] = 1

design_scaled = design_all
design_scaled[, num_cols] = sweep(design_scaled[, num_cols, drop = FALSE], 2, center, "-")
design_scaled[, num_cols] = sweep(design_scaled[, num_cols, drop = FALSE], 2, scale_vals, "/")

x_train = design_scaled[model_data$split == "train", , drop = FALSE]
x_valid = design_scaled[model_data$split == "validation", , drop = FALSE]
x_test = design_scaled[model_data$split == "test", , drop = FALSE]
y_train = train_m$isDropback
y_valid = valid_m$isDropback
y_test = test_m$isDropback

mean_pred = mean(y_train)
mean_results = tibble(
    model = "Training mean",
    test_log_loss = log_loss(y_test, rep(mean_pred, length(y_test))),
    test_accuracy = accuracy(y_test, rep(mean_pred, length(y_test)))
)

formation_rates_train = train_m |>
    group_by(offenseFormation, receiverAlignment) |>
    summarise(rate = mean(isDropback), n = n(), .groups = "drop")

formation_pred = test_m |>
    left_join(formation_rates_train, by = c("offenseFormation", "receiverAlignment")) |>
    mutate(rate = coalesce(rate, mean_pred)) |>
    pull(rate)

formation_results = tibble(
    model = "Formation rate",
    test_log_loss = log_loss(y_test, formation_pred),
    test_accuracy = accuracy(y_test, formation_pred)
)

glm_train = as.data.frame(x_train, check.names = TRUE)
glm_test = as.data.frame(x_test, check.names = TRUE)
glm_fit = glm(
    y_train ~ .,
    data = cbind(y_train = y_train, glm_train),
    family = binomial()
)
glm_pred = predict(glm_fit, glm_test, type = "response")

glm_results = tibble(
    model = "Logistic regression",
    test_log_loss = log_loss(y_test, glm_pred),
    test_accuracy = accuracy(y_test, glm_pred)
)

nnet_grid = expand_grid(
    size = c(2, 4, 6, 8),
    decay = c(0.0001, 0.001, 0.01, 0.1, 1)
) |>
    mutate(
        fit = lapply(
            seq_len(n()),
            function(i) {
                nnet(
                    x = x_train,
                    y = y_train,
                    size = size[i],
                    decay = decay[i],
                    entropy = TRUE,
                    maxit = 400,
                    trace = FALSE,
                    MaxNWts = 10000
                )
            }
        ),
        valid_pred = lapply(fit, predict, newdata = x_valid, type = "raw"),
        valid_log_loss = vapply(valid_pred, \(p) log_loss(y_valid, as.numeric(p)), numeric(1))
    )

best_nnet = nnet_grid$fit[[which.min(nnet_grid$valid_log_loss)]]
nnet_pred = as.numeric(predict(best_nnet, newdata = x_test, type = "raw"))

nnet_results = tibble(
    model = "Feedforward NN",
    test_log_loss = log_loss(y_test, nnet_pred),
    test_accuracy = accuracy(y_test, nnet_pred)
)

xgb_train = xgb.DMatrix(x_train, label = y_train, nthread = 2)
xgb_valid = xgb.DMatrix(x_valid, label = y_valid, nthread = 2)
xgb_test = xgb.DMatrix(x_test, label = y_test, nthread = 2)

xgb_grid = expand_grid(
    max_depth = c(2, 4),
    eta = c(0.03, 0.1),
    min_child_weight = c(1, 5)
) |>
    mutate(
        fit = lapply(
            seq_len(n()),
            function(i) {
                xgb.train(
                    params = xgb.params(
                        objective = "binary:logistic",
                        eval_metric = "logloss",
                        max_depth = max_depth[i],
                        eta = eta[i],
                        min_child_weight = min_child_weight[i],
                        subsample = 0.9,
                        colsample_bytree = 0.9,
                        nthread = 2,
                        seed = 17
                    ),
                    data = xgb_train,
                    nrounds = 300,
                    evals = list(validation = xgb_valid),
                    early_stopping_rounds = 20,
                    verbose = 0
                )
            }
        ),
        valid_pred = lapply(fit, predict, newdata = xgb_valid),
        valid_log_loss = vapply(valid_pred, \(p) log_loss(y_valid, as.numeric(p)), numeric(1)),
        best_iteration = vapply(
            fit,
            function(m) {
                early_stop = attr(m, "early_stop")
                if (is.null(early_stop) || length(early_stop$best_iteration) == 0) {
                    return(NA_real_)
                }
                early_stop$best_iteration
            },
            numeric(1)
        )
    )

best_xgb = xgb_grid$fit[[which.min(xgb_grid$valid_log_loss)]]
xgb_pred = as.numeric(predict(best_xgb, xgb_test))

xgb_results = tibble(
    model = "XGBoost",
    test_log_loss = log_loss(y_test, xgb_pred),
    test_accuracy = accuracy(y_test, xgb_pred)
)

model_results = bind_rows(
    mean_results,
    formation_results,
    glm_results,
    nnet_results,
    xgb_results
) |>
    mutate(
        model = factor(model, levels = model[order(test_log_loss)])
    )

model_plot = ggplot(model_results, aes(model, test_log_loss, fill = model)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    geom_text(
        aes(label = number(test_log_loss, accuracy = 0.001)),
        vjust = -0.45,
        size = 3.6,
        color = "gray20"
    ) +
    scale_fill_manual(values = c("#4D908E", "#277DA1", "#F9C74F", "#D95D39", "#6A4C93")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
        title = "Predicting Dropback From Snap-Frame Information",
        subtitle = "Train weeks 1-6, validate week 7, test weeks 8-9",
        x = NULL,
        y = "Test log loss"
    ) +
    theme_week +
    theme(
        axis.text.x = element_text(angle = 18, hjust = 1),
        panel.grid.major.x = element_blank()
    )

ggsave(
    file.path(figure_dir, "17_model-comparison.png"),
    model_plot,
    width = 7.6,
    height = 4.8,
    dpi = 300,
    bg = "white"
)

validation_plot = nnet_grid |>
    mutate(
        size = factor(size)
    ) |>
    ggplot(aes(decay, valid_log_loss, color = size, group = size)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    scale_x_log10(
        breaks = c(0.0001, 0.001, 0.01, 0.1, 1),
        labels = c("0.0001", "0.001", "0.01", "0.1", "1")
    ) +
    labs(
        title = "Validation Chooses the Neural Network Complexity",
        subtitle = "One-hidden-layer feedforward network on snap-frame features",
        x = "Weight decay",
        y = "Validation log loss",
        color = "Hidden units"
    ) +
    theme_week

ggsave(
    file.path(figure_dir, "17_fnn-validation.png"),
    validation_plot,
    width = 7.4,
    height = 4.8,
    dpi = 300,
    bg = "white"
)

print(model_results)
print(nnet_grid |> select(size, decay, valid_log_loss) |> arrange(valid_log_loss))
print(xgb_grid |> select(max_depth, eta, min_child_weight, best_iteration, valid_log_loss) |> arrange(valid_log_loss))

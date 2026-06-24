###################
### Authors: JP ###
###################

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("cluster", "dplyr", "ggplot2", "readr", "tidyr"))

library(cluster)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

################
### SETTINGS ###
################

set.seed(18)

data_path = "2026/lectures/data/18_unsupervised-learning-nba-clusters.csv"
figure_dir = "2026/lectures/figures"
table_dir = "2026/lectures/tables"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)

feature_labels = c(
    FGA_freq_Spotup_OG = "Spot-up",
    FGA_freq_PRRollMan_OG = "Roll man",
    FGA_freq_Postup_OG = "Post-up",
    FGA_freq_Cut_OG = "Cut",
    FGA_freq_OffRebound_OG = "Off. rebound",
    FGA_freq_PRBallHandler_OG = "P&R handler",
    FGA_freq_Isolation_OG = "Isolation",
    FGA_freq_OffScreenOrHandoff_OG = "Off-screen/HO"
)

feature_cols = names(feature_labels)
feature_order = unname(feature_labels)

cluster_palette = c(
    "Ball-handlers" = "#277DA1",
    "Movement shooters" = "#D95D39",
    "Post bigs" = "#6A4C93",
    "Rim runners" = "#43AA8B",
    "Spot-up wings" = "#F9A03F"
)

theme_week = theme_minimal(base_size = 12) +
    theme(
        legend.position = "top",
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA)
    )

################
### LOAD DATA ##
################

nba = read_csv(data_path, show_col_types = FALSE) |>
    mutate(across(all_of(feature_cols), ~ replace_na(.x, 0)))

X = nba |>
    select(all_of(feature_cols))

X_scaled = scale(X)

############################
### PCA INTUITION FIGURE ###
############################

set.seed(1818)
rotation_angle = pi / 6
rotation_matrix = matrix(
    c(
        cos(rotation_angle), -sin(rotation_angle),
        sin(rotation_angle), cos(rotation_angle)
    ),
    nrow = 2,
    byrow = TRUE
)

oval_points = matrix(rnorm(520), ncol = 2) |>
    (\(z) z %*% diag(c(2.2, 0.65)) %*% rotation_matrix)()

colnames(oval_points) = c("feature_1", "feature_2")
oval_data = as_tibble(oval_points)
oval_pca = prcomp(oval_points, center = TRUE, scale. = FALSE)
oval_lengths = 1.85 * oval_pca$sdev

pc_axis_data = tibble(
    component = c("PC1", "PC2"),
    x = -oval_pca$rotation[1, ] * oval_lengths,
    y = -oval_pca$rotation[2, ] * oval_lengths,
    xend = oval_pca$rotation[1, ] * oval_lengths,
    yend = oval_pca$rotation[2, ] * oval_lengths
)

pc_arrow_data = tibble(
    component = c("PC1", "PC2"),
    x = 0,
    y = 0,
    xend = oval_pca$rotation[1, ] * oval_lengths,
    yend = oval_pca$rotation[2, ] * oval_lengths
)

pc_label_data = pc_arrow_data |>
    mutate(
        label = c("PC1: most variation", "PC2: next-most variation"),
        label_x = xend * 1.13,
        label_y = yend * 1.13
    )

pca_oval_plot = ggplot(oval_data, aes(feature_1, feature_2)) +
    geom_point(color = "gray55", alpha = 0.45, size = 1.15) +
    geom_segment(
        data = pc_axis_data,
        aes(x = x, y = y, xend = xend, yend = yend, color = component),
        linewidth = 1.0,
        lineend = "round",
        show.legend = FALSE
    ) +
    geom_segment(
        data = pc_arrow_data,
        aes(x = x, y = y, xend = xend, yend = yend, color = component),
        linewidth = 1.15,
        arrow = grid::arrow(length = grid::unit(0.16, "inches")),
        show.legend = FALSE
    ) +
    geom_label(
        data = pc_label_data,
        aes(x = label_x, y = label_y, label = label, color = component),
        fill = "white",
        linewidth = 0.18,
        size = 3.4,
        show.legend = FALSE
    ) +
    scale_color_manual(values = c(PC1 = "#277DA1", PC2 = "#D95D39")) +
    coord_equal() +
    labs(
        title = "PCA Finds Principal Component Directions",
        subtitle = "PC1 follows the largest spread.\nPC2 is perpendicular and explains the next-largest spread.",
        x = "Feature 1",
        y = "Feature 2"
    ) +
    theme_week +
    theme(
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 11, lineheight = 1.1)
    )

ggsave(
    file.path(figure_dir, "18_unsupervised-learning-pca-oval.png"),
    pca_oval_plot,
    width = 7.0,
    height = 4.5,
    dpi = 300,
    bg = "white"
)

##############################
### PRINCIPAL COMPONENTS #####
##############################

pca_fit = prcomp(X, center = TRUE, scale. = TRUE)
pca_var = pca_fit$sdev^2 / sum(pca_fit$sdev^2)

pca_scores = bind_cols(
    nba |> select(row_idx, player_szn, szn, PLAYER_ID, PLAYER_NAME),
    as_tibble(pca_fit$x[, 1:4])
)

pca_loadings = as_tibble(pca_fit$rotation[, 1:2], rownames = "feature") |>
    mutate(feature_label = recode(feature, !!!feature_labels)) |>
    pivot_longer(
        cols = c(PC1, PC2),
        names_to = "component",
        values_to = "loading"
    ) |>
    mutate(
        component = recode(
            component,
            PC1 = paste0("PC1: ", round(100 * pca_var[1]), "%"),
            PC2 = paste0("PC2: ", round(100 * pca_var[2]), "%")
        ),
        feature_label = factor(feature_label, levels = rev(feature_order))
    )

pca_loading_plot = ggplot(pca_loadings, aes(loading, feature_label, fill = component)) +
    geom_vline(xintercept = 0, color = "gray70", linewidth = 0.35) +
    geom_col(width = 0.72, show.legend = FALSE) +
    facet_wrap(vars(component), ncol = 1) +
    scale_fill_manual(values = c("#277DA1", "#D95D39")) +
    labs(
        title = "What Do the First Two Principal Components Measure?",
        subtitle = "Loadings from standardized NBA play-type proportions",
        x = "Loading",
        y = NULL
    ) +
    theme_week +
    theme(strip.text = element_text(face = "bold", hjust = 0))

ggsave(
    file.path(figure_dir, "18_unsupervised-learning-nba-pca-loadings.png"),
    pca_loading_plot,
    width = 7.2,
    height = 6.0,
    dpi = 300,
    bg = "white"
)

label_players = c(
    "Stephen Curry 2022",
    "Klay Thompson 2022",
    "Duncan Robinson 2020",
    "Jalen Brunson 2022",
    "Luka Doncic 2022",
    "James Harden 2022",
    "LeBron James 2022",
    "Giannis Antetokounmpo 2022",
    "Nikola Jokic 2022",
    "Joel Embiid 2022",
    "Rudy Gobert 2022",
    "Clint Capela 2021"
)

pca_label_points = pca_scores |>
    filter(player_szn %in% label_players)

pca_plot = ggplot(pca_scores, aes(PC1, PC2)) +
    geom_hline(yintercept = 0, color = "gray84", linewidth = 0.35) +
    geom_vline(xintercept = 0, color = "gray84", linewidth = 0.35) +
    geom_point(color = "gray55", alpha = 0.28, size = 0.75) +
    geom_point(
        data = pca_label_points,
        color = "#D95D39",
        size = 2.0
    ) +
    geom_text(
        data = pca_label_points,
        aes(label = player_szn),
        hjust = 0,
        nudge_x = 0.08,
        size = 2.65,
        check_overlap = TRUE
    ) +
    labs(
        title = "A Two-Dimensional Map of NBA Shot Diets",
        subtitle = paste0(
            "PC1 explains ", round(100 * pca_var[1]),
            "% and PC2 explains ", round(100 * pca_var[2]),
            "% of standardized feature variation"
        ),
        x = "PC1: perimeter creation vs. paint finishing",
        y = "PC2: spot-up shooting vs. isolation creation"
    ) +
    theme_week +
    theme(legend.position = "none")

ggsave(
    file.path(figure_dir, "18_unsupervised-learning-nba-pca.png"),
    pca_plot,
    width = 8.0,
    height = 5.5,
    dpi = 300,
    bg = "white"
)

###########################
### K-MEANS CLUSTERING ####
###########################

chosen_k = 5
kmeans_fit = kmeans(X_scaled, centers = chosen_k, nstart = 100, iter.max = 100)

cluster_profiles = nba |>
    mutate(cluster_id = kmeans_fit$cluster) |>
    group_by(cluster_id) |>
    summarize(
        n = n(),
        across(all_of(feature_cols), mean),
        .groups = "drop"
    ) |>
    rowwise() |>
    mutate(
        archetype = case_when(
            FGA_freq_PRBallHandler_OG > 0.35 ~ "Ball-handlers",
            FGA_freq_OffScreenOrHandoff_OG > 0.18 ~ "Movement shooters",
            FGA_freq_Postup_OG > 0.18 ~ "Post bigs",
            FGA_freq_Cut_OG > 0.20 & FGA_freq_PRRollMan_OG > 0.20 ~ "Rim runners",
            FGA_freq_Spotup_OG > 0.55 ~ "Spot-up wings",
            TRUE ~ "Balanced wings"
        )
    ) |>
    ungroup()

cluster_labels = setNames(cluster_profiles$archetype, cluster_profiles$cluster_id)
cluster_levels = names(cluster_palette)

nba_clustered = pca_scores |>
    mutate(
        cluster_id = kmeans_fit$cluster,
        archetype = factor(cluster_labels[as.character(cluster_id)], levels = cluster_levels)
    )

###############################
### WORKED REPLACEMENT CASE ###
###############################

target_player_szn = "Jalen Brunson 2022"
target_idx = match(target_player_szn, nba$player_szn)
target_player = nba$PLAYER_NAME[target_idx]
target_season = nba$szn[target_idx]
recent_window = 1

target_distances = sqrt(rowSums((sweep(X_scaled, 2, X_scaled[target_idx, ], "-"))^2))

target_neighbors = nba |>
    mutate(
        archetype = cluster_labels[as.character(kmeans_fit$cluster)],
        distance = target_distances
    ) |>
    filter(
        player_szn != target_player_szn,
        PLAYER_NAME != target_player,
        szn >= target_season - recent_window,
        szn <= target_season
    ) |>
    arrange(distance) |>
    slice_head(n = 8) |>
    transmute(
        player_szn,
        archetype,
        distance,
        pr_handler = FGA_freq_PRBallHandler_OG,
        isolation = FGA_freq_Isolation_OG,
        spotup = FGA_freq_Spotup_OG
    )

latex_escape = function(x) {
    x |>
        gsub("\\\\", "\\\\textbackslash{}", x = _, fixed = TRUE) |>
        gsub("&", "\\\\&", x = _, fixed = TRUE) |>
        gsub("%", "\\\\%", x = _, fixed = TRUE) |>
        gsub("_", "\\\\_", x = _, fixed = TRUE) |>
        gsub("#", "\\\\#", x = _, fixed = TRUE)
}

format_pct = function(x) paste0(round(100 * x), "\\%")

neighbor_rows = target_neighbors |>
    mutate(
        player_szn = latex_escape(player_szn),
        archetype = latex_escape(archetype),
        distance = sprintf("%.2f", distance),
        pr_handler = format_pct(pr_handler),
        isolation = format_pct(isolation),
        spotup = format_pct(spotup),
        line = paste(
            player_szn,
            archetype,
            distance,
            pr_handler,
            isolation,
            spotup,
            sep = " & "
        )
    ) |>
    pull(line)

writeLines(
    c(
        "\\begin{table}[H]",
        "\\centering",
        "\\small",
        "\\begin{tabular}{llrrrr}",
        "\\hline",
        "\\textbf{Player-season} & \\textbf{Archetype} & \\textbf{Dist.} & \\textbf{P\\&R} & \\textbf{Iso} & \\textbf{Spot-up} \\\\",
        "\\hline",
        paste0(neighbor_rows, " \\\\"),
        "\\hline",
        "\\end{tabular}",
        "\\caption{Nearest recent player-seasons to Jalen Brunson 2022 by Euclidean distance in the standardized eight-feature play-type space. Candidates are restricted to the 2021 and 2022 seasons.}",
        "\\label{tab:brunson-neighbors}",
        "\\end{table}"
    ),
    file.path(table_dir, "18_unsupervised-learning-brunson-neighbors.tex")
)

cluster_profile_plot_data = cluster_profiles |>
    select(cluster_id, archetype, n, all_of(feature_cols)) |>
    pivot_longer(
        cols = all_of(feature_cols),
        names_to = "feature",
        values_to = "mean_frequency"
    ) |>
    mutate(
        feature = factor(recode(feature, !!!feature_labels), levels = rev(feature_order)),
        archetype = factor(archetype, levels = cluster_levels),
        facet_label = paste0(archetype, " (n=", n, ")")
    )

profile_plot = ggplot(cluster_profile_plot_data, aes(mean_frequency, feature, fill = archetype)) +
    geom_col(width = 0.72, show.legend = FALSE) +
    facet_wrap(vars(facet_label), ncol = 1) +
    scale_x_continuous(labels = \(x) paste0(round(100 * x), "%"), limits = c(0, 0.7)) +
    scale_fill_manual(values = cluster_palette, drop = FALSE) +
    labs(
        title = "Five K-Means Clusters Split into NBA Roles",
        subtitle = "Average share of shot attempts by play type within each cluster (k = 5)",
        x = "Average shot frequency",
        y = NULL
    ) +
    theme_week +
    theme(strip.text = element_text(face = "bold", hjust = 0))

ggsave(
    file.path(figure_dir, "18_unsupervised-learning-nba-players.png"),
    profile_plot,
    width = 7.4,
    height = 8.6,
    dpi = 300,
    bg = "white"
)

############################
### HIERARCHICAL CHECK #####
############################

set.seed(18)
dendro_sample = nba_clustered |>
    group_by(archetype) |>
    slice_sample(n = 16) |>
    ungroup()

dendro_dist = dist(X_scaled[dendro_sample$row_idx, ])
dendro_fit = hclust(dendro_dist, method = "ward.D2")

png(
    filename = file.path(figure_dir, "18_unsupervised-learning-nba-dendrogram.png"),
    width = 1800,
    height = 1100,
    res = 220,
    bg = "white"
)
plot(
    dendro_fit,
    labels = dendro_sample$player_szn,
    main = "Hierarchical Clustering of a Sample of Player-Seasons",
    xlab = "",
    sub = "",
    cex = 0.55
)
rect.hclust(dendro_fit, k = chosen_k, border = "#D95D39")
dev.off()

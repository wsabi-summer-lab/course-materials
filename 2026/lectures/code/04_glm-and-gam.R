########################
### INSTALL PACKAGES ###
########################

# install.packages(c("dplyr", "ggplot2", "mgcv", "readr"))

library(dplyr)
library(ggplot2)
library(mgcv)
library(readr)

#################
### LOAD DATA ###
#################

draft_data = read_csv(
    "2026/lectures/data/02_nfl-draft-second-contracts.csv.gz",
    show_col_types = FALSE
)

#######################
### DRAFT GAM PLOT ###
#######################

draft_lm = lm(
    performance_value ~ draft_pos,
    data = draft_data
)

draft_gam = gam(
    performance_value ~ s(draft_pos, k = 12),
    data = draft_data,
    family = gaussian(),
    method = "REML"
)

draft_grid = tibble(
    draft_pos = seq(min(draft_data$draft_pos), max(draft_data$draft_pos), length.out = 250)
)

draft_grid = draft_grid |>
    mutate(
        lm_fit = predict(draft_lm, newdata = draft_grid),
        gam_fit = predict(draft_gam, newdata = draft_grid)
    )

draft_plot = ggplot(draft_data, aes(x = draft_pos, y = performance_value)) +
    geom_point(
        alpha = 0.28,
        color = "#4C78A8"
    ) +
    geom_line(
        data = draft_grid,
        aes(x = draft_pos, y = lm_fit),
        color = "#D62728",
        linewidth = 1
    ) +
    geom_line(
        data = draft_grid,
        aes(x = draft_pos, y = gam_fit),
        color = "#2CA02C",
        linewidth = 1
    ) +
    labs(
        x = "Draft position",
        y = "Second-contract value (% of cap)",
        title = "Straight line vs. GAM for NFL draft expected value"
    ) +
    annotate(
        "text",
        x = 180,
        y = 11.8,
        label = "Red: linear fit\nGreen: GAM",
        hjust = 1,
        size = 3.4
    ) +
    theme_minimal(base_size = 12)

ggsave(
    "2026/lectures/figures/04_draft-gam.png",
    draft_plot,
    width = 7,
    height = 4.8,
    dpi = 300
)

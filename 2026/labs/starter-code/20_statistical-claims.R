#######################
### Authors: JP, RB ###
#######################

#############
### SETUP ###
#############

# install.packages("tidyverse")
library(tidyverse)

set.seed(20)

#########################
### HOW THIS LAB WORKS ##
#########################

# This lab is short and writing-focused.
# Interpret findings from these three earlier labs:
#   * Lab 14: fully Bayesian models
#   * Lab 15: regularization and ridge
#   * Lab 19: kernel methods
#
# You may reuse code, plots, tables, and saved outputs from your earlier work.
# Unless you want to, do not refit large models from scratch.

#########################
### REQUIRED MEMOS ######
#########################

# Write three short notes:
#   * Lab 14: to the Dallas Cowboys coaching staff before a game vs. the Eagles
#   * Lab 15: to an NBA GM about whether to pursue Mikal Bridges in a trade
#   * Lab 19: to Liverpool's training staff about shot prioritization
#
# For each note, choose one result from your earlier work and explain what it
# means for that audience.

required_memos = tribble(
    ~prior_lab, ~memo_to, ~result_used, ~interpretation,
    "14_fully_bayesian_models", "Dallas coaching staff vs. Philadelphia", NA, NA,
    "15_regularization_and_ridge", "NBA GM re: Mikal Bridges trade", NA, NA,
    "19_kernel_methods", "Liverpool training staff re: shot prioritization", NA, NA
)

# TODO: Fill in result_used and interpretation for all three rows.

required_memos

#########################
### TRANSPARENCY CUES ###
#########################

# Keep each memo honest about:
#   * what quantity the result is actually about
#   * what sample or model produced it
#   * what uncertainty or validation matters
#   * what important context is missing
#   * how far the audience should trust or act on it

transparency_cues = tribble(
    ~topic, ~reminder,
    "Target", "Be clear about what the number or plot is actually about.",
    "Data", "Be clear about what sample or setting produced the finding.",
    "Uncertainty", "Be clear about what could move the estimate or weaken confidence.",
    "Validation", "Be clear about what was checked out of sample or otherwise verified.",
    "Limits", "Be clear about what the model leaves out.",
    "Use", "Be clear about how much decision weight the audience should place on the result."
)

transparency_cues

########################
### INSTALL PACKAGES ###
########################

# install.packages(c("readr", "splines"))

# load packages
library(readr)
library(splines)

#################
### LOAD DATA ###
#################

# run from the repository root
ncaa_data = read_csv("2026/lectures/data/02_ncaa-games.csv.gz", show_col_types = FALSE)
punts = read_csv("2026/lectures/data/02_punts.csv.gz", show_col_types = FALSE)
draft_data = read_csv("2026/lectures/data/02_nfl-draft-second-contracts.csv.gz", show_col_types = FALSE)

################################
### NCAA POWER RATINGS MODEL ###
################################

# list teams
teams = sort(unique(c(ncaa_data$home_team, ncaa_data$away_team)))

# create design matrix with one home-court column
# and one column for each team rating
X = matrix(0, nrow = nrow(ncaa_data), ncol = length(teams) + 1)
colnames(X) = c("home_court", teams)
X[, "home_court"] = 1

# fill in +1 for the home team and -1 for the away team
for (i in seq_len(nrow(ncaa_data))) {
  X[i, ncaa_data$home_team[i]] = 1
  X[i, ncaa_data$away_team[i]] = -1
}

# fit the linear model
model = lm(score_diff ~ X + 0, data = ncaa_data)

# display coefficient estimates and uncertainty summaries
summary(model)
confint(model)

###########################
### PUNT POSITION MODEL ###
###########################

# linear model
l_model = lm(next_ydl ~ ydl, data = punts)
l_coef = l_model$coefficients

# quadratic model
q_model = lm(next_ydl ~ ydl + I(ydl^2), data = punts)
q_coef = q_model$coefficients

#########################
### NFL DRAFT SPLINES ###
#########################

# fit the full spline
full_spline = lm(
  performance_value ~ bs(draft_pos, degree = 3, knots = seq(33, 225, by = 32)),
  data = draft_data
)

# get coefficients
full_coef = full_spline$coefficients

# fit reduced spline with 5 df
red_spline = lm(
  performance_value ~ bs(draft_pos, degree = 3, df = 5),
  data = draft_data
)

# get coefficients
red_coef = red_spline$coefficients

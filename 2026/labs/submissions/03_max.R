#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(4)

##############
### PART 1 ###
##############

# load data
field_goals = read_csv("../data/03_field-goals.csv.gz")
fg_summary <- field_goals %>%
  group_by(ydl) %>%
  summarize(
    make_pct = mean(fg_made),
    n        = n()              
  )

ggplot(fg_summary, aes(x = ydl, y = make_pct)) +
  geom_point() +
  labs(x = "Yard line (distance)", y = "FG make %") +
  scale_y_continuous(labels = scales::percent)

linear_model = lm(fg_made ~ ydl, data = field_goals)
log_model_1 = glm(fg_made ~ ydl, data = field_goals, family = "binomial")
log_model_2 = glm(fg_made ~ ydl + I(ydl^2), data = field_goals, family = "binomial")

grid <- tibble(ydl = seq(min(field_goals$ydl), max(field_goals$ydl), length.out = 200))

grid <- grid %>%
  mutate(
    `Linear (lm)`        = predict(linear_model, newdata = grid),
    `Logistic`           = predict(log_model_1,  newdata = grid, type = "response"),
    `Logistic + quad`    = predict(log_model_2,  newdata = grid, type = "response")
  )
grid

grid_long <- grid %>%
  pivot_longer(-ydl, names_to = "model", values_to = "pred")
grid_long

ggplot() +
  geom_point(data = fg_summary, aes(ydl, make_pct)) +
  geom_line(data = grid_long, aes(ydl, pred, color = model), linewidth = 1) +
  labs(x = "Yard line (distance)", y = "FG make %", color = "Model") +
  scale_y_continuous(labels = scales::percent)

n          <- nrow(field_goals)
train_idx  <- sample(seq_len(n), size = 0.8 * n) 
train_data <- field_goals[train_idx, ]
test_data  <- field_goals[-train_idx, ]

lm_tr   <- lm(fg_made ~ ydl, data = train_data)
log1_tr <- glm(fg_made ~ ydl, data = train_data, family = "binomial")
log2_tr <- glm(fg_made ~ ydl + I(ydl^2), data = train_data, family = "binomial")

test_data$p_lm   <- predict(lm_tr,   newdata = test_data)                    
test_data$p_log1 <- predict(log1_tr, newdata = test_data, type = "response")
test_data$p_log2 <- predict(log2_tr, newdata = test_data, type = "response")

logloss <- function(p, y) {
  p <- pmin(pmax(p, 1e-15), 1 - 1e-15) #bounding linear values to [0,1], <0 -> 0, >1 -> 1
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

results <- tibble(
  model   = c("Linear (lm)", "Logistic", "Logistic + quad"),
  logloss = c(logloss(test_data$p_lm,   test_data$fg_made),
              logloss(test_data$p_log1, test_data$fg_made),
              logloss(test_data$p_log2, test_data$fg_made))
)
results

#Logistic and Logistic + quad both have log loss of 0.347, smaller than linear's 0.361
#Interpretation of coefficients: 
#Linear model(b0 = 1.07, b1 = -0.0119): make percentage is 107% at 0yds(invalid), and a 1-yard increase in distance corresponds to a -1.2% make percentage
#Log model 1(b0 = 4.108, b1 = -0.105): p(make) at 0yds is 1/(1+e^(-b0)) or around 98%. Each additional yard multiplies the odds of making by exp(-0.105) = 0.9, or a roughly 10% drop in odds
#Log model 2(b0 = 4.574, b1 = -0.150, b2 = 0.00095): 



##############
### PART 2 ###
##############

# load data
ncaab_results = read_csv("../data/03_ncaab-results.csv.gz")
ncaab_team_info = read_csv("../data/03_ncaab-teams.csv.gz")

games <- ncaab_results %>%
  filter(Season == 2024) %>%
  transmute(
    home_id = if_else(WLoc == "A", LTeamID, WTeamID),  
    away_id = if_else(WLoc == "A", WTeamID, LTeamID),
    y       = if_else(WLoc == "A", 0L, 1L),            
    hca     = if_else(WLoc == "N", 0L, 1L)             
  )

teams <- sort(unique(c(games$home_id, games$away_id)))
games$home_id <- factor(games$home_id, levels = teams)
games$away_id <- factor(games$away_id, levels = teams)

X_home <- model.matrix(~ home_id - 1, games)
X_away <- model.matrix(~ away_id - 1, games) 
X <- X_home - X_away
X <- X[, -1] #drop first reference team/fix b(team in column 1) = 0

fit <- glm(games$y ~ hca + X - 1, data = games, family = "binomial")

co <- summary(fit)$coefficients  
co
team_rows <- grepl("^Xhome_id", rownames(co))   
team_rows
ratings <- tibble(
  TeamID = as.integer(sub("Xhome_id", "", rownames(co)[team_rows])),
  rating = co[team_rows, "Estimate"],
  se     = co[team_rows, "Std. Error"]
)

ref_id <- as.integer(levels(games$home_id)[1])
ratings <- bind_rows(ratings, tibble(TeamID = ref_id, rating = 0, se = 0))

ratings <- ratings %>%
  left_join(ncaab_team_info, by = "TeamID") %>%
  mutate(lo = rating - 1.96 * se,
         hi = rating + 1.96 * se)
ratings
ratings %>%
  slice_max(rating, n = 15) %>%
  mutate(TeamName = fct_reorder(TeamName, rating)) %>%
  ggplot(aes(rating, TeamName)) +
  geom_pointrange(aes(xmin = lo, xmax = hi)) +
  labs(x = "Team rating (log-odds, vs reference)", y = NULL,
       title = "2024 NCAAB Bradley-Terry ratings (95% CI)")
#all coefficients are relative to the fixed reference team, so only useful by comparison

#moneyline for purdue-uconn: p(uconn win) = Logistic(0(Neutral) + B_uconn - B_purdue)
uconn  <- ratings$rating[ratings$TeamName == "Connecticut"]
purdue <- ratings$rating[ratings$TeamName == "Purdue"]
p_uconn_win <- plogis(uconn-purdue)

prob_to_moneyline <- function(p) {
  if_else(p >= 0.5,
          -100 * p / (1 - p), 
          100 * (1 - p) / p)     
}

ml_uconn = prob_to_moneyline(p_uconn_win)
ml_uconn
#Uconn ~57% win probability, -134 ML

#SE
a <- numeric(length(coef(fit)))
names(a) <- names(coef(fit))
a["Xhome_id1163"] <-  1     # UConn
a["Xhome_id1345"] <- -1     # Purdue

est <- sum(a * coef(fit))
se  <- sqrt(t(a) %*% vcov(fit) %*% a)     
ci  <- est + c(-1, 1) * 1.96 * se
plogis(ci) # CI: [0.191, 0.884]
ci_ml = prob_to_moneyline(plogis(ci)) # +422 to -767
ci_ml



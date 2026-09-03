#############
### SETUP ###
#############

# install.packages(c("ggplot2", "splines", "tidyverse"))
library(ggplot2)
library(splines)
library(tidyverse)

# set seed
set.seed(12)

##########################
### NBA PLAYER QUALITY ###
##########################

# load data
nba_data = read_csv("../data/12_nba-box-scores.csv.gz")
# preview data
head(nba_data)

#Task 3 & 4

#define set
nba_data = nba_data %>%
  mutate(points_per_possession = pts / possessions)

nba_data_group = nba_data %>%
  group_by(namePlayer) %>%
  filter(length(idGame) >= 30) %>% 
  ungroup()

player_M = nba_data_group %>%
  group_by(namePlayer) %>%
  summarise(M_i = mean(points_per_possession, na.rm = TRUE),
            V_i = var(points_per_possession, na.rm = TRUE),
            .groups = "drop")

mu_hat = mean(player_M$M_i)
nu2_hat = var(player_M$M_i)

player_M_avg = player_M %>%
  filter(abs(M_i - mu_hat) < 0.05 * mu_hat)

sigma2_hat = mean(player_M_avg$V_i, na.rm = TRUE)

#Task 5

player_split = nba_data_group %>%
  group_by(namePlayer) %>%
  arrange(dateGame) %>%
  mutate(
    idx   = row_number(),
    G_i   = n(),
    half  = floor(G_i/2),
    which = if_else(idx <= half, "first_half", "second_half")) %>%
  summarise(
    M_ik = mean(points_per_possession[which == "first_half"], na.rm = TRUE),
    D2   = list(points_per_possession[which == "second_half"]),
    k_i  = first(half),
    .groups = "drop") %>% 
  ungroup()

tau2_grid = 10^seq(-7, -3, length.out = 50)

rmse = sapply(tau2_grid, function(tau2) {
  per_player_rmse = mapply(function(Mik, D2i, ki) {
    mu_ik = (sigma2_hat / (sigma2_hat + ki * tau2)) * mu_hat +
      (ki * tau2 / (sigma2_hat + ki * tau2)) * Mik
    sqrt(mean((mu_ik - unlist(D2i))^2, na.rm = TRUE))
  },
  Mik = player_split$M_ik,
  D2i = player_split$D2,
  ki  = player_split$k_i,
  SIMPLIFY = TRUE) 
  mean(per_player_rmse, na.rm = TRUE)
})

tau2_results = tibble(
  tau2     = tau2_grid,
  mean_RMSE = rmse)

best_tau2 = tau2_results %>%
  filter(mean_RMSE == min(mean_RMSE))

best_tau2

#Task 6

tau2_hat = best_tau2$tau2

nba_post = nba_data_group %>%
  arrange(namePlayer, dateGame) %>%
  group_by(namePlayer) %>%
  do({
    df = .
    n  = nrow(df)
    mu_post = numeric(n)
    d = df$points_per_possession     # X_{ij}/P_{ij}
    
    #from task 1
    mu_post[1] = (nu2_hat / (sigma2_hat + nu2_hat)) * d[1] +
      (sigma2_hat / (sigma2_hat + nu2_hat)) * mu_hat
    
    #from task 2
    for (j in 2:n) {
      mu_post[j] = (tau2_hat     / (sigma2_hat + tau2_hat)) * d[j] +
        (sigma2_hat   / (sigma2_hat + tau2_hat)) * mu_post[j-1]}
    
    df %>% 
      mutate(
        j        = row_number(),   # game index
        post_mu  = mu_post
      )
  }) %>%
  ungroup()

#view
nba_post %>% select(namePlayer, dateGame, j, points_per_possession, post_mu) %>% 
  head(10)


##########################
### NFL KICKER QUALITY ###
##########################

# load data
kick_data = read_csv("../data/12_field-goals.csv.gz")
# preview data
head(kick_data)

#task 1

model_fg = glm(fg_made ~ bs(ydl, df = 5), data = kick_data, family  = binomial(link = "logit"))

ggplot(kick_data, aes(x = ydl, y = fg_made)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(title = "Field Goal Success Rate by Distance",
       x = "Yards to Goal",
       y = "Field Goal Made (1) or Missed (0)") +
  theme_minimal()

kick_data = kick_data %>%
  mutate(P0 = predict(model_fg, newdata = ., type = "response"))

#task 2
library(purrr)

names(kick_data)

alpha = 0.8

kick_data = kick_data %>%
  mutate(FGPA = fg_made - P0,
         FGPA = as.numeric(FGPA))

kick_data = kick_data %>%
  arrange(kicker, week) %>%
  group_by(kicker) %>%
  mutate(KQ = accumulate(FGPA,
      ~ alpha * .x + .y,
      .init = 0
    )[-1]
  ) %>%
  ungroup()

#task 3
compute_kq_prior = function(df, alpha) {

  df = df %>% 
    arrange(week) %>% 
    mutate(j = row_number())
  n = nrow(df)
  KQ = numeric(n)  
  for (k in seq_len(n)) {
    if (k == 1) {
     
      KQ[k] = 0
    } else {
      KQ[k] = alpha * KQ[k-1] + df$FGPA[k-1]
    }
  }
  
  df %>% mutate(KQ_prior = KQ)
}

alpha = 0.8

kick_data_kq = kick_data %>%
  group_by(kicker) %>%
  group_modify(~ compute_kq_prior(.x, alpha)) %>%
  ungroup()

kick_data_kq %>% 
  filter(kicker == "A.Franks") %>% 
  select(kicker, week, j, FGPA, KQ_prior) %>% 
  head(6)

sample_kickers = kick_data_kq %>%
  distinct(kicker) %>%
  slice_sample(n = 4) %>%
  pull()

ggplot(
  kick_data_kq %>% filter(kicker %in% sample_kickers),
  aes(x = j, y = KQ_prior, color = kicker)
) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Kicker Quality Prior to Kick j",
    x     = "Kick number j",
    y     = expression(KQ[ij]),
    color = "Kicker"
  ) +
  theme_minimal()





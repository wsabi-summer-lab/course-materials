#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

##############
### PART 1 ###
##############

# load data
nba = read_csv("../data/03_nba-four-factors.csv.gz")

##############
### PART 2 ###
##############

# load data
punts = read_csv("../data/03_punts.csv.gz")


#task 1.1 

nba <- nba %>%
  rename_with(~ gsub("%", "_pct", .x)) %>%
  rename_with(~ gsub("\\.", "_", .x)) %>%
  rename_with(~ tolower(.x))
names(nba)
nba <- nba %>%
  rename(
    opp_efg_pct = `opp efg_pct`,
    opp_tov_pct = `opp tov _pct`,
    opp_ft_rate = `opp ft rate`,
    ft_rate = `ft rate`
  ) %>%
  mutate(
    x1 = efg_pct - opp_efg_pct,
    x2 = oreb_pct - dreb_pct,
    x3 = tov_pct - opp_tov_pct,
    x4 = ft_rate - opp_ft_rate
  )
nba <- nba %>%
  rename(wins = `win_pct`)
sapply(
  nba %>% select(wins, x1, x2, x3, x4),
  function(x) c(
    mean = mean(x, na.rm = TRUE),
    sd   = sd(x,   na.rm = TRUE),
    min  = min(x,  na.rm = TRUE),
    max  = max(x,  na.rm = TRUE)
  )
)

nba %>%
  select(wins, x1, x2, x3, x4) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ name, scales = "free")

cor(nba %>% select(wins, x1, x2, x3, x4), use = "complete.obs")

#task 1.2
model <- lm(wins ~ x1 + x2 + x3 + x4, data = nba)
summary(model)

nba_std <- nba %>%
  mutate(across(x1:x4, ~ ( . - mean(.) ) / sd(.)))
model_std <- lm(wins ~ x1 + x2 + x3 + x4, data = nba_std)
summary(model_std)

coef_std <- abs(coef(model_std)[-1])
tibble(factor = names(coef_std), std_coef = coef_std) %>%
  arrange(desc(std_coef)) #the standardized value because it shows everything on a common scale

#my code isn't working starting here 

set.seed(50)
n = nrow(nba)
train_idx = sample(1:n, prob = rep(0.8, n))
train = nba %>% 
  slice(train_idx)
test = slice(model_data, - train_idx)

test$predicted_wins <- predict(model, newdata = test)
head(test$predicted_wins)

#task 2.1

mod_linear <- lm(next_ydl ~ ydl + pq,               data = punts)
mod_quad   <- lm(next_ydl ~ ydl + I(ydl^2) + pq,    data = punts)
mod_cubic  <- lm(next_ydl ~ poly(ydl, 3) + pq,      data = punts)

#chatgpt helped from 97-109
aic_vals <- tibble(
  name = c("linear", "quadratic", "cubic"),
  AIC  = c(AIC(mod_linear), AIC(mod_quad), AIC(mod_cubic))
)
best_name <- aic_vals %>% slice_min(AIC) %>% pull(name)
model_sel <- switch(
  best_name,
  linear    = mod_linear,
  quadratic = mod_quad,
  cubic     = mod_cubic
)
pqi_vals <- quantile(punts$pq, probs = c(0.25, 0.5, 0.75))
ydl_seq <- seq(min(punts$ydl), max(punts$ydl), length.out = 200)

pred_grid <- crossing(
  ydl = ydl_seq,
  pq  = pqi_vals
) %>%
  mutate(predicted = predict(model_sel, newdata = tibble(ydl = ydl, pq = pq)))

ggplot() +
  geom_point(
    data = punts,
    aes(x = ydl, y = next_ydl),
    alpha = 0.4
  ) +
  geom_line(
    data = pred_grid,
    aes(x = ydl, y = predicted, color = factor(pq)),
    size = 1
  ) +
  labs(
    x     = "Yard Line at Punt (ydl)",
    y     = "Next Yard Line (next_ydl)",
    color = "Punter Quality\n(pq)",
    title = paste("Scatter + Predicted next_ydl vs ydl (", best_name, " model)", sep = "")
  ) +
  theme_minimal()

punts <- punts %>%
  mutate(
    expected_next = predict(model_sel, newdata = select(punts, ydl, pq)),
    PYOE          = next_ydl - expected_next
  )

punter_ranks <- punts %>%
  group_by(punter) %>%
  summarise(mean_PYOE = mean(PYOE)) %>%
  arrange(desc(mean_PYOE)) %>% 
  slice_head(n=20) #delete this line if we don't want ALL the data

ggplot(punter_ranks, aes(x = reorder(punter, mean_PYOE), y = mean_PYOE)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(
    x = "Punter",
    y = "Mean PYOE",
    title = "Punter Rankings by Punt Yards Over Expected (PYOE)"
  ) +
  theme_minimal()

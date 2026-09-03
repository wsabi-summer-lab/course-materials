
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
nba <- nba %>% 
  mutate(
    x1 = `EFG%` - `OPP EFG%`,
    x2 = `OREB%` - `DREB%`,
    x3 = `TOV%` - `OPP TOV %`,
    x4 = `FT Rate` - `OPP FT Rate`
  ) 
nba %>% 
dplyr::select(x1,x2,x3,x4) %>% 
summary()

# Marginal Plots

nba %>% 
  dplyr::select(x1, x2, x3, x4) %>% 
  pivot_longer(cols = everything(),
               names_to  = "metric",
               values_to = "value") %>% 
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30,
                 fill = "steelblue",
                 colour = "white",
                 alpha = .85) +
  facet_wrap(~ metric, scales = "free_x", ncol = 2) +
  labs(title   = "Marginal Distributions of Four‑Factor Differentials",
       x       = "Team − Opponent Difference",
       y       = "Count") +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = .5)
  )

#Correlation
corr_mat <- nba %>% 
  dplyr::select(x1, x2, x3, x4) %>% 
  cor(use = "pairwise.complete.obs")   # handles any missing rows

print(round(corr_mat, 3))   # round to 3 decimals for readability

#Task 2

#1.
model_data = lm(`WIN%` ~  x1 + x2 + x3 + x4, data = nba)
summary(model_data)

nba_std <- nba %>%
  mutate(across(x1:x4, ~ ( . - mean(.) ) / sd(.)))
model_std <- lm(`WIN%` ~ x1 + x2 + x3 + x4, data = nba_std)
summary(model_std)

coef_std <- abs(coef(model_std)[-1])
tibble(factor = names(coef_std), std_coef = coef_std) %>%
  arrange(desc(std_coef))
#3(1) 4(2)
#1 The standardized model is the best one.
  #x1, x3, x4, x2

set.seed(42)
n=nrow(nba)
train_index = sample(1:n,size = floor(.8*n))
train = slice(nba,train_index)
test = slice(nba,-train_index)

train_model = lm(`WIN%` ~  x1 + x2 + x3 + x4, data = train)
summary(model_data)

predictions = predict(train_model,test)

MSE=mean((test$`WIN%`-predictions)^2)

n=nrow(nba_std)
train_index = sample(1:n,size = floor(.8*n))
train = slice(nba_std,train_index)
test = slice(nba_std,-train_index)
train_model = lm(`WIN%` ~  x1 + x2 + x3 + x4, data = train)
summary(model_data)

predictions = predict(train_model,test)

MSE_std=mean((test$`WIN%`-predictions)^2)
MSE
MSE_std




#3.2 Expected Outcome of a Punt
punts = read_csv("../data/03_punts.csv.gz")
mod_linear <- lm(next_ydl ~ ydl + pq, data = punts)
mod_quad   <- lm(next_ydl ~ ydl + I(ydl^2) + pq,data = punts)
mod_cubic  <- lm(next_ydl ~ poly(ydl, 3) + pq,data = punts)
mod_spline = lm(next_ydl ~ splines::bs(ydl,degree=3, df=5), data=punts)


n=nrow(punts)
train_index = sample(1:n,size = floor(.8*n))
train = slice(punts,train_index)
test = slice(punts,-train_index)
mod_linear <- lm(next_ydl ~ ydl + pq, data = punts)
summary(mod_linear)

predictions = predict(mod_linear,test)
MSE_linear=mean((test$next_ydl-predictions)^2)

predictions = predict(mod_quad,test)
MSE_quad=mean((test$next_ydl-predictions)^2)

newgrid <- expand_grid(
  ydl = seq(min(punts$ydl), max(punts$ydl), by = 1),          # current yard line
  pq  = quantile(punts$pq, probs = c(.2, .5, .8))             # low / median / high PQ
) %>%
  mutate(pred = predict(mod_cubic, newdata = .))

# ---- plot --------------------------------------------------------
ggplot() +
  geom_point(data = punts, aes(ydl, next_ydl), colour = "grey70", alpha = .4) +
  geom_line(data = newgrid,
            aes(ydl, pred, colour = factor(pq)),
            size = 1.2) +
  scale_x_reverse() +                         # so 100 → midfield on offense’s POV
  labs(title   = "Cubic model: Expected Opponent Yard Line",
       subtitle = "Curves shown for punter‑quality 20th, 50th, 80th percentile",
       x = "Current Yard Line (yards from opponent goal line)",
       y = "Predicted Next Yard Line",
       colour = "Punter Quality\n(quantile)") +
  theme_bw()

predictions = predict(mod_cubic,test)
MSE_cubic=mean((test$next_ydl-predictions)^2)

predictions = predict(mod_spline,test)
MSE_spline=mean((test$next_ydl-predictions)^2)

MSE_linear
MSE_quad
MSE_cubic
MSE_spline


#TASK 2
############################################################
## Task 2 – Rank & Visualize Punters by PYOE
############################################################

library(tidyverse)



# 1)  Compute Punt Yards Over Expected (PYOE)
punts <- punts %>%
  mutate(
    pred   = predict(mod_linear, newdata = punts),
    pyoe   = next_ydl - pred
  )

# 2)  Summarize average PYOE by punter
punter_rank <- punts %>%
  group_by(punter) %>%
  summarise(
    avg_pyoe = mean(pyoe, na.rm = TRUE),
    n_punts  = n(),
    .groups  = "drop"
  ) %>%
  arrange(desc(avg_pyoe))

# Print the top 10 punters by Avg PYOE
cat("\nTop 10 Punters by Average PYOE (yards):\n")
print(punter_rank %>% select(punter, avg_pyoe, n_punts) %>% head(10))

# Print the bottom 10 punters as well
cat("\nBottom 10 Punters by Average PYOE (yards):\n")
print(punter_rank %>% select(punter, avg_pyoe, n_punts) %>% tail(10))






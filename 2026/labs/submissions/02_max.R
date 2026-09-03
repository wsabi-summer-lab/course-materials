#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(3)

##############
### PART 1 ###
##############

# load data
nba_four_factors = read_csv("../data/02_nba-four-factors.csv.gz")
head(nba_four_factors)

#computing and adding each variable as a column
nba_four_factors = nba_four_factors %>%
  mutate(
    x1 = `EFG%` - `OPP EFG%`,
    x2 = `OREB%` + `DREB%` - 100,
    x3 = `OPP TOV %` - `TOV%`,
    x4 = `FT Rate` - `OPP FT Rate`
  )

#each variable's mean, std. dev, maximum, minimum

summaries = nba_four_factors %>%
  summarise(
    across(
      c(x1, x2, x3, x4),
      list(mean = mean, sd = sd, max = max, min = min),
      .names = "{.col}_{.fn}"
    )
  )
summaries

pivot = nba_four_factors %>%
  pivot_longer(
    cols = c(x1, x2, x3, x4),
    names_to = "variable",
    values_to = "value"
  ) 
pivot

pivot %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ variable, scales = "free")

ggplot(data = nba_four_factors, aes(x = x1, y = W)) + geom_point(alpha = 0.4)
ggplot(data = nba_four_factors, aes(x = x2, y = W)) + geom_point(alpha = 0.4)
ggplot(data = nba_four_factors, aes(x = x3, y = W)) + geom_point(alpha = 0.4)
ggplot(data = nba_four_factors, aes(x = x4, y = W)) + geom_point(alpha = 0.4)

nba_four_factors %>%
  summarise(
    across(
      c(x1, x2, x3, x4),
      ~ cor(W, .x),
      .names = "cor_W_{.col}"
    )
  )
# x1 seems to have the highest correlation with wins between the four variables

model = lm(W ~ x1 + x2 + x3 + x4, data = nba_four_factors)
coef = model$coefficients
coef
# fitted equation: 40.19 + 3.67x1 + 1.34x2 + 3.06x3 + 77.06x4
#interpreting coefficients: B_0 = predicted wins for an average team, B_i = predicted added wins per 1 unit increase in the variable, holding everything else fixed
#sign values of coefficients make sense, increase in variables should lead to increase in wins
#x4 has by far the largest coefficient

nba_four_factors = nba_four_factors %>%
  mutate(
    z1 = (x1 - summaries$x1_mean) / summaries$x1_sd,
    z2 = (x2 - summaries$x2_mean) / summaries$x2_sd,
    z3 = (x3 - summaries$x3_mean) / summaries$x3_sd,
    z4 = (x4 - summaries$x4_mean) / summaries$x4_sd
  )
model_standardized = lm(W ~ z1 + z2 + z3 + z4, data = nba_four_factors)
coef_standardized = model_standardized$coefficients
coef_standardized
#rankings by absolute coefficient value: z1, z3, z2, z4
#The standardized model is easier to compare, since it depends on relative changes between teams

#both models are equal
all.equal(fitted(model), fitted(model_standardized))
#both models are optimized within the column space

rss <- sum(residuals(model)^2)
n <- nrow(nba_four_factors)
p <- length(coef(model)) 
sigma_hat <- sqrt(rss / (n - p))

X <- model.matrix(model)              
sigma2 <- sum(residuals(model)^2) / (nrow(X) - ncol(X))
v <- sigma2 * solve(t(X) %*% X)
se <- sqrt(diag(v))

t <- qt(0.975, df = n - p)

lower <- coef_standardized - t * se
upper <- coef_standardized + t * se

results <- data.frame(
  term     = names(coef_standardized),
  estimate = round(coef_standardized, 3),
  std_err  = round(se, 3),
  ci_lower = round(lower, 3),
  ci_upper = round(upper, 3),
  row.names = NULL
)
# CIs: a0 = [39.79, 40.50], a1 = [9.99, 10.27], a2 = [3.50, 3.78], a3 = [4.18, 4.70], a4 = [-10.35, 14.92]
results

#Team chosen: Lakers, 2005

lal_20 = nba_four_factors %>%
  filter(ID == 466)


#predicted wins: 54.89
predict(model_standardized, newdata = lal_20)

#CI: [54.23, 55.54]
predict(model_standardized, newdata = lal_20, interval = "confidence")

#PI: [47.04, 62.73]
predict(model_standardized, newdata = lal_20, interval = "prediction")

#prediction interval is wider because there is additional residual variance for predicting individual team performance on top of uncertainty in the mean

set.seed(3)

n <- nrow(nba_four_factors)
test_idx  <- sample(seq_len(n), size = floor(0.2 * n))
train <- nba_four_factors[-test_idx, ]
test  <- nba_four_factors[ test_idx, ]

model_orig <- lm(W ~ x1 + x2 + x3 + x4, data = train)

for (v in c("x1","x2","x3","x4")) {
  mu <- mean(train[[v]]); s <- sd(train[[v]])
  train[[paste0("z", substr(v,2,2))]] <- (train[[v]] - mu) / s
  test [[paste0("z", substr(v,2,2))]] <- (test [[v]] - mu) / s
}
model_std <- lm(W ~ z1 + z2 + z3 + z4, data = train)

rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))

pred_orig <- predict(model_orig, newdata = test)
pred_std  <- predict(model_std,  newdata = test)

rmse(test$W, pred_orig) 
rmse(test$W, pred_std)  
#both models give the exact same RMSE of 3.61


#############
### PART 2 ###
##############

# load data
punts = read_csv("../data/02_punts.csv.gz")

ggplot(data = punts, aes(x = ydl, y = next_ydl)) + geom_point(alpha = 0.4)

ggplot(punts, aes(x = ydl, y = next_ydl)) +
  stat_summary_bin(fun = mean, bins = 14, 
                   geom = "point", color = "red", size = 2)

#data appears to be straight at longer distances away but bends closer to the endzone

ggplot(data = punts, aes(x = pq)) + geom_histogram()

m1 = lm(next_ydl ~ ydl, data = punts)
m2 = lm(next_ydl ~ ydl + I(ydl^2) , data = punts)
m3 = lm(next_ydl ~ ydl + I(ydl^2) + pq, data = punts) 

grid <- tibble(
  ydl = seq(min(punts$ydl), max(punts$ydl), length.out = 200),
  pq  = mean(punts$pq)
)

grid <- grid %>%
  mutate(
    `m1: linear`    = predict(m1, newdata = grid),
    `m2: quadratic` = predict(m2, newdata = grid),
    `m3: quad + pq` = predict(m3, newdata = grid)
  )

curves <- grid %>%
  pivot_longer(
    cols = c(`m1: linear`, `m2: quadratic`, `m3: quad + pq`),
    names_to = "model", values_to = "pred"
  )

ggplot() +
  geom_point(data = punts, aes(ydl, next_ydl), alpha = 0.05) +
  geom_line(data = curves, aes(ydl, pred, color = model), linewidth = 1) +
  labs(x = "starting yard line (ydl)", y = "post-punt yard line",
       color = "model")

set.seed(3)
test_idx <- sample(nrow(punts), 0.2 * nrow(punts))
train <- punts[-test_idx, ]
test  <- punts[ test_idx, ]

rmse <- function(m) sqrt(mean((test$next_ydl - predict(m, test))^2))

rmse(lm(next_ydl ~ ydl,                  train))  
rmse(lm(next_ydl ~ ydl + I(ydl^2),       train))   
rmse(lm(next_ydl ~ ydl + I(ydl^2) + pq,  train))   


#quad + pq has the lowest RMSE of 11.58 compared to 11.59(quad) and 11.81(linear)
#punter quality has a small positive effect on prediction
#coefficient: 1 unit increase in pq has a 1.26 yard gain on punt distance

one_punt <- punts %>% slice(1)
predict(m3, one_punt)
predict(m3, one_punt, interval = "confidence") #[79.62, 80.11]
predict(m3, one_punt, interval = "prediction") #[58.68, 101.05]
#prediction interval is wider because there is additional residual variance for predicting individual team performance on top of uncertainty in the mean

ci <- predict(m3, newdata = grid, interval = "confidence", level = 0.95)

pi <- predict(m3, newdata = grid, interval = "prediction", level = 0.95)

grid_bands <- grid %>%
  mutate(
    fit     = ci[, "fit"],
    ci_lwr  = ci[, "lwr"], ci_upr  = ci[, "upr"],
    pi_lwr  = pi[, "lwr"], pi_upr  = pi[, "upr"]
  )

ggplot(grid_bands, aes(x = ydl)) +
  geom_point(data = punts, aes(ydl, next_ydl), alpha = 0.05) +
  geom_ribbon(aes(ymin = pi_lwr, ymax = pi_upr), fill = "orange",    alpha = 0.25) +  # wide
  geom_ribbon(aes(ymin = ci_lwr, ymax = ci_upr), fill = "steelblue", alpha = 0.6)  +  # thin
  geom_line(aes(y = fit), color = "steelblue", linewidth = 1)
#the bands seem to widen at closer to ydl due to fewer punts

punts <- punts %>%
  mutate(
    expected = predict(m3, newdata = punts),  
    pyoe     = next_ydl - expected 
  )

punts_pyoe <- punts %>%
  group_by(punter) %>%
  summarise(avg_pyoe = mean(pyoe), n = n()) %>%
  arrange(desc(avg_pyoe)) %>%
  head()

punts_pyoe

#highest scores are dominated by 1/2 punt small samples

# Final reflection:
# 1. Adding columns (ydl^2, then pq) widened the space of shapes the model could fit from a straight line, to a curve, to a curve that shifts with punter quality.
# 2. Flexibility helped going linear -> quadratic, where cross-validated RMSE fell
# 4. sigma-hat ~= 10.8 yards is the typical size of the model's errors -- any single punt's outcome lands about 10-11 yards from its prediction on average.
# 5. The prediction interval is wider because it adds the irreducible punt-to-punt scatter (sigma^2) on top of the confidence interval's uncertainty in the estimated mean.
# 6. punters with few punts have large standard errors that make their extreme positions unstable.
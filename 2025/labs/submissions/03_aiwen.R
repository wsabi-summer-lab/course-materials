#############
### SETUP ###
#############

rm(list=ls())
# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(data.table)

# set working directory to wherever your data is
# setwd("filepath")

##############
### PART 1 ###
##############

# load data
nba_four_factors = as.data.table(read_csv("../data/03_nba-four-factors.csv.gz"))
names(nba_four_factors)

# task 1 eda

setnames(nba_four_factors, old = c("EFG%", "OPP EFG%", "OREB%", "DREB%", "TOV%", "OPP TOV %", "FT Rate", "OPP FT Rate"), 
         new = c("efg", "opp_efg", "oreb", "dreb", "tov", "opp_tov", "ft_rate", "opp_ft_rate"))

nba_four_factors <- nba_four_factors %>% 
  mutate(x1 = efg - opp_efg,
         x2 = oreb - dreb,
         x3 = tov - opp_tov,
         x4 = ft_rate - opp_ft_rate)

summary(nba_four_factors)
sd(nba_four_factors$W)
sd(nba_four_factors$x1)
sd(nba_four_factors$x2)
sd(nba_four_factors$x3)
sd(nba_four_factors$x4)

ggplot(nba_four_factors, aes(x = W)) +
  geom_histogram()
ggplot(nba_four_factors, aes(x = x1)) +
  geom_histogram()
ggplot(nba_four_factors, aes(x = x2)) +
  geom_histogram()
ggplot(nba_four_factors, aes(x = x3)) +
  geom_histogram()
ggplot(nba_four_factors, aes(x = x4)) +
  geom_histogram()

corr_matrix <- nba_four_factors %>%
  select(W, x1, x2, x3, x4) %>%
  cor()
corr_matrix

# task 2 modeling

model_1 <- lm(W ~ x1 + x2 + x3 + x4, data = nba_four_factors)
summary(model_1)

nba_four_factors <- nba_four_factors %>% 
  mutate(x1_std = (x1 - mean(x1)) / sd(x1),
         x2_std = (x2 - mean(x2)) / sd(x2),
         x3_std = (x3 - mean(x3)) / sd(x3),
         x4_std = (x4 - mean(x4)) / sd(x4))

model_2 <- lm(W ~ x1_std + x2_std + x3_std + x4_std, data = nba_four_factors)
summary(model_2)

# standardized model (model_2) is better because all the x variables are on the same scale
coefs <- coef(summary(model_2))[-1, 1] # coefficients of each standardized x variable
ordered_coefs <- sort(abs(coefs), decreasing = TRUE)
ordered_coefs

# comparing two models
set.seed(3)
n <- dim(nba_four_factors)[1]
train_indices <- sample(1:n, size = floor(0.8 * n))

train <- nba_four_factors[train_indices, ]
test <- nba_four_factors[-train_indices, ]

mod1 <- lm(W ~ x1 + x2 + x3 + x4, data = train)
mod2 <- lm(W ~ x1_std + x2_std + x3_std + x4_std, data = train)

pred1 <- predict(mod1, newdata = test)
pred2 <- predict(mod2, newdata = test)

# RMSE
rmse1 <- sqrt(mean((test$W - pred1)^2))
rmse2 <- sqrt(mean((test$W - pred2)^2))

print(c("Model 1 RMSE" = rmse1, "Model 2 RMSE (standardized x)" = rmse2))
# pretty much the same

##############
### PART 2 ###
##############

# load data
punts <- read_csv("../data/03_punts.csv.gz")
names(punts) # y = next_ydl

# scatterplots
ggplot(punts, aes(x = ydl, y = next_ydl)) +
  geom_point() # quadratic? or splines?

ggplot(punts, aes(x = pq, y = next_ydl)) +
  geom_point() # splines?

# quad: model with quadratic terms for both x variables
q_model = lm(next_ydl ~ ydl + I(ydl**2) + pq + I(pq**2), data = punts)
summary(q_model)

spline_model = lm(next_ydl ~ splines::bs(ydl, degree = 3, df = 5) + splines::bs(pq, degree = 3, df = 5),
                  data = punts)
summary(spline_model)

# selecting model: spline vs. quadratic
set.seed(3)
n <- dim(punts)[1]
train_indices <- sample(1:n, size = floor(0.8 * n))

train <- punts[train_indices, ]
test <- punts[-train_indices, ]

mod1 <- lm(next_ydl ~ ydl + I(ydl**2) + pq + I(pq**2), data = train)
mod2 <- lm(next_ydl ~ splines::bs(ydl, degree = 3, df = 5) + splines::bs(pq, degree = 3, df = 5),
           data = train)

pred1 <- predict(mod1, newdata = test)
pred2 <- predict(mod2, newdata = test)

# RMSE
rmse1 <- sqrt(mean((test$next_ydl - pred1)^2))
rmse2 <- sqrt(mean((test$next_ydl - pred2)^2))

print(c("Model 1 RMSE" = rmse1, "Model 2 RMSE (standardized x)" = rmse2))
rmse2 < rmse1 # true, therefore splines model is better

# new col for expected next_ydl based on splines model
names(punts)
punts <- punts %>% 
  mutate(exp_next_ydl = predict(spline_model, newdata = punts))

# expected next yard line vs current yard line for various punter quality values

ggplot(punts, aes(x = ydl, y = exp_next_ydl, color = pq)) +
  geom_point(alpha = 0.5) +
  labs(title = "Spline: Expected vs Current Yard Line",
       x = "Current Yard Line (ydl)",
       y = "Expected Next Yard Line") +
  theme_minimal()

punters <- punts %>% 
  mutate(pyoe = next_ydl - exp_next_ydl) %>% 
  group_by(punter) %>% 
  summarise(pyoe = mean(pyoe, na.rm = TRUE)) %>%
  arrange(desc(pyoe))%>%
  mutate(punter = factor(punter, levels = unique(punter)))

ggplot(punters, aes(x = punter, y = pyoe))+
  geom_col(fill = "steelblue")

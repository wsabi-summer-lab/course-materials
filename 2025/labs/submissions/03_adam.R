#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(splines)


# set working directory to wherever your data is
setwd("filepath")

##############
### PART 1 ###
##############

# load data
nba_4 = read_csv("2025/labs/data/03_nba-four-factors.csv.gz")

##############
### PART 2 ###
##############

# load data
punts = read_csv("2025/labs/data/03_punts.csv.gz")

#start

#### 3.1.3
### Task 1
data <- nba_4 %>% 
  mutate(
    x1 = `EFG%` - `OPP EFG%`,
    x2 = `OREB%` - `DREB%`,
    x3 = `TOV%` - `OPP TOV %`,
    x4 = `FT Rate` - `OPP FT Rate`
  ) %>% 
  select(x1,x2,x3,x4,W)


summary(data)
sd(data$x1)
sd(data$x2)
sd(data$x3)
sd(data$x4)

x1_plot = data %>% 
  ggplot(aes(x=x1)) +
  geom_histogram() +
  theme_minimal()

x2_plot = data %>% 
  ggplot(aes(x=x2)) +
  geom_histogram() +
  theme_minimal()

x3_plot = data %>% 
  ggplot(aes(x=x3)) +
  geom_histogram() +
  theme_minimal()

x4_plot = data %>% 
  ggplot(aes(x=x4)) +
  geom_histogram() +
  theme_minimal()

grid.arrange(x1_plot, x2_plot, x3_plot, x4_plot, nrow = 2, ncol = 2)

cor(data)

### Task 2

model = lm(W ~ x1+x2+x3+x4, data)
summary(model)

data = data %>% 
  mutate(
    x1_norm = (x1 - mean(data$x1))/sd(data$x1),
    x2_norm = (x2 - mean(data$x2))/sd(data$x2),
    x3_norm = (x3 - mean(data$x3))/sd(data$x3),
    x4_norm = (x4 - mean(data$x4))/sd(data$x4)
  )



norm_model = lm(W ~ x1_norm + x2_norm + x3_norm + x4_norm, data)
summary(model)
summary(norm_model)


#norm is better for relative value
df = data.frame(norm_model$coefficients, c('intercept', 'x1', 'x2', 'x3', 'x4'))

df %>% 
  ggplot(aes(y=norm_model$coefficients, x = df[,2]))+
  geom_point()

RE= 1 - sd(model$residuals)/sd(data$W)
RE_norm = 1 - sd(norm_model$residuals)/sd(data$W)
# reduction in errors are the same, same predictive performance

#### 3.2.2
### Task 1

punts %>% 
  ggplot(aes(x=pq, y=next_ydl))+
  geom_point(size=0.1)


model1 = lm(next_ydl ~ ydl + pq, data = train)
summary(model1)

model2 = lm(next_ydl ~ splines::bs(ydl, degree=1, df=3)+pq, data = train)
summary(model2)

model3 = lm(next_ydl ~ splines::bs(ydl, degree=2, df=4)+pq, data = train)
model4 = lm(next_ydl ~ splines::bs(ydl, degree=3, df=5)+pq, data = train)
model5 = lm(next_ydl ~ splines::bs(ydl, degree=1, df=3) + I(pq^2), data = train)
model6 = lm(next_ydl ~ splines::bs(ydl, degree=1, df=3)+splines::bs(pq, degree = 1, df = 4), data = train)

n = nrow(punts)
set.seed(3)
train_idx = sample(1:n, size = 0.8*n)

train = punts %>% 
  slice(train_idx)

test = slice(punts, -train_idx)


test$pred1 = predict(model1, test)
test$pred2 = predict(model2, test)
test$pred3 = predict(model3, test)
test$pred4 = predict(model4, test)
test$pred5 = predict(model5, test)
test$pred6 = predict(model6, test)

RE_1 = 1- sd(test$pred1-test$next_ydl)/sd(test$next_ydl)
RE_2 = 1- sd(test$pred2-test$next_ydl)/sd(test$next_ydl)
RE_3 = 1- sd(test$pred3-test$next_ydl)/sd(test$next_ydl)
RE_4 = 1- sd(test$pred4-test$next_ydl)/sd(test$next_ydl)
RE_5 = 1- sd(test$pred5-test$next_ydl)/sd(test$next_ydl)
RE_6 = 1- sd(test$pred6-test$next_ydl)/sd(test$next_ydl)

#model 5 win!!


test %>% 
  ggplot(aes(y=pred5, x=ydl))+
  geom_point(size=0.1)

test = test %>% 
  mutate(
    resids5 = next_ydl-pred5
  )


PYOE = test %>% 
  group_by(punter) %>% 
  summarize(
    PYOE = mean(resids5)
  ) %>% 
  arrange(desc(PYOE))

head(PYOE)

head(PYOE) %>% 
  ggplot(aes(x = reorder(punter, -PYOE)))+
  geom_col(aes(y=PYOE))

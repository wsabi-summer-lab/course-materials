#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)
library(splines)
library(tibble)
library(dplyr)

##############
### PART 1 ###
##############

# load data
field_goals = read_csv("2025/labs/data/04_field-goals.csv.gz")

##############
### PART 2 ###
##############

# load data
ncaab_results = read_csv("2025/labs/data/04_ncaab-results.csv.gz")
ncaab_team_info = read_csv("2025/labs/data/04_ncaab-teams.csv.gz")

n = nrow(field_goals)
set.seed(3)
train_idx = sample(1:n, size = 0.8*n)

train = field_goals %>% 
  slice(train_idx)

test = slice(field_goals, -train_idx)

percent = field_goals %>% 
  group_by(ydl,kicker) %>%
  summarise(fg_made = mean(fg_made))

train_percent = train %>% 
  group_by(ydl,kicker) %>%
  summarise(fg_made = mean(fg_made))

test_percent = test %>% 
  group_by(ydl,kicker) %>%
  summarise(fg_made = mean(fg_made))


train_percent %>% 
  ggplot(aes(x = ydl, y = fg_made)) +
  geom_point() 

model_1 = lm(fg_made ~ I(ydl)+I(ydl^2)+I(ydl^3) + kq, data = train_percent)
summary(model_1)
model_2 = glm(fg_made ~ I(ydl)+I(ydl^2)+I(ydl^3) + kq, data = train, family = "binomial")
summary(model_2)
model_3 = glm(fg_made ~ I(ydl)+I(ydl^2)+I(ydl^3) + splines::bs(kq, degree=3, df=5), data = train, family = "binomial")
summary(model_3)


test$pred1 = predict(model_1, test)
test$pred2 = predict(model_2, test, type = "response")
test$pred3 = predict(model_3, test, type = "response")

RE_1 = 1- sd(test$pred1-test$fg_made)/sd(test$fg_made)
RE_2 = 1- sd(test$pred2-test$fg_made)/sd(test$fg_made)
RE_3 = 1- sd(test$pred3-test$fg_made)/sd(test$fg_made)


test_percent %>% 
  ggplot(aes(x = ydl, y = fg_made)) +
  geom_point() +
  geom_line(aes(y = pred1), color = "blue") +
  geom_line(aes(y = pred2), color = "red") +
  geom_line(aes(y = pred3), color = "green") +
  labs(title = "Field Goal Predictions",
       x = "Yards to Goal",
       y = "Field Goal Made Probability") +
  theme_minimal()
#green is best apparently


test_summary <- test %>%
  group_by(ydl) %>%
  summarise(
    actual_fg = mean(fg_made),
    predicted_fg = mean(pred3)
  )


ggplot(total_summary, aes(x = ydl)) +
  geom_line(aes(y = actual_fg, color = "Actual FG%"), linewidth = 1) +
  geom_line(aes(y = predicted_fg, color = "Predicted FG% (Model 3)"), linewidth = 1) +
  labs(
    title = "Actual vs Predicted Field Goal % by Distance",
    x = "Yards to Goal",
    y = "Field Goal %",
    color = "Legend"
  ) +
  theme_minimal()

x= seq(min(field_goals$ydl), max(field_goals$ydl), len = 140)
linedata = data.frame(ydl = x, kq = mean(field_goals$kq)) %>% 
  rename(ydl = x)

linedata$predictions = predict(model_3, newdata = linedata, type = "response")
actual_summary <- field_goals %>%
  group_by(ydl) %>%
  summarise(actual_fg = mean(fg_made))

ggplot() +
  geom_line(data = actual_summary, aes(x = ydl, y = actual_fg, color = "Actual FG%"), linewidth = 1) +
  geom_line(data = linedata, aes(x = ydl, y = predictions, color = "Predicted FG% (Model 3)"), linewidth = 1) +
  labs(
    title = "Predicted vs Actual FG% by Distance",
    x = "Yards to Goal",
    y = "Field Goal %",
    color = "Legend"
  ) +
  theme_minimal()

########
ncaab_results = read_csv("2025/labs/data/04_ncaab-results.csv.gz")
ncaab_team_info = read_csv("2025/labs/data/04_ncaab-teams.csv.gz")
#ncaab_results only from season 2024-2025
ncaab_results = ncaab_results %>% 
  filter(Season == 2023)
data = ncaab_results %>% 
  mutate(
    W = ifelse(WLoc == "H", 1, 0)
  ) 

for (team_id in (1101:1478)) {
  col_name <- paste0("team_", team_id)
  
  data[[col_name]] <- case_when(
    data$WTeamID == team_id & data$W == 1 ~ 1,
    data$LTeamID == team_id & data$W == 0 ~ 1,
    data$WTeamID == team_id & data$W == 0 ~ -1,
    data$LTeamID == team_id & data$W == 1 ~ -1,
    TRUE ~ 0
  )
}


data = data %>% 
  filter(WLoc != 'N')

matrix = data %>% 
  select(-c(1:9)) %>% 
  as.matrix()

model = glm(data$W ~ matrix, family = "binomial")
summary(model)

ratings = model$coefficients
ratings = as.data.frame(ratings, na.rm = TRUE)


# Step 1: Extract and clean coefficients
coef_df <- coef(model) |> 
  na.omit() |> 
  as.data.frame() |> 
  rownames_to_column(var = "term") |>
  rename(estimate = `na.omit(coef(model))`)

# Step 2: Order the terms by estimate
coef_df <- coef_df |> 
  arrange(desc(estimate)) 

coef_df <- coef_df |> 
  rename(team = term)

# Step 2: Extract numeric ID from "matrixteam_<ID>"
coef_df <- coef_df |> 
  mutate(TeamID = str_extract(team, "\\d+")) |>     # extract digits
  mutate(TeamID = as.integer(TeamID))               # convert to numeric

# Step 3: Join with team names
coef_df <- coef_df |> 
  left_join(ncaab_team_info, by = "TeamID")


coef_df <- coef_df %>%
  mutate(TeamName = fct_reorder(TeamName, estimate))

ggplot(coef_df, aes(x = TeamName, y = estimate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # Flip for readability
  labs(
    title = "Model Coefficients",
    x = "Term",
    y = "Estimate"
  ) +
  theme_minimal()


#purdue 1345
#uconn 1163


purdueW = 1/(1+exp(-model$coefficients["matrixteam_1345"]+
               model$coefficients["matrixteam_1163"]))




#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

####################
### PARK EFFECTS ###
####################

# load data
park_data = read_csv("2025/labs/data/05_park-effects.csv.gz")

base_avg = park_data %>% 
  group_by(PARK) %>%
  summarise(avg = mean(INN_RUNS))

#plot each park average and sort highest to least
park_data %>% 
  group_by(PARK) %>%
  summarise(avg = mean(INN_RUNS)) %>%
  arrange(desc(avg)) %>%
  ggplot(aes(x = reorder(PARK, -avg), y = avg)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Runs per Inning by Park",
       x = "Park",
       y = "Average Runs per Inning") +
  theme_minimal()


#order this histogram by average runs and coord flip
park_data %>% 
  group_by(DT_YR) %>%
  summarise(avg = mean(INN_RUNS)) %>%
  arrange(desc(avg)) %>%
  ggplot(aes(x = reorder(DT_YR, -avg), y = avg)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Runs per Inning by Year",
       x = "Year",
       y = "Average Runs per Inning") +
  theme_minimal()

n = nrow(park_data)
set.seed(3)
train_idx = sample(1:n, size = 0.8*n)

train = park_data %>% 
  slice(train_idx)

test = slice(park_data, -train_idx)

# make OT_YR and DT_YR and PARK factors in park_data
park_data$OT_YR = as.factor(park_data$OT_YR)
park_data$DT_YR = as.factor(park_data$DT_YR)
park_data$PARK = as.factor(park_data$PARK)

model1 = lm(INN_RUNS ~ OT_YR + DT_YR + PARK + 0, data = train)
model2 = lm(INN_RUNS ~  PARK + 0, data = train)

#add in predictons from the model as well as average runs per park
test = test %>% 
  mutate(pred_model1 = predict(model1, newdata = test)) %>% 
  mutate(pred_model2 = predict(model2, newdata = test))

RE_1 = 1- sd(test$pred_model1-test$INN_RUNS)/sd(test$INN_RUNS)
RE_2 = 1- sd(test$pred_model2-test$INN_RUNS)/sd(test$INN_RUNS)  

# now redoing on full data to make all predictions
model1 = lm(INN_RUNS ~ OT_YR + DT_YR + PARK + 0, data = park_data)
model2 = lm(INN_RUNS ~  PARK + 0, data = park_data)
park_data = park_data %>% 
  mutate(pred_model1 = predict(model1, newdata = park_data)) %>% 
  mutate(pred_model2 = predict(model2, newdata = park_data))

offcoef = as.data.frame(coef[c(1:90),])
mean_o = mean(offcoef$coef, na.rm = TRUE)

defcoef = as.data.frame(coef[c(91:179),])
mean_d = mean(defcoef$coef, na.rm = TRUE)

# add row names as variable names
coef$coef_name = rownames(coef)
coef

park_coef = coef %>% 
  filter(str_detect(coef_name, "PARK"))

park_coef$predicitions = mean_o + mean_d + park_coef$coef

park_coef %>% 
  ggplot(aes(x = reorder(coef_name, -predicitions), y = predicitions)) +
  geom_col() +
  coord_flip() +
  labs(title = "Predicted Runs per Inning by Park",
       x = "Park",
       y = "Predicted Runs per Inning") +
  theme_minimal()

park_coef$avg = avg$avg[-1]

park_coef$resids = park_coef$predicitions - park_coef$avg
park_coef %>% 
  ggplot(aes(x = reorder(coef_name, -resids), y = resids)) +
  geom_col() +
  coord_flip() +
  labs(title = "Residuals of Predicted Runs per Inning by Park",
       x = "Park",
       y = "Residuals") +
  theme_minimal()

#cleveland differs the most





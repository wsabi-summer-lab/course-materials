#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set working directory to wherever your data is
getwd()


##############
### PART 1 ###
##############

# load data
nba_four_factors = read_csv("../data/03_nba-four-factors.csv.gz")

head(nba_four_factors)

df <- nba_four_factors %>% 
  select(`EFG%`,`OPP EFG%`,`OREB%`,`DREB%`,`TOV%`,`OPP TOV %`,`FT Rate`,`OPP FT Rate`, `WIN%`) %>% 
  mutate(x1 =`EFG%`-`OPP EFG%`,
         x2 = `OREB%`-`DREB%`,
         x3 = `TOV%`-`OPP TOV %`,
         x4 = `FT Rate`-`OPP FT Rate`)
unique(colnames(df))

for (col in colnames(df)){
  print(paste("Summary of", col))
  print(summary(df[[col]]))
  
  print(paste("Standard deviation of", col))
  print(sd(df[[col]], na.rm = TRUE))
}
for (col in colnames(df)){
  print(
    ggplot(data=df, aes(x=.data[[col]]))+
      geom_histogram(fill="sky blue", color = "black")+
      theme_minimal()+
      ggtitle(paste("Histogram of", col))
  )
}

  

# Compute correlation matrix
cor_matrix <- cor(df, use = "pairwise.complete.obs")
#install.packages("corrplot")
library(corrplot)

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.cex = 0.8)

colnames(df)

model<-lm(`WIN%`~x1+x2+x3+x4, data = df)
model

dfstandard<-df %>% 
  mutate(wp=`WIN%`) %>% 
  select(wp,x1,x2,x3,x4) %>% 
  mutate(x1=(x1-mean(x1))/sd(x1),
         x2=(x2-mean(x2))/sd(x2),
         x3=(x3-mean(x3))/sd(x3),
         x4=(x4-mean(x4))/sd(x4))
#checking if standar
for (col in colnames(dfstandard)) {
  print(paste("Mean for column:", col))
  print(mean(dfstandard[[col]]))
  
  print(paste("ST Dev for column:", col))
  print(sd(dfstandard[[col]]))
}

model2<-lm(wp~x1+x2+x3+x4, data = dfstandard)
model2

df$residuals<-resid(model)
dfstandard$residuals<-resid(model2)

ROE1=1-sd(df$residuals)/sd(df$`WIN%`)
ROE2=1-sd(dfstandard$residuals)/sd(df$`WIN%`)

# Extract coefficients and their names
coef_vals <- abs(coef(model2))
coef_names <- names(coef_vals)

# Remove the intercept if you want (optional)
coef_vals_no_intercept <- coef_vals[-1]
coef_names_no_intercept <- coef_names[-1]

# Create a data frame for plotting
coef_df <- data.frame(
  term = coef_names_no_intercept,
  estimate = coef_vals_no_intercept
)

# Basic bar plot using base R
barplot(coef_df$estimate, names.arg = coef_df$term,
        , # rotate labels for readability
        col = "skyblue",
        main = "Model Coefficients Magnitude",
        ylab = "Coefficient Estimate",
        cex.names=0.8)

ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() + # horizontal bars for better label fit
  labs(title = "Model Coefficients", x = "Term", y = "Estimate") +
  theme_minimal()

## the order is x1,x3,x4,x2, which is scoring, protecting, attacking, crashing
##the models are the same mathematically as we proved in class


##############
### PART 2 ###
##############

library(tidyverse)
library(splines)

# Load data
punts <- read_csv("../data/03_punts.csv.gz")

# Set seed for reproducibility
set.seed(123)

# Create train/test split indices (80% train)
train_indices <- sample(seq_len(nrow(punts)), size = 0.8 * nrow(punts))

# Split data
train_data <- punts[train_indices, ]
test_data <- punts[-train_indices, ]

# Summaries and plots can be done on full data or train_data as needed

# Fit first model on train data
puntmodel <- lm(next_ydl ~ ydl + I(ydl^2) + pq, data = train_data)

# Predict on test data
test_data$predicted_next_ydl <- predict(puntmodel, newdata = test_data)

# Plot Actual vs Predicted on test data
ggplot(test_data, aes(x = predicted_next_ydl, y = next_ydl)) +
  geom_point(alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Predicted next_ydl", y = "Actual next_ydl",
       title = "Actual vs Predicted next_ydl (Model 1, Test Data)") +
  theme_minimal()

# Calculate Residuals-based measure of fit on test data
ROE1 <- 1 - sd(test_data$next_ydl - test_data$predicted_next_ydl) / sd(test_data$next_ydl)
print(ROE1)

# Fit spline model on train data
puntmodel2 <- lm(next_ydl ~ bs(ydl, df = 4) + pq, data = train_data)

# Predict on test data
test_data$predicted_next_ydl2 <- predict(puntmodel2, newdata = test_data)

# Plot Actual vs Predicted on test data
ggplot(test_data, aes(x = predicted_next_ydl2, y = next_ydl)) +
  geom_point(alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Predicted next_ydl", y = "Actual next_ydl",
       title = "Actual vs Predicted next_ydl (Model 2, Test Data)") +
  theme_minimal()

library(dplyr)

# Calculate residuals for Model 1 on test_data
test_data <- test_data %>%
  mutate(residuals_model1 = next_ydl - predicted_next_ydl)

# Average residual per punter (punter quality)
punter_quality <- test_data %>%
  group_by(punter) %>%
  summarize(avg_residual = mean(residuals_model1, na.rm = TRUE)) %>%
  ungroup()

# Create categories based on quantiles (you can adjust breaks)
punter_quality <- punter_quality %>%
  mutate(quality_group = ntile(avg_residual, 3)) %>%  # 3 groups: 1=low, 3=high
  mutate(quality_group = factor(quality_group,
                                labels = c("Low Quality", "Medium Quality", "High Quality")))

# Join quality group back to test_data
test_data <- test_data %>%
  left_join(punter_quality %>% select(punter, quality_group), by = "punter")

ggplot(test_data, aes(x = ydl, y = predicted_next_ydl)) +
  geom_point(alpha = 0.3, color = "blue") +
  geom_smooth(method = "loess", color = "darkblue") +
  facet_wrap(~quality_group) +
  labs(title = "Model 1: Expected Next Yardline vs Current Yardline\nby Punter Quality",
       x = "Current Yardline (ydl)",
       y = "Expected Next Yardline") +
  theme_minimal()

ggplot(test_data, aes(x = ydl, y = predicted_next_ydl2)) +
  geom_point(alpha = 0.3, color = "red") +
  geom_smooth(method = "loess", color = "darkred") +
  facet_wrap(~quality_group) +
  labs(title = "Model 2: Expected Next Yardline vs Current Yardline\nby Punter Quality",
       x = "Current Yardline (ydl)",
       y = "Expected Next Yardline") +
  theme_minimal()

# Calculate Residuals-based measure of fit on test data
ROE2 <- 1 - sd(test_data$next_ydl - test_data$predicted_next_ydl2) / sd(test_data$next_ydl)
print(ROE2)

# For Model 1 residuals (linear + quadratic)
test_data <- test_data %>%
  mutate(residuals_model1 = next_ydl - predicted_next_ydl)

# For Model 2 residuals (spline)
test_data <- test_data %>%
  mutate(residuals_model2 = next_ydl - predicted_next_ydl2)

# Rank punters by average residuals for Model 1
yoe_model1 <- test_data %>%
  group_by(punter) %>%
  summarize(avg_yards_over_expected = mean(residuals_model1, na.rm = TRUE),
            n_punts = n()) %>%
  arrange(desc(avg_yards_over_expected))

# Rank punters by average residuals for Model 2
yoe_model2 <- test_data %>%
  group_by(punter) %>%
  summarize(avg_yards_over_expected = mean(residuals_model2, na.rm = TRUE),
            n_punts = n()) %>%
  arrange(desc(avg_yards_over_expected))

# View top punters by yards over expected in Model 1
print(yoe_model1)

# View top punters by yards over expected in Model 2
print(yoe_model2)

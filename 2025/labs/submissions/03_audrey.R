install.packages(tidyverse)
library(tidyverse) 
nba_data <- read.csv("03_nba-four-factors.csv.gz", stringsAsFactors = FALSE)

head(nba_data)
names(nba_data)

#Task 1
nba_ffactors <- nba_data %>%
  select(W,Shooting.Factor,Crashing.Factor,Protecting.Factor,Attacking.Factor)
summary(nba_ffactors)
install.packages("skimr")
library(skimr)
skim(nba_ffactors)
 #Win Plot
ggplot(nba_numeric,aes(x=W))+geom_histogram(binwidth = 1, fill = "lightskyblue",color = "white")+theme_minimal()
ggplot(nba_numeric, aes(x = Shooting.Factor)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") + 
  theme_minimal()
ggplot(nba_numeric, aes(x = Crashing.Factor)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") + 
  theme_minimal()
ggplot(nba_numeric, aes(x = Protecting.Factor)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") + 
  theme_minimal()
ggplot(nba_numeric, aes(x = Attacking.Factor)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") + 
  theme_minimal()
cor(nba_numeric,use ="complete.obs")
install.packages("corrplot")
library(corrplot)
corrplot(cor(nba_numeric, use = "complete.obs"), method = "circle")
corrplot(cor(nba_numeric, use = "complete.obs"), method = "color",
         type = "upper",    
         addCoef.col = "black",  
         tl.col = "black",  
         tl.srt = 45,       
         number.cex = 0.8,  
         col = colorRampPalette(c("red", "white", "blue"))(200))   
install.packages("ggcorrplot")
library(ggcorrplot)

cor_matrix <- cor(nba_numeric, use = "complete.obs")

ggcorrplot(cor_matrix, 
           method = "square", 
           type = "upper", 
           lab = TRUE,        
           lab_size = 4, 
           colors = c("red", "white", "blue"),
           title = "Correlation Matrix of NBA Four Factors",
           ggtheme = theme_minimal())

#Task 2 
lm1 <- lm(W ~ Shooting.Factor + Crashing.Factor + Protecting.Factor + Attacking.Factor, data = nba_numeric)
summary(lm1)
nba_standardized <- nba_numeric %>% 
  mutate(across(c(Shooting.Factor, Crashing.Factor, Protecting.Factor, Attacking.Factor), scale))
lm2 <- lm(W ~ Shooting.Factor + Crashing.Factor + Protecting.Factor + Attacking.Factor, data = nba_standardized)
summary(lm2)

set.seed(123)  
sample_rows <- sample(1:nrow(nba_numeric), size = 0.8 * nrow(nba_numeric))

train_data <- nba_numeric[sample_rows, ]
test_data  <- nba_numeric[-sample_rows, ]
coef_lm2 <- summary(lm2)$coefficients[-1, 1]
coef_df <- data.frame(
  Factor = c("Shooting", "Crashing", "Protecting", "Attacking"),
  Coefficient = coef_lm2
)

ggplot(coef_df, aes(x = reorder(Factor, Coefficient), y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Relative Importance of Four Factors (Standardized)",
       x = "Factor",
       y = "Coefficient") +
  scale_fill_manual(values = c("red", "darkgreen"), guide = FALSE) +
  theme_minimal()

lm1_train <- lm(W ~ Shooting.Factor + Crashing.Factor + Protecting.Factor + Attacking.Factor, data = train_data)
pred_test_lm1 <- predict(lm1_train, newdata = test_data)
rmse_lm1 <- sqrt(mean((test_data$W - pred_test_lm1)^2))
print(paste("Out-of-sample RMSE for lm1:", rmse_lm1))


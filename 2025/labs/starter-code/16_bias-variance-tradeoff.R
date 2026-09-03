#############
### SETUP ###
#############

# install.packages(c("ggplot2", "glmnet", "tidyverse"))
library(ggplot2)
library(glmnet)
library(tidyverse)

# set seed
set.seed(16)

####################
### PARK EFFECTS ###
####################

# load park effects data
park_effects = read_csv("../data/16_park-effects.csv.gz")
# preview data
head(park_effects)
#############
### SETUP ###
#############

# install.packages(c("ggplot2", "pdp", "ranger", "tidyverse", "vip", "xgboost"))
library(ggplot2)
library(pdp)
library(ranger)
library(tidyverse)
library(vip)
library(xgboost)

# set seed
set.seed(18)

###########################
### NFL WIN PROBABILITY ###
###########################

# read in data
nfl_data = read_csv("../data/18_nfl-wp.csv.gz")

# preview data
head(nfl_data)
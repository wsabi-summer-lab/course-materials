#############
### SETUP ###
#############

# install.packages(c("ggplot2", "rstan", "tidyverse"))
library(ggplot2)
library(rstan)
library(tidyverse)

# set seed
set.seed(14)

#################
### NFL GAMES ###
#################

# load NFL games data from 2018-2023
nfl_data = read_csv("../data/14_nfl-games.csv.gz")
# preview data
head(nfl_data)
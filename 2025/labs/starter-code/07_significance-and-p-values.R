#############
### SETUP ###
#############

# install.packages(c("ggplot2", "splines", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(7)

#########################
### PERMUTATION TESTS ###
#########################

# load data
diving_data = read_csv("../data/07_diving.csv.gz")


############################
### PARAMETRIC INFERENCE ###
############################

# load data
mlb_data = read_csv("../data/07_tto.csv.gz")
#############
### SETUP ###
#############

# install.packages(c("ggplot2", "nnet", "splines", "tidyverse"))
library(ggplot2)
library(nnet)
library(splines)
library(tidyverse)

# set seed
set.seed(6)

#######################
### EXPECTED POINTS ###
#######################

# load data
nfl_data = read_csv("../data/06_expected-points.csv.gz")

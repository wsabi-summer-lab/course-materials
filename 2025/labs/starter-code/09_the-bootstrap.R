#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(9)

#######################
### NBA FREE THROWS ###
#######################

# load data
nba_players = read_delim("../data/09_nba-free-throws.csv.gz", delim = ";")
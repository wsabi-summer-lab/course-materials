#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

##############
### PART 1 ###
##############

# load data
mlb_team_seasons = read_csv("../02_mlb-team-seasons.csv.gz")

##############
### PART 2 ###
##############

# load data
mlb_payrolls = read_csv("../02_mlb-payrolls.csv.gz")
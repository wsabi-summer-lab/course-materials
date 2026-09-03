#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(4)

##############
### PART 1 ###
##############

# load data
field_goals = read_csv("../data/04_field-goals.csv.gz")

##############
### PART 2 ###
##############

# load data
ncaab_results = read_csv("../data/04_ncaab-results.csv.gz")
ncaab_team_info = read_csv("../data/04_ncaab-teams.csv.gz")
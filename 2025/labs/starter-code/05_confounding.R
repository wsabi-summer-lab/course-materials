#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(5)

####################
### PARK EFFECTS ###
####################

# load data
park_data = read_csv("../data/05_park-effects.csv.gz")
#############
### SETUP ###
#############

# install.packages(c("ggplot2", "splines", "tidyverse"))
library(ggplot2)
library(splines)
library(tidyverse)

# set seed
set.seed(12)

##########################
### NBA PLAYER QUALITY ###
##########################

# load data
nba_data = read_csv("../data/12_nba-box-scores.csv.gz")
# preview data
head(nba_data)

##########################
### NFL KICKER QUALITY ###
##########################

# load data
kick_data = read_csv("../data/12_field-goals.csv.gz")
# preview data
head(kick_data)
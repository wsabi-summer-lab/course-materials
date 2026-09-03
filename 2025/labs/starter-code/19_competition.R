#############
### SETUP ###
#############

# install.packages(c("tidyverse"))
library(tidyverse)

# set unique 5-digit seed
set.seed(your_seed)

####################
### SPOTIFY DATA ###
####################

# read in data
spotify_data = read_csv("../data/19_spotify-train.csv.gz")

# preview data
head(spotify_data)
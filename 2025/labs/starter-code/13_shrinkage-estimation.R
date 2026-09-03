#############
### SETUP ###
#############

# install.packages(c("ggplot2", "tidyverse"))
library(ggplot2)
library(tidyverse)

# set seed
set.seed(13)

####################
### GOLF PUTTING ###
####################

# load training data (first half of season)
putts_train = read_csv("../data/13_putts-train.csv.gz")
# preview data
head(putts_train)

# load test data (second half of season)
putts_test = read_csv("../data/13_putts-test.csv.gz")
# preview data
head(putts_test)
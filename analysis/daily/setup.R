library(ggplot2)
library(ggthemes)
library(tswgewrapped)
source("../../R/preprocessing.R")
source("../../R/helpers.R")
# data import

china <- preprocess("../../data/")
bj <- china$BeijingPM_$PM_US
bjs <- resample(bj)
bjus <- bjs$day
bjus
# split
train <- window(bjus, end = 5)
test <- window(bjus, start = 5)

# clean up
rm(china, bj, bjus, bjs)


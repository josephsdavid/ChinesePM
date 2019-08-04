library(forecast)
library(ggplot2)
library(ggthemes)

## setup

source("../../R/preprocessing.R")
source("../../R/helpers.R")
china <- preprocess("../../data/")
beijing <- china$BeijingPM_$PM_US
bj <- resample(beijing)$day
bj <- msts(bj, seasonal.periods = c(7,365))
train <- window(bj, end = 5)
test <- window(bj, start = 5)

# gc
rm(china, beijing, bj)

# First model: tbats model

bjbats <- tbats(train, use.parallel = TRUE, num.cores=11)

load("tbats.Rda")
forecast::forecast(bjbats, h = 365)-> batFor

bat <- as.fore(batFor)
scores(bat)
#       MAPE        ASE Conf.Score 
#   123.8026  9542.3830    77.0000 
autoplot(bat)

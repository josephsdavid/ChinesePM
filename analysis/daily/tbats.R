library(forecast)
library(ggplot2)
library(ggthemes)

## setup

source("../../R/preprocessing.R")
source("../../R/helpers.R")
china <- preprocess("../../data/")
beijing <- china$BeijingPM_$PM_US
bj <- resample(beijing)$day
bj <- msts(bj, seasonal.periods = c(7,365.25))
train <- window(bj, end = 5)
test <- window(bj, start = 5)

# gc
rm(china, beijing, bj)

# First model: tbats model

bjbats <- tbats(train, use.parallel = TRUE, num.cores=11)
save(bjbats, file = "tbats.Rda")
load("tbats.Rda")

batF <- newFore(obj = bjbats,  test, h = 365)
autoplot(batF)
forbats <- as.fore(batF)
options(scipen = 999)
autoplot(forbats)
scores(forbats)
#          MAPE           ASE    Conf.Score 
#      81.37734 2602563.60211      75.00000 

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
rm(china, beijing)

# first we set up the fourier representation of the time series
expansion <- fourier(train, K = c(3,100))
expansion2 <- fourier(test, K = c(3,100))
str(expansion2)

mseadyn <- beep(auto.arima(train, 
                        seasonal = FALSE, 
                        lambda = 0, 
                        xreg = expansion))
save(mseadyn, file = "mseadyn.Rda")
load("mseadyn.Rda")
mseafor <- newFore(mseadyn, newdata = test, xreg =expansion2, h = 1)
autoplot(mseafor)
mf <- as.fore(mseafor)
autoplot(mf)
options(scipen = 999)
scores(mf)
#          MAPE           ASE    Conf.Score 
#      84.40622 2734198.10108      95.00000 

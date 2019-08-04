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
rm(china, beijing)

# first we set up the fourier representation of the time series
expansion <- fourier(train, K = c(3,100))

mseadyn <- beep(auto.arima(train, 
                        seasonal = FALSE, 
                        lambda = 0, 
                        xreg = expansion))
save(mseadyn, file = "mseadyn.Rda")
load("mseadyn.Rda")
mseafor <- beep( forecast(mseadyn, h = 1, xreg = expansion) )
autoplot(mseafor) 
mseas <- as.fore(mseafor)
scores(mseas)
scores(bat)
autoplot(mseas)

load("forecasts.Rda")
scores(mseas)

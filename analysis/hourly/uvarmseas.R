source("../../R/preprocessing.R")
source("../../R/helpers.R")
library(forecast)
library(ggplot2)
library(ggthemes)
bj <- preprocess("../../data/")
bj <- bj$BeijingPM_ %>>% as.data.frame
uvar <- bj$PM_US.Post
uvar <- uvar %>>% forecast::tsclean() %>>% abs
trainU <- window(uvar, start = 6,end = c(6, 8760-48))
testU <- window(uvar, start = c(6, 8760-48))[-1]
toMsts <- function(x){
  msts(x, seasonal.periods = c(24,24*7, 8760))
}

trainU <- toMsts(trainU)
testU <- toMsts(testU)

## Expand the things
trainExpand <- fourier(trainU, K = c(10,20,100))
testExpand <- fourier(testU, K = c(10,20,100))
str(testExpand)
library(dplyr)
24*7
testExpand %>% data.frame %>% select(ends_with("24")) -> shortEx
testExpand %>% data.frame %>% select(ends_with("168")) -> midEx
testExpand %>% data.frame %>% select(ends_with("8760")) -> longEx
pls <- data.frame(rowSums(shortEx), rowSums(midEx), rowSums(longEx), testU)
par(mfrow = c(2,2))
lapply(pls, plot, type = "l")
par(mfrow = c(1,1))







# train the model

mseaDay <- auto.arima(trainU, xreg = trainExpand, seasonal = FALSE, lambda = 0)
beepr::beep(0)
save(mseaDay, file = "mseaday.Rda")
mseasFor <- forecast(mseaDay, xreg = trainExpand)
autoplot(mseasFor)
autoplot.fore
trainExpand[(nrow(trainExpand)-71):(nrow(trainExpand)),]
mseasFor <- forecast(mseaDay, xreg = trainExpand[(nrow(trainExpand)-71):(nrow(trainExpand)),])
save(mseasFor, file = "mseasfor.Rda")
autoplot(mseasFor)
aut
str(mseasFor)
plot(mseasFor$mean[1:72])
str(mseasFor$upper)
plot(mseasFor$mean[1:72], type = "l")
autoplot(as.fore(mseasFor)) 
scores(as.fore(mseasFor))
#       MAPE        ASE Conf.Score 
#   117.2476 18419.8091    20.0000 
ASE(mseasFor$mean, testU)
str(mseasFor$upper)
as.fore(mseasFor) %>% scores.fore
load("tbats.Rda")
load("classical.Rda")
autoplot(as.fore(mseasFor)) + autolayer(ts(batF$mean), series ="tbats" ) + autolayer(ts(seafor$f), series = "ARUMA", color = "red")
autoplot(mseaDay)
scores(as.fore(msetran)) %>% t %>% data.frame
scores(as.fore(batF))

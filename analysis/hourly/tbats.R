source("../../R/preprocessing.R")
source("../../R/helpers.R")
library(forecast)
library(ggplot2)
library(ggthemes)
bj <- preprocess("../../data/")
bj <- bj$BeijingPM_ %>>% as.data.frame
uvar <- bj$PM_US.Post
uvar <- uvar %>>% forecast::tsclean() %>>% abs
trainU <- window(uvar, end = c(6, 8760-48))
testU <- window(uvar, start = c(6, 8760-48))[-1]
toMsts <- function(x){
  msts(x, seasonal.periods = c(24,24*7, 8760))
}

trainU <- toMsts(trainU)
testU <- toMsts(testU)


bjbats <- tbats(trainU, use.parallel = TRUE, num.cores=11)

batF <- forecast(bjbats, h = 24*3)
autoplot(batF)
str(batF)
batF$mean
forecast:::autoplot.forecast
str(batF)
autoplot(as.fore(batF))
scores(as.fore(batF))
#       MAPE        ASE Conf.Score 
#   255.8904 22434.5199    43.0000 
splitForBag.fore <- function(pred){
  val <- pred[1:48]
  test <- pred[49:72]
  return(list(val = val, test = test))
  
}

values <- splitForBag.fore(batF$mean)
batVal <- values$val
batTest <- values$test
save(bjbats, batF, batVal, batTest, file = "tbats.Rda")

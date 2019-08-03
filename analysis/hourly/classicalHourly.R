library(tswgewrapped)
library(ggplot2)
library(ggthemes)
library(cowplot)
library(forecast)
source("../R/helpers.R")
source("../R/preprocessing.R")

## Data Import: Univariate


china <- preprocess("../data/")
beij <- china$BeijingPM_

bj <- beij$PM_US %>>% tsclean %>>% abs %>>% 
  msts(seasonal.periods = c(24, (24*7), (24*365.25)))


# train test split
train <-  window(bj, end = 6)
test <- window(bj, start=6)


# sample plotts.wge

plotts.sample.wge(train)

# looking for yearly seasonality, data looks airline but we shall see

train %>>% ( difference(seasonal,.,8760) ) -> j
train %>>% diff(lag=8760) -> trainy
trainy %>>% ( difference(arima,.,1) ) -> trainyt

aicbic(trainyt)
plot(train)
plot(trainy)

plotts.sample.wge(trainy)
acf(trainy)

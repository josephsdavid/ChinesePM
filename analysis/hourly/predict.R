library(tidyverse)
library(forcats)
library(magrittr)
library(caret)
library(keras)
library(tswgewrapped)
library(forecast)
library(gbm)
source("../../R/preprocessing.R")
source("../../R/helpers.R")
bj <- preprocess("../../data/")
bj <- bj$BeijingPM_ %>>% as.data.frame
uvar <- bj$PM_US.Post
uvar <- uvar %>>% forecast::tsclean() %>>% abs

processData <- function(){
  bjnots <- purrr::discard(bj, is.ts)
  bjnots <- data.frame(lapply(bjnots, as.factor))
  bjts <- purrr::keep(bj, is.ts)
  bjts$PM_US.Post <- uvar # just because it is already cleaned
  alldata <<- cbind(bjts, bjnots)
}

processData()

hoursToDayNight <- function(df){
  df[["hour"]] %>% 
    fct_collapse(
                 night =  c(as.character(18:23), as.character(0:5)), 
                 day = as.character(6:17)) %>% as.numeric %>% `-`(1)
}
alldata$dayNight <- hoursToDayNight(alldata)

alldata[c(1:3,10:18)] <- NULL
alldata$DEWP <- NULL


# VAR first

makeExo <- function(n){
  day <- c(rep(0,5), rep(1,12), rep(0,7))
  return(matrix(rep(day,n), dimnames = list(NULL, "dayNight")))
}

varmod <-  VAR(alldata[-6], exogen = matrix(alldata[[6]], dimnames = list(NULL, "dayNight")), type = "both", p = 30)
varpred <- predict(varmod, n.ahead = 72, dumvar = makeExo(3))
xvars <- rapply(varpred$fcst, function(x) x[,1], how = "list") %>% 
  data.frame %>% dplyr::select(-PM_US.Post) 
xvars$dayNight <- c(makeExo(3))
predictions <- data.frame(VAR = varpred$fcst$PM[,1])

# next tswge
load("classical.Rda")
aruma <- fore.aruma.wge(
               x = alldata[[1]],
               theta = 0,
               s = 24*7,
               n.ahead = 72,
               phi = est7$phi, 
               plot = FALSE
)$f

predictions$ARUMA <- aruma

# now we do the mseasonalA

load("mseaday.Rda")

expansion3 <- fourier(msts(alldata$PM, seasonal.periods = c(24,24*7,8760)), K = c(10,20,100))

mseamod <- Arima(model = mseaDay, y = msts(alldata$PM, seasonal.periods = c(24,24*7,8760)), xreg = expansion3)


harmonF <- forecast(mseamod, xreg = fourier(msts(tail(alldata$PM,72), seasonal.periods = c(24,24*7, 8760)), K = c(10,20,100)))

predictions$harmonic  <- harmonF$mean

# tbats

load("tbats.Rda")
tbatmod <- tbats(model = bjbats,y = msts(alldata$PM, seasonal.periods = c(24,24*7,8760)) )
batfor <- forecast(tbatmod, h = 72)

predictions$TBATS <- batfor$mean

# nnetar
load("nnetar.Rda")
newnet <- nnetar(model = nnet, y = alldata[[1]], xreg = alldata[-1])
str(xvars)
netfor <- forecast(newnet, xreg = xvars)

predictions$nnet <- netfor$mean

# dreaded lstm
model_lstm <- load_model_hdf5("lstm.h5")

# dummy data for lstm
# it is ignored but input is required to be that shape because of how i made it
input <- data.matrix(data.frame(aruma, xvars))
# data setup
input <- scale(input, center = apply(input,2,mean), scale = apply(input,2,sd))
inputScale <- attr(input, 'scaled:scale')[1]
inputCent <- attr(input,'scaled:center')[1]
# descaler
descaleInput <- function(x){
  x*inputScale + inputCent 
}
# to array
input <- array(input, c(nrow(input),10,6))
lstmPred <- predict(model_lstm, input, n.ahead = 72)
lstmPred <- descaleInput(lstmPred)
predictions$LSTM <- lstmPred

# boost them
load("intervals.Rda")
load("ensemble.Rda")
load("pint.Rda")
pm <- pm*(ncol(valdf2)-1)
futuresight <- predict(finalModel, newdata = predictions, n.trees = 700) 
futuresight <- makeInterval(futuresight)
plot(futuresight$predicted, type = "o")
lines(futuresight$upper, type = "b", col = "blue")
lines(futuresight$lower, type = "b", col = "blue")

# make a final plot (well first a data frame)
numbah <- 72
past <- as.numeric(tail(alldata$PM, numbah))
tAdd <- length(past)
library(lubridate)
library(stringr)
col2date <- function(v){
  v <- as.character(v)
  v %<>% vapply(function(x) str_pad(x,2, pad = "0"), character(1))
  dte <- paste(v[1],v[2],v[3], sep = "-")
  tme <- paste0(v[4],":00:00")
  dtme <- paste(dte,tme)
  as_datetime(dtme)
}
start <- tail(bj[2:5], tAdd) %>% head(1) %>% col2date
pastdates <- start + hours(x = seq.int(0,tAdd-1))
futuredates <- tail(pastdates,1) + hours(x = seq.int(1, 72))

pastdf <- data.frame(time = pastdates, PM2.5 = past)
futuredf <- data.frame(time = futuredates, futuresight)
models <- data.frame(time = futuredates, predictions)
modeldf <- models %>% gather_(key = "model", value = "value", gather_cols = names(models)[-1])
connectdf <- data.frame(
                        time = c(tail(pastdf$time,1), head(futuredf$time,1)), 
                        val = c(tail(pastdf$PM2.5,1), head(futuredf$predicted,1)))
library(ggthemes)
ggplot() + 
  geom_line(data = pastdf, aes(x = time, y = PM2.5), color = "black") +
  geom_line(data = futuredf, aes(x = time, y = predicted), color = "blue") +
  geom_line(data = futuredf, aes(x = time, y = upper), linetype = 3003)+ 
  geom_line(data = futuredf, aes(x = time, y = lower), linetype = 3003) +
  geom_line(data = connectdf, aes(x = time, y = val), color = "blue") + 
  geom_ribbon(data = futuredf, aes(x = time,ymin = lower, ymax = upper), alpha = 0.1)+
  #   geom_line(data = modeldf, aes(x = time, y = value, color = model), alpha = 0.4) +
  theme_hc() + scale_color_calc()+
  ggtitle("Gradient Boosted Ensemble Forecast of PM2.5 in Beijing")+
  theme(plot.title = element_text(hjust = 0.5))

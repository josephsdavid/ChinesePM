# fun little beep function
beep <- function(f){
  out <- f; beepr::beep(0); out
}

# fix up outliers
clean <- forecast::tsclean

# Generator function to fix up sampling rate to something reasonable

change_samples <- function(n){
  function(xs){
    out <- unname(tapply(
                         xs,
                         (seq_along(xs)-1) %/% n,
                         sum
                         ))
    out <- ts(out, frequency = (8760/n))
    out
  }
}

# daily and weekly sampling, monthly is 4 weeks
to_daily <- change_samples(24)
to_weekly <- change_samples(24*7)
to_monthly <- change_samples(24*7*4)
to_season <- change_samples(24*(365/4))

# pipelining final cleaning and conversion, removing crappy NA
cleandays <- function(xs) {
  xs %>>% clean %>>% abs %>>% to_daily
}

cleanweeks <- function(xs) {
  xs %>>% clean %>>% abs %>>% to_weekly
}
cleanmonths <- function(xs) {
  xs %>>% clean %>>% abs %>>% to_monthly
}
cleanseas <- function(xs) {
  xs %>>% clean %>>% abs %>>% to_season
}
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
to_dailyNots <-   function(xs){
    out <- unname(tapply(
                         xs,
                         (seq_along(xs)-1) %/% 24,
                         Mode
                         ))
    out[1:1826]
  }

# hourly ts to daily df
dlist <- function(xs){
  xs %>>%purrr::keep(is.ts) -> tsbj
  xs %>>% purrr::discard(is.ts) -> tsnobj
  out <- append(lapply(tsbj,function(x) window(cleandays(x), end = 6)), lapply(tsnobj,to_dailyNots)) 
  as.data.frame(out)
}
# daily train test split

dfsplit <- function(df,n = 5){
  index <- length(window(df[[1]], end = n))
  tsdf <- df %>% purrr::keep(is.ts)
  nodf <- df %>% purrr::discard(is.ts)
  outs <- lapply(tsdf, function(x) window(x,end = n))
  outn <- nodf[1:index,]
  train<<-as.data.frame(append(outs,outn))
  tests <- lapply(tsdf, function(x) window(x, start = n))
  test <<- as.data.frame(append(tests, nodf[(index):nrow(nodf),]))
}

# resample a time series
resample <- function(xs){
  xs %>>% cleandays %>>% window(end = 6) -> day
  xs %>>% cleanweeks %>>% window(end = 6) -> week
  xs %>>% cleanmonths %>>% window(end = 6) -> month
  xs %>>% cleanseas %>>% window(end = 6) -> seas
  list(day = day, week = week, month = month, season = seas)
}


# some plts
seasonplot <- forecast::ggseasonplot
subseriesplot <- forecast::ggsubseriesplot
lagplot <- forecast::gglagplot

# forecast and assess

fore_and_assess <- function(...){
  f <- forecast(...)
  out <- assess(..., plot = FALSE)
  f$ASE <- out
  f
}

getASE <- function(model){
  accuracy(model)[2]^2
}


# plot test set vs predictions

.testPredPlot <- function(xs){
  p <- ggplot() + theme_hc() + scale_color_hc()

  doplot <- function(df){
    p <<- p + geom_line(data = df,
                        aes(
                            x = t,
                            y = ppm,
                            color = type
                            ))
  }
  out <- lapply(xs, doplot)
  out[[2]]
}

# some metrics, including a made up one
ASE <- function(predicted, actual){
  mean((actual -predicted)^2)
}

MAPE <- function(predicted, actual){
  100*mean(abs((actual-predicted)/actual))
}

checkConfint <- function(upper,lower, actual){
  (actual < lower) | (actual > upper)
}

confScore <- function(upper, lower, actual){
  rawScores <- ifelse(
    checkConfint(upper,lower,actual),
    1,
    0
  )
  sum(rawScores)
}

# generic for getting scores of a model
scores <- function(obj){
  UseMethod("scores")
}

# coerce something into a wge object
as.wge <- function(x) structure(x,class = "wge")

# method for getting scores of a wge object
scores.wge <- function(xs){
  mape <- MAPE(xs$f,testU)
  ase  <- ASE(xs$f,testU)
  confs <- confScore(xs$ul, xs$ll,testU)
  c("MAPE" = mape, "ASE" = ase, "Conf.Score" = confs)
}


# autoplot method for wge objects
autoplot.wge <- function(obj){
  testdf <- data.frame(type = "actual", 
                       t = seq_along(testU), 
                       ppm = as.numeric(testU))
  preddf <- data.frame(type = "predicted", 
                       t = seq_along(obj$f), 
                       ppm = as.numeric( obj$f ))
  confdf <- data.frame(upper = obj$ul, lower = obj$ll, t = seq_along(testU))
  dfl <- list(preddf,testdf)
  .testPredPlot(dfl) + geom_line(data = confdf,
                                 aes(x = t, y = lower, alpha = 0.2), linetype = 3003) + geom_line(data = confdf,
                                 aes(x = t, y = upper, alpha = 0.2), linetype = 3003) + guides(alpha = FALSE)
}


# forecast methods


as.fore <- function(x) structure(x, class = "fore")

autoplot.fore <- function(obj){
  testdf <- data.frame(type = "actual", 
                       t = seq_along(testU), 
                       ppm = as.numeric(testU))
  preddf <- data.frame(type = "predicted", 
                       t = seq_along(testU), 
                       ppm = as.numeric( obj$mean[1:length(testU)] ))

  confdf <- data.frame(upper = obj$upper[1:length(testU),2], lower = obj$lower[1:length(testU),2], t = seq_along(testU))
  dfl <- list(preddf,testdf)
  .testPredPlot(dfl)+ geom_line(data = confdf, aes(x = t, y = lower, alpha = 0.2), 
         linetype = 3003) + geom_line(data = confdf, aes(x = t, y = upper, alpha = 0.2), 
         linetype = 3003) + guides(alpha = FALSE)
 }

scores.fore <- function(obj){
  mape <- MAPE(as.numeric(obj$mean[1:length(testU)]), as.numeric(testU))
  ase <- ASE(as.numeric(obj$mean[1:length(testU)]), as.numeric(testU))
  confs <- confScore(as.numeric(obj$upper[1:length(testU),2]), as.numeric(obj$lower[1:length(testU),2]), as.numeric(testU))
  c("MAPE" = mape, "ASE" = ase, "Conf.Score" = confs)
}


# new forecasts using different objects (so we can predict off of new data)

newFore <- function(...){
  UseMethod("newFore")
}
newFore.Arima <- function(obj, newdata, xreg = NULL, h = 1){
  refit <- Arima(newdata, model = obj, xreg = xreg)
  forecast(refit, h = h, xreg = xreg)
}

newFore.bats <- function(obj, newdata, h = 1){
  refit <- tbats( model = obj,y  = newdata)
  forecast(refit,  h)
}



as.var <- function(x) structure(x, class = "var")
autoplot.var <- function(obj){
  us <- obj$fcst$PM_US.Post
  testdf <- data.frame(type = "actual", 
                       t = seq_along(testM$PM_US.Post), 
                       ppm = as.numeric(testM$PM_US.Post))
  preddf <- data.frame(type = "predicted", 
                       t = seq_along(testM$PM_US.Post), 
                       ppm = ( obj$fcst$PM_US.Post[,1]))
  dfl <- list(testdf,preddf)
  confdf <- data.frame(t = seq_along(testM$PM_US.Post), upper = us[,3], lower = us[,2])
  .testPredPlot(dfl)+ geom_line(data = confdf, aes(x = t, y = lower, alpha = 0.2), 
         linetype = 3003) + geom_line(data = confdf, aes(x = t, y = upper, alpha = 0.2), 
         linetype = 3003) + guides(alpha = FALSE)
}
scores.var <- function(obj){
  mape <- MAPE(obj$fcst$PM[,1], testM$PM_US)
  ase <- ASE(obj$fcst$PM[,1], testM$PM_US)
  us <- obj$fcst$PM_US.Post
  conf  <- confScore(upper = us[,3], lower = us[,2], testM$PM_US)
  c("ASE" = ase,"MAPE" = mape , "Conf.Score" = conf)
}


as.nfor <- function(x) structure(x, class = "nfor")

scores.nfor <- function(obj){
  mape <- MAPE(obj$mean, testM[[1]])
  ase <- ASE(obj$mean, testM[[1]])
  confs <- confScore(upper = obj$upper[,2], lower = obj$lower[,2], testM[[1]])
  c("ASE" = ase,"MAPE" = mape,  "Conf.Score" = confs)
}
autoplot.nfor <- function(obj){
  testdf <- data.frame(type = "actual", 
                       t = seq_along(testM[[1]]), 
                       ppm = as.numeric(testM[[1]]))
  preddf <- data.frame(type = "predicted", 
                       t = seq_along(testM[[1]]), 
                       ppm = as.numeric( obj$mean ))
  confdf <- data.frame(t = seq_along(testM[[1]]), upper = obj$upper[,2], lower = obj$lower[,2])
  dfl <- list(testdf,preddf)
  .testPredPlot(dfl)+ geom_line(data = confdf, aes(x = t, y = lower, alpha = 0.2), 
         linetype = 3003) + geom_line(data = confdf, aes(x = t, y = upper, alpha = 0.2), 
         linetype = 3003) + guides(alpha = FALSE)
}


scores.bag <- function(obj){
  ase <- ASE(obj, bagtest[[1]])
  mape <- MAPE(obj, bagtest[[1]])
  c("ASE" = ase, "MAPE" = mape)
}
as.bag <- function(x) structure(x, class = "bag")


as.keras <- function(x) structure(x, class = "keras")
autoplot.keras <- function(obj){
 testdf <- data.frame(type = "actual", 
                      t = seq_along(test[,1]), 
                      ppm = as.numeric(test[,1]))
 preddf <- data.frame(type = "predicted", 
                      t = seq_along(obj), 
                      ppm = as.numeric( obj))
 dfl <- list(preddf,testdf)
 testPredPlot(dfl)
}
scores.keras <- function(obj){
  c(ASE(obj, test[,1]), MAPE(obj, test[,1]))
}


scores.proph <- function(obj){
  c(
    ase = ASE(obj$yhat[-(1:nrow(train))], test[[1]]),
    mape = MAPE(obj$yhat[-(1:nrow(train))], test[[1]])
  )
}
as.proph <- function(x) structure(x, class = "proph")
autoplot.proph <- function(obj){
   testdf <- data.frame(type = "actual", 
                        t = seq_along(test[[1]]), 
                        ppm = as.numeric(test[[1]]))
   preddf <- data.frame(type = "predicted", 
                        t = seq_along(test[[1]]), 
                        ppm = as.numeric( obj$yhat[-(1:nrow(train))] ))
   dfl <- list(preddf,testdf)
   testPredPlot(dfl)
 }

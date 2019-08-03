source("setup.R")

# MORE EDA

plotts.sample.wge(train)

# Try out seasonal model

trainSea <- difference(seasonal, train, 365)
par(mfrow = c(2,1))

acf(train, lag.max = 400) # control
acf(trainSea, lag.max = 400)

par(mfrow = c(1,1))


# also try out wandering model

trainTrend <- difference(arima, train,1)
par(mfrow = c(1,1))


# lets get the aics of all of them


trainers <- list("ARMA" = train, "Seasonal" = trainSea,"ARIMA"=trainTrend)

aics <- lapply(trainers, aicbic, 0:10, 0:8)
pander(aics)
save(aics, file = "aics.Rda")


# estimation of parameters

estARMA <- estimate(train, 9, 8)
ljung_box(estARMA$res, 9,8)
plot_res(estARMA$res)

# looks good

estSeas <- estimate(trainSea,7,6)
ljung_box(estSeas$res,7,6)
plot_res(estSeas$res)

# ok

estTrend <- estimate(trainTrend,6,3)

ljung_box(estTrend$res, 6,3)
plot_res(estTrend$res)

# make some forecasts

armaCast <- forecast(type = aruma, 
                     x = train, 
                     phi = estARMA$phi,
                     theta = estARMA$theta, 
                     n.ahead = length(test)) %>>% as.wge

# this is crap, but for short term forecasts maybe cool

seaCast <- forecast(type = aruma, 
                    x = train, 
                    phi = estSeas$phi, 
                    theta = estSeas$theta, 
                    s = 365, 
                    n.ahead = length(test)) %>>% as.wge


# now we do trend, which should be a pretty bad forecast as well

trendCast <- forecast(type = aruma, 
                    x = train, 
                    phi = estTrend$phi, 
                    theta = estTrend$theta, 
                    d = 1, 
                    n.ahead = length(test)) %>>% as.wge

# Now let us assess our models

casts <- list("arma" = armaCast, "seasonal" = seaCast, "arima" = trendCast)

pander( lapply(casts, scores) )
# 
# 
#   * **arma**:
# 
#     ------------------------------
#      MAPE      ASE     Conf.Score
#     ------- --------- ------------
#      132.2   3621726       25
#     ------------------------------
# 
#   * **seasonal**:
# 
#     ------------------------------
#      MAPE      ASE     Conf.Score
#     ------- --------- ------------
#      159.4   7189987       33
#     ------------------------------
# 
#   * **arima**:
# 
#     ------------------------------
#      MAPE      ASE     Conf.Score
#     ------- --------- ------------
#      122.2   3641632       26
#     ------------------------------
# 
# 
# <!-- end of list -->
# 
# 
# NULL

autoplot(seaCast)

autoplot(trendCast)

autoplot(armaCast)

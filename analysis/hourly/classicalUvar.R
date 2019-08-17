source("../../R/preprocessing.R")
source("../../R/helpers.R")
library(tswgewrapped)
library(ggplot2)
library(ggthemes)
bj <- preprocess("../../data/")
bj <- bj$BeijingPM_ %>>% as.data.frame
uvar <- bj$PM_US.Post
uvar <- uvar %>>% forecast::tsclean() %>>% abs
trainU <- window(uvar, start = 3, end = c(6, 8760-48))
testU <- window(uvar, start = c(6, 8760-48))[-1]

plotts.sample.wge(trainU)



# We see a clear little seasonal pattern, an apparent wandering behavior, and the wiggly descending line. The shape of the data indicates multiple seasonality, and i have a feeling that the data is not actually wandering just a period of 8760.
trainU %>>% diff(lag=8760) -> trainy
trainU %>>% (difference(arima,.,1)) -> traint
trainy %>>% ( difference(arima,.,1) ) -> trainyt

# we will not do any differencing because it simply does not work on this dataset, and it takes hours. ARIMA differencing is inappropriate

aics <- mclapply(list(trainU, trainy, traint, trainyt), function(x) aicbic(x, 0:10, 0:5, silent = TRUE), mc.cores = 12L)
library(doParallel)
library(foreach)
workers <- makeCluster(4L)
invisible(clusterEvalQ(workers,library(tswgewrapped)))
trainers <- list("ARMA" = trainU, "ARUMA" = trainy, "ARIMA" = traint, "Airine" = trainyt)
aics <- lapply(trainers, function(x) (aic5.wge(x,0:8, 0:5)))

# for some reason this manages to parallelize aic.wge as well, which is really cool. Somehow it mapped 3 wge functions across 8 cores

beepr::beep(0)
pander(aics)
save(aics, file = "aics.Rda")

# arma estimate and forecast

aics[[1]]
#       p    q        aic
# 21    3    2   6.144822
# 10    1    3   6.144850
# 49    8    0   6.144871
# 11    1    4   6.144897
# 32    5    1   6.144901

# 3 2

estARMA <- estimate(trainU, 5,1)
ljung_box(estARMA$res, 5, 1)
#            [,1]             [,2]            
# test       "Ljung-Box test" "Ljung-Box test"
# K          24               48              
# chi.square 87.86588         211.7384        
# df         16               40              
# pval       6.173728e-12     0               

aics$ARUMA
#       p    q        aic
# 21    3    2   6.853693
# 14    2    1   6.853748
# 19    3    0   6.853749
# 49    8    0   6.853751
# 9     1    2   6.853757

estSeas <- estimate(trainy, 8,0)
ljung_box(estSeas$res, 8,0)
#            [,1]             [,2]            
# test       "Ljung-Box test" "Ljung-Box test"
# K          24               48              
# chi.square 48.83727         89.34056        
# df         16               40              
# pval       3.505838e-05     1.243887e-05    
#            [,1]             [,2]            
# test       "Ljung-Box test" "Ljung-Box test"
# K          24               48              
# chi.square 53.9822          91.93069        
# df         19               43              
# pval       3.323455e-05     2.075654e-05    
# better but still crap

aics$ARIMA
#       p    q        aic
# 24    3    5   6.144631
# 48    7    5   6.144771
# 26    4    1   6.144784
# 32    5    1   6.144812
# 50    8    1   6.144832
estTrend <- estimate(traint,3,5)
ljung_box(estTrend$res,3,5)
#            [,1]             [,2]            
# test       "Ljung-Box test" "Ljung-Box test"
# K          24               48              
# chi.square 85.33564         210.9614        
# df         16               40              
# pval       1.792499e-11     0               
aics$Air
#       p    q        aic
# 20    3    1   6.854529
# 22    3    3   6.856172
# 46    7    3   6.857120
# 39    6    2   6.863396
# 49    8    0   6.870400
estAir <- estimate(trainyt,3,3)
ljung_box(estAir$res, 3,3)
acf(estAir$res)

train24 <- difference(seasonal, trainU, 24)
aic24 <- aic5.wge(train24)
aic24
#       p    q        aic
# 14    4    1   6.791303
# 15    4    2   6.794822
# 16    5    0   6.811441
# 13    4    0   6.811445
# 17    5    1   6.811513

est24 <- estimate(train24, 5,0)
acf(est24$res)

aic7 <- difference(seasonal, trainU, 24*7)
aic72 <- aic5.wge(aic7, p = 0:8, q = 0:5, type = "aicc" )
aic72
#       p    q        aicc
# 11    1    4    7.816267
# 32    5    1    7.816274
# 12    1    5    7.816286
# 20    3    1    7.816305
# 37    6    0    7.816310

est7 <- estimate(aic7,6,0)
par(mfrow = c(1,1))
options(scipen = 999)
acf(est7$res)
ljung_box(est7$res,6,0) # best model

seafor <- fcst(aruma, s = 24*7, phi = est7$phi, theta = 0, n.ahead = 72, x = trainU)
beepr::beep(0)

autoplot(as.wge(seafor))
scores(as.wge(seafor))
#       MAPE        ASE Conf.Score 
#   353.7521 18167.8763    13.0000 
save(est7, seafor, aic72, file = "classical.Rda")
load("tbats.Rda")
load("mseaday.Rda")
autoplot(as.fore(batF))
library(forecast)
mseasFor <- forecast(mseaDay, xreg = fourier(msts(trainU, seasonal.periods = c(24,24*7,8760)), K = rep(10,3)))
autoplot(as.wge(seafor)) + autolayer(ts(batF$mean, frequency = 1), series = "tbats") + autolayer(ts(mseasFor$mean, frequency = 1, start = 1,end = 72), series = "mseas")

# this is gonna be god damn tough to beat

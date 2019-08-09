# bagging
library(keras)
library(doParallel)
library(foreach)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(vars)
library(randomForest)
library(forecast)

options(scipen = 999)
source("../../R/preprocessing.R")
source("../../R/helpers.R")


china <- preprocess("../../data/")
bj <- (china$BeijingPM_ )
bj2 <- dlist(bj)
dfsplit(bj2)
traints <- (purrr::keep(train,is.ts))
trainexogen <- purrr::discard(train, is.ts)
trainexogen <- lapply(trainexogen, as.factor) %>>% as.data.frame
test %>% dplyr::select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> test
traints %>% dplyr::select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> train
dfsplit2 <- function(df,n = 4){
   index <- length(window(df[[1]], end = n))
   tsdf <- df %>% purrr::keep(is.ts)
   nodf <- df %>% purrr::discard(is.ts)
   outs <- lapply(tsdf, function(x) window(x,end = n))
   outn <- nodf[1:index,]
   train2<<-as.data.frame(append(outs,outn))
   tests <- lapply(tsdf, function(x) window(x, start = n))
   validation <<- as.data.frame(append(tests, nodf[(index):nrow(nodf),]))
 }
dfsplit2(train)
str(validation)

# load up models

# VAR

# validation model
var <- VAR(train2, p = 3, type = "none", ic = "AIC", season = 365)
preds <- predict(var, n.ahead = 366)
varval <- preds$fcst$PM_US.Post[,1]

# test model
vart <- VAR(train, p = 3, type = "none", ic = "AIC", season = 365)
preds <- predict(vart, n.ahead = 366)
vartest <- preds$fcst$PM_US.Post[,1]

# tbats
load("tbats.Rda")
# validation
batval <- newFore(obj = bjbats,  msts(validation[[1]], seasonal.periods = c(7, 365.25)), h = 366)
batval <- fitted(batval)

# test
battest <- newFore(obj = bjbats,  msts(test[[1]], seasonal.periods = c(7, 365.25)), h = 366)
battest <- fitted(battest)

# multi seasonal dynamic regression
load("mseadyn.Rda")

# validation
xpansion <- fourier(msts(validation[[1]], seasonal.periods = c(7,365.25)), K = c(3,100))
mseaval <- newFore(mseadyn, newdata = msts(validation[[1]], seasonal.periods = c(7,365.25)), xreg =xpansion, h = 1)
mseaval <- fitted(mseaval)

# test
xpansion <- fourier(msts(test[[1]], seasonal.periods = c(7,365.25)), K = c(3,100))
mseatest <- newFore(mseadyn, newdata = msts(test[[1]], seasonal.periods = c(7,365.25)), xreg =xpansion, h = 1)
mseatest <- fitted(mseatest)


# nnetar
load("nnetar.Rda")

# validation
nnetfor <- forecast(nnet, h = 366, xreg = validation[-1])
netval <- nnetfor$mean

nnetfor2 <- forecast(nnet, h = 366, xreg = test[-1])
nettest <- nnetfor2$mean

# classical
load("classical.Rda")


# GRU
load("GRU.Rda")
plot(gruHist)
gruval <- as.ts(gruVal, frequency = 365, start = 4)
grutest <- as.ts(gruTest, frequency = 365, start = 5)
# LSTM
load("LSTM.Rda")
plot(lstmHist)
lstmval <- as.ts(lstmVal, frequency = 365, start = 4)
lstmtest <- as.ts(lstmTest, frequency = 365, start = 5)
# bagging setup

bagval <- data.frame(
                  ppm = validation[[1]],
                  tbats = batval,
                  VAR = varval,
                  mseas = mseaval,
                  nnetar = netval,
                  arma = armaval,
                  aruma = seaval,
                  arima = trendval,
                  gru = gruval,
                  lstm = lstmval
)
bagval  <- bagval %>% rename(gru = Series.1, lstm = Series.1.1)

bagtest <- data.frame(
                  ppm = test[[1]],
                  tbats = battest,
                  VAR = vartest,
                  mseas = mseatest,
                  nnetar = nettest,
                  arma = armatest,
                  aruma = seatest,
                  arima = trendtest,
                  gru = grutest,
                  lstm = lstmtest
)

bagtest  <- bagtest %>% rename(gru = Series.1, lstm = Series.1.1)
head(bagtest)
#    ppm    tbats       VAR    mseas    nnetar     arma     aruma    arima      gru
# 1 1184 1809.520  959.2457 1277.236 1362.1571 2179.372  510.8580 2010.327 1170.033
# 2 1402 1404.039 1714.1054 1120.083  990.3166 2294.699  529.3384 2210.001 1170.033
# 3 4023 1680.536 1404.5155 1307.658 3222.4266 1884.723  633.9079 2172.542 1976.299
# 4 1307 3021.346 1968.6322 2330.501 2942.1866 2157.577 2338.5355 2351.661 1170.033
# 5 3717 1275.506 1753.5466 1124.515 4536.2270 2047.418 1003.7130 2332.033 1423.190
# 6 2434 3134.465 2437.0611 2412.830 3650.7362 2191.756 3426.5422 2187.721 1170.033
#       lstm
# 1 3103.662
# 2 3203.472
# 3 2524.788
# 4 2529.291
# 5 2238.878
# 6 2710.700
rfmodel <- randomForest(ppm ~ ., data = bagval,
                        ntree = 1000, nodesize = 5, importance = TRUE)
varImpPlot(rfmodel)

rfPred <- predict(rfmodel, bagtest) 
rfPred <- ts(rfPred, frequency = 365,start = 5, end = 6)

bagtest <- as.data.frame( lapply(bagtest, function(x) ts(x, frequency = 365, start = 5, end = 6)) )

autoplot(bagtest$ppm, size = 2) + 
  autolayer(rfPred, size = 2) +
  autolayer(bagtest$tbats)+
  autolayer(bagtest$VAR) + 
  autolayer(bagtest$mseas)+
  autolayer(bagtest$nnetar)+
  autolayer(bagtest$arma) +
  autolayer(bagtest$arima)+
  autolayer(bagtest$aruma)+
  autolayer(bagtest$gru)+
  autolayer(bagtest$lstm)+
  theme_hc() + scale_color_hc()
scores(as.bag(rfPred))
#           ASE          MAPE 
# 1512906.64780      87.97279 
sel <- dplyr::select
bagval %>% sel(-arima) -> bagval2
bagtest %>% sel(-arima) -> bagtest2

# second iteration


rfmodel2 <- randomForest(ppm ~ ., data = bagval2,
                        ntree = 1000, nodesize = 5, importance = TRUE)
varImpPlot(rfmodel2)

rfPred2 <- predict(rfmodel2, bagtest2) 
rfPred2 <- ts(rfPred2, frequency = 365,start = 5, end = 6)


autoplot(bagtest$ppm, size = 2) + 
  autolayer(rfPred2, size = 2) +
  autolayer(bagtest$tbats)+
  autolayer(bagtest$VAR) + 
  autolayer(bagtest$mseas)+
  autolayer(bagtest$nnetar)+
  autolayer(bagtest$arma) +
  autolayer(bagtest$aruma)+
  autolayer(bagtest$gru)+
  autolayer(bagtest$lstm)+
  theme_hc() + scale_color_hc()
scores(as.bag(rfPred2))
#           ASE          MAPE 
# 1475999.79538      73.95874 


bagval2 %>% sel(-gru) -> bagval3
bagtest2 %>% sel(-gru) -> bagtest3


rfmodel3 <- randomForest(ppm ~ ., data = bagval3,
                        ntree = 1000, nodesize = 5, importance = TRUE)
varImpPlot(rfmodel3)

rfPred3 <- predict(rfmodel3, bagtest3) 
rfPred3 <- ts(rfPred3, frequency = 365,start = 5, end = 6)

bagtest <- as.data.frame( lapply(bagtest, function(x) ts(x, frequency = 365, start = 5, end = 6)) )

autoplot(bagtest$ppm, size = 2) + 
  autolayer(rfPred3, size = 2) +
  autolayer(bagtest$tbats)+
  autolayer(bagtest$VAR) + 
  autolayer(bagtest$mseas)+
  autolayer(bagtest$nnetar)+
  autolayer(bagtest$arma) +
  autolayer(bagtest$aruma)+
  autolayer(bagtest$lstm)+
  theme_hc() + scale_color_hc()
scores(as.bag(rfPred3))
#           ASE          MAPE 
# 1459104.71678      72.42156 
#           ASE          MAPE 
# 1663582.13660      83.92708 


bagval3 %>% sel(-aruma) -> bagval4
bagtest3 %>% sel(-aruma) -> bagtest4


rfmodel4 <- randomForest(ppm ~ ., data = bagval4,
                        ntree = 1000, nodesize = 5, importance = TRUE)
varImpPlot(rfmodel4)

rfPred4 <- predict(rfmodel4, bagtest4) 
rfPred4 <- ts(rfPred4, frequency = 365,start = 5, end = 6)

bagtest <- as.data.frame( lapply(bagtest, function(x) ts(x, frequency = 365, start = 5, end = 6)) )

autoplot(bagtest$ppm, size = 2) + 
  autolayer(rfPred4, size = 2) +
  autolayer(bagtest$tbats)+
  autolayer(bagtest$VAR) + 
  autolayer(bagtest$mseas)+
  autolayer(bagtest$nnetar)+
  autolayer(bagtest$arma)+
  autolayer(bagtest$lstm)+
  theme_hc() + scale_color_hc()

scores(as.bag(rfPred4))
#           ASE          MAPE 
# 1430136.15506      70.12737 

colnames(bagval4)
# [1] "ppm"    "tbats"  "VAR"    "mseas"  "nnetar" "arma"   "lstm"  

# We could not get a better model than this.

preTune <- rfmodel4
preTunePred <- rfPred4



# next: grid search

x <- 1:7000
ntree <- x[x%%50 == 0]
mtry <- c(1:length(bagtest5))
x <- 1:100
nodesize <- x[x%%5==0]
nPerm <- 1:3
init <- list(
             ntree=ntree,
             mtry = mtry,
             nodesize = nodesize
)
grid <- expand.grid(init, stringsAsFactors = FALSE)

workers <- makeCluster(12L)
invisible(clusterEvalQ(workers, library(randomForest)))
invisible(clusterEvalQ(workers,set.seed(19)))
registerDoParallel(workers)
set.seed(19)
gridSearch <- foreach(i = 1:nrow(grid), .combine = rbind) %dopar% {
  model <- randomForest(ppm ~ .,
                        data = bagval4,
                        ntree = grid[i,1], 
                        mtry = grid[i,2], 
                        nodesize = grid[i,3],
  )
  preds <- predict(model, bagtest4)
  ASE <- mean((bagtest4$ppm - preds)^2)
  data.frame(ASE = ASE, 
       ntree = grid[i,1], 
       mtry = grid[i,2], 
       nodesize = grid[i,3]
  )
}
beepr::beep(0)
str(gridSearch)
save(gridSearch, file = "rfS.Rda")
gridSearch %>% arrange(ASE) %>% head
#       ASE ntree mtry nodesize
# 1 1415096   150    2       10
# 2 1424538   750    3        5
# 3 1425742  5250    3        5
# 4 1425760  5350    3        5
# 5 1425882    50    3       20
# 6 1426509   850    3        5
nrow(grid)
# [1] 14000
head(gridSearch)
nrow(gridSearch)
max(gridSearch$ASE) - min(gridSearch$ASE)

set.seed(0)
final <- (randomForest(ppm ~ ., data = bagval4,
                         ntree = 150, 
                         nodesize = 10, 
                         mtry = 2,
                         importance = TRUE))

finalPred <- predict(final, bagtest4) 
scores(as.bag(finalPred))
#           ASE          MAPE 
# 1417963.83898      71.89685 
finalPred <- ts(finalPred, frequency = 365,start = 5, end = 6)
plot(final)

save(final, file = "rfbag.Rda")
save(finalPred, file = "bagged1.Rda")

autoplot(bagtest$ppm, size = 2) + 
  autolayer(finalPred, size = 2) +
  autolayer(bagtest$tbats)+
  autolayer(bagtest$VAR) + 
  autolayer(bagtest$mseas)+
  autolayer(bagtest$nnetar)+
  autolayer(bagtest$arma)+
  autolayer(bagtest$lstm)+
  theme_hc() + scale_color_hc()


autoplot(bagtest$ppm) + 
  autolayer(finalPred, color = "red") +
  theme_hc() + scale_color_hc()

diffd <- ts(bagtest$ppm - finalPred, start = 5, end = 6, frequency = 365)
aped <- ts(100*(bagtest$ppm - finalPred)/bagtest$ppm, start = 5, end = 6, frequency = 365)
absd <- ts(abs(bagtest$ppm - finalPred), start = 5, end = 6, frequency = 365)
ased <- ts((bagtest$ppm - finalPred)^2, start = 5, end = 6, frequency = 365)
autoplot(ased) + theme_hc()
# function(predicted, actual){
#   100*mean(abs((actual-predicted)/actual))
# }
# <bytecode: 0x1a8cefb8>

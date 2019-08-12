# RF bagging
library(keras)
library(doParallel)
library(foreach)
library(dplyr)
library(prophet)
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

# prophet

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
lstmVal$fitted
lstmval <- as.ts(lstmVal$fitted, frequency = 365, start = 4)
lstmtest <- as.ts(lstmTest$fitted, frequency = 365, start = 5)
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
                  lstm = lstmval
)

bagtest <- data.frame(
                  ppm = test[[1]],
                  tbats = battest,
                  VAR = vartest,
                  mseas = mseatest,
                  nnetar = nettest,
                  arma = armatest,
                  aruma = seatest,
                  arima = trendtest,
                  lstm = lstmtest
)

# 
head(bagtest)
#    ppm    tbats       VAR    mseas   nnetar     arma     aruma    arima     lstm
# 1 1184 1809.520  959.2457 1277.236 1339.205 2179.372  510.8580 2010.327 1443.134
# 2 1402 1404.039 1714.1054 1120.083 1072.267 2294.699  529.3384 2210.001 1535.414
# 3 4023 1680.536 1404.5155 1307.658 3281.698 1884.723  633.9079 2172.542 1917.625
# 4 1307 3021.346 1968.6322 2330.501 3064.481 2157.577 2338.5355 2351.661 1533.292
# 5 3717 1275.506 1753.5466 1124.515 4556.193 2047.418 1003.7130 2332.033 2794.444
# 6 2434 3134.465 2437.0611 2412.830 3650.694 2191.756 3426.5422 2187.721 2557.595
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
  autolayer(bagtest$lstm)+
  theme_hc() + scale_color_hc()
scores(as.bag(rfPred))
#           ASE          MAPE 
# 1501173.78431      90.58278 
# holy shit
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
  autolayer(bagtest$lstm)+
  theme_hc() + scale_color_hc()
scores(as.bag(rfPred2))
#           ASE          MAPE 
# 1431535.21879      67.72437 
# got damn


bagval2 %>% sel(-aruma) -> bagval3
bagtest2 %>% sel(-aruma) -> bagtest3


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
  autolayer(bagtest$lstm)+
  theme_hc() + scale_color_hc()
scores(as.bag(rfPred3))
#           ASE          MAPE 
# 1367400.85703      66.22863 
#           ASE          MAPE 
# 1381956.53668      66.18103 
#           ASE          MAPE 
# 1459104.71678      72.42156 
#           ASE          MAPE 
# 1663582.13660      83.92708 


autoplot(bagtest$ppm) + 
  autolayer(rfPred3) +
  theme_hc() + scale_color_hc()



colnames(bagval3)
# [1] "ppm"    "tbats"  "VAR"    "mseas"  "nnetar" "arma"   "lstm"  

# We could not get a better model than this.

preTune <- rfmodel3
preTunePred <- rfPred3

save(preTunePred, file = "rfpredgood.Rda")


# next: grid search

x <- 1:7000
ntree <- x[x%%50 == 0]
mtry <- c(1:length(bagtest3))
x <- 1:100
nodesize <- x[x%%5==0]
nPerm <- 1:3
init <- list(
             ntree=ntree,
             mtry = mtry,
             nodesize = nodesize
)
grid <- expand.grid(init, stringsAsFactors = FALSE)
workers <- makeCluster(8L)
invisible(clusterEvalQ(workers, library(randomForest)))
invisible(clusterEvalQ(workers,set.seed(19)))
registerDoParallel(workers)
gridSearch <- foreach(i = 1:nrow(grid), .combine = rbind) %dopar% {
  model <- randomForest(ppm ~ .,
                        data = bagval3,
                        ntree = grid[i,1], 
                        mtry = grid[i,2], 
                        nodesize = grid[i,3],
  )
  preds <- predict(model, bagtest3)
  ASE <- mean((bagtest3$ppm - preds)^2)
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
# 1 1317781    50    2       15
# 2 1357228   250    2        5
# 3 1367815  1600    2       10
# 4 1367874    50    2       20
# 5 1371829   400    2        5
# 6 1372623   300    2       10
nrow(grid)
nrow(gridSearch)
max(gridSearch$ASE) - min(gridSearch$ASE)
# [1] 725732.4

set.seed(0)
final <- (randomForest(ppm ~ ., data = bagval3,
                         ntree = 50, 
                         nodesize = 15, 
                         mtry = 2,
                         importance = TRUE))

finalPred <- predict(final, bagtest3) 
scores(as.bag(finalPred))
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


load("bagged1.Rda")
load("bags.Rda")

power <- function(n){
  function(x) x^n
}

square <- power(2)

.trainAndPredict <- function() {
  trained <- randomForest(ppm ~ ., data = bagval,
                         ntree = 50, 
                         nodesize = 15, 
                         mtry = 2)
  return(predict(trained, bagval))

}

.predMat <- function(n){
  preds <- matrix(rep(double(366),n),n)
  for (i in 1:n){
    preds[i,] <- .trainAndPredict()
  }
  return(preds)
}

.errorMat <- function(n){
  truth <- matrix(rep(bagval$ppm,n),n)
  return(truth - .predMat(n))
}

.errors <- function(n){
  (.errorMat(n))^2
}
.errAvgs <- function(n){
  sqrt(rowMeans(.errors(n)))
}

MakeParams <- function(n){
  out <- .errAvgs(n)
  return(c(mean = mean(out), sd = sd(out)))
}
setup <- MakeParams(10000)
setup
save(setup, file = "bagint.Rda")

MakeEnsembleInterval <- function(predicted){
  distance <- sum(setup)
  upper <- predicted + distance
  lower  <- predicted - distance
  return(
         data.frame(fitted = predicted, upper = upper, lower = lower)
  )
}


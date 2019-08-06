# bagging
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


# bagging setup

bagval <- data.frame(
                  ppm = validation[[1]],
                  tbats = batval,
                  VAR = varval,
                  mseas = mseaval,
                  nnetar = netval,
                  arma = armaval,
                  aruma = seaval,
                  arima = trendval
)


bagtest <- data.frame(
                  ppm = test[[1]],
                  tbats = battest,
                  VAR = vartest,
                  mseas = mseatest,
                  nnetar = nettest,
                  arma = armatest,
                  aruma = seatest,
                  arima = trendtest
)


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
  theme_economist() + scale_color_few()
scores(as.bag(rfPred))
#           ASE          MAPE 
# 1627238.33191      95.58204 
sel <- dplyr::select
bagval %>% sel(-aruma) -> bagval2
bagtest %>% sel(-aruma) -> bagtest2

# second iteration


rfmodel2 <- randomForest(ppm ~ ., data = bagval2,
                        ntree = 1000, nodesize = 5, importance = TRUE)
varImpPlot(rfmodel2)

rfPred2 <- predict(rfmodel2, bagtest2) 
rfPred2 <- ts(rfPred2, frequency = 365,start = 5, end = 6)

bagtest <- as.data.frame( lapply(bagtest, function(x) ts(x, frequency = 365, start = 5, end = 6)) )

autoplot(bagtest$ppm, size = 2) + 
  autolayer(rfPred2, size = 2) +
  autolayer(bagtest$tbats)+
  autolayer(bagtest$VAR) + 
  autolayer(bagtest$mseas)+
  autolayer(bagtest$nnetar)+
  autolayer(bagtest$arma) +
  autolayer(bagtest$arima)+
  theme_economist() + scale_color_few()
scores(as.bag(rfPred2))
#           ASE          MAPE 
# 1582612.04491      91.39309 


bagval2 %>% sel(-arima) -> bagval3
bagtest2 %>% sel(-arima) -> bagtest3


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
  theme_economist() + scale_color_few()
scores(as.bag(rfPred3))
#           ASE          MAPE 
# 1663582.13660      83.92708 


bagval3 %>% sel(-arma) -> bagval4
bagtest3 %>% sel(-arma) -> bagtest4


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
  theme_economist() + scale_color_few()
scores(as.bag(rfPred4))
#           ASE          MAPE 
# 1587319.75626      73.99608 
#          ASE         MAPE 
# 1678434.7941      80.2246 


bagval3 %>% sel(-VAR) -> bagval5
bagtest3 %>% sel(-VAR) -> bagtest5


rfmodel5 <- randomForest(ppm ~ ., data = bagval5,
                        ntree = 1000, nodesize = 5, importance = TRUE)
varImpPlot(rfmodel5)

rfPred5 <- predict(rfmodel5, bagtest5) 
rfPred5 <- ts(rfPred5, frequency = 365,start = 5, end = 6)

bagtest <- as.data.frame( lapply(bagtest, function(x) ts(x, frequency = 365, start = 5, end = 6)) )

autoplot(bagtest$ppm, size = 2) + 
  autolayer(rfPred5, size = 2) +
  autolayer(bagtest$tbats)+
  autolayer(bagtest$mseas)+
  autolayer(bagtest$nnetar)+
  autolayer(bagtest$arma) +
  theme_economist() + scale_color_few()
scores(as.bag(rfPred5))
#           ASE          MAPE 
# 1519627.28610      74.51625 
#           ASE          MAPE 
# 1588473.62314      80.85361 


bagval5 %>% sel(-arma) -> bagval6
bagtest5 %>% sel(-arma) -> bagtest6


rfmodel6 <- randomForest(ppm ~ ., data = bagval6,
                        ntree = 1000, nodesize = 5, importance = TRUE)
varImpPlot(rfmodel6)

rfPred6 <- predict(rfmodel6, bagtest6) 
rfPred6 <- ts(rfPred6, frequency = 365,start = 5, end = 6)

bagtest <- as.data.frame( lapply(bagtest, function(x) ts(x, frequency = 365, start = 5, end = 6)) )

autoplot(bagtest$ppm, size = 2) + 
  autolayer(rfPred6, size = 2) +
  autolayer(bagtest$tbats)+
  autolayer(bagtest$mseas)+
  autolayer(bagtest$nnetar)+
  theme_economist() + scale_color_few()
scores(as.bag(rfPred6))
#           ASE          MAPE 
# 1614809.18838      76.22955 

 # winner: number 5

preTune <- (randomForest(ppm ~ ., data = bagval5,
                         ntree = 1000, nodesize = 5, importance = TRUE))

varImpPlot(preTune)

prePred <- predict(preTune, bagtest5) 
prePred <- ts(prePred, frequency = 365,start = 5, end = 6)

bagtest <- as.data.frame( lapply(bagtest, function(x) ts(x, frequency = 365, start = 5, end = 6)) )

autoplot(bagtest$ppm, size = 2) + 
  autolayer(prePred, size = 2) +
  autolayer(bagtest$tbats)+
  autolayer(bagtest$mseas)+
  autolayer(bagtest$nnetar)+
  autolayer(bagtest$arma) +
  theme_economist() + scale_color_few()
scores(as.bag(prePred))


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
                        data = bagval5,
                        ntree = grid[i,1], 
                        mtry = grid[i,2], 
                        nodesize = grid[i,3],
  )
  preds <- predict(model, bagtest5)
  ASE <- mean((bagtest5$ppm - preds)^2)
  data.frame(ASE = ASE, 
       ntree = grid[i,1], 
       mtry = grid[i,2], 
       nodesize = grid[i,3]
  )
}
beepr::beep(0)
str(gridSearch)
gridSearch  <- gridSearch %>% arrange(ASE) %>% head
#       ASE ntree mtry nodesize
# 1 1503203   350    1        5
# 2 1505175   400    1        5
# 3 1505247   500    1        5
# 4 1505972   150    1        5
# 5 1506314   400    2        5
# 6 1506468   200    1        5
save(gridSearch, file = "rfS.Rda")
nrow(grid)
# [1] 14000
head(gridSearch)
nrow(gridSearch)
max(gridSearch$ASE) - min(gridSearch$ASE)

set.seed(6969)
final <- (randomForest(ppm ~ ., data = bagval5,
                         ntree = 350, 
                         nodesize = 5, 
                         mtry = 1,
                         importance = TRUE))
save(final, file = "rfbag.Rda")

finalPred <- predict(final, bagtest5) 
save()
scores(as.bag(finalPred))
#           ASE          MAPE 
# 1499160.06166      73.78823 
finalPred <- ts(finalPred, frequency = 365,start = 5, end = 6)
plot(final)

save(finalPred, file = "bagged1.Rda")
bagtest <- as.data.frame( lapply(bagtest, function(x) ts(x, frequency = 365, start = 5, end = 6)) )

autoplot(bagtest$ppm, size = 2) + 
  autolayer(finalPred, size = 2) +
  autolayer(bagtest$tbats)+
  autolayer(bagtest$mseas)+
  autolayer(bagtest$nnetar)+
  autolayer(bagtest$arma) +
  theme_economist() + scale_color_few()

# libraries
library(tidyverse)
library(magrittr)
library(caret)
library(keras)
library(tswgewrapped)
library(forecast)
source("mvarsetup.R")


# create validation set
colnames(trainM)
# [1] "PM_US.Post" "HUMI"       "PRES"       "TEMP"       "Iws"        "dayNight"  
N <- nrow(trainM)
n <- 72
valdf <- data.frame(ppm = trainM[(N-n+1):(N),1])
nrow(valdf)
# [1] 72
valstack <- trainM[1:(N-n),]
nrow(valstack)  - nrow(trainM)
# [1] 72
# excellent
ncol(valstack)
valstack[,1:5] %<>% lapply(function(x) ts(x,frequency = 8760))
str(valstack)

# create a test set
teststack <- data.frame(ppm = as.numeric(testM$PM))

# tswge forecast

load("classical.Rda")
teststack$ARUMA <- seafor$f
valwge <- fcst(
               type = aruma, 
               x = valstack[[1]],
               theta = 0,
               s = 24*7,
               n.ahead = 72,
               phi = est7$phi, 
               plot = FALSE
)
valdf$ARUMA <- valwge$f
valdf
plot(valdf[[1]], type = "l")
lines(valdf[[2]], col = "blue")

load("nnetar.Rda")

# nnetar next
teststack$nnet <-  nnetfor$mean
newnet <- nnetar(model = nnet, y = valstack[[1]], xreg = valstack[-1])
netval <- forecast(newnet, xreg = valstack[(N-2*n+1):(nrow(valstack)),-1])
valdf$nnet <- netval$mean
plot(valdf[[1]], type = "l")
lines(as.numeric(netval$mean), col = "red")

# now we do VAR
load("VAR.Rda")
scores.var
opred$fcst$PM[,1]
#           fcst      lower    upper        CI
#  [1,] 250.2510 208.498705 292.0034  41.75234
#  [2,] 244.2537 181.987416 306.5200  62.26630
#  [3,] 238.8646 162.329395 315.3998  76.53521
#  [4,] 229.5052 141.763556 317.2469  87.74167
#  [5,] 219.3299 122.630538 316.0293  96.69939
#  [6,] 210.6969 106.449524 314.9444 104.24742
#  [7,] 204.0722  93.441099 314.7034 110.63113
#  [8,] 199.8760  83.822541 315.9295 116.05348
#  [9,] 196.1378  75.545527 316.7300 120.59223
# [10,] 192.8323  68.274687 317.3900 124.55763
# [11,] 188.8195  60.831629 316.8074 127.98788
# [12,] 183.9010  52.736464 315.0655 131.16450
# [13,] 177.1592  43.166331 311.1521 133.99290
# [14,] 172.3066  35.750017 308.8633 136.55662
# [15,] 168.9284  30.103406 307.7535 138.82502
# [16,] 168.5071  27.674207 309.3399 140.83285
# [17,] 170.5134  27.913697 313.1132 142.59973
# [18,] 173.7948  29.602820 317.9868 144.19200
# [19,] 177.4309  31.763634 323.0982 145.66727
# [20,] 182.3130  35.258484 329.3675 147.05450
# [21,] 185.6617  37.292318 334.0311 148.36939
# [22,] 187.8046  38.200039 337.4092 149.60457
# [23,] 187.7192  36.918726 338.5197 150.80050
# [24,] 186.8632  34.889334 338.8371 151.97390
# [25,] 185.3083  32.213503 338.4030 153.09475
# [26,] 182.3167  28.140141 336.4934 154.17661
# [27,] 178.4515  23.245828 333.6572 155.20570
# [28,] 172.9618  16.734280 329.1894 156.22756
# [29,] 166.8425   9.661017 324.0239 157.18146
# [30,] 161.7264   3.666054 319.7867 158.06033
# [31,] 157.2587  -1.588182 316.1057 158.84693
# [32,] 153.4924  -6.054173 313.0390 159.54658
# [33,] 150.2775  -9.895115 310.4500 160.17258
# [34,] 146.9757 -13.758221 307.7096 160.73393
# [35,] 143.4197 -17.821448 304.6609 161.24118
# [36,] 139.5811 -22.117596 301.2797 161.69865
# [37,] 135.5794 -26.531128 297.6900 162.11055
# [38,] 132.4992 -29.978519 294.9768 162.47768
# [39,] 130.6342 -32.171734 293.4401 162.80593
# [40,] 130.1880 -32.912024 293.2881 163.10007
# [41,] 131.0499 -32.316925 294.4168 163.36685
# [42,] 132.7889 -30.819004 296.3968 163.60788
# [43,] 135.4572 -28.368424 299.2829 163.82564
# [44,] 138.4056 -25.616830 302.4281 164.02244
# [45,] 141.1289 -23.072259 305.3300 164.20114
# [46,] 142.8363 -21.532069 307.2046 164.36835
# [47,] 143.3251 -21.204648 307.8548 164.52974
# [48,] 142.6717 -22.018933 307.3623 164.69063
# [49,] 141.3060 -23.547715 306.1596 164.85367
# [50,] 139.0016 -26.014175 304.0174 165.01577
# [51,] 136.0041 -29.167775 301.1760 165.17190
# [52,] 132.5697 -32.746425 297.8857 165.31608
# [53,] 129.2076 -36.237349 294.6526 165.44498
# [54,] 126.5175 -39.041547 292.0766 165.55906
# [55,] 124.1648 -41.495058 289.8247 165.65987
# [56,] 122.2627 -43.487921 288.0133 165.75060
# [57,] 120.4358 -45.398919 286.2706 165.83476
# [58,] 118.6750 -47.239999 284.5900 165.91502
# [59,] 116.8146 -49.178677 282.8079 165.99329
# [60,] 114.8521 -51.217564 280.9217 166.06961
# [61,] 112.7823 -53.360692 278.9254 166.14303
# [62,] 111.3727 -54.839887 277.5852 166.21257
# [63,] 110.9372 -55.340528 277.2149 166.27771
# [64,] 111.6292 -54.709269 277.9676 166.33843
# [65,] 113.3210 -53.073373 279.7154 166.39437
# [66,] 115.6281 -50.816407 282.0726 166.44452
# [67,] 118.7615 -47.726529 285.2494 166.48799
# [68,] 122.0940 -44.430937 288.6189 166.52491
# [69,] 125.2754 -41.281341 291.8321 166.55673
# [70,] 127.5650 -39.020788 294.1509 166.58583
# [71,] 128.6639 -37.950809 295.2787 166.61475
# [72,] 128.5843 -38.061260 295.2298 166.64551
teststack$VAR <- opred$fcst$PM_US.Post[,1]
valVar <-  VAR(valstack[-6], exogen = matrix(valstack[[6]], dimnames = list(NULL, "dayNight")), type = "both", p = 30)

makeExo <- function(n){
  day <- c(rep(0,5), rep(1,12), rep(0,7))
  return(matrix(rep(day,n),dimnames = list(NULL, "dayNight")))
}
valVarP <- predict(valVar, n.ahead = 72, dumvar = makeExo(3))
plot(valVarP)
valdf$VAR <- valVarP$fcst$PM_US.Post[,1]

# tbats
load("tbats.Rda")
teststack$TBATS <- as.numeric(batF$mean)
valbat <- tbats(model = bjbats,y = msts(valstack$PM, seasonal.periods = c(24,24*7,8760)) )
valbatfor <- forecast(valbat, h = 72)
valdf$TBATS <- valbatfor$mean

# mseadyn
load("mseaday.Rda")
load("mseasfor.Rda")
teststack$harmonic <- mseasFor$mean
expansion3 <- fourier(msts(valstack$PM, seasonal.periods = c(24,24*7,8760)), K = c(10,20,100))

mseaval <- Arima(model = mseaDay, y = msts(valstack$PM, seasonal.periods = c(24,24*7,8760)), xreg = expansion3)
mseavalF <- forecast(mseaval, xreg = fourier(msts(valdf$ppm, seasonal.periods = c(24,24*7, 8760)), K = c(10,20,100)))
valdf$harmonic <- mseavalF$mean

# lstm
load("lstmInt.Rda")
str(lstmVal)
valdf$LSTM <- lstmVal[-1]
valdf %<>% lapply(as.numeric) %>% data.frame
ncol(valdf)
plot(valdf[[1]], t = "l")
lines(valdf[[2]], col = "red")
lines(valdf[[3]], col = "blue")
lines(valdf[[4]], col = "green")
lines(valdf[[5]], col = "purple")
lines(valdf[[6]], col = "orange")
lines(valdf[[7]], col = "turquoise")
legend(1,95, legend = names(valdf), col = c("black","red","blue","green","purple","orange","turquoise"), lty = 1:7)
teststack$LSTM <- lstmTest$f

# time to train some models
# we will combine with 4 methods:
# random forest
# xgb
# svm
# neural network
# rpart
# and pick the best result



# time series specific train method

# test scores of a model

scores.bag <- function(obj){
   ase <- ASE(obj, teststack[[1]])
   mape <- MAPE(obj, teststack[[1]])
   c("ASE" = ase, "MAPE" = mape)
 }
testModel <- function(model, ...){
  scores(as.bag(predict(model,... ,newdata = teststack)))
}
# library(doParallel)
# library(foreach)
# workers <- makeCluster(11L)
# registerDoParallel(workers)
# rf.pick <- train(
#                        ppm ~ .,
#                        data = valdf,
#                        method = "rf",
#                        trControl = trainMethod,
#                        tuneLength = 10,
#                        importance = TRUE
# )
# plot(teststack$ppm, type = "l")
# varImp(rf.pick) %>% plot
# testModel(rf.pick)
#        ASE       MAPE 
# 20043.1554   210.8731 

# valdf2 <- valdf %>% dplyr::select(-ARUMA)
# rf.pick2 <- train(
#                        ppm ~ .,
#                        data = valdf2,
#                        method = "rf",
#                        trControl = trainMethod,
#                        tuneLength = 10,
#                        importance = TRUE
# )
# varImp(rf.pick2)
# testModel(rf.pick2)
#        ASE       MAPE 
# 18097.9108   200.2199 
# No improvement here

## boosting

# gbm.pick <- train(
#                    ppm ~ .,
#                    data = valdf,
#                    method = "gbm",
#                    trControl = trainMethod,
#                    tuneLength = 10,
#                    importance = T
# )
library(randomForest)
rf1 <- randomForest(ppm~., data = valdf, importance = T)
varImpPlot(rf1)
testModel(rf1)
valdf2 <- valdf %>% dplyr::select(-ARUMA)
rf2 <- randomForest(ppm~., data = valdf2, importance = T)
varImpPlot(rf2)
testModel(rf2)
valdf3 <- valdf2 %>% dplyr::select(-VAR)
rf3 <- randomForest(ppm~., data = valdf3, importance = T)
varImpPlot(rf3)
testModel(rf3)

## gbm

library(gbm)
gb1 <- gbm(ppm~., data = valdf )
plot(gb1)
testModel(gb1, n.trees = 100)
#        ASE       MAPE 
# 13645.5778   121.7035 
plot(valdf$ppm, type = "l")
lines(predict(gb1, newdata = teststack, n.trees = 100), col = "red")
lines(valdf$L, col = "blue")

set.seed(1235)
gb2 <- gbm(ppm~., data = valdf2 )
testModel(gb2, n.trees = 100)
save(gb2,file = "gbm.Rda" )
gb3 <- gbm(ppm~., data = (valdf2 %>% dplyr::select(-VAR)), n.trees = 100)
testModel(gb3, n.trees = 100)

# baseline mean
ASE(rowMeans(teststack[-1]), teststack[[ 1 ]])
# [1] 16526.7

# tuning our gbm
args(gbm)
# function (formula = formula(data), distribution = "bernoulli", 
#     data = list(), weights, var.monotone = NULL, n.trees = 100, 
#     interaction.depth = 1, n.minobsinnode = 10, shrinkage = 0.1, 
#     bag.fraction = 0.5, train.fraction = 1, cv.folds = 0, keep.data = TRUE, 
#     verbose = FALSE, class.stratify.cv = NULL, n.cores = NULL) 
# NULL
n.trees <- 100*(1:10)
interaction.depth  <-  1:6
shrinkage <- 10^(-(1:3))
gbmGrid <- expand.grid(list(n.trees, interaction.depth, shrinkage))

ncol(gbmGrid)
nrow(gbmGrid)
head(gbmGrid)
registerDoSEQ()
#4
search <-  foreach(g = 1:nrow(gbmGrid),.combine = rbind) %do% {
  set.seed(503)
  model <- gbm(
               ppm ~ ., 
               data = valdf2, 
               n.trees = gbmGrid[g,1], 
               interaction.depth = gbmGrid[g,2], 
               shrinkage = gbmGrid[g,3],distribution ="gaussian"  
  )
  score <- testModel(model, n.trees = gbmGrid[g,1])
  return(data.frame(
                    ASE = score[1],
                    MAPE = score[2],
                    n.trees = gbmGrid[g,1],
                    interaction.depth = gbmGrid[g,2],
                    shrinkage = gbmGrid[g,3]
                    ))
  }
search %>% as_tibble %>% arrange(ASE)  # 
# # A tibble: 180 x 5
#      ASE  MAPE n.trees interaction.depth shrinkage
#    <dbl> <dbl>   <dbl>             <int>     <dbl>
#  1 5405.  142.     700                 2       0.1
#  2 5405.  142.     700                 3       0.1
#  3 5405.  142.     700                 4       0.1
#  4 5405.  142.     700                 5       0.1
#  5 5405.  142.     700                 6       0.1
#  6 5481.  142.     800                 2       0.1
#  7 5481.  142.     800                 3       0.1
#  8 5481.  142.     800                 4       0.1
#  9 5481.  142.     800                 5       0.1
# 10 5481.  142.     800                 6       0.1
# # … with 170 more rows
set.seed(503)
finalModel <- gbm(ppm~., data = valdf2, n.trees = 700, interaction.depth = 2,distribution = "gaussian")
testModel(finalModel, n.trees = 700)
plot(teststack$ppm, type = "l")
lines(predict(finalModel, newdata = teststack, n.trees = 700), col = "blue")
finalPred <- predict(finalModel, newdata = teststack, n.trees = 700)
scores(as.bag(finalPred))
#       ASE      MAPE 
# 6629.0749  145.1991 

finaldf <- data.frame(time = 1:nrow(teststack), teststack, boosted = finalPred, mean = (rowMeans(teststack %>% dplyr::select(-c(ppm, ARUMA)))))
finaldf %<>% rename(test = ppm)
finalplot <- finaldf %>% 
  gather_(gather_cols = names(.)[-1], key = "series", value = "ppm") %>%
  ggplot() + geom_line(aes(x = time, y = ppm, color = series)) 
finalplot + scale_color_ptol() 
finalresponses <- finaldf %>% dplyr::select(-test) %>%
  gather_(key = "series", value = "ppm", gather_cols = names(.)[-1])
ggplot() +
  geom_line(data = finalresponses, aes(x = time, y = ppm, color = series)) +
  geom_line(data = finaldf, aes(x = time, y = test), color = "black") + scale_color_hc(palette = "darkunica") + theme_hc()

save(finalModel, valdf, valdf2, teststack, file = "ensemble.Rda")


# calculate pint
set.seed(NULL)
getErrors <- function(){
  model <- gbm(ppm~., data = valdf2, n.trees = 200, interaction.depth = 1)
  pred <- predict(model, newdata = teststack, n.trees = 200)
  sqrt(mean((teststack$ppm - pred)^2))
}
errors <- numeric(1000)
for (i in seq_along(errors)){
  errors[i]  <- getErrors()
}
meanErr <- mean(errors)
stdErr <- sd(errors)
pm <- meanErr + stdErr*2
makeInterval <- function(x){
  upper <- x + pm
  lower <- x - pm
  return(data.frame(predicted = x, upper = upper, lower = lower))
}
save(pm, makeInterval, file = "intervals.Rda")
finalPred <- makeInterval(finalPred)
save(finalPred, file = "gbmfinalpred.Rda")

autoplot.keras

autoplot.ens <- function(obj) {
    testdf <- data.frame(type = "actual", t = seq_along(testM[, 1]), ppm = as.numeric(testM[,1]))
    preddf <- data.frame(type = "predicted", t = seq_along(testM[, 1]), ppm = as.numeric(obj$predicted))
    confdf <- data.frame(t = seq_along(testM[, 1]), upper = obj$upper, lower = obj$lower)
    dfl <- list(testdf, preddf)
    .testPredPlot(dfl) + geom_line(data = confdf, aes(x = t, y = lower, alpha = 0.2), 
        linetype = 3003) + geom_line(data = confdf, aes(x = t, y = upper, alpha = 0.2), 
        linetype = 3003) + guides(alpha = FALSE)
}
scores.ens <- function(obj) {
    c(ase = ASE(obj$predicted, testM[, 1]), mape = MAPE(obj$predicted, testM[, 1]), 
        Conf.Score = confScore(upper = obj$upper, lower = obj$lower, testM[,1]))
}
as.ens <- function(x) structure(x, class = "ens")

autoplot(as.ens(finalPred))
scores(as.ens(finalPred))
#        ase       mape Conf.Score 
#  6629.0749   145.1991    10.0000 

load("ensemble.Rda")
library(boot)
library(gbm)

bootfun <- function(data, indices) {
  data <- data[indices,]
  tr <- gbm(ppm~., data = data, n.trees = 700, interaction.depth = 2,distribution = "gaussian")
  predict(tr, newdata = teststack, n.trees = 700)
}

b <- boot(data = valdf2, statistic = bootfun, R = 10000, parallel = "multicore")
lims <- t(apply(b$t, 2, FUN = function(x) quantile(x, c(0.05, 0.975))))
pm <- (lims[,2]-lims[,1])[order(lims[,2]-lims[,1])]/2

save(pm, file = "pint.Rda")

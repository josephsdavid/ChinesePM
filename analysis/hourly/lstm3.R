library(keras)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(tswgewrapped)
source("mvarsetup.R")
Ktrain <- data.matrix(trainM)
Ktest <- data.matrix(testM)
dat <- rbind(Ktrain,Ktest)
str(dat)
nrow(dat)
x <- nrow(dat)
str(dat)
dat <- dat[-1,]
mn <- apply(dat,2,mean)
std <- apply(dat,2,sd)
maxt <- floor( nrow(dat)*4/5 )
maxv <- floor(nrow(dat))
val <- dat[(maxv-72*2):( maxv - 72),]
val <- scale(val, center = apply(val,2,mean), scale = apply(val,2,sd))
valScale <- attr(val, 'scaled:scale')[1]
# PM_US.Post 
#   1907.681 
valCent <- attr(val,'scaled:center')[1]
# PM_US.Post 
#    2333.48 
val <- array(val, c(nrow(val),10,6))
dat <- scale(dat, center = mn, scale = std)
test2 <- scale(Ktest, center = apply(Ktest,2,mean), scale = apply(Ktest,2,sd))
test3  <- array(test2, c(nrow(test2),10,6))


generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }

    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
                      
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }           
    list(samples, targets)
  }
}

lookback  <- 60*24
step <- 6*24
delay  <- 1*24
batch_size <- 150*24
maxt <- floor( nrow(dat)*4/5 )
# [1] 1460
maxv <- floor(nrow(dat))
# [1] 1826
maxt-maxv
# [1] -366



train_gen <- generator(
  dat,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = maxt,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  dat,
  lookback = lookback,
  delay = delay,
  min_index = maxt+1,
  max_index = maxv,
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- ((maxv - maxt+1 - lookback) / batch_size)


model_lstm <- keras_model_sequential() %>%
  layer_lstm(units = 10, dropout = 0.1, recurrent_dropout = 0.1, 
             activation = "sigmoid",
             input_shape = list(NULL, dim(dat)[[-1]])
                             ,return_sequences = TRUE
              ) %>%
bidirectional(layer_lstm(units = 40,  dropout = 0.1, recurrent_dropout = 0.1,
            activation = "sigmoid", return_sequences = TRUE)) %>%
bidirectional(layer_lstm(units = 80,  dropout = 0.1, recurrent_dropout = 0.1,
            activation = "sigmoid")) %>%
#  layer_lstm(units = 32) %>%
  layer_dense(units = 1)

model_lstm %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.001),
  loss = "mse"
)

val_steps <- ((maxv - maxt+1 - lookback) / batch_size)
getOption("browser")
history <- model_lstm %>% fit_generator(
  train_gen,
  steps_per_epoch = 40,
  epochs = 40,
  validation_data = val_gen,
  validation_steps = val_steps
)
beepr::beep(0)
Lhist <- history
save(Lhist, file = "lstmhist.Rda")
tail(history$metrics$l,1)
summary(model_lstm)
plot(history)
load("LSTM.Rda")

scores.keras <- function(obj){
   c(ASE(as.numeric(obj), testM[,1]), MAPE(as.numeric(obj), testM[,1]))
 }
autoplot.keras <- function(obj){
  testdf <- data.frame(type = "actual", 
                       t = seq_along(testM[,1]), 
                       ppm = as.numeric(testM[,1]))
  preddf <- data.frame(type = "predicted", 
                       t = seq_along(obj), 
                       ppm = as.numeric( obj))
  dfl <- list(preddf,testdf)
  .testPredPlot(dfl)
 }
preds <- predict(model_lstm, test3, n.ahead = nrow(test2))
preds <- preds*attr(test2, 'scaled:scale')[1] + attr(test2, 'scaled:center')[1]
preds <- as.keras(preds)
autoplot(preds)
scores(preds)
# [1] 25945.9677   461.0343
# [1] 15560.929   344.284
# [1] 19945.5919   375.2665
# [1] 2620311.331     112.932
# [1] 2264006.7650     108.4946
# [1] 2447509.8712      93.5238
# [1] 2349747.4759     119.7062
# [1] 2634292.3251     109.8894
# [1] 2323102.16798      76.38773
options(scipen = 999)
evaluate(object = model_lstm, steps = 40, val)
?evaluate
evaluate_generator(model_lstm, val_gen, 40)

# [1] 2.323102e+06 7.638773e+01
# [1] 2622627.1637     106.8089
# [1] 2622627.1637     106.8089
# [1] 2847394.0867     110.9623
# [1] 2847394.0867     110.9623
# [1] 2875346.5665     129.8638
# [1] 2482367
# [1] 3719713
# [1] 3568660
# [1] 3911693
# [1] 7087635
Lpreds <- preds
Lhist <- history
# [1] 6096521
# [1] 6447781
# [1] 8029918
# [1] 8344244
# [1] 5182820
# [1] 53420.1
# [1] 6775773
# [1] 6476729
# [1] 5314482
# [1] 4824247
# [1] 7693198
# [1] 6011114
# [1] 4748499
# [1] 4459849
# [1] 5369241
# [1] 4627842
# [1] 4119371
# [1] 4415863
# [1] 4123450
# [1] 4459115
# [1] 4163845
# [1] 5555495
# [1] 4499888
# [1] 5258252
# [1] 4202779
# [1] 4103326
# [1] 6583929
# [1] 4273433
# [1] 4301346
# [1] 4731800
# [1] 4963140
# [1] 4513471
# [1] 4335735
# [1] 3798197
# [1] 3468027
# [1] 3958313
# [1] 3680186
# [1] 4449020
# [1] 3791790
# [1] 3629293
# [1] 3637191
# [1] 3481038
# [1] 3282870
# [1] 4214927
# [1] 5367963
# [1] 5227413
# [1] 5215032
# [1] 5320862
# [1] 5551002
# [1] 11187367
# [1] 6407172
# [1] 1000.101
# [1] 11065454
# [1] 13792633
load('trainErr.Rda')
errorMean <- mean(trainErr)
errorStd <- sd(trainErr)
makeInterval <- function(prediction){
  pm <- errorMean+errorStd
  upper <- prediction + pm
  lower  <- prediction - pm
  return(data.frame(fitted = prediction, upper = upper, lower = lower))
}


lstmTest <- predict(model_lstm, test3, n.ahead = nrow(test2))
lstmTest <- makeInterval(lstmTest)
descaleTest <- function(x){
  x*attr(test2, 'scaled:scale')[1] + attr(test2, 'scaled:center')[1]
}
lstmTest <- lapply(lstmTest, descaleTest)
lstmTest <- as.keras(lstmTest)

lstmVal <- predict(model_lstm, val, n.ahead = nrow(test2))
lstmVal <- makeInterval(lstmVal)
descaleVal <- function(x){
  x*valScale + valCent 
}
lstmVal <- lapply(lstmVal, descaleVal)
lstmVal <- as.keras(lstmVal)


lstmTest <- predict(model_lstm, test3, n.ahead = nrow(test2))
lstmTest2 <- predict(model_lstm, test3, n.ahead = nrow(test2))
lstmTest <- lstmTest*attr(test2, 'scaled:scale')[1] + attr(test2, 'scaled:center')[1]
lstmTest <- as.keras(lstmTest)
autoplot.keras <- function(obj){
  testdf <- data.frame(type = "actual", 
                       t = seq_along(testM[,1]), 
                       ppm = as.numeric(testM[,1]))
  preddf <- data.frame(type = "predicted", 
                       t = seq_along(obj), 
                       ppm = as.numeric( obj))
  dfl <- list(preddf,testdf)
  .testPredPlot(dfl)
 }

autoplot(lstmTest)
scores(as.keras(lstmTest))
autoplot(lstmTest)
ASE(as.numeric(lstmTest),testM$PM_US.Post)
str(lstmTest)
save(lstmTest, lstmVal,file = "LSTM.Rda")

scores(lstmTest)
# [1] 2264006.7650     108.4946
# [1] 2622627.1637     106.8089
load("lstmHist.Rda")
lstmHist
lstmVal <- predict(model_lstm, val, n.ahead = nrow(test2))
lstmVal <- lstmVal*valScale + valCent
lstmVal <- as.keras(lstmVal)
autoplot(lstmVal)
scores(lstmVal)
save(lstmTest, lstmVal,file = "LSTM.Rda")
model_lstm <- load_model_hdf5( "lstm.h5")
maxt - lookback
# [1] 26610/
26610/batch_size
# [1] 7.391667
getMeanError <- function(mod) {
    # 28 is simply the integer number of times we need to sample from our
    # training set to represent the whole thing.  In this case it will run
    # through the entire set 4 times
    sqerror <- evaluate_generator(mod, train_gen, 28)
    return(sqrt(sqerror))
}
GetErrors <- function(n) {
    errors <- double(n)
    for (i in seq_along(errors)) {
        cat("evaluation number", i, "\n")
        errors[i] <- getMeanError(model_lstm)
    }
    return(errors)
}
trainErr <- GetErrors(1000)
save(trainErr, file = "trainErr.Rda")
hist(trainErr)
errorMean <- mean((trainErr))
errorStd <- sd((trainErr))
descale <- function(x) {
    x * attr(test2, "scaled:scale")[1] + attr(test2, "scaled:center")[1]
}
descale(2)
pm <- descale(errorMean)+descale(errorStd)
lstmTest +pm 
makeInterval <- function(prediction) {
    pm <- descale(errorMean) + descale(errorStd)
    upper <- unlist(prediction) + pm
    lower <- unlist(prediction) - pm
    return(data.frame(fitted = unlist(prediction), upper = (upper), lower = (lower)))
}
load("LSTM.Rda")
lstmTest <- makeInterval(lstmTest)
unlist(lstmTest)
lstmVal <- unlist(lstmVal)
save(lstmVal, lstmTest, file = "lstmInt.Rda")
as.keras <- function(x) structure(x, class = "keras")
autoplot.keras <- function(obj) {
    testdf <- data.frame(type = "actual", t = seq_along(testM[, 1]), ppm = as.numeric(testM[,1]))
    preddf <- data.frame(type = "predicted", t = seq_along(testM[, 1]), ppm = as.numeric(obj$fitted))
    confdf <- data.frame(t = seq_along(testM[, 1]), upper = obj$upper, lower = obj$lower)
    dfl <- list(testdf, preddf)
    .testPredPlot(dfl) + geom_line(data = confdf, aes(x = t, y = lower, alpha = 0.2), 
        linetype = 3003) + geom_line(data = confdf, aes(x = t, y = upper, alpha = 0.2), 
        linetype = 3003) + guides(alpha = FALSE)
}
scores.keras <- function(obj) {
    c(ase = ASE(obj$fitted, testM[, 1]), mape = MAPE(obj$fitted, testM[, 1]), 
        Conf.Score = confScore(upper = obj$upper, lower = obj$lower, testM[,1]))
}

# Model
# Model: "sequential_7"
# ____________________________________________________________________________________
# Layer (type)                         Output Shape                      Param #      
# ====================================================================================
# lstm_21 (LSTM)                       (None, None, 10)                  680          
# ____________________________________________________________________________________
# bidirectional_14 (Bidirectional)     (None, None, 80)                  16320        
# ____________________________________________________________________________________
# bidirectional_15 (Bidirectional)     (None, 160)                       103040       
# ____________________________________________________________________________________
# dense_7 (Dense)                      (None, 1)                         161          
# ====================================================================================
# Total params: 120,201
# Trainable params: 120,201
# Non-trainable params: 0
# ____________________________________________________________________________________
# 
# 
kplot <- autoplot(as.keras(lstmTest))
kscores <- scores(as.keras(lstmTest))
kplot
kscores[1]^(1/2) %>% unname
save_model_hdf5(model_lstm, "lstm.h5")
str(test3)


# CNN TimeA



train_gen <- generator(
  dat,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = maxt,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  dat,
  lookback = lookback,
  delay = delay,
  min_index = maxt+1,
  max_index = maxv,
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- ((maxv - maxt+1 - lookback) / batch_size)

model_cnn <- keras_model_sequential() %>%
  layer_conv_1d(filters = 32, kernel_size = 2, activation = 'sigmoid', input_shape = list(NULL, dim(dat)[[-1]]), padding = 'causal') %>% 
  layer_max_pooling_1d(pool_size = 2) %>%
   bidirectional(layer_lstm(units = 20, dropout = 0.1, recurrent_dropout = 0.1, return_sequences = TRUE)) %>% 
   bidirectional(layer_lstm(units = 40, dropout = 0.1, recurrent_dropout = 0.1, return_sequences = TRUE)) %>% 
   bidirectional(layer_lstm(units = 80, dropout = 0.1, recurrent_dropout = 0.1)) %>% 
  layer_dense(units = 1)


model_cnn %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.01),
  loss = "mse"
)
history <- model_cnn %>% fit_generator(
  train_gen,
  steps_per_epoch = 24,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)
plot(history)

preds <- predict(model_cnn, test3, n.ahead = nrow(test2))
preds <- preds*attr(test2, 'scaled:scale')[1] + attr(test2, 'scaled:center')[1]
preds <- as.keras(preds)
autoplot(preds)
scores(preds)
# [1] 41049.9690   452.9713
# [1] 40.15.4906   555.3783

library(keras)
library(ggplot2)
library(ggthemes)
library(ggplot2)
library(dplyr)
library(tswgewrapped)
source("../../R/preprocessing.R")
source("../../R/helpers.R")


china <- preprocess("../../data/")
bj <- (china$BeijingPM_ )
bj2 <- dlist(bj)
dfsplit(bj2)

train[1:3]  <- NULL
train[9:14] <- NULL
test[1:3]  <- NULL
test[9:14] <- NULL
traints <- (purrr::keep(train,is.ts))
trainexogen <- purrr::discard(train, is.ts)
trainexogen <- lapply(trainexogen, as.factor) %>>% as.data.frame
test %>% dplyr::select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> test
traints %>% dplyr::select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> train
test <- mlag_dfr(test, names(test), 1:50)
train <- mlag_dfr(train, names(train), 1:50)
test <- impute(test) %>% as.data.frame
train <- impute(train) %>% as.data.frame
train <- data.matrix(train)
test <- data.matrix(test)
nrow(train)/nrow(test)
dat <- rbind(train,test)
str(dat)
nrow(dat)
x <- nrow(dat)
str(dat)
dat <- dat[-1,]
mn <- apply(dat,2,mean)
std <- apply(dat,2,sd)
dat <- scale(dat, center = mn, scale = std)
test2 <- scale(test, center = apply(test,2,mean), scale = apply(test,2,sd))
test3  <- array(test2, c(nrow(test2),2,255))


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

lookback  <- 16*3
step <- 8*3
delay  <- 4*3
batch_size <- 300
maxt <- floor( nrow(dat)*4/5 )
maxv <- floor(nrow(dat))

val <- dat[(maxt+1):maxv,]
val <- array(val, c(nrow(val),2,255))


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
val_steps <- (maxv - maxt+1 - lookback) / batch_size
test_steps <- (nrow(dat) - maxv+1 - lookback) / batch_size

model_lstm <- keras_model_sequential() %>%
  layer_lstm(units = 10, dropout = 0.4, recurrent_dropout = 0.4, 
             activation = "sigmoid",
             input_shape = list(NULL, dim(dat)[[-1]])
                             ,return_sequences = TRUE
              ) %>%
bidirectional(layer_lstm(units = 20,  dropout = 0.4, recurrent_dropout = 0.4,
            activation = "sigmoid", return_sequences = TRUE)) %>%
bidirectional(layer_lstm(units = 40,  dropout = 0.4, recurrent_dropout = 0.4,
            activation = "sigmoid")) %>%
#  layer_lstm(units = 32) %>%
  layer_dense(units = 1)
model_lstm %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mse",
  metrics = c("mae")
)
history <- model_lstm %>% fit_generator(
  train_gen,
  steps_per_epoch = 40,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)
beepr::beep(0)
tail(history$metrics$l,1)
summary(model_lstm)
# Model: "sequential_13"
# ____________________________________________________________________________________
# Layer (type)                         Output Shape                      Param #      
# ====================================================================================
# lstm_39 (LSTM)                       (None, None, 10)                  10640        
# ____________________________________________________________________________________
# bidirectional_26 (Bidirectional)     (None, None, 40)                  4960         
# ____________________________________________________________________________________
# bidirectional_27 (Bidirectional)     (None, 80)                        25920        
# ____________________________________________________________________________________
# dense_13 (Dense)                     (None, 1)                         81           
# ====================================================================================
# Total params: 41,601
# Trainable params: 41,601
# Non-trainable params: 0
# ____________________________________________________________________________________
# NULL
# [1] 0.729292
# [1] 0.7533519
# [1] 0.6793835
# [1] 0.6712329
# [1] 0.5938697
# [1] 0.6511578
# [1] 0.6154889
# [1] 1.028304
# [1] 0.7092065 0.6140919 0.7543543 1.2878188
# [1] 0.7092065 0.6140919 0.7543543
plot(history)

preds <- predict(model_lstm, test3, n.ahead = nrow(test2))
preds <- preds*attr(test2, 'scaled:scale')[1] + attr(test2, 'scaled:center')[1]
preds <- as.keras(preds)
autoplot(preds)
scores(preds)
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
# [1] 5342044
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
# [1] 10008401
# [1] 11065454
# [1] 13792633

lstmTest <- predict(model_lstm, test3, n.ahead = nrow(test2))
lstmTest <- lstmTest*attr(test2, 'scaled:scale')[1] + attr(test2, 'scaled:center')[1]
lstmTest <- as.keras(lstmTest)
scores(lstmTest)
length(lstmTest)

lstmVal <- predict(model_lstm, val, n.ahead = nrow(test2))
lstmVal <- lstmTest*attr(test2, 'scaled:scale')[1] + attr(test2, 'scaled:center')[1]
lstmVal <- as.keras(lstmTest)
scores(lstmVal)
lstmHist
lstmHist <- history
save(lstmTest, lstmVal, lstmHist,file = "LSTM.Rda")
save_model_hdf5(model_lstm, 'lstm.h5')
load_model_hdf5("lstm.h5")

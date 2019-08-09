library(abind)
library(keras)
library(ggplot2)
library(ggthemes)
library(ggplot2)
library(dplyr)
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
test %>% select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> test
traints %>% select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> train

train <- data.matrix(train)
test <- data.matrix(test)
nrow(train)/nrow(test)
dat <- rbind(train,test)
str(dat)
nrow(dat)
x <- nrow(dat)
str(dat)
dat <- dat[-nrow(dat),]
mn <- apply(dat,2,mean)
std <- apply(dat,2,sd)
dat <- scale(dat, center = mn, scale = std)
test2 <- scale(test, center = apply(test,2,mean), scale = apply(test,2,sd))
test3  <- array(test2, c(nrow(test2),2,5))
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

lookback  <- 16
step <- 8
delay  <- 4
batch_size <- 30
maxt <- floor( nrow(dat)*3/5 )
maxv <- floor(nrow(dat)*4/5)

val <- dat[(maxt+1):maxv,]
val <- array(val, c(nrow(val),2,5))



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

evaluate_naive_method <- function() {
  batch_maes <- c()
  for (step in 1:val_steps) {
    c(samples, targets) %<-% val_gen()
    preds <- samples[,dim(samples)[[2]],2]
    mae <- mean(abs(preds - targets))
    batch_maes <- c(batch_maes, mae)
  }
  print(mean(batch_maes))
}

evaluate_naive_method()


model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(lookback / step, dim(dat)[-1])) %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 1)
model
# Model
# Model: "sequential_1"
# ______________________________________
# Layer (type)     Output Shape   Param 
# ======================================
# flatten_1 (Flatt (None, 2)      0     
# ______________________________________
# dense_2 (Dense)  (None, 32)     96    
# ______________________________________
# dense_3 (Dense)  (None, 1)      33    
# ======================================
# Total params: 129
# Trainable params: 129
# Non-trainable params: 0
# ______________________________________
# 
# 

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mse"
)

history <- model %>% fit_generator(
  train_gen[[1]],
  steps_per_epoch = 50,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)
save(model, history, file = "simpleML.Rda")
preds <- model %>% predict(dat)
model_gru <- keras_model_sequential() %>%
  layer_gru(units = 32, input_shape = list(NULL, dim(dat)[[-1]]))%>%
  layer_dense(units = 1)

model_gru %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.4),
  loss = "mse"
)

history <- model_gru %>% fit_generator(
  train_gen,
  steps_per_epoch = 60,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)
plot(history)
gruHist <- history
save(gruHist, model_gru, gruPred, file = "GRU.Rda")

gruTest <- predict(model_gru, test3, n.ahead = nrow(test2))
gruTest <- gruTest*attr(test2, 'scaled:scale')[1] + attr(test2, 'scaled:center')[1]
gruTest <- as.keras(gruTest)


gruVal <- predict(model_gru, val, n.ahead = nrow(test2))
gruVal <- gruTest*attr(test2, 'scaled:scale')[1] + attr(test2, 'scaled:center')[1]
gruVal <- as.keras(gruTest)

save(gruTest, gruVal, gruHist,file = "GRU.Rda")
descale <- function(x)
gruPred <- as.lstm(gruPred)
autoplot(as.lstm(gruPred))
scores(as.lstm(gruPred))
# [1] 2592578 lr = 0.4, pretty dang good
# [1] 4607975 lr = 0.3
# [1] 7108423 lr = 0.03
# [1] 25784174 lr = 0.9
# [1] 3912819 lr = 0.5
